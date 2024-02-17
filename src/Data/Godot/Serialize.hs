{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TupleSections  #-}

module Data.Godot.Serialize where

import Data.ByteString (ByteString, pack, unpack, singleton)
import Data.String(fromString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32, Word16)
import Data.Store(encode, decode, PeekException, Store)
import Data.Int (Int32, Int64)
import Control.Arrow (Arrow(first), ArrowChoice (left))
import Control.Category ((>>>))
import Data.Char (chr, ord)
import Control.Applicative ((<|>), Alternative (many))
import Data.Default (Default(..))
import Control.Monad (replicateM, void)
import Data.Functor (($>))
import Data.Attoparsec.ByteString
    ( Parser, word8, anyWord8, parseOnly )
import GHC.Generics
    ( Generic(..),
      U1(..),
      K1(K1),
      M1(M1),
      type (:+:)(..),
      type (:*:)(..) )
import Data.Map(Map)
import qualified Data.Map as M
import Data.Proxy (Proxy(Proxy))
import Data.Foldable (traverse_)

data DesErr = DesErrWrongPrefix String -- ^ Error if first 4 bytes, that encode godot type, are incorrect
            | DesErrWrongValues String -- ^ Error if prefix is correct, but rest of the message is incorrect
            | DesErrStore String       -- ^ Error returned during deserialization with 'Data.Store'
            | DesErrParser String
            | DesErrDecoder PeekException
            deriving(Show, Eq)

bytes :: [Word8] -> Parser [Word8]
bytes = traverse word8

prefix :: Word8 -> Parser [Word8]
prefix a = bytes [a,0,0,0]

singleByte = anyWord8

byte2Block :: Parser ByteString
byte2Block = BS.pack <$> replicateM 2 anyWord8

byte4Block :: Parser ByteString
byte4Block = BS.pack <$> replicateM 4 anyWord8

byte8Block :: Parser ByteString
byte8Block = BS.pack <$> replicateM 8 anyWord8

-- | Parser that uses Store's decode function and channels decoding error accordingly
decode8P :: forall m a. (Store a) => Parser a
decode8P = byte8Block  >>= (decode >>> either (fail . show) pure)

-- | 4 byte version of decodeP
decode4P :: forall m a. (Store a) => Parser a
decode4P = byte4Block  >>= (decode >>> either (fail . show) pure)

class Serializable a where
  -- | Serialization function
  ser :: a -> ByteString
  default ser :: (Generic a, SR (Rep a)) => a -> ByteString
  ser = genericSer
  -- | Deserialization function, default implementation is parsing with a desP parser
  des :: ByteString -> Either DesErr a
  des = parseOnly (desP @a) >>> left DesErrParser
  -- | Deserialization parser
  desP :: Parser a
  default desP :: (Generic a, DS (Rep a)) => Parser a
  desP = genericDesP

instance Serializable () where
  ser () = pack [0,0,0,0]
  desP = void $ prefix 0


instance Serializable Bool where
  ser False = pack [1,0,0,0,0,0,0,0]
  ser True  = pack [1,0,0,0,1,0,0,0]
  desP = prefix 1
      *> (    bytes [0,0,0,0] $> False
          <|> bytes [1,0,0,0] $> True
         )

-- | Int32 clean. Int32 with serialization instance without prefixes (used for length etc.)
newtype Int32Cl = Int32Cl { i32 :: Int32 } deriving (Eq, Ord)

instance Serializable Int32Cl where
  ser = encode . i32
  desP = Int32Cl <$> decode4P


instance Serializable Int32 where
  ser n = pack [2,0,0,0] <> encode n
  desP = prefix 2 *> decode4P

-- | Check if Int64 is inside subset of Int32 numbers
isInt32 :: Int64 -> Bool
isInt32 = (==) <$> fromIntegral . fromIntegral @Int64 @Int32 <*> id

instance Serializable Int64 where
  ser n = if isInt32 n
             then pack [2,0,0,0] <> encode (fromIntegral n :: Int32) -- serialize in 4-byte chunks
             else pack [2,0,1,0] <> encode n                         -- serialize in 8-byte chunks

  desP = bytes [2,0,0,0] *> (fromIntegral <$> decode4P @_ @Int32)
     <|> bytes [2,0,1,0] *> decode8P

-- | Check if Double (float 64) is inside the subset of Float (float 32) numbers
isFloat32 :: Double -> Bool
isFloat32 = (==) <$> realToFrac . realToFrac @Double @Float <*> id

instance Serializable Int where
  ser w16 = encode w16 <> BS.pack [0,0]
  desP = fromIntegral <$> desP @Word32

instance Serializable Word16 where
  ser w16 = encode w16 <> BS.pack [0,0]
  desP = fromIntegral <$> desP @Word32

instance Serializable Word32 where
  ser = encode
  desP = decode4P

instance Serializable Double where
  ser x = if isFloat32 x
             then pack [3,0,0,0] <> encode (realToFrac x :: Float) -- serialize in 4-byte chunks
             else pack [3,0,1,0] <> encode x                       -- serialize in 8-byte chunks

  desP = bytes [3,0,0,0] *> (realToFrac <$> decode4P @_ @Float)
     <|> bytes [3,0,1,0] *> decode8P

instance (Serializable a, Serializable b) => Serializable (a, b)
instance (Serializable a, Serializable b, Serializable c) => Serializable (a, b, c)
instance (Serializable a, Serializable b, Serializable c, Serializable d) => Serializable (a, b, c, d)


digitsToFull4 :: Integral a => a -> a
digitsToFull4 n = (4 - n `rem` 4) `rem` 4

-- | Pad bytestring with zeroes so its length is divisible by 4
padTo4 :: ByteString -> ByteString
padTo4 bs = bs <> BS.replicate (digitsToFull4 $ BS.length bs) 0

instance {-# OVERLAPS #-} Serializable String where
  ser s = pack [4,0,0,0] <> lenSer s  <> padTo4 (BS.pack $ BS.head . encode <$> s)
  desP = do
    prefix 4
    -- Parse length of the array
    (n :: Int32) <- decode4P
    res <- nTimesP charP n
    nTimesP (word8 0) (digitsToFull4 n)
    pure res
    where
      charP = toEnum . fromEnum <$> singleByte

-- List serialization

-- | Serialized length
lenSer :: Foldable f => f a -> ByteString
lenSer = encode . fromIntegral @Int @Int32 . length

-- | List of elements serialized with its length
listSer :: Serializable a => [a] -> ByteString
listSer xs = lenSer xs <> foldMap ser xs

-- | Parse a list of elements with its length encoded in the beginning
listDesP :: Parser a -> Parser [a]
listDesP p = do
  -- Parse length of the array
  decode4P @Int32 >>= nTimesP p

-- | Serializable instance for List
instance {-# OVERLAPPABLE #-} Serializable a => Serializable [a] where
  ser = (pack [28,0,0,0] <>) . listSer
  desP = prefix 28 *> listDesP desP

-- | Parse something n times
nTimesP :: Parser a -> Int32 -> Parser [a]
nTimesP _ 0 = pure []
nTimesP p n = (:) <$> p <*> nTimesP p (pred n)

-- Map serialization

-- | Serializable instance for List
instance (Serializable a, Serializable b, Ord a) => Serializable (Map a b) where
  ser xs = pack [18,0,0,0] <> lenSer xs <> foldMap ser (M.toList xs)
  desP = do
    prefix 18
    M.fromList <$> listDesP desP

-- | Desialize 'Generic' value (sum types are 2-elem. lists [<constructor index>, <value>])
-- Maybe (Int32,Int32) is used after deserializing constructor index, that number is used to select the appropriate parser for next step
-- based on the index and total number of constructors
class DS g                                        where dsGP :: Maybe (Int32, Int32) -> Parser (g a)
instance DS g => DS (M1 x y g)                    where dsGP n = M1 <$> dsGP n
instance (DS g, DS h, IX g, IX h) => DS (g :+: h) where dsGP Nothing = dsGP . Just . (,size (Proxy @(g :+: h))) . i32 =<< desP @Int32Cl
                                                        dsGP (Just (i,n)) = let k = n `div` 2
                                                                              in if i < n `div` 2
                                                                                then L1 <$> dsGP (Just (i, k))
                                                                                else R1 <$> dsGP (Just (i-k,n-k))
instance (DS g, DS h) => DS (g :*: h)             where dsGP n = (:*:) <$> dsGP n <*> dsGP n
instance (Serializable a) => DS (K1 x a)          where dsGP _ = K1 <$> desP
instance DS U1                                    where dsGP _ = pure U1

genericDesP :: (Generic a, DS (Rep a)) => Parser a
genericDesP = to <$> dsGP Nothing

-- | Serialize 'Generic' value (sum types are 2-elem. lists [<constructor index>, <value>])
class SR g                                                      where srG :: g a -> ByteString
instance SR g => SR (M1 x y g)                                  where srG (M1 v) = srG v
instance (SR g, SR h, IX g, IX h, SRV g, SRV h) => SR (g :+: h) where srG v = ser (Int32Cl $ ix' v) <> srvG v
instance (SR g, SR h) => SR (g :*: h)                           where srG (x :*: y) = srG x <> srG y
instance (Serializable a) => SR (K1 x a)                        where srG (K1 v) = ser v
instance SR U1                                                  where srG U1 = ""

genericSer :: (Generic a, SR (Rep a)) => a -> ByteString
genericSer = srG . from

-- | Serialize inner values (skip constructors)
class SRV g                                          where srvG :: g a -> ByteString
instance (SRV g, SRV h, IX g, IX h) => SRV (g :+: h) where srvG = \case L1 x -> srvG x; R1 y -> srvG y
instance (SRV g, SRV h) => SRV (g :*: h)             where srvG (x :*: y) = srvG x <> srvG y
instance SRV g => SRV (M1 x y g)                     where srvG (M1 v) = srvG v
instance (Serializable a) => SRV (K1 x a)            where srvG (K1 v) = ser v
instance SRV U1                                      where srvG U1 = ""

-- | Number of constructors
class Sized (f :: * -> *)                      where size :: Proxy f -> Int32
instance (Sized f, Sized g) => Sized (f :+: g) where size _ = size (Proxy @f) + size (Proxy @g)
instance Sized (f :*: g)                       where size _ = 1
instance Sized (K1 i c)                        where size _ = 1
instance (Sized f) => Sized (M1 i t f)         where size _ = size (Proxy @f)
instance Sized U1                              where size _ = 1

-- | Index of a constructor
class (Sized f) => IX (f :: * -> *)   where ix' :: f p -> Int32
instance (IX f, IX g) => IX (f :+: g) where ix' = \case (L1 x) -> ix' x; (R1 x) -> size (Proxy @f) + ix' x
instance IX (f :*: g)                 where ix' _ = 0
instance IX (K1 i c)                  where ix' _ = 0
instance (IX f) => IX (M1 i t f)      where ix' (M1 x) = ix' x
instance IX U1                        where ix' _ = 0
