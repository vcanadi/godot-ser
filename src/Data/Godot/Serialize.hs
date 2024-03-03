{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Godot.Serialize where

import Data.ByteString (ByteString, pack, unpack, singleton)
import Data.String(fromString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32, Word16, Word64)
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
    ( Parser, word8, anyWord8, parseOnly)

import qualified Data.Attoparsec.ByteString as Att
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
import Linear (V2)

data DesErr = DesErrWrongPrefix String -- ^ Error if first 4 bytes, that encode godot type, are incorrect
            | DesErrWrongValues String -- ^ Error if prefix is correct, but rest of the message is incorrect
            | DesErrStore String       -- ^ Error returned during deserialization with 'Data.Store'
            | DesErrParser String
            | DesErrDecoder PeekException
            deriving(Show, Eq)

bytes :: [Word8] -> ByteString
bytes = pack

prefix :: Word8 -> ByteString
prefix n =  bytes [n,0,0,0]

bytesP :: [Word8] -> Parser [Word8]
bytesP = traverse word8

prefixP :: Word8 -> Parser [Word8]
prefixP a = bytesP [a,0,0,0]

singleByte :: Parser Word8
singleByte = anyWord8

any4BytesP :: Parser ByteString
any4BytesP = BS.pack <$> replicateM 4 anyWord8

any8BytesP :: Parser ByteString
any8BytesP = BS.pack <$> replicateM 8 anyWord8

-- | Parser that uses Store's decode function and channels decoding error accordingly
decode8P :: forall m a. (Store a) => Parser a
decode8P = any8BytesP  >>= (decode >>> either (fail . show) pure)

-- | 4 byte version of decodeP
decode4P :: forall m a. (Store a) => Parser a
decode4P = any4BytesP  >>= (decode >>> either (fail . show) pure)

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

instance Serializable ByteString where
  ser = id
  desP = Att.takeWhile $ const True

instance Serializable () where
  ser () = bytes [0,0,0,0]
  desP = void $ prefixP 0


instance Serializable Bool where
  ser False = bytes [1,0,0,0,0,0,0,0]
  ser True  = bytes [1,0,0,0,1,0,0,0]
  desP = prefixP 1
      *> (    bytesP [0,0,0,0] $> False
          <|> bytesP [1,0,0,0] $> True
         )

-- | Int32 clean. Int32 with serialization instance without prefixes (used for length etc.)
newtype Int32Cl = Int32Cl { i32 :: Int32 } deriving (Eq, Ord)

instance Serializable Int32Cl where
  ser = encode . i32
  desP = Int32Cl <$> decode4P

-- | Tuple with serialization instance without prefixes (used for pair ser/des in map as list)
newtype TupleCl a b = TupleCl { tpl :: (a,b) } deriving (Eq, Ord)

instance (Serializable a, Serializable b) => Serializable (TupleCl a b) where
  ser (TupleCl (x,y)) = ser x <> ser y
  desP = fmap TupleCl $ (,) <$> desP <*> desP

instance Serializable Int32 where
  ser n = bytes [2,0,0,0] <> encode n
  desP = prefixP 2 *> decode4P

-- | Check if Int64 is inside subset of Int32 numbers
isInt32 :: Int64 -> Bool
isInt32 = (==) <$> fromIntegral . fromIntegral @Int64 @Int32 <*> id

instance Serializable Int64 where
  ser n = if isInt32 n
             then bytes [2,0,0,0] <> encode (fromIntegral n :: Int32) -- serialize in 4-byte chunks
             else bytes [2,0,1,0] <> encode n                         -- serialize in 8-byte chunks

  desP = bytesP [2,0,0,0] *> (fromIntegral <$> decode4P @_ @Int32)
     <|> bytesP [2,0,1,0] *> decode8P

-- | Check if Double (float 64) is inside the subset of Float (float 32) numbers
isFloat32 :: Double -> Bool
isFloat32 = (==) <$> realToFrac . realToFrac @Double @Float <*> id

instance Serializable Int where
  ser = ser @Int64 . fromIntegral
  desP = fromIntegral <$> desP @Int64

instance Serializable Word8 where
  ser = ser @Int32 . fromIntegral
  desP = fromIntegral <$> desP @Int32

instance Serializable Word16 where
  ser = ser @Int32 . fromIntegral
  desP = fromIntegral <$> desP @Int32

instance Serializable Word32 where
  ser = ser @Int32 . fromIntegral
  desP = fromIntegral <$> desP @Int32

instance Serializable Word64 where
  ser = ser @Int64 . fromIntegral
  desP = fromIntegral <$> desP @Int64

instance Serializable Float where
  ser = ser @Double . realToFrac
  desP = realToFrac <$> desP @Double

instance Serializable Double where
  ser x = if isFloat32 x
             then bytes [3,0,0,0] <> encode (realToFrac x :: Float) -- serialize in 4-byte chunks
             else bytes [3,0,1,0] <> encode x                       -- serialize in 8-byte chunks
  desP = bytesP [3,0,0,0] *> (realToFrac <$> decode4P @_ @Float)
     <|> bytesP [3,0,1,0] *> decode8P

serBytes :: (Serializable a) => a -> [Word8]
serBytes = BS.unpack . ser

-- | Serialize 2-tuple as two element heterogeneous list (as would godot represent it)
instance (Serializable a, Serializable b) => Serializable (a, b) where
  ser (x,y) = prefix 28 <> ser (Int32Cl 2) <> ser x <> ser y
  desP = prefixP 28 *> bytesP [2,0,0,0] *> ((,) <$> desP <*> desP)

-- | Serialize 3-tuple as two element heterogeneous list (as would godot represent it)
instance (Serializable a, Serializable b, Serializable c) => Serializable (a, b, c) where
  ser (x,y,z) = prefix 28 <> ser (Int32Cl 3) <> ser x <> ser y <> ser z
  desP = prefixP 28 *> bytesP [3,0,0,0] *> ((,,) <$> desP <*> desP <*> desP)

-- | Serialize 4-tuple as two element heterogeneous list (as would godot represent it)
instance (Serializable a, Serializable b, Serializable c, Serializable d) => Serializable (a, b, c, d) where
  ser (x,y,z,w) = prefix 28 <> ser (Int32Cl 4) <> ser x <> ser y <> ser z <> ser w
  desP = prefixP 28 *> bytesP [4,0,0,0] *> ((,,,) <$> desP <*> desP <*> desP <*> desP)

digitsToFull4 :: Integral a => a -> a
digitsToFull4 n = (4 - n `rem` 4) `rem` 4

-- | Pad bytestring with zeroes so its length is divisible by 4
padTo4 :: ByteString -> ByteString
padTo4 bs = bs <> BS.replicate (digitsToFull4 $ BS.length bs) 0

instance {-# OVERLAPS #-} Serializable String where
  ser s = bytes [4,0,0,0] <> lenSer s  <> padTo4 (BS.pack $ BS.head . encode <$> s)
  desP = do
    prefixP 4
    -- Parse length of the array
    (n :: Int32) <- decode4P
    res <- nTimesP charP n
    nTimesP (word8 0) (digitsToFull4 n)
    pure res
    where
      charP = toEnum . fromEnum <$> singleByte

-- | Maybe serialization
instance (Serializable a) => Serializable (Maybe a)

-- | Vector serialization
instance (Serializable n) => Serializable (V2 n)

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

-- | Serializable instance for List (OVERLAPPABLE because of String == [Char])
instance {-# OVERLAPPABLE #-} Serializable a => Serializable [a] where
  ser = (prefix 28<>) . listSer
  desP = prefixP 28 *> listDesP desP

-- | Parse something n times
nTimesP :: Parser a -> Int32 -> Parser [a]
nTimesP _ 0 = pure []
nTimesP p n = (:) <$> p <*> nTimesP p (pred n)

-- Map serialization

-- | Serializable instance for dictionary (prefix 18, length and string/value pairs)
instance {-# OVERLAPS #-} Serializable a => Serializable (Map String a) where
  ser xs = bytes [18,0,0,0] <> lenSer xs <> foldMap (ser . TupleCl) (M.toList xs)
  desP = do
    prefixP 18
    M.fromList . fmap tpl <$> listDesP desP

-- | Serializable instance for any map (as godot's object with unlimited fields of type (a,b,), i.e. as list )
-- prefix 28 (list), length and sequence of lists (representing tuples)
instance {-# OVERLAPPABLE #-} (Serializable a, Serializable b, Ord a) => Serializable (Map a b) where
  ser xs = bytes [28,0,0,0] <> lenSer xs <> foldMap ser (M.toList xs)
  desP = do
    prefixP 28
    M.fromList <$> listDesP desP


-- | Desialize 'Generic' value (sum type serialization is prefixed with constructor index)
-- Maybe (Int32,Int32) is used after deserializing constructor index, that number is used to select the appropriate parser for next step
-- based on the index and total number of constructors
class DS f                               where dsGP :: Maybe (Int32, Int32) -> Parser (f a)
instance DS f => DS (M1 x y f)           where dsGP n = M1 <$> dsGP n
instance (DS f, DS g, IX f, IX g
         , MaxSizeH (f:+:g)
         ) => DS (f :+: g)               where dsGP Nothing = (if maxSizeH (Proxy @(f :+: g)) > 0 then  prefixP 28 *> any4BytesP else pure "")
                                                           *> (dsGP . Just . (,sizeV (Proxy @(f :+: g))) =<< desP)
                                               dsGP (Just (i,n)) = let k = n `div` 2
                                                                     in if i < n `div` 2
                                                                       then L1 <$> dsGP (Just (i, k))
                                                                       else R1 <$> dsGP (Just (i-k,n-k))
instance (DS f, DS g) => DS (f :*: g)    where dsGP Nothing = prefixP 28 *> desP @Int32Cl *> ((:*:) <$> dsGP (Just (0,0)) <*> dsGP (Just (0,0)))
                                               dsGP n       = (:*:) <$> dsGP n <*> dsGP n
instance (Serializable a) => DS (K1 x a) where dsGP _ = K1 <$> desP
instance DS U1                           where dsGP _ = pure U1

genericDesP :: (Generic a, DS (Rep a)) => Parser a
genericDesP = to <$> dsGP Nothing

-- | Serialize 'Generic' value <constructor index> <constructor payloads>
-- For sum type make an exception if all constructors are 0-arry
-- Serialize pure product (not sum of products) as object (17)
class SR f                               where srG :: f a -> ByteString
instance SR f => SR (M1 x y f)           where srG (M1 v) = srG v
instance (IX f, IX g, SRV (f :+: g)
         , SizeH (f:+:g)
         , MaxSizeH (f :+: g)
         ) => SR (f :+: g)               where srG v = (if maxSizeH (Proxy @(f :+: g)) > 0 then prefix 28 <> ser (Int32Cl $ sizeH v + 1) else "")
                                                    <> ser (ix' v) <> srvG v
instance (SRV (f :*: g), SizeH (f :*: g)
         ) => SR (f :*: g)               where srG v = prefix 28 <> ser (Int32Cl $ sizeH v ) <> srvG v
instance (Serializable a) => SR (K1 x a) where srG (K1 v) = ser v
instance SR U1                           where srG U1 = ""

genericSer :: (Generic a, SR (Rep a)) => a -> ByteString
genericSer = srG . from

-- | Serialize inner values (skip constructors)
class SRV f                               where srvG :: f a -> ByteString
instance (SRV f, SRV g, IX f, IX g)
         => SRV (f :+: g)                 where srvG = \case L1 x -> srvG x; R1 y -> srvG y
instance (SRV f, SRV g) => SRV (f :*: g)  where srvG (x :*: y) = srvG x <> srvG y
instance SRV f => SRV (M1 x y f)          where srvG (M1 v) = srvG v
instance (Serializable a) => SRV (K1 x a) where srvG (K1 v) = ser v
instance SRV U1                           where srvG U1 = ""

-- | "Vertical" size (number of constructors in a sum type)
class SizeV (f :: * -> *)              where sizeV :: Proxy f -> Int32
instance (SizeV f, SizeV g)
         => SizeV (f :+: g)            where sizeV _ = sizeV (Proxy @f) + sizeV (Proxy @g)
instance SizeV (f :*: g)               where sizeV _ = 1
instance SizeV (K1 i c)                where sizeV _ = 1
instance (SizeV f) => SizeV (M1 i t f) where sizeV _ = sizeV (Proxy @f)
instance SizeV U1                      where sizeV _ = 1

-- | Index of a constructor
class (SizeV f) => IX (f :: * -> *)   where ix' :: f p -> Int32
instance (IX f, IX g) => IX (f :+: g) where ix' = \case (L1 x) -> ix' x; (R1 x) -> sizeV (Proxy @f) + ix' x
instance IX (f :*: g)                 where ix' _ = 0
instance IX (K1 i c)                  where ix' _ = 0
instance (IX f) => IX (M1 i t f)      where ix' (M1 x) = ix' x
instance IX U1                        where ix' _ = 0

-- | "Horizontal" size (number of fields of specific product)
class SizeH (f :: * -> *)                      where sizeH :: f p -> Int32
instance (SizeH f, SizeH g) => SizeH (f :+: g) where sizeH = \case (L1 x) -> sizeH x; (R1 y) -> sizeH y
instance (SizeH f, SizeH g) => SizeH (f :*: g) where sizeH (x:*:y) = sizeH x + sizeH y
instance SizeH (K1 i c)                        where sizeH _ = 1
instance (SizeH f) => SizeH (M1 i t f)         where sizeH (M1 x) = sizeH x
instance SizeH U1                              where sizeH _ = 0

-- | "Horizontal" size (max number of fields of any product)
class MaxSizeH (f :: * -> *)               where maxSizeH :: Proxy f -> Int32
instance (MaxSizeH f, MaxSizeH g)
         => MaxSizeH (f :+: g)             where maxSizeH _ = max (maxSizeH (Proxy @f)) (maxSizeH (Proxy @g))
instance (MaxSizeH f, MaxSizeH g)
         => MaxSizeH (f :*: g)             where maxSizeH _ = maxSizeH (Proxy @f) + maxSizeH (Proxy @g)
instance MaxSizeH (K1 i c)                 where maxSizeH _ = 1
instance MaxSizeH f => MaxSizeH (M1 i t f) where maxSizeH _ = maxSizeH (Proxy @f)
instance MaxSizeH U1                       where maxSizeH _ = 0
