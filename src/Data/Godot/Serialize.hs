{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Godot.Serialize where

import Data.ByteString (ByteString, pack, unpack, singleton)
import Data.String(fromString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Word (Word8)
import Data.Store(encode, decode, PeekException, Store)
import Data.Int
import Control.Arrow (Arrow(first), ArrowChoice (left))
import Control.Category ((>>>))
import Data.Char (chr, ord)
import Control.Applicative ((<|>), Alternative (many))
import Data.Default (Default(..))
import Control.Monad (replicateM, void)
import Data.Functor (($>))
import Data.Attoparsec.ByteString
import GHC.Generics
import Data.Map

data DesErr = DesErrWrongPrefix String -- ^ Error if first 4 bytes, that encode godot type, are incorrect
            | DesErrWrongValues String -- ^ Error if prefix is correct, but rest of the message is incorrect
            | DesErrStore String       -- ^ Error returned during deserialization with 'Data.Store'
            | DesErrParser String
            | DesErrDecoder PeekException
            deriving(Show)

bytes :: [Word8] -> Parser [Word8]
bytes = traverse word8

prefix :: Word8 -> Parser [Word8]
prefix a = bytes [a,0,0,0]

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
  default ser :: (Generic a, SerializableGen (Rep a)) => a -> ByteString
  ser = genericSer
  -- | Deserialization function, default implementation is parsing with a desP parser
  des :: ByteString -> Either DesErr a
  des = parseOnly (desP @a) >>> left DesErrParser
  -- | Deserialization parser
  desP :: Parser a
  default desP :: (Generic a, SerializableGen (Rep a)) => Parser a
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

instance Serializable Int32 where
  ser n = pack [2,0,0,0] <> encode n
  desP = prefix 2 *> decode4P

-- | Check if Int64 is inside subset of Int32 numbers
isInt32 :: Int64 -> Bool
isInt32 = (==) <$> fromIntegral . fromIntegral @Int64 @Int32 <*> id

instance Serializable Int64 where
  ser n = if isInt32 n then pack [2,0,0,0] <> encode (fromIntegral n :: Int32) -- serialize in 4-byte chunks
                       else pack [2,0,1,0] <> encode n                         -- serialize in 8-byte chunks

  desP = bytes [2,0,0,0] *> (fromIntegral <$> decode4P @_ @Int32)
     <|> bytes [2,0,1,0] *> decode8P

-- | Check if Double (float 64) is inside the subset of Float (float 32) numbers
isFloat32 :: Double -> Bool
isFloat32 = (==) <$> realToFrac . realToFrac @Double @Float <*> id

instance Serializable Double where
  ser x = if isFloat32 x then pack [3,0,0,0] <> encode (realToFrac x :: Float) -- serialize in 4-byte chunks
                         else pack [3,0,1,0] <> encode x                       -- serialize in 8-byte chunks

  desP = bytes [3,0,0,0] *> (realToFrac <$> decode4P @_ @Float)
     <|> bytes [3,0,1,0] *> decode8P

-- | Serialized length
serLen :: [a] -> ByteString
serLen = encode . fromIntegral @Int @Int32 . length

instance Serializable a => Serializable [a] where
  ser xs = pack [28,0,0,0] <> serLen xs <> foldMap ser xs
  desP = do
    prefix 28
    -- Parse length of the array
    (n :: Int32) <- decode4P
    nTimesP n

-- | Parse something n times
nTimesP :: Serializable a => Int32 -> Parser [a]
nTimesP 0 = pure []
nTimesP n = (:) <$> desP <*> nTimesP (pred n)

-- | Pad 4block to 8block
pad :: ByteString -> ByteString
pad = (<> BS.pack [0,0,0,0])

-- | Desialize a 'Generic' value to
class SerializableGen g where
  serGen :: g a -> ByteString
  desGenP :: Parser (g a)

instance SerializableGen g => SerializableGen (M1 x y g) where
  serGen (M1 v) = serGen v
  desGenP = M1 <$> desGenP

instance (SerializableGen g, SerializableGen h) => SerializableGen (g :+: h) where
  serGen = \case
    (L1 x) -> serGen x
    (R1 y) -> serGen y
  desGenP = L1 <$> desGenP <|> R1 <$> desGenP

instance (SerializableGen g, SerializableGen h) => SerializableGen (g :*: h) where
  serGen (x :*: y) = serGen x <> serGen y
  desGenP = (:*:) <$> desGenP  <*> desGenP

instance (Serializable a) => SerializableGen (K1 x a) where
  serGen (K1 v) = ser v
  desGenP = K1 <$> desP

instance SerializableGen U1 where
  serGen U1 = ""
  desGenP = pure U1

genericSer :: (Generic a, SerializableGen (Rep a)) => a -> ByteString
genericSer = serGen . from
genericDesP :: (Generic a, SerializableGen (Rep a)) => Parser a
genericDesP = to <$> desGenP

newtype A = A Int32 deriving(Generic, Show)
newtype B = B { bInt32 :: Int32  } deriving(Generic, Show)

instance Serializable A where
instance Serializable B where
