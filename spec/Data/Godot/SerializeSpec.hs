module Data.Godot.SerializeSpec
  ( spec
  ) where

import           Test.Hspec
import qualified Data.ByteString as BS
import Data.Godot.Serialize
import Data.Int (Int32, Int64)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Typeable (typeRep, Typeable)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Map as M
import Data.Word (Word8)

newtype AInt32 = AInt32 Int32 deriving(Generic, Show, Eq)
instance Serializable AInt32

data IntOrBool
  = IOBInt Int32
  | IOBBool Bool
  deriving(Generic, Show, Eq)
instance Serializable IntOrBool

data Complex = Complex Double Double deriving (Generic,Show, Eq)
instance Serializable Complex

shouldSerTo :: forall a. (Typeable a, Serializable a, Show a) => a -> [Word8] -> SpecWith ()
shouldSerTo x bytes =
  it ("Serializes type " <> show (typeRep (Proxy :: Proxy a)) <> " correctly. Sample: " <> show x)
    $ BS.unpack (ser x) `shouldBe` bytes

shouldDesTo :: forall a. (Eq a, Serializable a, Show a) => [Word8] ->  a -> SpecWith ()
shouldDesTo bytes x =
  it ("Deserializes bytestring correctly. Bytestring: " <> show bytes)
    $ des (BS.pack bytes) `shouldBe` Right x

shouldSerDesTo :: forall a. (Typeable a, Eq a, Serializable a, Show a) => a -> [Word8] -> SpecWith ()
shouldSerDesTo x bytes = do
  x `shouldSerTo` bytes
  bytes `shouldDesTo` x

spec :: Spec
spec = describe "Hardcoded samples:" $ do
  specPrimitives
  specString
  specCustomTypes
  specList
  specMap

specPrimitives :: Spec
specPrimitives =
  describe "Primitives:" $ do
    (10 :: Int32)         `shouldSerDesTo` [2,0,0,0 ,10,0,0,0]
    (20 :: Int32)         `shouldSerDesTo` [2,0,0,0 ,20,0,0,0]
    (10 :: Int64)         `shouldSerDesTo` [2,0,0,0 ,10,0,0,0]
    (256^4 :: Int64)      `shouldSerDesTo` [2,0,1,0 ,0,0,0,0,1,0,0,0]
    (256^5 :: Int64)      `shouldSerDesTo` [2,0,1,0 ,0,0,0,0,0,1,0,0]
    (0 :: Double)         `shouldSerDesTo` [3,0,0,0 ,0,0,0,0]
    (1 :: Double)         `shouldSerDesTo` [3,0,0,0 ,0,0,128,63]
    (2 :: Double)         `shouldSerDesTo` [3,0,0,0 ,0,0,0,64]
    (1.1 :: Double)       `shouldSerDesTo` [3,0,1,0 ,154,153,153,153,153,153,241,63]
    (10::Int32,20::Int32) `shouldSerDesTo` [2,0,0,0,10,0,0,0 ,2,0,0,0,20,0,0,0]

specString :: Spec
specString = do
  describe "String:" $ do
    ("a"::String)     `shouldSerDesTo` [4,0,0,0 ,1,0,0,0 ,97,0,0,0]
    ("ab"::String)    `shouldSerDesTo` [4,0,0,0 ,2,0,0,0 ,97,98,0,0]
    ("abc"::String)   `shouldSerDesTo` [4,0,0,0 ,3,0,0,0 ,97,98,99,0]
    ("abcd"::String)  `shouldSerDesTo` [4,0,0,0 ,4,0,0,0 ,97,98,99,100]
    ("abcde"::String) `shouldSerDesTo` [4,0,0,0 ,5,0,0,0 ,97,98,99,100,101,0,0,0]

specCustomTypes :: Spec
specCustomTypes =
  describe "Custom types:" $ do
  describe "Custom types:" $ do
    AInt32 10    `shouldSerDesTo` [2,0,0,0 ,10,0,0,0]
    AInt32 20    `shouldSerDesTo` [2,0,0,0 ,20,0,0,0]
    IOBInt 10    `shouldSerDesTo` [2,0,0,0 ,10,0,0,0]
    IOBBool True `shouldSerDesTo` [1,0,0,0 ,1,0,0,0]
    Complex 1 2  `shouldSerDesTo` [3,0,0,0,0,0,128,63 ,3,0,0,0,0,0,0,64]

specList :: Spec
specList =
  describe "List:" $ do
    [10::Int32,20]
      `shouldSerDesTo`
      [28,0,0,0
        ,2,0,0,0
          ,2,0,0,0
            ,10,0,0,0
          ,2,0,0,0
            ,20,0,0,0]
    [True,False,True]
      `shouldSerDesTo`
      [28,0,0,0
        ,3,0,0,0
           ,1,0,0,0
             ,1,0,0,0
           ,1,0,0,0
             ,0,0,0,0
           ,1,0,0,0
             ,1,0,0,0]

specMap :: Spec
specMap =
  describe "Map:" $ do
    M.fromList [(10::Int32,100::Int32)]
       `shouldSerDesTo`
       [18,0,0,0
         ,1,0,0,0
           ,2,0,0,0
             ,10,0,0,0
           ,2,0,0,0
             ,100,0,0,0]
    M.fromList [(10::Int32,100::Int32)
               ,(20       ,200)]
      `shouldSerDesTo`
      [18,0,0,0
        ,2,0,0,0
          ,2,0,0,0
            ,10,0,0,0
          ,2,0,0,0
            ,100,0,0,0
          ,2,0,0,0
            ,20,0,0,0
          ,2,0,0,0
            ,200,0,0,0]
    M.fromList [("a"::String,100::Int32)]
       `shouldSerDesTo`
       [18,0,0,0
         ,1,0,0,0
           ,4,0,0,0,1,0,0,0,97,0,0,0
           ,2,0,0,0
             ,100,0,0,0]
    M.fromList [("a"::String,100::Int32)
               ,("b"        ,200)
               ,("c"        ,300)]
       `shouldSerDesTo`
       [18,0,0,0
         ,3,0,0,0
           ,4,0,0,0,1,0,0,0,97,0,0,0
           ,2,0,0,0
             ,100,0,0,0
           ,4,0,0,0,1,0,0,0,98,0,0,0
           ,2,0,0,0
             ,200,0,0,0
           ,4,0,0,0,1,0,0,0,99,0,0,0
           ,2,0,0,0
             ,44,1,0,0]
