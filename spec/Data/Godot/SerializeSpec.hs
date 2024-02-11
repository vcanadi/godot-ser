{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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

newtype AInt32 = AInt32 Int32 deriving(Generic, Show)
instance Serializable AInt32

data IntOrBool
  = IOBInt Int32
  | IOBBool Bool
  deriving(Generic, Show)
instance Serializable IntOrBool

data Complex = Complex Double Double deriving (Generic,Show)
instance Serializable Complex

shouldSerTo :: forall a. (Typeable a, Serializable a, Show a) => a -> [Word8] -> SpecWith ()
shouldSerTo x bytes =
  it ("serializes type " <> show (typeRep (Proxy :: Proxy a)) <> " correctly. Sample: " <> show x)
    $ BS.unpack (ser x) `shouldBe` bytes

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
    (10 :: Int32)         `shouldSerTo` [2,0,0,0 ,10,0,0,0]
    (20 :: Int32)         `shouldSerTo` [2,0,0,0 ,20,0,0,0]
    (10 :: Int64)         `shouldSerTo` [2,0,0,0 ,10,0,0,0]
    (256^4 :: Int64)      `shouldSerTo` [2,0,1,0 ,0,0,0,0,1,0,0,0]
    (256^5 :: Int64)      `shouldSerTo` [2,0,1,0 ,0,0,0,0,0,1,0,0]
    (0 :: Double)         `shouldSerTo` [3,0,0,0 ,0,0,0,0]
    (1 :: Double)         `shouldSerTo` [3,0,0,0 ,0,0,128,63]
    (2 :: Double)         `shouldSerTo` [3,0,0,0 ,0,0,0,64]
    (1.1 :: Double)       `shouldSerTo` [3,0,1,0 ,154,153,153,153,153,153,241,63]
    (10::Int32,20::Int32) `shouldSerTo` [2,0,0,0,10,0,0,0 ,2,0,0,0,20,0,0,0]

specString :: Spec
specString = do
  describe "String:" $ do
    ("a"::String)     `shouldSerTo` [4,0,0,0 ,1,0,0,0 ,97,0,0,0]
    ("ab"::String)    `shouldSerTo` [4,0,0,0 ,2,0,0,0 ,97,98,0,0]
    ("abc"::String)   `shouldSerTo` [4,0,0,0 ,3,0,0,0 ,97,98,99,0]
    ("abcd"::String)  `shouldSerTo` [4,0,0,0 ,4,0,0,0 ,97,98,99,100]
    ("abcde"::String) `shouldSerTo` [4,0,0,0 ,5,0,0,0 ,97,98,99,100,101,0,0,0]

specCustomTypes :: Spec
specCustomTypes =
  describe "Custom types:" $ do
  describe "Custom types:" $ do
    AInt32 10    `shouldSerTo` [2,0,0,0 ,10,0,0,0]
    AInt32 20    `shouldSerTo` [2,0,0,0 ,20,0,0,0]
    IOBInt 10    `shouldSerTo` [2,0,0,0 ,10,0,0,0]
    IOBBool True `shouldSerTo` [1,0,0,0 ,1,0,0,0]
    Complex 1 2  `shouldSerTo` [3,0,0,0,0,0,128,63 ,3,0,0,0,0,0,0,64]

specList :: Spec
specList =
  describe "List:" $ do
    [10::Int32,20]
      `shouldSerTo`
      [28,0,0,0
        ,2,0,0,0
          ,2,0,0,0
            ,10,0,0,0
          ,2,0,0,0
            ,20,0,0,0]
    [True,False,True]
      `shouldSerTo`
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
       `shouldSerTo`
       [18,0,0,0
         ,1,0,0,0
           ,2,0,0,0
             ,10,0,0,0
           ,2,0,0,0
             ,100,0,0,0]
    M.fromList [(10::Int32,100::Int32)
               ,(20       ,200)]
      `shouldSerTo`
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
       `shouldSerTo`
       [18,0,0,0
         ,1,0,0,0
           ,4,0,0,0,1,0,0,0,97,0,0,0
           ,2,0,0,0
             ,100,0,0,0]
    M.fromList [("a"::String,100::Int32)
               ,("b"        ,200)
               ,("c"        ,300)]
       `shouldSerTo`
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
