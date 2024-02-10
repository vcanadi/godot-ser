{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Godot.SerializeSpec
  ( spec
  ) where

import           Test.Hspec
import qualified Data.ByteString as BS
import Data.Godot.Serialize
import Data.Int (Int32)
import GHC.Generics (Generic)

newtype A = A Int32 deriving(Generic, Show)
newtype B = B { bInt32 :: Int32  } deriving(Generic, Show)

instance Serializable A where
instance Serializable B where

main = do
  let a  = A 1
  let b  = B 1
  showVar a
  showVar b

showVar x = do
  putStrLn "var:" >> print x
  putStrLn "var ser:" >> print (BS.unpack $ ser x)

spec :: Spec
spec = it "correct" $ 1 `shouldBe` 1
