{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Data.Godot.SerializeSpec
  ( spec
  ) where

import           Test.Hspec
import qualified Data.ByteString as BS
import Data.Godot.Serialize

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
