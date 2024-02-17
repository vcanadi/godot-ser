{-# LANGUAGE DeriveAnyClass #-}

module Data.Godot.SerializeSpec ( spec) where
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
import Data.Foldable (traverse_)
import Control.Monad (zipWithM_)

newtype AInt32 = AInt32 Int32 deriving(Generic, Show, Eq)
instance Serializable AInt32

data IntOrBool
  = IOBInt Int32
  | IOBBool Bool
  deriving(Generic, Show, Eq, Serializable)

data Complex = Complex Double Double deriving (Generic,Show, Eq, Serializable)

data X1  = X10                                                                                     deriving(Eq, Generic, Show, Enum, Bounded, Serializable)
data X2  = X20 | X21                                                                               deriving(Eq, Generic, Show, Enum, Bounded, Serializable)
data X3  = X30 | X31 | X32                                                                         deriving(Eq, Generic, Show, Enum, Bounded, Serializable)
data X4  = X40 | X41 | X42 | X43                                                                   deriving(Eq, Generic, Show, Enum, Bounded, Serializable)
data X5  = X50 | X51 | X52 | X53 | X54                                                             deriving(Eq, Generic, Show, Enum, Bounded, Serializable)
data X6  = X60 | X61 | X62 | X63 | X64 | X65                                                       deriving(Eq, Generic, Show, Enum, Bounded, Serializable)
data X7  = X70 | X71 | X72 | X73 | X74 | X75 | X76                                                 deriving(Eq, Generic, Show, Enum, Bounded, Serializable)
data X8  = X80 | X81 | X82 | X83 | X84 | X85 | X86 | X87                                           deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)
data X9  = X90 | X91 | X92 | X93 | X94 | X95 | X96 | X97 | X98                                     deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)
data XA  = XA0 | XA1 | XA2 | XA3 | XA4 | XA5 | XA6 | XA7 | XA8 | XA9                               deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)
data XB  = XB0 | XB1 | XB2 | XB3 | XB4 | XB5 | XB6 | XB7 | XB8 | XB9 | XBA                         deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)
data XC  = XC0 | XC1 | XC2 | XC3 | XC4 | XC5 | XC6 | XC7 | XC8 | XC9 | XCA | XCB                   deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)
data XD  = XD0 | XD1 | XD2 | XD3 | XD4 | XD5 | XD6 | XD7 | XD8 | XD9 | XDA | XDB | XDC             deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)
data XE  = XE0 | XE1 | XE2 | XE3 | XE4 | XE5 | XE6 | XE7 | XE8 | XE9 | XEA | XEB | XEC | XED       deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)
data XF  = XF0 | XF1 | XF2 | XF3 | XF4 | XF5 | XF6 | XF7 | XF8 | XF9 | XFA | XFB | XFC | XFD | XFE deriving(Eq, Generic, Show ,Enum, Bounded, Serializable)

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
spec = describe "Samples:" $ do
  specPrimitives
  specString
  specCustomTypes
  specList
  specMap
  specGenerics

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
specString = describe "String:" $ do
  ("a"::String)     `shouldSerDesTo` [4,0,0,0 ,1,0,0,0 ,97,0,0,0]
  ("ab"::String)    `shouldSerDesTo` [4,0,0,0 ,2,0,0,0 ,97,98,0,0]
  ("abc"::String)   `shouldSerDesTo` [4,0,0,0 ,3,0,0,0 ,97,98,99,0]
  ("abcd"::String)  `shouldSerDesTo` [4,0,0,0 ,4,0,0,0 ,97,98,99,100]
  ("abcde"::String) `shouldSerDesTo` [4,0,0,0 ,5,0,0,0 ,97,98,99,100,101,0,0,0]

specCustomTypes :: Spec
specCustomTypes =
  describe "Custom types:" $ describe "Custom types:" $ do
  AInt32 10    `shouldSerDesTo` [2,0,0,0, 10,0,0,0]
  AInt32 20    `shouldSerDesTo` [2,0,0,0 ,20,0,0,0]
  IOBInt 10    `shouldSerDesTo` [0,0,0,0, 2,0,0,0 ,10,0,0,0] -- sum type's first constructor
  IOBBool True `shouldSerDesTo` [1,0,0,0, 1,0,0,0 ,1,0,0,0]  -- sum type's second constructor
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

-- | Generate tests like:
-- X10 `shouldDesSerTo` [0,0,0,0]
-- X20 `shouldDesSerTo` [0,0,0,0]
-- X21 `shouldDesSerTo` [1,0,0,0]
-- X30 `shouldDesSerTo` [0,0,0,0]
-- X31 `shouldDesSerTo` [1,0,0,0]
-- X32 `shouldDesSerTo` [2,0,0,0]
-- ...
-- i.e. Xnk `shouldDesSerTo` [k,0,0,0]
-- For all Xnk (0 <= k < n) constructors of all Xn types
specGenerics :: Spec
specGenerics = describe "Xnk" $ do
  zipWithM_ shouldSerDesTo (allVals @X2) (ixsLessThan 2 )
  zipWithM_ shouldSerDesTo (allVals @X3) (ixsLessThan 3 )
  zipWithM_ shouldSerDesTo (allVals @X4) (ixsLessThan 4 )
  zipWithM_ shouldSerDesTo (allVals @X5) (ixsLessThan 5 )
  zipWithM_ shouldSerDesTo (allVals @X6) (ixsLessThan 6 )
  zipWithM_ shouldSerDesTo (allVals @X7) (ixsLessThan 7 )
  zipWithM_ shouldSerDesTo (allVals @X8) (ixsLessThan 8 )
  zipWithM_ shouldSerDesTo (allVals @X9) (ixsLessThan 9 )
  zipWithM_ shouldSerDesTo (allVals @XA) (ixsLessThan 10)
  zipWithM_ shouldSerDesTo (allVals @XB) (ixsLessThan 11)
  zipWithM_ shouldSerDesTo (allVals @XC) (ixsLessThan 12)
  zipWithM_ shouldSerDesTo (allVals @XD) (ixsLessThan 13)
  zipWithM_ shouldSerDesTo (allVals @XE) (ixsLessThan 14)
  zipWithM_ shouldSerDesTo (allVals @XF) (ixsLessThan 15)
    where
      ixsLessThan :: Word8 -> [[Word8]]
      ixsLessThan n = [[i,0,0,0] | i <- [0..pred n]]

      allVals :: (Enum a, Bounded a) => [a]
      allVals = [minBound..maxBound]


