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
import Data.Word (Word8, Word32, Word16)
import Data.Foldable (traverse_)
import Control.Monad (zipWithM_)
import Data.Map (Map)

-- Sample types

newtype AInt32 = AInt32 Int32 deriving(Generic, Show, Eq)
instance Serializable AInt32

data IntOrBool
  = IOBInt Int32
  | IOBBool Bool
  deriving(Generic, Show, Eq, Serializable)

data Complex = Complex Double Double deriving (Generic, Show, Eq, Serializable)

newtype PortNumber = PortNumber Word16 deriving (Ord, Generic, Show, Eq)
instance Serializable PortNumber

type HostAddr = Word32
data SockAddr
  = SockAddrInet PortNumber HostAddr
  | SockAddrUnix String
  deriving (Ord, Generic, Show, Eq, Serializable)

data Msg
  = Join
  | State (Map SockAddr (Int,Int))
  | Leave deriving (Generic, Show, Eq, Serializable)

data Dir = L | R | U | D deriving (Show, Eq, Generic, Enum, Bounded, Read, Serializable)

data CliMsg
  = JOIN
  | LEAVE
  | MOVE Dir
  | GET_STATE
  deriving (Show, Eq, Read, Generic, Serializable)

type State = Map Int Bool

newtype SrvMsg = PUT_STATE State deriving (Show, Eq, Generic, Serializable)

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
  specList
  specMap
  specGenerics
  specCustomTypes

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
    -- (10::Int32,20::Int32) `shouldSerDesTo` [2,0,0,0,10,0,0,0 ,2,0,0,0,20,0,0,0]

specString :: Spec
specString = describe "String:" $ do
  ("a"::String)     `shouldSerDesTo` [4,0,0,0 ,1,0,0,0 ,97,0,0,0]
  ("ab"::String)    `shouldSerDesTo` [4,0,0,0 ,2,0,0,0 ,97,98,0,0]
  ("abc"::String)   `shouldSerDesTo` [4,0,0,0 ,3,0,0,0 ,97,98,99,0]
  ("abcd"::String)  `shouldSerDesTo` [4,0,0,0 ,4,0,0,0 ,97,98,99,100]
  ("abcde"::String) `shouldSerDesTo` [4,0,0,0 ,5,0,0,0 ,97,98,99,100,101,0,0,0]

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
    describe "General Map:" $ do
      M.fromList [(10::Int32,100::Int32)]
         `shouldSerDesTo`
         [28,0,0,0
           ,1,0,0,0
             ,28,0,0,0, 2,0,0,0
               ,2,0,0,0
                 ,10,0,0,0
               ,2,0,0,0
                 ,100,0,0,0]
      M.fromList [(10::Int32,100::Int32)
                 ,(20       ,200)]
        `shouldSerDesTo`
        [28,0,0,0
          ,2,0,0,0
            ,28,0,0,0, 2,0,0,0
              ,2,0,0,0
                ,10,0,0,0
              ,2,0,0,0
                ,100,0,0,0
            ,28,0,0,0, 2,0,0,0
              ,2,0,0,0
                ,20,0,0,0
              ,2,0,0,0
                ,200,0,0,0]
    describe "Dictionary:" $ do
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
      ixsLessThan n = [[2,0,0,0 ,i,0,0,0] | i <- [0..pred n]]

      allVals :: (Enum a, Bounded a) => [a]
      allVals = [minBound..maxBound]

specCustomTypes :: Spec
specCustomTypes =
  describe "Custom types:" $ do
    describe "AInt32" $ do
      AInt32 10 `shouldSerDesTo` [2,0,0,0, 10,0,0,0]
      AInt32 20 `shouldSerDesTo` [2,0,0,0 ,20,0,0,0]
    describe "IntOrBool" $ do
      IOBInt 10    `shouldSerDesTo` [28,0,0,0,2,0,0,0     -- Serialize sum type as godot's array of len 2 (constructor index * single payload constructor field)
                                      ,2,0,0,0, 0,0,0,0   -- Fst elem is constructor's index (Int32 serialization)
                                      ,2,0,0,0 ,10,0,0,0] -- Snd elem is constructor payload
      IOBBool True `shouldSerDesTo` [28,0,0,0,2,0,0,0
                                      ,2,0,0,0, 1,0,0,0
                                      ,1,0,0,0 ,1,0,0,0]
    describe "Complex" $ do
      Complex 1 2 `shouldSerDesTo` [17,0,0,0,2,0,0,0,3,0,0,0,0,0,128,63 ,3,0,0,0,0,0,0,64]
    describe "Msg" $ do
      Join
        `shouldSerDesTo`
        [28,0,0,0, 1,0,0,0         -- State is serialized as 2-elem array (sum type)
          ,2,0,0,0 ,0,0,0,0]       -- 0. constructor (State) of Msg
      State (M.fromList
        [
          ( SockAddrInet (PortNumber 88) 127
          , (66,77))
        ])
        `shouldSerDesTo`
        [ 28,0,0,0 ,2,0,0,0          -- State is serialized as 2-elem array (sum type)
            ,2,0,0,0 ,1,0,0,0        -- 1. constructor (State) of Msg
            ,28,0,0,0 ,1,0,0,0       -- General Map (not dict.) of 1 elements
              ,28,0,0,0, 2,0,0,0     -- Generic Map Tuple
                ,28,0,0,0 ,3,0,0,0   -- HostAddr is serialized as 3-elem array
                  ,2,0,0,0 ,0,0,0,0  -- 0. constructor (SockAddrInet) of HostAddr
                  ,2,0,0,0,88,0,0,0  -- PortNumber
                  ,2,0,0,0,127,0,0,0 -- Host
                ,28,0,0,0 ,2,0,0,0   -- Tuple is serialized as object (17)
                  ,2,0,0,0 ,66,0,0,0 -- fst Int
                  ,2,0,0,0 ,77,0,0,0 -- snd Int
        ]
      Leave
        `shouldSerDesTo`
        [28,0,0,0, 1,0,0,0         -- State is serialized as 2-elem array (sum type)
          ,2,0,0,0 ,2,0,0,0]       -- 0. constructor (State) of Msg

    describe "CliMsg" $ do
      JOIN
        `shouldSerDesTo`
        [28,0,0,0, 1,0,0,0
          ,2,0,0,0 ,0,0,0,0]
      LEAVE
        `shouldSerDesTo`
        [28,0,0,0, 1,0,0,0
          ,2,0,0,0 ,1,0,0,0]
      MOVE L
        `shouldSerDesTo`
        [28,0,0,0, 2,0,0,0
          ,2,0,0,0 ,2,0,0,0
          ,2,0,0,0, 0,0,0,0
          ]

    describe "SrvMsg" $ do
      PUT_STATE (M.fromList [(10,True)])
        `shouldSerDesTo`
        [28,0,0,0, 1,0,0,0
          ,28,0,0,0, 2,0,0,0
            ,2,0,0,0 ,10,0,0,0
            ,1,0,0,0, 1,0,0,0
          ]


