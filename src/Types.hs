{-# LANGUAGE DeriveGeneric     #-}
module Types
  (   DWIInfo (..)
    , DWIPair (..)
    , PhaseDirection (..)
    , DirType (..)
  ) where

import           GHC.Generics
import           Data.Yaml

data DirType = Pos | Neg
  deriving (Show, Generic)

data DWIInfo = DWIInfo
    {_pid                  :: Int
    ,_dirType              :: DirType
    ,_dwi                  :: FilePath
    ,_size                 :: Int
    ,_b0indices            :: [Int]
    ,_b0indicesWithMinDist :: [Int]
    ,_b0indicesToUse       :: [Int]
    }
  deriving (Show, Generic)

instance ToJSON DirType
instance ToJSON DWIInfo
instance FromJSON DWIInfo
instance FromJSON DirType

data DWIPair = DWIPair
  { _pos :: DWIInfo
  , _neg :: DWIInfo }
  deriving (Show, Generic)

instance ToJSON DWIPair
instance FromJSON DWIPair

data PhaseDirection = RL | PA
  deriving (Show, Read)

