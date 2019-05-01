{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sk.Sample
  ( Sample (..)
  , Label (..)
  ) where

import Data.Int (Int64)

import qualified Data.ByteString.Short as BSS
import qualified Data.Vector as V

--------------------------------------------------------------------------------

data Sample =
  Sample
    { sampleInstant :: Int64
    , sampleValue   :: Double
    , sampleLabels  :: V.Vector Label }

newtype Label =
  Label
    { labelName :: BSS.ShortByteString }

--------------------------------------------------------------------------------

deriving stock instance Show Label
deriving stock instance Show Sample
