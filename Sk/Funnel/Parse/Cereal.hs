{-# LANGUAGE GADTs #-}

-- |
-- Interpret syntax rules to cereal.
module Sk.Funnel.Parse.Cereal
  ( parse
  , runSyntax
  ) where

import Control.Lens (_Right, preview)
import Control.Applicative.Free.Fast (Ap, runAp)
import Data.Functor ((<&>))
import Sk.Funnel.Parse (Syntax (..))

import qualified Data.ByteString as BS
import qualified Data.Serialize.Get as SG
import qualified Data.Serialize.IEEE754 as SI
import qualified Data.Vector as V

parse :: Ap Syntax a -> BS.ByteString -> Maybe a
parse = (preview _Right .) . SG.runGet . runAp runSyntax

runSyntax :: Syntax a -> SG.Get a
runSyntax Int64     = SG.getInt64be
runSyntax Double    = SI.getFloat64be
runSyntax Bytes     = do { n <- SG.getWord8 <&> fromIntegral
                         ; SG.getShortByteString n }
runSyntax (Array a) = do { n <- SG.getWord8 <&> fromIntegral
                         ; V.replicateM n (runAp runSyntax a) }
