{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Parsing of packets that the funnel receives.
module Sk.Funnel.Parse
  ( Syntax (..)

  , packet
  , sample
  , label
  ) where

import Control.Applicative.Free.Fast (Ap, liftAp)
import Data.Int (Int64)
import Sk.Sample (Label (..), Sample (..))

import qualified Data.ByteString.Short as BSS
import qualified Data.Vector as V

--------------------------------------------------------------------------------

-- |
-- Syntax rule for parsing a part of a packet.
data Syntax :: * -> * where
  Int64  :: Syntax Int64
  Double :: Syntax Double
  Bytes  :: Syntax BSS.ShortByteString
  Array  :: Ap Syntax a -> Syntax (V.Vector a)

--------------------------------------------------------------------------------

-- |
-- Packet syntax.
packet :: Ap Syntax (V.Vector Sample)
packet = liftAp $ Array sample

-- |
-- Sample syntax.
sample :: Ap Syntax Sample
sample = Sample <$> liftAp Int64
                <*> liftAp Double
                <*> liftAp (Array label)

-- |
-- Label syntax.
label :: Ap Syntax Label
label = Label <$> liftAp Bytes
