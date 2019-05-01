{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- See "Sk" for information on the funnel component.
module Sk.Funnel
  ( Funnel (..)
  , Step (..)
  , funnel
  , step
  ) where

import Prelude hiding (id)

import Control.Applicative.Free.Fast (Ap)
import Control.Category ((>>>), id)
import Sk.Sample (Sample)
import Support.FreeCategory (Cat, liftCat)

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Sk.Funnel.Parse as Parse

--------------------------------------------------------------------------------

-- |
-- Funnel process. This describes a sequential process.
data Funnel :: * -> * -> * where

  -- |
  -- Apply a step repeatedly, forever. The funnel is a never-ending process.
  Loop :: Cat Step () () -> Funnel () a

-- |
-- Funnel step. This describes a sequential process.
data Step :: * -> * -> * where

  -- |
  -- Receive a packet from the socket. The socket is not dealt with in this
  -- EDSL; it is a detail of the interpreter.
  Receive :: Step () BS.ByteString

  -- |
  -- Parse a packet into a list of the samples that it contains.
  Parse :: Ap Parse.Syntax (V.Vector Sample)
        -> Step BS.ByteString (V.Vector Sample)
    -- TODO: What to do if parsing fails? Return Either?

  -- |
  -- Emit the given samples from the funnel.
  Emit :: Step (V.Vector Sample) ()

--------------------------------------------------------------------------------

-- |
-- Funnel process.
funnel :: Funnel () a
funnel = Loop step

-- |
-- Funnel step.
step :: Cat Step () ()
step = id
  >>> liftCat Receive
  >>> liftCat (Parse Parse.packet)
  >>> liftCat Emit
