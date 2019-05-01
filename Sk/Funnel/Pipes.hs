{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Interpret funnel processes to pipes. Pipes admit five interesting
-- categories, as seen in "Pipes.Core". We interpret to the Kleisli category.
--
-- The pipes created here are concerned with the internal communication of the
-- funnel only; sockets are not dealt with here. Sockets may be connected to
-- the pipes that are created here, to achieve a fully functional funnel.
module Sk.Funnel.Pipes
  ( I
  , O
  , runFunnel
  , runStep
  ) where

import Control.Arrow (Kleisli (..), arr)
import Control.Monad (forever)
import Data.Foldable (fold, traverse_)
import Pipes (Pipe, await, yield)
import Sk.Funnel (Funnel (..), Step (..))
import Sk.Funnel.Parse.Cereal (parse)
import Sk.Sample (Sample)
import Support.FreeCategory (foldCat)

import qualified Data.ByteString as BS

type I = BS.ByteString
type O = Sample

runFunnel :: Monad m => Funnel a b -> Kleisli (Pipe I O m) a b
runFunnel (Loop steps) =
  let Kleisli k = foldCat runStep steps in
  Kleisli (forever . k)

runStep :: Monad m => Step a b -> Kleisli (Pipe I O m) a b
runStep Receive   = Kleisli $ \() -> await
runStep (Parse s) = arr (fold @Maybe . parse s)
runStep Emit      = Kleisli $ traverse_ yield
