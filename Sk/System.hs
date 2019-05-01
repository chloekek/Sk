{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- System.
module Sk.System
  ( System (..)
  , system
  ) where

import Prelude hiding (id)

import Control.Category ((>>>), id)
import Sk.Sample (Sample)
import Support.FreeGArrow (GArr, liftGArr)
import Support.GArrow ((-&&&-), gcoalesce)

--------------------------------------------------------------------------------

-- |
-- System data flow. This is the highest-level view of the system. It does not
-- describe a sequential process, but rather how data flows between several
-- concurrent processes.
data System :: * -> * -> * where

  -- |
  -- The funnel component receives network packets containing samples and emits
  -- all of the samples for further processing.
  Funnel :: System () Sample

  -- |
  -- The recorder component receives samples from the funnel component and
  -- records them in the database for later analysis.
  Recorder :: System Sample ()

  -- |
  -- The reactor component receives samples from the funnel component and
  -- immediately analyses them to send alerts.
  Reactor :: System Sample ()

--------------------------------------------------------------------------------

-- |
-- System data flow.
system :: GArr System () ()
system = id
  >>> liftGArr Funnel
  >>> liftGArr Recorder
      -&&&- liftGArr Reactor
  >>> gcoalesce

--------------------------------------------------------------------------------

deriving stock instance Show (System a b)
