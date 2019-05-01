-- See http://www.megacz.com/berkeley/garrows/.

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Support.GArrow
  ( GArrow (..)
  , Generalize (..)
  ) where

import Prelude hiding ((.))

import Control.Arrow (Arrow (..), Kleisli (..))
import Control.Category (Category (..), (>>>))
import Control.Monad (join)
import Data.Tuple (swap)

infixr 3 -***-
infixr 3 -&&&-

--------------------------------------------------------------------------------

-- |
-- 'Arrow' without 'arr'.
class Category k => GArrow k where
  {-# MINIMAL gfirst, gsecond,
              (gfst | gsnd),
              (gunfst | gunsnd),
              gassoc, gunassoc,
              gdrop, gcopy, gswap #-}

  gfirst    :: k a b -> k (a, c) (b, c)
  gsecond   :: k a b -> k (c, a) (c, b)

  gfst      :: k (a, ()) a
  gsnd      :: k ((), a) a
  gfst      = gsnd . gswap
  gsnd      = gfst . gswap

  gunfst    :: k a (a, ())
  gunsnd    :: k a ((), a)
  gunfst    = gswap . gunsnd
  gunsnd    = gswap . gunfst

  gassoc    :: k (a, (b, c)) ((a, b), c)
  gunassoc  :: k ((a, b), c) (a, (b, c))

  gdrop     :: k a ()
  gcopy     :: k a (a, a)
  gswap     :: k (a, b) (b, a)

  gcoalesce :: k ((), ()) ()
  gcoalesce = gdrop

  (-***-)   :: k a b -> k c d -> k (a, c) (b, d)
  a -***- b = gfirst a >>> gsecond b

  (-&&&-)   :: k a b -> k a c -> k a (b, c)
  a -&&&- b = gcopy >>> a -***- b

--------------------------------------------------------------------------------

-- |
-- Every 'Arrow' gives a 'GArrow'.
newtype Generalize k a b =
  Generalize (k a b)

deriving newtype instance Category k => Category (Generalize k)

instance Arrow k => GArrow (Generalize k) where
  gfirst (Generalize a) = Generalize (first a)
  gsecond (Generalize a) = Generalize (second a)
  gfst = Generalize (arr fst)
  gsnd = Generalize (arr snd)
  gunfst = Generalize (arr (, ()))
  gunsnd = Generalize (arr ((), ))
  gassoc = Generalize (arr (\(a, (b, c)) -> ((a, b), c)))
  gunassoc = Generalize (arr (\((a, b), c) -> (a, (b, c))))
  gdrop = Generalize (arr (const ()))
  gcopy = Generalize (arr (join (,)))
  gswap = Generalize (arr swap)
  gcoalesce = Generalize (arr (const ()))
  Generalize a -***- Generalize b = Generalize (a *** b)
  Generalize a -&&&- Generalize b = Generalize (a &&& b)

--------------------------------------------------------------------------------

deriving via Generalize (->) instance GArrow (->)
deriving via Generalize (Kleisli m) instance Monad m => GArrow (Kleisli m)
