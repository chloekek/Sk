{-# LANGUAGE RankNTypes #-}

module Support.FreeGArrow
  ( GArr (..)
  , liftGArr
  , foldGArr
  ) where

import Prelude hiding ((.), id)

import Control.Category (Category (..))
import Support.GArrow (GArrow (..))

newtype GArr f a b =
  GArr (forall k. GArrow k => (forall x y. f x y -> k x y) -> k a b)

liftGArr :: f a b -> GArr f a b
liftGArr a = GArr $ \f -> f a

foldGArr :: GArrow k => (forall x y. f x y -> k x y) -> GArr f a b -> k a b
foldGArr a (GArr r) = r a

instance Category (GArr f) where
  id = GArr (const id)
  GArr a . GArr b = GArr $ \f -> a f . b f

instance GArrow (GArr f) where
  gfirst (GArr a)  = GArr $ \f -> gfirst (a f)
  gsecond (GArr a) = GArr $ \f -> gsecond (a f)

  gfst = GArr (const gfst)
  gsnd = GArr (const gsnd)

  gunfst = GArr (const gunfst)
  gunsnd = GArr (const gunsnd)

  gassoc   = GArr (const gassoc)
  gunassoc = GArr (const gunassoc)

  gdrop = GArr (const gdrop)
  gcopy = GArr (const gcopy)
  gswap = GArr (const gswap)

  gcoalesce = GArr (const gcoalesce)

  GArr a -***- GArr b = GArr $ \f -> a f -***- b f
  GArr a -&&&- GArr b = GArr $ \f -> a f -&&&- b f
