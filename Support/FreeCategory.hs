{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Support.FreeCategory
  ( Cat (..)
  , liftCat
  , foldCat
  ) where

import Prelude hiding ((.), id)

import Control.Category (Category (..))

data Cat :: (* -> * -> *) -> (* -> * -> *) where
  Id    :: Cat f a a
  (:.:) :: f b c -> Cat f a b -> Cat f a c

liftCat :: f a b -> Cat f a b
liftCat = (:.: Id)

foldCat :: Category k => (forall x y. f x y -> k x y) -> Cat f a b -> k a b
foldCat _ Id        = id
foldCat f (a :.: b) = f a . foldCat f b

instance Category (Cat f) where
  id = Id
  Id        . f  = f
  f         . Id = f
  (f :.: g) . h  = f :.: (g . h)

deriving stock instance (forall x y. Show (f x y)) => Show (Cat f a b)
