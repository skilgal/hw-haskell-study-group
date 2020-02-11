{-# LANGUAGE FlexibleInstances #-}
module FunctorChapter where

import GHC.Arr

newtype Mu f =
  InF { outF :: f (Mu f) }

instance Functor (fa Mu) where
  fmap f (InF fb) = undefined

data D = D (Array Word Word) Int Int
