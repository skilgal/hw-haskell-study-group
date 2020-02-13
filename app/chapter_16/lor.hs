module FunctorChapter where

import Data.Functor.Constant
import Data.Functor.Identity

type E e = Either e
type C e = Constant e
type I = Identity

data FixMePls a = FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a = ItDoesnt
  | Matter a
  | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

data CountingBad a =
  HeisenbergB Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (HeisenbergB n a) =
    HeisenbergB (n+1) (f a)

data CountingGood a =
  HeisenbergG Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (HeisenbergG n a) =
    HeisenbergG (n) (f a)

replaceWithP = const 'p'
