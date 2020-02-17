module Functors10 where

import Data.Functor.Identity hiding (Identity)
import Test.QuickCheck

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-- Impossible. There is no type parameter
data Trivial = Trivial

functorIdentity :: (Functor f, Eq (f a)) =>
                   f a -> Bool
functorIdentity f = fmap id f == f
functorCompose :: (Eq (f c), Functor f) => (a -> b)
               -> (b -> c) -> f a
               -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

f :: [Int] -> Bool
f x = functorIdentity x

c = functorCompose (+1) (*2)
li x = c (x :: [Int])

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity a -> Bool)
  quickCheck li
