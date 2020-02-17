module Functor8 where

import Test.QuickCheck

data Two a b =
  Two a b
  deriving (Eq, Show)


data Or a b =
  First a
  | Second b
  deriving (Eq, Show)

-- instance Functor (Two a) where
--   fmap f (Two a b) = Two $ (a) (f b)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor (Two a) where
-- We can make the type of fmap
-- more concrete, like this:
  -- fmap :: (a -> b) -> (Two z) a -> (Two z) b

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

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
  quickCheck f
  quickCheck li
