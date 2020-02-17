{-# LANGUAGE RankNTypes #-}
module Functors15 where

-- nat :: (f -> g) -> f a -> g a
-- nat = undefined

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This will not work, not allowed
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]
