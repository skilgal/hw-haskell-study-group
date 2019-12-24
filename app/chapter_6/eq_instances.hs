{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Test where

data TisAnInteger = TisAn Integer
data TwoIntegers = Two Integer Integer
data StringOrInt = TisAnInt Int | TisAString String
data Pair a = Pair a a
data Tuple a b = Tuple a b
data Which a = ThisOne a | ThatOne a
data EitherOr a b = Hello a | Goodbye b

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn a') = a == a'

instance Eq TwoIntegers where
  (==) (Two t1 t2) (Two t1' t2') = t1 == t1' && t2 == t2'

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

instance Eq a => Eq (Pair a) where
  (==) (Pair f s) (Pair f' s') = f == f' && s == s'

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b )(Tuple a' b') = a == a' && b == b'

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False
