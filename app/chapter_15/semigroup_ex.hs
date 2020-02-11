module SemigroupExercises where

import Data.Monoid
import Test.QuickCheck

-- Aliases
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdAssoc a = Identity a -> Identity a -> Identity a -> Bool
type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool
type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- Data typesn
data Trivial = Trivial deriving (Eq, Show)
newtype Identity a = Identity a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)


-- Semigroup Instances
instance Semigroup Trivial where
  _ <> _ = Trivial

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

-- Arbitrary Instances
instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = Four  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: (IdAssoc  String) )
  quickCheck (semigroupAssoc :: (IdAssoc (Sum Int)) )
  quickCheck (semigroupAssoc :: (IdAssoc (Maybe (Sum Int))) )
  quickCheck (semigroupAssoc :: (TwoAssoc String String) )
  quickCheck (semigroupAssoc :: (TwoAssoc String String) )

  quickCheck (semigroupAssoc :: (ThreeAssoc String String String))
  quickCheck (semigroupAssoc :: (ThreeAssoc String String String))

  quickCheck (semigroupAssoc :: (FourAssoc String String String String))
  quickCheck (semigroupAssoc :: (FourAssoc String String String String))
