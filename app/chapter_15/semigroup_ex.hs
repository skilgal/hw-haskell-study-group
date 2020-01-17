module SemigroupExercises where

import Test.QuickCheck
import Data.Monoid

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdAssoc a = Identity a -> Identity a -> Identity a -> Bool
type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool
type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

data Trivial = Trivial deriving (Eq, Show)
newtype Identity a = Identity a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

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

  quickCheck (semigroupAssoc :: (Two String String -> Two String String -> Two String String))
  -- quickCheck (semigroupAssoc :: (TwoAssoc (Sum Float) Bool))

  -- quickCheck (semigroupAssoc :: (ThreeAssoc (Sum Int) String Bool))
  -- quickCheck (semigroupAssoc :: (ThreeAssoc (Sum Float) Bool Bool))

  -- quickCheck (semigroupAssoc :: (FourAssoc (Sum Int) String Bool Double))
  -- quickCheck (semigroupAssoc :: (FourAssoc (Sum Float) Bool Bool String))
