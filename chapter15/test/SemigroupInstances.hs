module SemigroupInstances where

import           Data.Semigroup
import           Test.QuickCheck

-- [1]
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- [2]
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- [3]
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- [4]
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- [5]
-- More of the same ...
-- (Four a b c d)
-- [6]
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

-- [7]
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

-- [8]
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [(Fst <$> arbitrary), (Snd <$> arbitrary)]

instance Semigroup (Or a b) where
  x@(Snd _) <> _ = x
  _ <> x = x

-- [9]
newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine h
    where
      h x = (f x) <> (g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

combineAssoc_prop ::
     (Semigroup b, Eq b)
  => Blind (Combine a b)
  -> Blind (Combine a b)
  -> Blind (Combine a b)
  -> a
  -> Bool
combineAssoc_prop (Blind f) (Blind g) (Blind h) x = left == right
  where
    left = unCombine ((f <> g) <> h) $ x
    right = unCombine (f <> (g <> h)) $ x

-- [10]
newtype Comp a = Comp
  { unComp :: (a -> a)
  }

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

compAssoc_prop ::
     (Eq a) => Blind (Comp a) -> Blind (Comp a) -> Blind (Comp a) -> a -> Bool
compAssoc_prop (Blind f) (Blind g) (Blind h) x = left == right
  where
    left = unComp ((f <> g) <> h) $ x
    right = unComp (f <> (g <> h)) $ x

-- [11]
data Validation a b
  = Fail a
  | Win b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Fail a1) <> (Fail a2) = Fail (a1 <> a2)
  (Win b) <> _ = Win b
  _ <> (Win b) = Win b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Fail <$> arbitrary, Win <$> arbitrary]

semigroupAssoc_prop :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc_prop a b c = (a <> (b <> c)) == ((a <> b) <> c)

type Assoc a = a -> a -> a -> Bool

type FuncAssoc f x = Blind f -> Blind f -> Blind f -> x -> Bool

main :: IO ()
main = do
  putStr "Trivial: "
  quickCheck (semigroupAssoc_prop :: Assoc Trivial)
  putStr "Identity: "
  quickCheck (semigroupAssoc_prop :: Assoc (Identity String))
  putStr "Two: "
  quickCheck (semigroupAssoc_prop :: Assoc (Two String String))
  putStr "Three: "
  quickCheck (semigroupAssoc_prop :: Assoc (Three String String String))
  putStr "BoolConj: "
  quickCheck (semigroupAssoc_prop :: Assoc BoolConj)
  putStr "BoolDisj: "
  quickCheck (semigroupAssoc_prop :: Assoc BoolDisj)
  putStr "Or: "
  quickCheck (semigroupAssoc_prop :: Assoc (Or String String))
  putStr "Combine: "
  quickCheck (combineAssoc_prop :: FuncAssoc (Combine Int (Sum Int)) Int)
  putStr "Comp: "
  quickCheck (compAssoc_prop :: FuncAssoc (Comp (Sum Int)) (Sum Int))
  putStr "Validation: "
  quickCheck (semigroupAssoc_prop :: Assoc (Validation String (Sum Int)))
