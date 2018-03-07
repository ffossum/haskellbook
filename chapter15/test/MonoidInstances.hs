module MonoidInstances where

import           Data.Monoid
import qualified Data.Semigroup  as SG
import           Test.QuickCheck

-- [1]
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance SG.Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (SG.<>)

--[2]
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a1) (Identity a2) = Identity (a1 <> a2)

--[3]
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a1 b1) (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

-- [4]
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Arbitrary (BoolConj) where
  arbitrary = BoolConj <$> arbitrary

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj a) (BoolConj b) = BoolConj (a && b)

--[5]
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Arbitrary (BoolDisj) where
  arbitrary = BoolDisj <$> arbitrary

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

-- [6]
newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend (Combine f) (Combine g) = (Combine h)
    where
      h x = (f x) <> (g x)

instance Show (Combine a b) where
  show _ = "(*)"

combineAssoc ::
     (Eq b, Monoid b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineAssoc f g h x = left == right
  where
    left = unCombine (f <> (g <> h)) $ x
    right = unCombine ((f <> g) <> h) $ x

combineLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineLeftIdentity f x = left == right
  where
    left = unCombine (mempty <> f) $ x
    right = unCombine f $ x

combineRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineRightIdentity f x = left == right
  where
    left = unCombine (f <> mempty) $ x
    right = unCombine f $ x

-- [7]
newtype Comp a = Comp
  { unComp :: a -> a
  }

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

instance Monoid (Comp a) where
  mempty = (Comp id)
  mappend (Comp f) (Comp g) = Comp (f . g)

instance Show (Comp a) where
  show _ = "(*)"

compAssoc :: (Eq a) => (Comp a) -> (Comp a) -> (Comp a) -> a -> Bool
compAssoc f g h x = left == right
  where
    left = unComp (f <> (g <> h)) $ x
    right = unComp ((f <> g) <> h) $ x

compLeftIdentity :: (Eq a) => (Comp a) -> a -> Bool
compLeftIdentity f x = left == right
  where
    left = unComp (mempty <> f) $ x
    right = unComp f $ x

compRightIdentity :: (Eq a) => (Comp a) -> a -> Bool
compRightIdentity f x = left == right
  where
    left = unComp (f <> mempty) $ x
    right = unComp f $ x

-- [8]
newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem f) (Mem g) = Mem h
    where
      h s0 = (a1 <> a2, s2)
        where
          (a1, s1) = f s0
          (a2, s2) = g s1

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary

instance Show (Mem s a) where
  show _ = "(*)"

memAssoc ::
     (Eq s, Eq a, Monoid a) => (Mem s a) -> (Mem s a) -> (Mem s a) -> s -> Bool
memAssoc f g h x = left == right
  where
    left = runMem (f <> (g <> h)) $ x
    right = runMem ((f <> g) <> h) $ x

memLeftIdentity :: (Monoid a, Eq a, Eq s) => (Mem s a) -> s -> Bool
memLeftIdentity f x = left == right
  where
    left = runMem (mempty <> f) $ x
    right = runMem f $ x

memRightIdentity :: (Monoid a, Eq a, Eq s) => (Mem s a) -> s -> Bool
memRightIdentity f x = left == right
  where
    left = runMem (f <> mempty) $ x
    right = runMem f $ x

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type Assoc a = a -> a -> a -> Bool

type FuncAssoc f x = f -> f -> f -> x -> Bool

main :: IO ()
main = do
  putStrLn "Trivial:"
  quickCheck (monoidAssoc :: Assoc Trivial)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "Identity:"
  quickCheck (monoidAssoc :: Assoc (Identity String))
  quickCheck (monoidLeftIdentity :: (Identity String) -> Bool)
  quickCheck (monoidRightIdentity :: (Identity String) -> Bool)
  putStrLn "Two:"
  quickCheck (monoidAssoc :: Assoc (Two String [Int]))
  quickCheck (monoidLeftIdentity :: (Two String [Int]) -> Bool)
  quickCheck (monoidRightIdentity :: (Two String [Int]) -> Bool)
  putStrLn "BoolConj:"
  quickCheck (monoidAssoc :: Assoc BoolConj)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "BoolDisj:"
  quickCheck (monoidAssoc :: Assoc BoolDisj)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "Combine:"
  quickCheck (combineAssoc :: FuncAssoc (Combine Int String) Int)
  quickCheck (combineLeftIdentity :: (Combine Int String) -> Int -> Bool)
  quickCheck (combineRightIdentity :: (Combine Int String) -> Int -> Bool)
  putStrLn "Comp:"
  quickCheck (compAssoc :: FuncAssoc (Comp String) String)
  quickCheck (compLeftIdentity :: (Comp String) -> String -> Bool)
  quickCheck (compRightIdentity :: (Comp String) -> String -> Bool)
  putStrLn "Mem:"
  quickCheck (memAssoc :: FuncAssoc (Mem Int String) Int)
  quickCheck (memLeftIdentity :: (Mem Int String) -> Int -> Bool)
  quickCheck (memRightIdentity :: (Mem Int String) -> Int -> Bool)
