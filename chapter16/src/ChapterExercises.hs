{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

import           GHC.Arr

import           FunctorInstances
import           Test.Hspec
import           Test.QuickCheck

-- Determine if a valid Functor can be written for the datatype provided.
-- [1]
data Bool
  = False
  | True

-- No, impossible because Bool has kind *. A type must have kind * -> * to have a valid Functor.
-- [2]
data BoolAndSomethingElse a
  = False' a
  | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)  = True' (f a)

-- [3]
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

-- Yes, this is Maybe
-- [4]     Use the kinds to guide you on this one, don’t get too hung up on the details.
newtype Mu f = InF
  { outF :: f (Mu f)
  }

-- Mu does not have a valid Functor. Since f has kind * -> *, Mu has kind (* -> *) -> *,
-- which does not match the required kind * -> *. If we apply the f, like:
-- instance Functor (Mu f) where
-- we "go too far", since (Mu f) has kind *, which also does not match the required * -> *.
data D =
  D (Array Word Word)
    Int
    Int

-- D has kind *, and cannot have a Functor.
-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.
-- [1]
data Sum b a
  = First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap _ (Second b) = Second b

-- [2]
data Company a c b
  = DeepBlue a
             c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a
  = L a
      b
      a
  | R b
      a
      b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.
-- [1]
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a)  = Desk a
  fmap _ Finance   = Finance

-- [2]
data K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- [3]
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

-- [4]
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- [5]
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (f <$> fa)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary

-- [6]
data Parappa f g a =
  DaWrappa (f a)
           (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

-- [7]
data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (f <$> gb)

-- [8]
data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) =>
         Arbitrary (Notorious g o a t) where
  arbitrary = do
    go <- arbitrary
    ga <- arbitrary
    gt <- arbitrary
    return (Notorious go ga gt)

-- [9]
data List a
  = Nil
  | Cons a
         (List a)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- [10]
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (f <$> gl1) (f <$> gl2) (f <$> gl3)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = frequency [(1, noGoat), (2, oneGoat), (1, moreGoats)]
    where
      noGoat = return NoGoat
      oneGoat = OneGoat <$> arbitrary
      moreGoats = do
        gl1 <- arbitrary
        gl2 <- arbitrary
        gl3 <- arbitrary
        return (MoreGoats gl1 gl2 gl3)

-- [11]
data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt          = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read g)      = Read (f . g)

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary = oneof [h, p, r]
    where
      h = return Halt
      p = do
        str <- arbitrary
        a <- arbitrary
        return (Print str a)
      r = Read <$> arbitrary

testExercises :: IO ()
testExercises =
  hspec $ do
    describe "Functor instances" $ do
      describe "LiftItOut" $ do
        it "identity" $
          property $ (functorIdentity :: IdentityTest (LiftItOut Maybe))
        it "compose" $
          property $ (functorCompose' :: ComposeTest (LiftItOut Maybe))
      describe "Notorious" $ do
        it "identity" $
          property $ (functorIdentity :: IdentityTest (Notorious Maybe Int Int))
        it "compose" $
          property $ (functorCompose' :: ComposeTest (Notorious Maybe Int Int))
      describe "GoatLord" $ do
        it "identity" $ property $ (functorIdentity :: IdentityTest GoatLord)
        it "compose" $ property $ (functorCompose' :: ComposeTest GoatLord)
