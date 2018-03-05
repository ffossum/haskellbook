import           Data.List.NonEmpty as N
import           Data.Monoid        as M
import qualified Data.Semigroup     as S
import           Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

main :: IO ()
main = do
  quickCheck
    (monoidAssoc :: (First' String) -> (First' String) -> (First' String) -> Bool)
  quickCheck (monoidLeftIdentity :: (First' String) -> Bool)
  quickCheck (monoidRightIdentity :: (First' String) -> Bool)

newtype First' a = First'
  { getFirst' :: Maybe a
  } deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return $ First' a

instance Monoid (First' a) where
  mempty = First' Nothing
  mappend a@(First' (Just _)) _ = a
  mappend _ b                   = b

l :: NonEmpty Int
l = 1 :| [2, 3, 4, 5]

-- Prefix, works.
data P =
  Prefix Int
         String
  deriving (Eq, Show)

-- Infix, works.
data Q =
  Int :!!: String
  deriving (Eq, Show)
