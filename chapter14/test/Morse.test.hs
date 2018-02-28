import qualified Data.Map            as M
import           Morse
import           Test.QuickCheck
import           Test.QuickCheck.Gen (oneof)

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen f
  where
    f c = ((charToMorse c) >>= morseToChar) == Just c

-- main :: IO ()
-- main = quickCheck prop_thereAndBackAgain
data Trivial =
  Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = do
  sample sumGenCharIntFirst

data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

data Pair a b =
  Pair a
       b
  deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls
