import           Data.Char
import           Data.List       (sort)
import           Test.QuickCheck

half x = x / 2

halfIdentity = (* 2) . half

prop_halfIdentity = forAll (arbitrary :: Gen Double) (\d -> d == halfIdentity d)

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_listReverseIndentity
  quickCheck prop_foldrCons
  quickCheck prop_foldrConcat
  quickCheck prop_nList
  quickCheck prop_readShowInteger
  quickCheck prop_readShowString
  quickCheck prop_capitalizeWordIdempotence
  quickCheck prop_sortIdempotence

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, _)       = (Just y, x >= y)

prop_listOrdered = forAll (arbitrary :: Gen [Int]) (listOrdered . sort)

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = f x (f y z) == f (f x y) z

commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutative f x y = f x y == f y x

prop_plusAssociative =
  forAll
    (arbitrary :: Gen (Integer, Integer, Integer))
    (\(x, y, z) -> associative (+) x y z)

prop_plusCommutative =
  forAll (arbitrary :: Gen (Integer, Integer)) (\(x, y) -> commutative (+) x y)

prop_multAssociative =
  forAll
    (arbitrary :: Gen (Integer, Integer, Integer))
    (\(x, y, z) -> associative (*) x y z)

prop_multCommutative =
  forAll (arbitrary :: Gen (Integer, Integer)) (\(x, y) -> commutative (*) x y)

prop_quotRem = forAll divModGen (\(x, y) -> (quot x y) * y + (rem x y) == x)

prop_divMod = forAll divModGen (\(x, y) -> (div x y) * y + (mod x y) == x)

nonZeroIntegerGen :: Gen Integer
nonZeroIntegerGen = do
  a <- arbitrary
  if a /= 0
    then return a
    else return $ a + 1

divModGen :: Gen (Integer, Integer)
divModGen = do
  n <- arbitrary
  d <- nonZeroIntegerGen
  return (n, d)

prop_listReverseIndentity =
  forAll (arbitrary :: Gen [Int]) (\xs -> xs == (reverse (reverse xs)))

prop_foldrCons =
  forAll
    (arbitrary :: Gen ([Int], [Int]))
    (\(as, bs) -> foldr (:) as bs == as ++ bs)

prop_foldrConcat =
  forAll (arbitrary :: Gen [[Int]]) (\as -> foldr (++) [] as == concat as)

nListGen :: Gen (Int, [Int])
nListGen = do
  xs <- suchThat (arbitrary :: Gen [Int]) ((> 0) . length)
  n <- suchThat (arbitrary :: Gen Int) (\y -> y >= 0 && y < length xs)
  return $ (n, xs)

prop_nList = forAll nListGen (\(n, xs) -> length (take n xs) == n)

prop_readShowInteger =
  forAll (arbitrary :: Gen Integer) (\x -> (read (show x)) == x)

prop_readShowString =
  forAll (arbitrary :: Gen String) (\x -> (read (show x)) == x)

twice f = f . f

fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (a:as) = (toUpper a) : (map toLower as)

prop_capitalizeWordIdempotence = forAll (arbitrary :: Gen String) f
  where
    f x =
      (capitalizeWord x == twice capitalizeWord x) &&
      (capitalizeWord x == fourTimes capitalizeWord x)

prop_sortIdempotence = forAll (arbitrary :: Gen [Int]) f'
  where
    f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = elements [Fulse, Frue]

genFoolEqual :: Gen Fool
genFoolEqual = arbitrary

genFoolTwoThirdsFulse :: Gen Fool
genFoolTwoThirdsFulse = frequency [(2, pure Fulse), (1, pure Frue)]