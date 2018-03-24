import           Moi
import           Test.Hspec

-- [1]
get :: Moi s s
get = Moi (\s -> (s, s))

-- [2]
put :: s -> Moi s ()
put s = Moi (\_ -> ((), s))

-- [3]
exec :: Moi s a -> s -> s
exec (Moi sa) s =
  let (_, s') = sa s
  in s'

-- [4]
eval :: Moi s a -> s -> a
eval (Moi sa) s =
  let (a, _) = sa s
  in a

-- [5]
modify :: (s -> s) -> Moi s ()
modify f = do
  s <- get
  put (f s)

testExercises :: IO ()
testExercises =
  hspec $ do
    describe "Chapter exercises" $ do
      it "eval" $ do
        runMoi get "curryIsAmaze" `shouldBe` ("curryIsAmaze", "curryIsAmaze")
      it "put" $ do runMoi (put "blah") "woot" `shouldBe` ((), "blah")
      it "exec" $ do
        exec (put "wilma") "daphne" `shouldBe` "wilma"
        exec get "scooby papu" `shouldBe` "scooby papu"
      it "eval" $ do
        eval get "bunnicula" `shouldBe` "bunnicula"
        eval get "stake a bunny" `shouldBe` "stake a bunny"
      it "modify" $ do
        runMoi (modify (+ 1)) 0 `shouldBe` ((), 1 :: Int)
        runMoi (modify (+ 1) >> modify (+ 1)) 0 `shouldBe` ((), 2 :: Int)
