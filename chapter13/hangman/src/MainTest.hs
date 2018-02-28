module MainTest where

import           Main
import           Test.Hspec

runTest :: IO ()
runTest =
  hspec $ do
    describe "show" $ do
      it "shows guessed characters" $ do
        show (Puzzle {word = "abcd", guessed = "ab"}) `shouldBe`
          "a b _ _ Guessed so far: ab"
    describe "fillInCharacter" $ do
      let initialState = freshPuzzle "word"
      it "fills in guessed character" $ do
        let state = fillInCharacter initialState 'w'
        state `shouldBe` Puzzle {word = "word", guessed = "w"}
        wrongGuesses state `shouldBe` 0
      it "wrong guess increases wrong guesses" $ do
        let state = fillInCharacter initialState 'x'
        wrongGuesses state `shouldBe` 1
    describe "handleGuess" $ do
      let initialState = freshPuzzle "word"
      it "correct guess is filled in" $ do
        next <- handleGuess initialState 'w'
        next `shouldBe` Puzzle {word = "word", guessed = "w"}
      it "already guessed character is not filled in" $ do
        state2 <- handleGuess initialState 'w'
        state3 <- handleGuess state2 'w'
        state3 `shouldBe` state2
      it "wrong guess is filled in" $ do
        next <- handleGuess initialState 'x'
        next `shouldBe` Puzzle {word = "word", guessed = "x"}
