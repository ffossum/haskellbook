module RandomExample2 where

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           RandomExample
import           System.Random

rollDie :: State StdGen Die
rollDie =
  state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- rollsToGetTwenty :: StdGen -> Int
-- rollsToGetTwenty g = go 0 0 g
--   where
--     go :: Int -> Int -> StdGen -> Int
--     go sum count gen
--       | sum >= 20 = count
--       | otherwise =
--         let (die, nextGen) = randomR (1, 6) gen
--         in go (sum + die) (count + 1) nextGen
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = evalState (go 0 0)
  where
    go :: Int -> Int -> State StdGen Int
    go sum count
      | sum >= n = return count
      | otherwise = do
        die <- state $ randomR (1, 6)
        go (sum + die) (count + 1)

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = evalState (go 0 [])
  where
    go :: Int -> [Die] -> State StdGen (Int, [Die])
    go sum dies
      | sum >= n = return (sum, dies)
      | otherwise = do
        value <- state $ randomR (1, 6)
        let die = intToDie value
        go (sum + value) (die : dies)
