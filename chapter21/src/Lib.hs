module Lib where

import           Data.Functor.Const
import           Data.Functor.Identity

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Query =
  Query

data SomeObj =
  SomeObj

data IoOnlyObj =
  IoOnlyObj

data Err =
  Err

-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the
-- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer",
-- that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)

fmap' :: (Traversable t) => (a -> b) -> t a -> t b
fmap' f = runIdentity . traverse (Identity . f)

foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = getConst . traverse (Const . f)
