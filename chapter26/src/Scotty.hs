{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Text.Lazy             (Text)
import           Web.Scotty

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k =
  ExceptT $
  rescue
    (Right <$> param k)
    (const (return (Left $ "The key: " ++ show k ++ " was missing!")))

main =
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      a <-
        runExceptT $ do
          a <- param' "1"
          liftIO $ print (a :: Int)
          return a
      let a' = either (const 0) id a
      liftIO $ print (a' :: Int)
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
