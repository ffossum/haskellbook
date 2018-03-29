{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config = Config
  -- that's one, one click!
  -- two...two clicks!
  -- Three BEAUTIFUL clicks! ah ah ahhhh
  { counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let oldValue = fromMaybe 0 (M.lookup k m)
      newValue = oldValue + 1
      newMap = M.insert k newValue m
  in (newMap, newValue)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift $ ask
    let key' = mappend (prefix config) unprefixed
    newInteger <- liftIO $ atomicModifyIORef' (counts config) (bumpBoomp key')
    html $
      mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR :: (ReaderT Config IO a -> IO a)
      runR m = runReaderT m config
  scottyT 3000 runR app
