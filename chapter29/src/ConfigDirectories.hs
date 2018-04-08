module ConfigDirectories where

import           Control.Monad.Trans.Maybe
import           Data.Ini
import           Data.List
import qualified Data.Map                  as M
import           System.Directory
import           Text.Trifecta.Parser

isIni :: FilePath -> Bool
isIni = isSuffixOf ".ini"

createConfigs :: [FilePath] -> MaybeT IO (M.Map FilePath Config)
createConfigs paths = do
  configs <- traverse createConfig paths
  let pairs = zip paths configs
  return $ M.fromList pairs

createConfig :: FilePath -> MaybeT IO Config
createConfig = MaybeT . parseFromFile parseIni

main :: IO ()
main = do
  dirs <- listDirectory "."
  let iniPaths = filter isIni dirs
  configs <- runMaybeT $ createConfigs iniPaths
  print configs
