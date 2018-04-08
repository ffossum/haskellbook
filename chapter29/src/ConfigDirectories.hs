module ConfigDirectories where

import           Data.Ini
import           Data.List
import qualified Data.Map             as M
import           System.Directory
import           Text.Trifecta.Parser

isIni :: FilePath -> Bool
isIni = isSuffixOf ".ini"

createConfigs :: [FilePath] -> IO (Maybe (M.Map FilePath Config))
createConfigs paths = do
  configs <- sequence <$> traverse createConfig paths
  let pairs = (zip paths) <$> configs
  return $ M.fromList <$> pairs

createConfig :: FilePath -> IO (Maybe Config)
createConfig = parseFromFile parseIni

main :: IO ()
main = do
  dirs <- listDirectory "."
  let iniPaths = filter isIni dirs
  configs <- createConfigs iniPaths
  print configs
