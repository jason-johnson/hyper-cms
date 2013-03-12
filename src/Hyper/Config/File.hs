module Hyper.Config.File
(
  loadConfiguration
)
where

import           Hyper.Config.File.Parser
import           Hyper.Config.Types


loadConfiguration :: FilePath -> Configuration -> IO Configuration
loadConfiguration file config = do
    res <- parseConfigFile file config
    return $ case res of
        Left err -> error $ show err
        Right (_,s) -> s
