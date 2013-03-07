module Hyper.Config.File
(
  loadConfiguration
)
where

import Hyper.Config.Types
import Hyper.Config.File.Parser


loadConfiguration :: FilePath -> Configuration -> IO Configuration
loadConfiguration file config =
        do
                res <- parseConfigFile file
                let sections = case res of
                                Left err -> error $ show err
                                Right s -> s
                return config