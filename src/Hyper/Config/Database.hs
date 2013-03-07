module Hyper.Config.Database
(
  loadConfiguration
)
where

import Hyper.Config.Types

loadConfiguration :: Configuration -> IO Configuration
loadConfiguration config = do
                                return config