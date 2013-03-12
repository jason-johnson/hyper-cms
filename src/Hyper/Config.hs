module Hyper.Config
(
  loadConfiguration
, ConfigurationStore(..)
, Configuration(..)
, SiteConfiguration(..)
)
where

import           Hyper.Config.Types

import qualified Hyper.Config.Database as DB
import qualified Hyper.Config.File     as F

loadConfiguration :: Configuration -> IO Configuration
loadConfiguration config = loadConfigStore $ configurationStore config
    where
        loadConfigStore Database = DB.loadConfiguration config
        loadConfigStore (ConfigFile file) = F.loadConfiguration file config
