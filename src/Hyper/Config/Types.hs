module Hyper.Config.Types
(
  SiteConfiguration(..)
, ConfigurationStore(..)
, Configuration(..)
)
where

import Data.Map (Map)

data SiteConfiguration = SiteConfiguration {
          root                            :: FilePath           -- TODO: Routing also goes in here, cache directory and so on
}
          deriving (Show)

data ConfigurationStore = Database | ConfigFile String
        deriving (Show)

data Configuration = Configuration {
          configurationStore            :: ConfigurationStore
        , configurationSinglePort       :: Bool
        , configurationPorts            :: [Int]
        , configurationSSlPort          :: Maybe Int
        , configurationMultiSite        :: Bool
        , configurationResourcePerReq   :: Bool
        , configurationDefaultSite      :: SiteConfiguration
        , configurationSites            :: Map String SiteConfiguration -- TODO: The key should be using some kind of Host name that Yesod uses.
}
        deriving (Show)