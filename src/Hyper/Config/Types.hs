module Hyper.Config.Types
(
  SiteConfiguration(..)
, ConfigurationStore(..)
, Configuration(..)
)
where

import Data.Map (Map)

data SiteConfiguration = SiteConfiguration {
          root                          :: FilePath           -- TODO: For now we'll simply do file routing but we might let clients override this later
        , index                         :: FilePath
        , passthrough                   :: [String]
        , cacheDirectory                :: FilePath
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