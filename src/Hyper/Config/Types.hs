module Hyper.Config.Types
(
  SiteConfiguration(..)
, ConfigurationStore(..)
, Configuration(..)
)
where

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
        , configurationSites            :: [(String, SiteConfiguration)]        -- TODO: This should be using some kind of Host name that Yesod uses.  Also we should probably be using a map here (then again maybe not since this layout is easier in the case of a single site)
}
        deriving (Show)