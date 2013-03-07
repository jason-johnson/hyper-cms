module Hyper.Config.Config
(
  ConfigurationStore(..)
, Configuration(..)
)
where

data ConfigurationStore = Database | ConfigFile String
        deriving (Show)

data Configuration = Configuration {
          configurationStore            :: ConfigurationStore
        , configurationSinglePort       :: Bool
        , configurationPorts            :: [Int]
        , configurationSSlPort          :: Maybe Int
        , configurationMultiSite        :: Bool
        , configurationResourcePerReq   :: Bool
}
        deriving (Show)