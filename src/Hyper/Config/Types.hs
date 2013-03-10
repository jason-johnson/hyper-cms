module Hyper.Config.Types
(
  SiteConfiguration(..)
, ConfigurationStore(..)
, Configuration(..)
, siteConfigurationRoot
, siteConfigurationIndex
, siteConfigurationPassthrough
, siteConfigurationCacheDirectory
)
where

import           Data.Map    (Map, unionWith)
import           Data.Monoid

-- TODO: These names are too short, they need the site config prefix or they will clash with something
data SiteConfiguration = SiteConfiguration {
          root           :: FilePath           -- TODO: For now we'll simply do file routing but we might let clients override this later
        , index          :: FilePath
        , passthrough    :: [String]
        , cacheDirectory :: FilePath
}
          deriving (Eq, Show)

data ConfigurationStore = Database | ConfigFile String
        deriving (Eq, Show)

data Configuration = Configuration {
          configurationStore          :: ConfigurationStore
        , configurationSinglePort     :: Bool
        , configurationPorts          :: [Int]
        , configurationSSlPort        :: Maybe Int
        , configurationMultiSite      :: Bool
        , configurationResourcePerReq :: Bool
        , configurationDefaultSite    :: SiteConfiguration
        , configurationSites          :: Map String SiteConfiguration -- TODO: The key should be using some kind of Host name that Yesod uses.
}
        deriving (Eq, Show)

instance Monoid SiteConfiguration where
    a `mappend` b = SiteConfiguration (root a           `mset` root b)
                                      (index a          `mset` index b)
                                      (passthrough a    `mset` passthrough b)
                                      (cacheDirectory a `mset` cacheDirectory b)
    mempty = SiteConfiguration mempty mempty mempty mempty

instance Monoid ConfigurationStore where
    mappend = mset
    mempty = ConfigFile mempty

instance Monoid Configuration where
    a `mappend` b = Configuration (configurationStore a `mappend` configurationStore b)
                                  (configurationSinglePort b)
                                  (configurationPorts a `mset` configurationPorts b)
                                  (configurationSSlPort b)
                                  (configurationMultiSite b)
                                  (configurationResourcePerReq b)
                                  (configurationDefaultSite a `mappend` configurationDefaultSite b)
                                  (unionWith mset (configurationSites a) (configurationSites b))
    mempty = Configuration mempty False mempty Nothing False True mempty mempty

mset :: (Eq a, Monoid a) => a -> a -> a
mset a b | b == mempty = a
         | otherwise = b

-- TODO: This stuff should be moved to Hyper.Config.Types.Helpers
siteConfigurationRoot :: FilePath -> SiteConfiguration
siteConfigurationRoot r = SiteConfiguration r mempty mempty mempty

siteConfigurationIndex :: FilePath -> SiteConfiguration
siteConfigurationIndex i = SiteConfiguration mempty i mempty mempty

siteConfigurationPassthrough :: [String] -> SiteConfiguration
siteConfigurationPassthrough ss = SiteConfiguration mempty mempty ss mempty

siteConfigurationCacheDirectory :: FilePath -> SiteConfiguration
siteConfigurationCacheDirectory c = SiteConfiguration mempty mempty mempty c