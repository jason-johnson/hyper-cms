module Hyper.Network.Wai.Handler.Multi
(
  app
)
where

{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 () -- Just for an orphan instance
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Hyper.Config.Types (SiteConfiguration)
import Hyper.Network.Wai.Handler

app :: Map String SiteConfiguration -> SiteConfiguration -> Application
app sites defaultSite request = dispatch site request
    where
        site = fromMaybe defaultSite . M.lookup server $ sites
        server = unpack . serverName $ request      -- TODO: We could convert server names into ByteStrings when we load the configuration since that is ok to be slow and this part isn't