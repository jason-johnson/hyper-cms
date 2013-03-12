module Hyper.Network.Wai.Handler.Single
(
  app
)
where

{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Data.ByteString.Lazy.Char8 () -- Just for an orphan instance

import Hyper.Config.Types (SiteConfiguration)
import Hyper.Network.Wai.Handler

app :: SiteConfiguration -> Application
app = dispatch