module Hyper.Network.Wai.Handler
(
  dispatch
, isPassthrough
, dispatchStaticFile
)
where

import Network.Wai
import Network.HTTP.Types
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 () -- Just for an orphan instance
import           System.FilePath     (takeExtension, dropTrailingPathSeparator, (</>))

import Hyper.Config.Types (SiteConfiguration)
import qualified Hyper.Config.Types as T

isPassthrough :: FilePath -> SiteConfiguration -> Bool
isPassthrough file = any (== (tail . takeExtension) file) . T.passthrough

dispatchStaticFile :: String -> String -> Response
dispatchStaticFile root file = ResponseFile status200 [("Content-Type", "text/html")] (root <\> file) Nothing

dispatchPlainFile :: String -> String -> Response
dispatchPlainFile root file = ResponseFile status200 [("Content-Type", "text/plain")] (root <\> file) Nothing

dispatch :: SiteConfiguration -> Application
dispatch site request = case rawPathInfo request of
    "/" -> return index'
    path   -> return . handle . unpack $ path
    where
        index' = handle idx
        handle path
            | isPassthrough path site = dispatchStaticFile root path
            | otherwise = dispatchPlainFile root path
        root = T.root site
        idx = T.index site

(<\>) :: FilePath -> FilePath -> FilePath
a <\> b@('/':_) = dropTrailingPathSeparator a ++ b
a <\> b = a </> b