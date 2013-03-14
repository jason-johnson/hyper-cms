module Hyper.Network.Wai.Handler
(
  dispatch
, isPassthrough
, dispatchStaticFile
)
where

import           Data.ByteString.Char8      (ByteString, append, pack, unpack)
import           Data.ByteString.Lazy.Char8 ()
import           Network.HTTP.Types
import           Network.Wai
import           System.FilePath            (dropTrailingPathSeparator,
                                             takeExtension, (</>))

import           Data.String                (IsString)
import           Hyper.Config.Types         (SiteConfiguration)
import qualified Hyper.Config.Types         as T

isPassthrough :: FilePath -> SiteConfiguration -> Bool
isPassthrough file = any (== (tail . takeExtension) file) . T.passthrough

dispatchStaticFile :: String -> String -> Response
dispatchStaticFile root file = ResponseFile status200 [textContentType file] (root <\> file) Nothing

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

textContentType :: IsString t => FilePath -> (t, ByteString)
textContentType file = ("Content-Type", append "text/" t)
    where
        t = pack . tail . takeExtension $ file
