module Hyper.Options
(
  parse
)
where

import           Data.ByteString.Char8 (hPutStrLn, pack)
import qualified Data.Map              as M
import           System.Console.GetOpt
import           System.Exit           (ExitCode (ExitSuccess),
                                        ExitCode (ExitFailure), exitWith)
import           System.IO             (stderr)

import           Data.Version          (showVersion)
import           Paths_hyper_cms       (version)

import           Hyper.Config
import qualified Hyper.Constants       as Const

data Flag   = Config String
            | DatabaseConfig
            | Version
            | Help
            deriving (Eq,Ord,Show)

flags :: [OptDescr Flag]
flags = [
      Option ['c'] ["config"]       (ReqArg Config "FILE")      "Location of configuration file."
    , Option ['d'] ["database"]     (NoArg DatabaseConfig)      "Use Database for config.  If this is specified -c will be ignored."
    , Option ['v'] ["version"]      (NoArg Version)             "Displays version."
    , Option ['h'] ["help"]         (NoArg Help)                "Print this help message"
    ]

parse :: String -> [String] -> IO Configuration
parse progName argv = case getOpt Permute flags argv of
    (args,[],[]) -> handleArgs args
    (_,extra,[]) -> crash "unexpected arguments: " $ extra ++ ["\n"]
    (_,_,errs)   -> crash "" errs
    where
        header = "Usage: " ++ progName ++ " [-cdvh]"
        crash msg msgs = do
            hPutStrLn stderr $ pack $ msg ++ concat msgs ++ usageInfo header flags
            exitWith (ExitFailure 1)
        handleArgs args
            | Help `elem` args = do
                hPutStrLn stderr . pack $ usageInfo header flags
                exitWith ExitSuccess
            | Version `elem` args = do
                hPutStrLn stderr . pack $ progName ++ ": " ++ showVersion version
                exitWith ExitSuccess
            | otherwise = return $
                Configuration {
                      configurationStore            = confStore args
                    , configurationSinglePort       = True
                    , configurationPorts            = [Const.defaultHTTPPort]
                    , configurationSSlPort          = Nothing
                    , configurationMultiSite        = False
                    , configurationResourcePerReq   = True
                    , configurationSites            = M.empty
                    , configurationDefaultSite      =
                        SiteConfiguration {
                              root = "~"
                            , index = "index.html"
                            , passthrough = [ "html", "css", "js" ]
                            , cacheDirectory = "cache" }
                    }

        confStore args  | DatabaseConfig `elem` args = Database
                        | otherwise = ConfigFile $ setConfigFile args

        setConfigFile [] = progName ++ ".config"
        setConfigFile (Config file:_) = file
        setConfigFile (_:args) = setConfigFile args