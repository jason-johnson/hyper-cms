module Hyper.Options
(
  parse
) where

import           Data.ByteString.Char8 (hPutStrLn, pack)
import           System.Console.GetOpt
import           System.Exit           (ExitCode (ExitSuccess),
                                        ExitCode (ExitFailure), exitWith)
import           System.IO             (stderr)

import Paths_hyper_cms (version)
import Data.Version (showVersion)

import qualified Hyper.Constants as Const
import Hyper.Config.Config

data Flag
        = Config String
        | DatabaseConfig
        | SSL (Maybe String)
        | MultiSite
        | ResourcePerRequest
        | Version
        | Help
        deriving (Eq,Ord,Show)

flags :: [OptDescr Flag]
flags =
   [Option ['c'] ["config"]     (ReqArg Config "FILE")
        "Location of configuration file."
   ,Option ['d'] ["database"]   (NoArg DatabaseConfig)
        "Use Database for config.  If this is specified -c will be ignored."
   ,Option ['s'] ["ssl"]        (OptArg SSL "PORT")
        "Also run https."
   ,Option ['m'] ["multi"]       (NoArg MultiSite)
        "Run with multisite support."
   ,Option ['r'] []              (NoArg ResourcePerRequest)
        "Turns on ResourceTPerRequest in WARP."
   ,Option ['v'] ["version"]    (NoArg Version)
        "Displays version."
   ,Option ['h']    ["help"]    (NoArg Help)
        "Print this help message"
   ]

-- TODO: This should be returning some config structure, not flags.  When the parse is finished it should be over

parse :: String -> [String] -> IO Configuration
parse progName argv = case getOpt Permute flags argv of
    (args,ports,[]) -> handleArgs args $ if null ports then [show Const.defaultHTTPPort] else ports
    (_,_,errs)      -> do
        hPutStrLn stderr (pack $ concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where
        header = "Usage: " ++ progName ++ " [-cdsmrvh] [port ...]"
        handleArgs args ports | Help `elem` args = do
                                                        hPutStrLn stderr . pack $ usageInfo header flags
                                                        exitWith ExitSuccess
                              | Version `elem` args = do
                                                        hPutStrLn stderr . pack $ progName ++ ": " ++ showVersion version
                                                        exitWith ExitSuccess
                              | otherwise = return $ Configuration {
                                                                          configurationStore = confStore args
                                                                        , configurationSinglePort = length ports == 1
                                                                        , configurationPorts = map read ports
                                                                        , configurationSSlPort = setSSLPort args
                                                                        , configurationMultiSite = MultiSite `elem` args
                                                                        , configurationResourcePerReq = ResourcePerRequest `elem` args
                                                                        }
        confStore args  | DatabaseConfig `elem` args = Database
                        | otherwise = ConfigFile $ setConfigFile args

        setConfigFile [] = progName ++ ".config"
        setConfigFile (Config file:_) = file
        setConfigFile (_:args) = setConfigFile args

        setSSLPort [] = Nothing
        setSSLPort (SSL Nothing:_) = Just Const.defaultSSLPort
        setSSLPort (SSL (Just port):_) = Just $ read port
        setSSLPort (_:args) = setSSLPort args
