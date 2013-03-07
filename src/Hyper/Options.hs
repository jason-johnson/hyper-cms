module Hyper.Options
(
parse,
Flag(..)
) where

import           Data.ByteString.Char8 (hPutStrLn, pack)
import           Data.List             (nub)
import           System.Console.GetOpt
import           System.Exit           (ExitCode (ExitSuccess),
                                        ExitCode (ExitFailure), exitWith)
import           System.IO             (stderr)

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

parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute flags argv of
    (args,ports,[]) -> do
        handleArgs args $ if null ports then ["80"] else ports
    (_,_,errs)      -> do
        hPutStrLn stderr (pack $ concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where
        header = "Usage: hyper [-cdsmrvh] [port ...]"
        handleArgs args ports | Help `elem` args = do
                                                        hPutStrLn stderr (pack $ usageInfo header flags)
                                                        exitWith ExitSuccess
                              | Version `elem` args = do
                                                        hPutStrLn stderr $ pack "Version: 01"
                                                        exitWith ExitSuccess
                              | otherwise = return (nub . correct $ args, ports)
        correct args | DatabaseConfig `elem` args = filter noConfigFile args
                     | otherwise = args
        noConfigFile (Config _) = False
        noConfigFile _ = True
        
