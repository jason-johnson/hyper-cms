module Main where

import           Hyper.Config       (loadConfiguration)
import           Hyper.Options
import           Hyper.Config.Types
import           System.Environment (getArgs, getProgName)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsResourceTPerRequest, settingsPort)

import qualified Hyper.Network.Wai.Handler.Single as S
import qualified Hyper.Network.Wai.Handler.Multi as M

main :: IO ()
main = do
    progName <- getProgName
    config <- getArgs >>= parse progName >>= loadConfiguration
    start (configurationSinglePort config) (configurationPorts config) config

start :: Bool -> [Int] -> Configuration -> IO ()
start True (port:_) config = do
    putStrLn $ "http://localhost:" ++ show port ++ "/"
    runSettings settings $ app multi
        where
            app True    = M.app sites defaultSite
            app _       = S.app defaultSite
            
            multi       = configurationMultiSite config
            sites       = configurationSites config
            defaultSite = configurationDefaultSite config
            rpr         = configurationResourcePerReq config
            settings    = defaultSettings { settingsResourceTPerRequest = rpr, settingsPort = port }
start _ ports config = do
    error $ "Support for multiple ports currently disabled.  Ports: " ++ show ports ++ ", config: " ++ show config