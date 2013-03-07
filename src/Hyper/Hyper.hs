module Main where

import Hyper.Options
import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
    (args, ports) <- getArgs >>= parse
    when (DatabaseConfig `elem` args) $ putStrLn "Got Database Config!"
    when (MultiSite `elem` args) $ putStrLn "MultiSite enabled!"
    when (ResourcePerRequest `elem` args) $ putStrLn "ResourcePerRequest enabled!"
    showSSL args
    showConfigFile args
    putStrLn $ "Ports are: " ++ concat ports
    putStrLn "And we're done!"
        where
                showSSL [] = putStrLn "No SSL options set"
                showSSL (SSL Nothing:_) = putStrLn "SSL option set with default port"
                showSSL (SSL (Just port):_) = putStrLn $ "SSL option set with port: " ++ show port
                showSSL (_:args) = showSSL args
                
                showConfigFile [] = putStrLn "No config file option"
                showConfigFile (Config file:_) = putStrLn $ "Config file configured at: " ++ file
                showConfigFile (_:args) = showConfigFile args