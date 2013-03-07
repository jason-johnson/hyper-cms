module Main where

import Hyper.Options
import Hyper.Config (loadConfiguration)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
        progName <- getProgName
        config <- getArgs >>= parse progName >>= loadConfiguration
        putStrLn $ show config