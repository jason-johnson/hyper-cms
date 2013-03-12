module Main where

import           Hyper.Config       (loadConfiguration)
import           Hyper.Options
import           System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    progName <- getProgName
    config <- getArgs >>= parse progName >>= loadConfiguration
    putStrLn $ show config
