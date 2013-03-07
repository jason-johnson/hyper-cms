module Main where

import Hyper.Options
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
        progName <- getProgName
        config <- getArgs >>= parse progName
        putStrLn $ show config