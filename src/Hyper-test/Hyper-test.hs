module Main where

import Test.QuickCheck
import Text.Printf

main :: IO ()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

-- reversing twice a finite list, is the same as identity
prop_reversereverse :: [Int] -> Bool
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]

-- and add this to the tests list
tests :: [([Char], IO ())]
tests  = [("reverse.reverse/id", quickCheck prop_reversereverse)]