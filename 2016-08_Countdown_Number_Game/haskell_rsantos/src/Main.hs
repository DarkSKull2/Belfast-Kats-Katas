module Main where

import System.IO
import System.Environment

import Lib

main :: IO()
main = do 
        arg1:arg2:[] <- getArgs
        let 
         nums = (read arg1) :: [Int]
         goal = (read arg2) :: Int
        --in 
        putStrLn ( startPointResult nums goal )

