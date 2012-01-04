module Main where

import System.Console.CmdArgs

import Database.HXournal.Store.ProgType
import Database.HXournal.Store.Command

main :: IO () 
main = do 
  putStrLn "hxournal-store"
  param <- cmdArgs mode

  commandLineProcess param