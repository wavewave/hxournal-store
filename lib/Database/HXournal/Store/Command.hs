module Database.HXournal.Store.Command where

import Database.HXournal.Store.ProgType
import Database.HXournal.Store.Job

commandLineProcess :: Hxournal_store -> IO ()
commandLineProcess (Add uuid file)  = do 
  putStrLn "test called"
  startAdd uuid file 
  return ()
