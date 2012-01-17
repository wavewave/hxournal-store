module Database.HXournal.Store.Command where

import Database.HXournal.Store.ProgType
import Database.HXournal.Store.Job

commandLineProcess :: Hxournal_store -> IO ()
commandLineProcess (Add uuid file)  = do 
  putStrLn "add called"
  startAdd uuid file 
  return ()
commandLineProcess (NewVersion uuid nver file) = do 
  putStrLn "new version called"
  startNewVersion uuid nver file 
  return ()
