module Database.HXournal.Store.Job where

import Data.UUID
import Database.HXournal.Store.Config 
import Data.Maybe

flipMaybe :: Maybe a -> b -> ( a-> b) -> b
flipMaybe m f s = maybe f s m  

startAdd :: String -> FilePath -> IO () 
startAdd uuidstr file = do 
  putStrLn "job started"
  mhxojstoreconf <- loadConfigFile >>= getHXournalStoreConfiguration

  flipMaybe mhxojstoreconf (putStrLn "cannot parse config file") 
            $ \hxojstoreconf -> do 
                 putStrLn $ show (hxojstoreconf)
                 flipMaybe (fromString uuidstr) 
                   ( putStrLn "not uuid")
                   $ \uuid -> putStrLn . show $ uuid 

                
-- checkUUID :: String -> (Bool,UUID) 
-- checkUUID = maybe return (False do 
