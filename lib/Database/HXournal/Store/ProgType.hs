{-# LANGUAGE DeriveDataTypeable #-}

module Database.HXournal.Store.ProgType where 

import System.FilePath
import System.Console.CmdArgs

data Hxournal_store = Add { uuidstr :: String 
                          , xojfile :: FilePath
                          } 
                    | NewVersion { uuidstr :: String 
                                 , versionnum :: Int 
                                 , xojfile :: FilePath }  
              deriving (Show,Data,Typeable)

add :: Hxournal_store
add = Add { uuidstr = def &= typ "UUID" &= argPos 0 
          , xojfile = def &= typ "FILE" &= argPos 1
          }  

newversion :: Hxournal_store
newversion = NewVersion { uuidstr = def &= typ "UUID" &= argPos 0 
                        , versionnum = def &= typ "Version" &= argPos 1
                        , xojfile = def &= typ "FILE" &= argPos 2 }

mode = modes [add, newversion]

