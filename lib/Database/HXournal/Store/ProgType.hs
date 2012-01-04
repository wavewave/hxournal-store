{-# LANGUAGE DeriveDataTypeable #-}

module Database.HXournal.Store.ProgType where 

import System.FilePath
import System.Console.CmdArgs

data Hxournal_store = Add { uuidstr :: String 
                          , xojfile :: FilePath
                          } 
              deriving (Show,Data,Typeable)

add :: Hxournal_store
add = Add { uuidstr = def &= typ "UUID" &= argPos 0 
          , xojfile = def &= typ "FILE" &= argPos 1
          }  

mode = modes [add]

