{-# LANGUAGE DeriveDataTypeable #-}

module Database.HXournal.Store.ProgType where 

import System.Console.CmdArgs

data Hxournal_store = Test 
              deriving (Show,Data,Typeable)

test :: Hxournal_store
test = Test 

mode = modes [test]

