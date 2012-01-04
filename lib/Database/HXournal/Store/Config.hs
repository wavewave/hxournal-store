{-# LANGUAGE OverloadedStrings #-}

module Database.HXournal.Store.Config where

import Data.Configurator as C
import Data.Configurator.Types
import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Environment
import System.Directory
import System.FilePath

data HXournalStoreConfiguration = 
       HXournalStoreConfiguration { hxournalstore_base :: FilePath } 
  deriving (Show)

emptyConfigString :: String
emptyConfigString = "# hxournal-store configuration\n# base = \"blah\"\n"

loadConfigFile :: IO Config 
loadConfigFile = do 
    homepath <- getEnv "HOME"
    let dothxournalstore = homepath </> ".hxournal-store"
    doesFileExist dothxournalstore >>= \b -> when (not b) $ do 
      writeFile dothxournalstore emptyConfigString
      threadDelay 1000000
    config <- load [Required "$(HOME)/.hxournal-store"] 
    return config 

getHXournalStoreConfiguration :: Config -> IO (Maybe HXournalStoreConfiguration)
getHXournalStoreConfiguration c = do 
    mbase <- C.lookup c "base"
    return (HXournalStoreConfiguration <$> mbase )
     