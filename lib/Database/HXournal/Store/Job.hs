-----------------------------------------------------------------------------
-- |
-- Module      : Database.HXournal.Store.Job  
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Database.HXournal.Store.Job where

import Control.Monad
import Data.UUID
import Database.HXournal.Store.Config 
import Data.Maybe
import Text.Xournal.Parse.Enumerator 
import Application.XournalConvert.Convert.MakeSVG
import System.Directory 
import System.Exit 
import System.FilePath
import System.Process
import System.Posix.Files 

-- | 

flipMaybe :: Maybe a -> b -> ( a-> b) -> b
flipMaybe m f s = maybe f s m  

-- | 

startAdd :: String -> FilePath -> IO Int
startAdd uuidstr file = do 
    putStrLn "in startAdd"
    mhxojstoreconf <- loadConfigFile >>= getHXournalStoreConfiguration
    flipMaybe mhxojstoreconf (error "cannot parse config file") 
      $ \hc -> checkUUIDNUpdate hc uuidstr 0 file generateAction

-- | 

startNewVersion :: String -> Int -> FilePath -> IO Int
startNewVersion uuidstr nver file = do 
    putStrLn "job started"
    mhxojstoreconf <- loadConfigFile >>= getHXournalStoreConfiguration
    print uuidstr 
    print nver 
    print file 
    print mhxojstoreconf 
    flipMaybe mhxojstoreconf (error "cannot parse config file") 
      $ \hc -> checkUUIDNUpdate hc uuidstr nver file generateAction

-- | 
                
checkUUIDNUpdate :: HXournalStoreConfiguration 
                 -> String  -- ^ UUID string
                 -> Int  -- ^ version num
                 -> FilePath -- ^ xoj file
                 -> (FilePath -> Int -> FilePath -> IO a)
                 -> IO a
checkUUIDNUpdate hc uuidstr ver file action = do 
    flipMaybe (fromString uuidstr) (error "not uuid") $ \uuid -> do 
      print hc 
      putStrLn . show $ uuid   
      let base = hxournalstore_base hc
          uuiddir = base </> toString uuid
      setCurrentDirectory base 
      b1 <- doesFileExist file 
      b <- doesDirectoryExist uuiddir
      if (not b1) 
        then error "file doesn't exist or directory exist.. please check file or use modify"
        else if b 
          then action uuiddir ver file
          else do 
            createDirectory uuiddir
            action uuiddir ver file
               
-- | 

generateAction :: FilePath -> Int -> FilePath -> IO Int
generateAction uuiddir ver xojfile = do 
  putStrLn "in generateAction"
  let verstr = "v" ++ show ver 
  setCurrentDirectory uuiddir 
  b <- doesDirectoryExist verstr
  if (not b) 
    then do 
      createDirectory verstr 
      let newvdir = uuiddir </> verstr
          newvdatadir = newvdir </> "data"
      setCurrentDirectory newvdir
      createDirectory "data"
      setCurrentDirectory newvdatadir
      let filename_wo_dir = takeFileName xojfile 
      copyFile xojfile (newvdatadir </> filename_wo_dir)
      exojcontent <- checkIfBinary xojfile >>= \b -> 
                      ifThenElse b (parseXojGzFile xojfile) (parseXournal xojfile)
                     -- (read_xojgz xojfile) (read_xournal xojfile)
      case exojcontent of 
        Left err -> do putStrLn err
                       error "err"
        Right xojcontent -> do 
          putStrLn " before makeSVGFromXournal"
          pages <- makeSVGFromXournal xojcontent xojfile newvdatadir
          putStrLn " after makeSVGFromXournal"
          let numpages = zip [1..] pages :: [(Int,String)] 
          createDirectory (newvdir </> "page")
          setCurrentDirectory (newvdir </> "page") 
          forM_ numpages $ \(num,name) -> do
            putStrLn $ "ln -s " ++ "../data" </> name ++ " " ++ show num ++ ".svg"
            system $ "ln -s " ++ "../data" </> name ++ " " ++ show num ++ ".svg"
          -- 
          setCurrentDirectory uuiddir 
          removelatestlink 
          -- 
          system $ "ln -s " ++ verstr ++ " latest"
          system $ "ln -s " ++ (verstr  </> "data" </> filename_wo_dir) ++ " latest.xoj"
          return (length pages)
    else error $ "directory " ++ verstr ++ " exist"
      
-- | 

removelatestlink :: IO () 
removelatestlink = do 
  b <- doesDirectoryExist "latest"
  when b $ removeLink "latest"
  b2 <- doesFileExist "latest.xoj"
  when b2 $ removeLink "latest.xoj"



