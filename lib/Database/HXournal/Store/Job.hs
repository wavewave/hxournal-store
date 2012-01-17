module Database.HXournal.Store.Job where

import Control.Monad
import Data.UUID
import Database.HXournal.Store.Config 
import Data.Maybe
import Text.Xournal.Parse

import Application.XournalConvert.Convert.MakeSVG

import System.Directory 
import System.Exit 
import System.FilePath
import System.Process
import System.Posix.Files 


flipMaybe :: Maybe a -> b -> ( a-> b) -> b
flipMaybe m f s = maybe f s m  

startAdd :: String -> FilePath -> IO Int
startAdd uuidstr file = do 
    putStrLn "job started"
    mhxojstoreconf <- loadConfigFile >>= getHXournalStoreConfiguration

    flipMaybe mhxojstoreconf (error "cannot parse config file") 
      $ \hc -> checkUUIDNUpdate hc uuidstr 0 file generateAction

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
               



generateAction :: FilePath -> Int -> FilePath -> IO Int
generateAction uuiddir ver xojfile = do 
  let verstr = "v" ++ show ver 
  -- createDirectory uuiddir 
  setCurrentDirectory uuiddir 
  b <- doesDirectoryExist verstr
  if (not b) 
    then do 
      createDirectory verstr 
      let newvdir = uuiddir </> verstr -- "v0"
          newvdatadir = newvdir </> "data"
      setCurrentDirectory newvdir
      createDirectory "data"
      setCurrentDirectory newvdatadir
      let filename_wo_dir = takeFileName xojfile 
      copyFile xojfile (newvdatadir </> filename_wo_dir)
      xojcontent <- checkIfBinary xojfile >>= \b -> 
                      ifThenElse b (read_xojgz xojfile) (read_xournal xojfile)
      pages <- makeSVGFromXournal xojcontent xojfile newvdatadir
      let numpages = zip [1..] pages :: [(Int,String)] 
      createDirectory (newvdir </> "page")
      setCurrentDirectory (newvdir </> "page") 
      forM_ numpages $ \(num,name) -> 
        system $ "ln -s " ++ newvdatadir </> name ++ " " ++ show num ++ ".svg"

      setCurrentDirectory uuiddir 
      removelatestlink 

      system $ "ln -s " ++ verstr ++ " latest"
      system $ "ln -s " ++ (verstr  </> "data" </> filename_wo_dir) ++ " latest.xoj"
      return (length pages)
    else error $ "directory " ++ verstr ++ " exist"
      
removelatestlink :: IO () 
removelatestlink = do 
  b <- doesDirectoryExist "latest"
  when b $ removeLink "latest"
  b2 <- doesFileExist "latest.xoj"
  when b2 $ removeLink "latest.xoj"


{-       
generateAction :: FilePath -> FilePath -> IO Int
generateAction uuiddir xojfile = do 
  createDirectory uuiddir 
  setCurrentDirectory uuiddir 
  createDirectory "v0"
  let newvdir = uuiddir </> "v0"
      newvdatadir = newvdir </> "data"
  setCurrentDirectory newvdir
  createDirectory "data"
  setCurrentDirectory newvdatadir
  let filename_wo_dir = takeFileName xojfile 
  copyFile xojfile (newvdatadir </> filename_wo_dir)
  xojcontent <- checkIfBinary xojfile >>= \b -> 
                  ifThenElse b (read_xojgz xojfile) (read_xournal xojfile)

  pages <- makeSVGFromXournal xojcontent xojfile newvdatadir
  let numpages = zip [1..] pages :: [(Int,String)] 
  createDirectory (newvdir </> "page")
  setCurrentDirectory (newvdir </> "page") 
  forM_ numpages $ \(num,name) -> 
    system $ "ln -s " ++ newvdatadir </> name ++ " " ++ show num ++ ".svg"

  setCurrentDirectory uuiddir 
  system $ "ln -s v0 latest"
  system $ "ln -s " ++ ("v0" </> "data" </> filename_wo_dir) ++ " latest.xoj"
  return (length pages)
-}









