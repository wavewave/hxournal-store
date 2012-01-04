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

flipMaybe :: Maybe a -> b -> ( a-> b) -> b
flipMaybe m f s = maybe f s m  

startAdd :: String -> FilePath -> IO () 
startAdd uuidstr file = do 
    putStrLn "job started"
    mhxojstoreconf <- loadConfigFile >>= getHXournalStoreConfiguration

    flipMaybe mhxojstoreconf (putStrLn "cannot parse config file") 
      $ \hc -> checkUUIDNCreate hc uuidstr file generateAction

                
checkUUIDNCreate :: HXournalStoreConfiguration 
                 -> String  -- ^ UUID string
                 -> FilePath -- ^ xoj file
                 -> (FilePath -> FilePath -> IO ())
                 -> IO ()
checkUUIDNCreate hc uuidstr file action = do 
    flipMaybe (fromString uuidstr) (putStrLn "not uuid") $ \uuid -> do 
      print hc 
      putStrLn . show $ uuid   
      let base = hxournalstore_base hc
          uuiddir = base </> toString uuid
      setCurrentDirectory base 
      b1 <- doesFileExist file 
      b <- doesDirectoryExist uuiddir
      if (not b1 || b) 
        then putStrLn "file doesn't exist or directory exist.. please check file or use modify"
        else action uuiddir file
      
       
generateAction :: FilePath -> FilePath -> IO ()
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


  return () 


  -- excode <- system $ "xournal-convert makesvg " ++ xojfile 

--  case excode of 
--    ExitFailure n -> error $ "exit failure with " ++ show n 
--     ExitSuccess -> do 
 


-- checkUUID :: String -> (Bool,UUID) 
-- checkUUID = maybe return (False do 








