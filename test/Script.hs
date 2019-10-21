{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables #-}
module Script where

import Build.Task
import Build.Store
import Data.Foldable
import Data.List
import qualified Data.Set as Set
import Control.Monad.IO.Class
import System.IO.Unsafe

data Dep = Cmd CmdString | File FilePath deriving (Ord, Eq)
data Val = Ret Int | Content FileContent deriving (Ord, Eq)
instance Hashable Val where
  hash (Ret i) = Ret <$> hash i
  hash (Content fc) = Content <$> hash fc
  
type CmdString = String
type FileContent = String
type Script = [CmdString]
type CmdInfo = CmdString -> Set.Set FilePath

getDeps :: CmdInfo -> CmdString -> IO (Set.Set FilePath)
getDeps info cmd = return $ info cmd

runCmd :: CmdInfo -> CmdString -> IO (Int, Set.Set FilePath)
runCmd cmdInfo cmd = do
  putStrLn cmd
  deps <- getDeps cmdInfo cmd
  return (0,deps)

scriptTask :: Script -> CmdInfo -> Tasks MonadIO Dep Val
scriptTask script cmdInfo (File _)  = Nothing
scriptTask script cmdInfo (Cmd cmd) = case elemIndex cmd script of
  Nothing -> Nothing
  Just 0 -> Just $ Task $ \fetch -> do
    (rt, deps) <- liftIO $ runCmd cmdInfo cmd
    forM_ deps (fetch . File)
    return $ Ret rt
  Just i -> Just $ Task $ \fetch -> do
    fetch (Cmd  $ script !! (i-1))
    (rt, deps) <- liftIO $ runCmd cmdInfo cmd
    forM_ deps (fetch . File)
    return $ Ret rt                
    
