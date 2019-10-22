{-# LANGUAGE ConstraintKinds, RankNTypes #-}

-- | Build systems and the properties they should ensure.
module Build (
    -- * Build
    Build,

    -- * Properties
    correctBuild, correctBuildIO
    ) where

import Build.Task
import Build.Task.Monad
import Build.Task.MonadIO
import Build.Store
import Build.Utilities
import Control.Monad.IO.Class
import Control.Monad 

-- | A build system takes a description of 'Tasks', a target key, and a store,
-- and computes a new store, where the key and its dependencies are up to date.
type Build c f i k v = Tasks c k v -> k -> Store i k v -> f (Store i k v)

-- | Given a description of @tasks@, an initial @store@, and a @result@ produced
-- by running a build system on a target @key@, this function returns 'True' if
-- the @result@ is a correct build outcome. Specifically:
-- * @result@ and @store@ must agree on the values of all inputs. In other words,
--   no inputs were corrupted during the build.
-- * @result@ is /consistent/ with the @tasks@, i.e. for every non-input key,
--   the result of recomputing its task matches the value stored in the @result@.
correctBuild :: (Ord k, Eq v) => Tasks Monad k v -> Store i k v -> Store i k v -> k -> Bool
correctBuild tasks store result = all correct . reachable deps
  where
    deps = maybe [] (\task -> snd $ trackPure task (flip getValue result)) . tasks
    correct k = case tasks k of
        Nothing   -> getValue k result == getValue k store
        Just task -> getValue k result == compute task result

correctBuildIO :: (Ord k, Eq v) => Tasks MonadIO k v -> Store i k v -> Store i k v -> k -> IO Bool
correctBuildIO tasks store result k = do
  ls <- reachableIO deps k
  allM correct ls
  where
    deps = maybe (return []) (\task -> do
                                 p <- trackDepsIO task (return . (flip getValue result))
                                 return $ snd p) . tasks
    correct k = case tasks k of
        Nothing   -> return (getValue k result == getValue k store)
        Just task -> do
          v <- computeIO task result
          return (getValue k result == v)
