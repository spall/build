{-# LANGUAGE ScopedTypeVariables #-}

module Build.Task.MonadIO (trackIO, trackDepsIO, computeIO) where

import Build.Task
import Build.Store
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans

trackIO :: forall m k v. MonadIO m => Task MonadIO k v -> (k -> m v) -> m (v, [(k,v)])
trackIO task fetch = runWriterT $ run task trackingFetch
  where
    trackingFetch :: k -> WriterT [(k, v)] m v
    trackingFetch k = do
        v <- lift $ fetch k
        tell [(k, v)]
        return v

trackDepsIO :: forall m k v. MonadIO m => Task MonadIO k v -> (k -> m v) -> m (v, [k])
trackDepsIO task fetch = runWriterT $ run task (\k -> do
                                                   v <- lift $ fetch k
                                                   tell [k]
                                                   return v)

computeIO :: MonadIO m => Task MonadIO k v -> Store i k v -> m v
computeIO task store = run task (\k -> return (getValue k store))

