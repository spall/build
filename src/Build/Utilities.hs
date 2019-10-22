-- | General utilities useful in the rest of the package
module Build.Utilities (
    -- * Graph operations
    graph, reachable, topSort, reach, reachM, reachableIO, allM
    ) where

import Algebra.Graph
import qualified Algebra.Graph.ToGraph as T

import Data.Functor.Identity
import qualified Data.Set as Set
import Control.Monad.IO.Class
import Control.Monad

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM p (x:xs) = do
  q <- p x
  if q
    then allM p xs
    else return False

-- | Build a dependency graph given a function for computing dependencies of a
-- key and a target key.
graph :: Ord k => (k -> [k]) -> k -> Graph k
graph deps key = transpose $ overlays [ star k (deps k) | k <- keys Set.empty [key] ]
  where
    keys seen []   = Set.toList seen
    keys seen (x:xs)
        | x `Set.member` seen = keys seen xs
        | otherwise           = keys (Set.insert x seen) (deps x ++ xs)

graphIO :: (MonadIO m, Ord k) => (k -> m [k]) -> k -> m (Graph k)
graphIO deps key = (liftM transpose) $ (liftM overlays) $ do
  ks <- keys Set.empty [key]
  mapM (\k -> (liftM (star k)) (deps k)) ks
  where
    keys seen []   = return $ Set.toList seen
    keys seen (x:xs)
        | x `Set.member` seen = keys seen xs
        | otherwise           = do
            xs2 <- deps x
            keys (Set.insert x seen) (xs2 ++ xs)

-- | Compute all keys reachable via dependecies from a target key.
reachable :: Ord k => (k -> [k]) -> k -> [k]
reachable deps key = vertexList (graph deps key)

reachableIO :: (MonadIO m, Ord k) => (k -> m [k]) -> k -> m [k]
reachableIO deps key = (liftM vertexList) $ graphIO deps key

-- | Compute the topological sort of a graph or return @Nothing@ if the graph
-- has cycles.
topSort :: Ord k => Graph k -> Maybe [k]
topSort = T.topSort

-- | Given a function to compute successors of a vertex, apply it recursively
-- starting from a given vertex. Returns @Nothing@ if this process does not
-- terminate because of cycles. Note that the current implementation is very
-- inefficient: it trades efficiency for simplicity. The resulting list is
-- likely to contain an exponential number of duplicates.
reach :: Eq a => (a -> [a]) -> a -> Maybe [a]
reach successors = runIdentity . reachM (return . successors)

-- | Given a monadic function to compute successors of a vertex, apply it
-- recursively starting from a given vertex. Returns @Nothing@ if this process
-- does not terminate because of cycles. Note that the current implementation is
-- very inefficient: it trades efficiency for simplicity. The resulting list is
-- likely to contain an exponential number of duplicates.
reachM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m (Maybe [a])
reachM successors a = fmap (filter (/= a)) <$> go [] a
  where
    go xs x | x `elem` xs = return Nothing -- A cycle is detected
            | otherwise   = do res <- traverse (go (x:xs)) =<< successors x
                               return $ ((x:xs)++) . concat <$> sequence res
