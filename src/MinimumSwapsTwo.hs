{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module MinimumSwapsTwo where

import           Debug.Trace
import           System.Environment
import           System.IO
import           Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
-- import           Data.Vector.Algorithms.Merge (sortBy)
import           Control.Monad.ST
import           Control.Monad
-- import           Control.Monad.Loops
import           Data.Ord

-- |Execute an action repeatedly as long as the given boolean expression
-- returns True.  The condition is evaluated before the loop body.
-- Collects the results into a list.
whileM :: Monad m => m Bool -> m a -> m [a]
whileM = whileM'

-- |Execute an action repeatedly as long as the given boolean expression
-- returns True. The condition is evaluated before the loop body.
-- Collects the results into an arbitrary 'MonadPlus' value.
whileM' :: (Monad m, MonadPlus f) => m Bool -> m a -> m (f a)
whileM' p f = go
    where go = do
            x <- p
            if x
                then do
                        x  <- f
                        xs <- go
                        return (return x `mplus` xs)
                else return mzero

-- Complete the minimumSwaps function below.
minimumSwaps :: [Int] -> Int
minimumSwaps arr = runST $ do
  v <- V.thaw (V.fromList sortedIndexedArr)
  -- v is already sorted
  swaps <- forM [ 0 .. l - 1] $
    \i -> do
      let cond = do
            x <- M.unsafeRead v i
            let index = snd x
            pure $ snd x /= i
      innerSwaps <- whileM cond $ do
        x <- M.unsafeRead v i
        M.swap v (snd x) i
        pure 1
      pure (sum innerSwaps)
  pure (sum swaps)
  where
    sortedIndexedArr = sortOn fst indexed
    l = length arr
    indexed = zip arr [0 .. ] :: [(Int, Int)]
  

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    arrTemp <- getLine

    let arr = fmap (read :: String -> Int) . words $ arrTemp

    let res = minimumSwaps arr

    hPutStrLn fptr $ show res

    hFlush fptr
    hClose fptr
