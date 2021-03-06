{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module TestMatch.Misc where

-- Imports
import Control.Monad.Primitive
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import TestMatch.Player

-- Relative probabilities for each possible number of runs.
-- Maximum should be 1.
runProbs :: U.Vector Double
runProbs = U.fromList [1.0, 0.2, 0.1, 0.03, 0.05, 0.0, 0.003]

-- Normalised version
runProbs' :: U.Vector Double
runProbs' = U.map (* invTot) runProbs
    where
        invTot = 1.0 / U.sum runProbs

-- Generate from the run probability distribution
genRuns :: PrimMonad m => Gen (PrimState m) -> m Int
genRuns rng = do
    k <- uniformR (0, U.length runProbs - 1) rng
    u <- uniform rng
    if u < runProbs U.! k
    then return $! k
    else genRuns rng

-- Assumed strike rate. Eventually, should depend on the batsman
-- and the bowler.
strikeRate :: Double
strikeRate = U.sum $ U.zipWith (*) runProbs' (U.fromList [0..6])

-- What possible outcomes are there from a ball of cricket?
-- For simplicity, start with either a wicket or a number of runs.
data BallOutcome = Wicket | Runs Int


-- Render a BallOutcome as text
render :: BallOutcome -> T.Text
render Wicket = T.pack "OUT"
render (Runs x) = T.pack $ case x of
    0 -> "no run"
    1 -> "one run"
    2 -> "two runs"
    3 -> "three runs"
    4 -> "FOUR"
    6 -> "SIX"
    _ -> "ERROR"

-- Simulate one ball of cricket and return the result and the
-- updated Player in a PrimMonad
simulateBall :: PrimMonad m
             => Player -> Gen (PrimState m)
             -> m (BallOutcome, Player)
simulateBall player@Player {..} rng = do

    -- Current hazard per run
    let h = hazard Player {..}

    -- Current hazard per ball
    let h' = strikeRate*h

    -- Test for a wicket, if no wicket, generate the number of runs
    u <- uniform rng
    if u < h'
    then return (Wicket, reset player)
    else do
        runs <- genRuns rng
        return (Runs runs, addRuns player runs)



-- Simulate one ball of cricket and do some I/O about what happened.
simulateBall' :: Player -> Gen RealWorld -> IO (BallOutcome, Player)
simulateBall' player rng = do
    putStr "Bowler to "
    T.putStr $ name player
    putStr ": "
    (outcome, player') <- simulateBall player rng
    T.putStrLn $ render outcome
    return (outcome, player')


-- Repeat simulateBall' many times
simulateBalls :: Player -> Int -> Gen RealWorld -> IO Player
simulateBalls !player n rng
    | n <= 0 = return player
    | otherwise = do
        (_, player') <- simulateBall' player rng
        simulateBalls player' (n-1) rng

