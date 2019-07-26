{-# LANGUAGE RecordWildCards #-}

module TestMatch.Misc where

-- Imports
import Control.Monad.Primitive
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
        invTot = 1.0/(U.sum runProbs)

-- Assumed strike rate. Eventually, should depend on the batsman
-- and the bowler.
strikeRate :: Double
strikeRate = U.sum $ U.zipWith (*) runProbs' (U.fromList [0..6])

-- What possible outcomes are there from a ball of cricket?
-- For simplicity, start with either a wicket or a number of runs.
data BallOutcome = Wicket | Runs Int

-- Simulate one ball of cricket and return the result in a PrimMonad
simulateBall :: PrimMonad m
             => Player -> Gen (PrimState m) -> m BallOutcome
simulateBall Player {..} rng = do
    -- Placeholder
    return $ Runs 0

