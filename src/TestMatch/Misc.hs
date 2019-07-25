module TestMatch.Misc where

-- Imports
import qualified Data.Vector.Unboxed as U

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

