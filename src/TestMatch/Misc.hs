module TestMatch.Misc where

-- Imports
import qualified Data.Vector.Unboxed as U

-- Relative probabilities for each possible number of runs
runProbs :: U.Vector Double
runProbs = U.fromList [0.5, 0.2, 0.1, 0.05, 0.13, 0.0, 0.02]

