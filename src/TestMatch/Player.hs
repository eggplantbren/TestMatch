{-
    Types and functions to do with players
-}

{-# LANGUAGE RecordWildCards #-}

module TestMatch.Player where

-- Imports
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

-- The player type
data Player =
        Player
        {
            -- The player's identity
            playerId :: !Int,
            name     :: !T.Text,

            -- Batting-related parameters
            battingMu2 :: !Double,
            battingC   :: !Double,
            battingD   :: !Double
        } deriving (Eq, Read, Show)


-- Evaluate ability on a given score
-- Score must be non-negative.
ability :: Player -> Int -> Double
ability Player {..} x =
    let
        mu1 = battingC*mu2
        mu2 = battingMu2
        l   = battingD*mu2
    in
        mu2 + (mu1 - mu2)*exp(-fromIntegral x/l)


-- Hazard function
hazard :: Player -> Int -> Double
hazard player = (\u -> 1.0/(u + 1.0)) . ability player


-- Probability distribution over score, up to 1000
probabilities :: Player -> U.Vector Double
probabilities player@(Player {..}) =
    let
        xs = U.generate 1001 id
        hazards = U.map (hazard player) xs
        complements = U.map (1.0 - ) hazards
        cumProd = U.scanl' (*) 1.0 complements
    in
        U.zipWith (*) hazards cumProd

