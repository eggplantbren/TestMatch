{-
    Types and functions to do with players
-}

module TestMatch.Player where

-- Imports
import qualified Data.Text as T

-- The player type
data Player =
        Player
        {
            -- The player's identity
            id   :: !Int,
            name :: !T.Text,

            -- Batting-related parameters
            battingMu2 :: !Double,
            battingC   :: !Double,
            battingD   :: !Double
        } deriving (Eq, Read, Show)


