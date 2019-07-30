module Main where

-- Imports
import qualified Data.Text as T
import System.Random.MWC
import TestMatch.Misc
import TestMatch.Player

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    let player = Player 0 (T.pack "John Smith") 50.0 0.7 0.5 0
    _ <- simulateBalls player 1000000 rng
    return ()

