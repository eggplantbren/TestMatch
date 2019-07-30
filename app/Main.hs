module Main where

-- Imports
import Control.Monad
import qualified Data.Text as T
import System.Random.MWC
import TestMatch.Misc
import TestMatch.Player

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    let player = Player 0 (T.pack "John Smith") 50.0 0.7 0.5
    replicateM_ 1000 (simulateBall' player rng)

