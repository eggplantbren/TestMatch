module Main where

-- Imports
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import TestMatch.Player

main :: IO ()
main = do
    let player = Player 0 (T.pack "John Smith") 50.0 0.7 0.5
    let ps = probabilities player
    print ps
    print $ U.length ps


