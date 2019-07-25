module Main where

-- Imports
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import TestMatch.Misc
import TestMatch.Player

main :: IO ()
main = do
    let player = Player 0 (T.pack "John Smith") 50.0 0.7 0.5
    print player
    putStrLn ""
    let xs = [0..101]
    print $ map (ability player) xs
    putStrLn ""
    print $ U.sum runProbs


