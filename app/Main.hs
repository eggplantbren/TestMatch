module Main where

-- Imports
import Control.Monad.Primitive
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import TestMatch.Misc
import TestMatch.Player

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    let player = Player 0 (T.pack "John Smith") 50.0 0.7 0.5
    runs <- U.replicateM 100000 (genRuns rng)
    let print' run = putStr $ show run ++ "\n"
    U.mapM_ print' runs

--    let ps = probabilities player
--    print ps
--    print $ U.length ps


