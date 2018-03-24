module Main where

import           Apecs
import           Datatypes
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (cmap, simulate)
import           System.Random
import Simulate

main :: IO ()
main = mainWith mkDiagram

mkDiagram :: Int -> IO (Diagram B)
mkDiagram i = do
  putStrLn $ "Generating artwork with seed: " ++ (show i)
  let rgen = mkStdGen i
  d <- runSystem (simulate 10000 rgen) =<< initWorld
  return $ bgFrame 10 beige d
