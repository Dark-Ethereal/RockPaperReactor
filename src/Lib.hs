{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Lib
  ( someFunc
  ) where

import           Apecs
import           Control.Monad.State.Strict
import           Datatypes
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (cmap, simulate)
import           Diagrams.TwoD.Vector         (e)
import           ParticleLaws
import           RandomizedComponents
import           System.Random

someFunc :: IO ()
someFunc = mainWith mkDiagram

mkDiagram :: Int -> IO (Diagram B)
mkDiagram i = do
  putStrLn $ "Generating artwork with seed: " ++ (show i)
  let rgen = mkStdGen i
  d <- runSystem (simulate 10000 rgen) =<< initWorld
  return $ bgFrame 10 beige d

getEffects ::
     (Flavour, Position)
  -> System World ((Flavour, Position, Velocity) -> Velocity)
getEffects = return . lop

step :: System World ()
step = do
  cmap $ \(Past p, Velocity v) -> Past (v:p)
  cmap $ \(Position p, Velocity v) -> Position (p+v)
  effect <- cmapM getEffects
  mapM_ cmap effect

getPaths :: System World [(Flavour, [V2 Double])]
getPaths = cmapM $ \(Past p, f :: Flavour) -> return (f, reverse p)

pastToDiag :: (Flavour, [V2 Double]) -> Diagram B
pastToDiag (f, vs) = fromOffsets vs # lc (colorFlavour f)

colorFlavour Rock    = cyan
colorFlavour Paper   = gold
colorFlavour Scissor = magenta

simulate :: Int -> StdGen -> System World (Diagram B)
simulate x rgen = do
  evalStateT mkEnts rgen
  sequence (replicate x step)
  p <- getPaths
  return $ foldMap pastToDiag p # opacityGroup 0.7
