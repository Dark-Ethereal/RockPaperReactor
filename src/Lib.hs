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
import           Datatypes
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (cmap, simulate)
import           Diagrams.TwoD.Vector         (e)
import           ParticleLaws
import System.Random
import Control.Monad.State.Strict

someFunc :: IO ()
someFunc = mainWith mkDiagram

mkDiagram :: Int -> IO (Diagram B)
mkDiagram i = do
  putStrLn $ "Generating artwork with seed: " ++ (show i)
  let rgen = mkStdGen i
  d <- runSystem (simulate 10000) =<< initWorld
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


simulate :: Int -> System World (Diagram B)
simulate x = do
  initEnts
  sequence (replicate x step)
  p <- getPaths
  return $ foldMap pastToDiag p # opacityGroup 0.7

data RandVars = RandVars
  { entMakers :: [System World Entity]
  , lops :: LOP
  }

mkEnt :: Angle Double -> Flavour -> System World Entity
mkEnt a f = newEntity (Position (V2 0 0), Velocity (e (i @@ deg)), Past [], f)

mkEnts :: StdGen -> [System World Entity]
mkEnts StdGen

initEnts ::   [System World Entity]
initEnts = do
  newEntity (Position (V2 0 0), Velocity (e (0 @@ deg)), Past [], Rock)
  newEntity (Position (V2 0 0), Velocity (e (120 @@ deg)), Past [], Paper)
  newEntity (Position (V2 0 0), Velocity (e (240 @@ deg)), Past [], Scissor)
