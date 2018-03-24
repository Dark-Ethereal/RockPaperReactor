{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module RandomizedComponents (mkEnts) where

import           Apecs
import           Control.Monad.State.Strict
import           Datatypes
import           Diagrams.Prelude           hiding (cmap, simulate)
import           Diagrams.TwoD.Vector       (e)
import           System.Random

mkEnt :: Flavour -> Double -> System World Entity
mkEnt f a = newEntity (Position (V2 0 0), Velocity (e (a @@ deg)), Past [], f)

mkEnts :: StateT StdGen (System World) ()
mkEnts = do
  state (randomR (0, 360)) >>= lift . (mkEnt Rock)
  state (randomR (0, 360)) >>= lift . (mkEnt Paper)
  state (randomR (0, 360)) >>= lift . (mkEnt Scissor)
  extraParts <- state $ randomR (0, 3)
  sequence_ . replicate extraParts $ do
    a <- state $ randomR (0, 360)
    fn :: Int <- state $ randomR (0, 2)
    lift $ mkEnt (selectFlavour fn) a
  where
    selectFlavour 0 = Rock
    selectFlavour 1 = Paper
    selectFlavour _ = Scissor
