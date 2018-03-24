{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Datatypes where

import           Apecs
import           Diagrams.Prelude
import           System.Random

data Flavour = Rock | Paper | Scissor deriving (Eq, Show)

instance Ord Flavour where
  (<=) Rock  Rock      = True
  (<=) Rock  Paper     = True
  (<=) Rock  Scissor   = False
  (<=) Paper Rock      = False
  (<=) Paper Paper     = True
  (<=) Paper Scissor   = True
  (<=) Scissor Rock    = True
  (<=) Scissor Paper   = False
  (<=) Scissor Scissor = True

instance Component Flavour where
  type Storage Flavour = Map Flavour

newtype Position = Position (V2 Double) deriving Show
instance Component Position where
  type Storage Position = Map Position

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where
  type Storage Velocity = Map Velocity

newtype Past = Past [V2 Double] deriving Show
instance Component Past where
  type Storage Past = Map Past

type LOP = (Flavour, Position) -> (Flavour, Position, Velocity) -> Velocity

-- Shitty library wants a monoid instance for no good reason.
--instance Monoid LOP where
--  mempty = LOP $ \_ _ -> 0 @@ deg
--  mappend = flip const

--instance Component LOP where
--  type Storage LOP = Global LOP

makeWorld "World" [''Position, ''Velocity, ''Past, ''Flavour]

