module ParticleLaws (lop) where

import           Apecs
import           Datatypes
import           Diagrams.Prelude

lop :: (Flavour, Position) -> (Flavour, Position, Velocity) -> Velocity
lop (f1, Position p1) (f2, Position p2, Velocity v)
  | f1 > f2 && dist > 500 = Velocity $ case f2 of
    Rock    -> v # rotate ( 4 @@ deg )
    Paper   -> v # rotate ( 2 @@ deg )
    Scissor -> v # rotate ( 1 @@ deg )
  | otherwise = Velocity v
    where dist = distance p1 p2
