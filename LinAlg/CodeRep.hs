module CodeRep where

import StateCPS
import Data.Traversable

data Dir = Up | Down

class CodeRep cr where
  retN :: b -> StateCPS s (cr w) (cr b)
  loopM :: cr Int -> cr Int -> (cr Int -> b -> (c -> d -> d) -> cr e) ->
    Dir -> StateCPS b (cr ()) f
