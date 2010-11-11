{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
-- basic functionality for staging

module Staged where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type RawCode b = Q Exp

data Code b = Code { c :: RawCode b, a :: Bool }
data Staged b = Now b | Later (Code b)

clift :: Bool -> RawCode b -> Code b
clift a x = Code x a

lift_atom :: RawCode b -> Code b
lift_atom = clift True
lift_comp :: RawCode b -> Code b
lift_comp = clift False

data Binary b c d = Binary {
  bnow :: b -> c -> d,
  blater :: Code b -> Code c -> Code d
}

of_immediate x = Now x
from_immediate (Now x) = x

mk_binary :: (Lift b, Lift c) => 
    Binary b c d -> Staged b -> Staged c -> Staged d
mk_binary f (Now x) (Now y) = Now (bnow f x y)
mk_binary f (Now x) (Later y) = Later (blater f (lift_atom [|x|]) y)
mk_binary f (Later x) (Now y) = Later (blater f x (lift_atom [|y|]))
mk_binary f (Later x) (Later y) = Later (blater f x y)

lift2 :: (b -> b -> b) -> Code b -> Code b -> Code b
lift2 f = \x y -> lift_comp [| $(dyn "f") $(c x) $(c y) |]
