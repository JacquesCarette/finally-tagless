{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances #-}

module Algebra where

import qualified Staged as S
import Language.Haskell.TH.Syntax

class MultiplicativeMonoid r where
    one :: r
    mul :: r -> r -> r

-- instance LMM r =>
--         MultiplicativeMonoid (S.Staged r) where
--     one = S.of_immediate one
--     mul = S.mk_binary (S.Binary (mul) 
--                                 (\a b -> S.lift_comp $ mul2 (S.c a) (S.c b)))

instance MultiplicativeMonoid (S.Staged Int) where
    one = S.of_immediate 1
    mul = S.mk_binary (S.Binary (*) 
                                (\a b -> S.lift_comp $ [| $(S.c a) * $(S.c b) |] ))
