{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Algebra where

import qualified Staged as S
import Language.Haskell.TH.Syntax

class MultiplicativeMonoid r where
    one :: r
    mul :: r -> r -> r

class (Lift r, MultiplicativeMonoid r) => LMM r where
instance LMM Int

instance LMM r =>
        MultiplicativeMonoid (S.Staged r) where
    one = S.of_immediate one
    mul = S.mk_binary (S.Binary (mul) (S.lift2 mul))

instance MultiplicativeMonoid Int where
    one = 1
    mul = (*)
