{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -W #-}

module Thincope2 where

import qualified Thincope as T

-- Below is a very torturous way to write down 'id'.
-- This "derives" an interpreter (isomorphic to T.R) from T.C
-- Note that all the lift functions go through the "compiler" C.

newtype S a = S a deriving Show
unS (S x) = x

instance T.Symantics S where
    int x = S $(T.lift1 id [|x|])
    bool b = S $(T.lift1 id [|b|])

    -- lam f = R (unR . f . R)
    lam f = S (let g = unS . f . S in $(T.liftfn1 T.lam [|g|]))
    -- app e1 e2 = R( (unR e1) (unR e2) )
    app (S e1) (S e2) = S $(T.lift2 T.app [|e1|] [|e2|])

    fix f = S( fx (unS . f . S)) where fx f = f (fx f)
    -- This won't work because it is a 'functional' fix which is not
    -- what Symantics is declared to be.
    -- fix f = S (let g = unS . f . S in
    --           $(T.unC $ T.fix (\(T.C ) -> T.C [|g $x|])))

    add (S e1) (S e2) = S $(T.lift2 T.add [|e1|] [|e2|])
    mul (S e1) (S e2) = S $(T.lift2 T.mul [|e1|] [|e2|])
    leq (S e1) (S e2) = S $(T.lift2 T.leq [|e1|] [|e2|])
    if_ (S be) (S et) (S ee) = S $(T.lift3 T.if_ [|be|] [|et|] [|ee|])

compS = unS

test1 :: () -> S Int
test1 = T.test1

test2 :: () -> S (Int -> Int)
test2 = T.test2
