{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -W #-}

module Thincope2 where

import qualified Thincope as T

-- Now we go 'the other way around' where we extract the interpreter
-- from the compiler

newtype R a = R a deriving Show
unR (R x) = x

instance T.Symantics R where
    int = R
    bool = R

    -- lam f = R (unR . f . R)
    lam f = R (let g = unR . f . R in $(T.liftfn1 T.lam [|g|]))
    -- app e1 e2 = R( (unR e1) (unR e2) )
    app (R e1) (R e2) = R $(T.lift2 T.app [|e1|] [|e2|])

    fix f = R( fx (unR . f . R)) where fx f = f (fx f)
    -- This won't work because it is a 'functional' fix which is not
    -- what Symantics is declared to be.
    -- fix f = R (let g = unR . f . R in
    --           $(T.unC $ T.fix (\(T.C ) -> T.C [|g $x|])))

    add (R e1) (R e2) = R $(T.lift2 T.add [|e1|] [|e2|])
    mul (R e1) (R e2) = R $(T.lift2 T.mul [|e1|] [|e2|])
    leq (R e1) (R e2) = R $(T.lift2 T.leq [|e1|] [|e2|])
    if_ (R be) (R et) (R ee) = R $(T.lift3 T.if_ [|be|] [|et|] [|ee|])

compR = unR
