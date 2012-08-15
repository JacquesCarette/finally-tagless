{-# LANGUAGE TypeOperators, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -W #-}

-- Interpreter, Compiler, Partial Evaluator
-- Code based on 'tagless final' paper by
--   Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
--
--   TH code for C by Jacques Carette

module Thincope where

import Language.Haskell.TH.Syntax

{-
  The language is simply-typed lambda-calculus with fixpoint,
  integers, booleans and comparison: essentially, PCF.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | B Bool | Add ie1 ie2 | Mul ie1 ie2 | Leq ie1 ie2 |
  IF b e-then e-else
  
  The language is just expressive enough for the Gibonacci function
  and the power function.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.
-}

-- This class defines syntax (and its instances, semantics) of our language
-- This class is Haskell98!
class Symantics repr where
    int  :: Int  -> repr Int              -- int literal
    bool :: Bool -> repr Bool             -- bool literal

    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a

    add :: repr Int  -> repr Int -> repr Int
    mul :: repr Int  -> repr Int -> repr Int
    leq :: repr Int  -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

-- ------------------------------------------------------------------------
-- The interpreter
-- It is a typed, tagless interpreter: R is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only

newtype R a = R a deriving Show
unR (R x) = x

instance Symantics R where
    int x = R x
    bool b = R b

    lam f = R (unR . f . R)
    app e1 e2 = R( (unR e1) (unR e2) )
    fix f = R( fx (unR . f . R)) where fx f = f (fx f)

    add e1 e2 = R( (unR e1) + (unR e2) )
    mul e1 e2 = R( (unR e1) * (unR e2) )
    leq e1 e2 = R( (unR e1) <= (unR e2) )
    if_ be et ee = R( if (unR be) then unR et else unR ee )

compR = unR

mkitest f = compR (f ())

-- ------------------------------------------------------------------------
-- Another interpreter: it interprets each term to give its size
-- (the number of constructors)
-- It is a typed, tagless interpreter: L is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only.
-- This interpreter is also total: it determines the size of the term
-- even if the term itself is divergent.

newtype L a = L Int deriving Show
unL (L x) = x

instance Symantics L where
    int _  = L 1
    bool _ = L 1

    lam f = L( unL (f (L 0)) + 1 )
    app e1 e2 = L( unL e1 + unL e2 + 1 )
    fix f = L( unL (f (L 0)) + 1 )

    add e1 e2 = L( unL e1 + unL e2 + 1 )
    mul e1 e2 = L( unL e1 + unL e2 + 1 )
    leq e1 e2 = L( unL e1 + unL e2 + 1 )
    if_ be et ee = L( unL be +  unL et + unL ee  + 1 )

compL = unL

-- ------------------------------------------------------------------------
-- The compiler
-- 
-- Here we use Template Haskell
-- The compiler is 'identical' to the one in metaocaml


newtype C a = C (Q Exp)
unC (C x) = x

instance Symantics C where
    int x = C [| x |]
    bool b = C [| b |]

    lam f = C [| \x -> $(unC . f . C $ [| x |]) |]
    app e1 e2 = C [| $(unC e1) $(unC e2) |]
    fix f = C [| 
        let self n = $(unC . f . C $ [| self |]) n in self |]

    add e1 e2 = C [| $(unC e1) + $(unC e2) |]
    mul e1 e2 = C [| $(unC e1) * $(unC e2) |]
    leq e1 e2 = C [| $(unC e1) <= $(unC e2) |]
    if_ be et ee = C [| if $(unC be) then $(unC et) else $(unC ee) |]

compC = unC

-- helpful in implementations
lift1 f x = unC $ f (C x) 
lift2 f x y = unC $ f (C x) (C y)
lift3 f x y z = unC $ f (C x) (C y) (C z)
-- lifts function f onto function g
liftfn1 f g = unC $ f (\(C x) -> C [| $g $x |])

-- ------------------------------------------------------------------------
-- The partial evaluator: the combination of the interpreter (R) and
-- the compiler (C).
-- We need no Lift byte-code instruction: no parametric CSP. That is great!

-- ------------------------------------------------------------------------
-- A partial evaluator that does not use GADTs

-- Ken thinks that, without GADTs, it's not possible to explain in Haskell or
-- MetaOCaml how PE is an instance of Symantics (unless Symantics is 
-- generalized to carry both `static' and `dynamic' parts). But we can 
-- still explain PE by itself. In some sense we're just observing 
-- that Asai's code type-checks in Hindley-Milner as soon as we
-- deforest the static code representation.

-- Generalized Semantics, with an inductive type map

class Symantics2 rep where
    zint  :: Int  -> rep Int Int              -- int literal
    zbool :: Bool -> rep Bool Bool            -- bool literal

    zlam :: (rep sa da -> rep sb db) -> rep (rep sa da -> rep sb db) (da->db)
    zapp :: rep (rep sa da -> rep sb db) (da->db) -> rep sa da -> rep sb db
    zfix :: (rep sa da -> rep sa da) -> rep sa da

    zadd :: rep Int Int -> rep Int Int -> rep Int Int
    zmul :: rep Int Int -> rep Int Int -> rep Int Int
    zleq :: rep Int Int -> rep Int Int -> rep Bool Bool
    zif_ :: rep Bool Bool -> rep s d -> rep s d -> rep s d


data RP repr static dynamic = 
    RP { dynamic :: repr dynamic, static :: Maybe static }
zE dynamic = RP dynamic Nothing

instance Symantics repr => Symantics2 (RP repr) where
    zint x  = RP (int x) (Just x)
    zbool b = RP (bool b) (Just b)

    zadd (RP _ (Just n1)) (RP _ (Just n2)) = zint (n1 + n2)
    zadd (RP n1 _       ) (RP n2 _       ) = zE (add n1 n2)
    zmul (RP _ (Just n1)) (RP _ (Just n2)) = zint (n1 * n2)
    zmul (RP n1 _       ) (RP n2 _       ) = zE (mul n1 n2)
    zleq (RP _ (Just n1)) (RP _ (Just n2)) = zbool (n1 <= n2)
    zleq (RP n1 _       ) (RP n2 _       ) = zE (leq n1 n2)
    zif_ (RP _ (Just b1)) et ee = if b1 then et else ee
    zif_ (RP be _       ) et ee = zE (if_ be (dynamic et) (dynamic ee))

    zlam f = RP (lam (dynamic . f . zE)) (Just f)
    zapp (RP _ (Just f)) = f
    zapp (RP f _       ) = zE . app f . dynamic
    zfix f = case f (zfix f) of
	      (RP _ (Just s)) -> RP dfix (Just s)
	      _               -> RP dfix Nothing
       where dfix = fix (dynamic . f . zE)

{-
zzfix3 f = case f (zzfix3 f)  of
	  RP _ (Just g) -> RP dfix
			        (Just (\x -> 
				       case x of
			                RP cde Nothing -> zE (app dfix cde)
			                x     -> g x))
	  RP _ Nothing -> zE dfix
     where dfix = fix (dynamic . f . zE)

zzfix4 f = case f (zzfix4 f)  of
	  r@(RP _ (Just _)) -> RP dfix (Just (zapp r))
	  RP _ Nothing -> zE dfix
     where dfix = fix (dynamic . f . zE)
-}

-- Every instance of Symantics can be injected into Symantics2

newtype S12 repr s d = S12{unS12:: repr d}

instance Symantics repr => Symantics2 (S12 repr) where
    zint x  = S12 $ int x
    zbool b = S12 $ bool b

    zadd (S12 e1) (S12 e2) = S12 $ add e1 e2
    zmul (S12 e1) (S12 e2) = S12 $ mul e1 e2
    zleq (S12 e1) (S12 e2) = S12 $ leq e1 e2
    zif_ (S12 e) (S12 e1) (S12 e2) = S12 $ if_ e e1 e2

    zlam f = S12 $ lam (unS12 . f . S12 )
    zapp (S12 f) (S12 a) = S12 $ app f a
    zfix f = S12 $ fix (unS12 . f . S12 )


-- These need to be defined here to be run 'later'
test1 () = add (int 1) (int 2)
test2 () = lam (\x -> add x x)
test3 () = lam (\x -> add (app x (int 1)) (int 2))

testgib () = lam (\x -> lam (\y ->
                  fix (\self -> lam (\n ->
                      if_ (leq n (int 0)) x
                        (if_ (leq n (int 1)) y
                         (add (app self (add n (int (-1))))
                              (app self (add n (int (-2))))))))))

testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)
testgib2 () = lam (\x -> (lam (\y ->app (app (app (testgib ()) x) y) (int 5))))

testpowfix () = lam (\x ->
                      fix (\self -> lam (\n ->
                        if_ (leq n (int 0)) (int 1)
                            (mul x (app self (add n (int (-1))))))))
testpowfix7 () = lam (\x -> app (app (testpowfix ()) x) (int 7))
