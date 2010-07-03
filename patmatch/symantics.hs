{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -W #-}

module Symantics where

import Control.Monad
import Data.Maybe (fromJust)

-- This class defines syntax (and its instances, semantics) of our language
-- Taken from tagless final paper.
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
-- An interpreter
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

-- some tests
otest1 () = add (int 1) (int 2)
otest2 () = lam (\x -> add x x)
otest3 () = lam (\x -> add (app x (int 1)) (int 2))
otest4 :: Symantics repr => () -> repr Int
otest4 () = fix (\_ -> int 1)

o1 = mkitest otest1
o2 = mkitest otest2
o3 = mkitest otest3
o4 = mkitest otest4

------------------------------------------------------------------------
-- similar to above, but now we are monadic *without* fixpoint.
-- The other difference is that mlam is a cheat, in that it can only
-- lift up someting that satisties (Symatics repr) and not a monadic version.
class Monad e => MSymantics e repr where
    mint  :: Int  -> e (repr Int)
    mbool :: Bool -> e (repr Bool)

    mlam :: (e (repr a -> repr b)) -> e (repr (a-> b))
    mapp :: e (repr (a->b)) -> e (repr a) -> e (repr b)

    madd :: e (repr Int)  -> e (repr Int) -> e (repr Int)
    mmul :: e (repr Int)  -> e (repr Int) -> e (repr Int)
    mleq :: e (repr Int)  -> e (repr Int) -> e (repr Bool)
    mif_ :: e (repr Bool) -> e (repr a) -> e (repr a) -> e (repr a)

-- And some test cases

test1 :: MSymantics e repr => () -> e (repr Int)
test1 () = madd (mint 1) (mint 2)
test2 :: (MSymantics e repr, Symantics repr) => () -> e (repr (Int->Int))
test2 () = mlam (return $ \x -> add x x)
test3 :: (MSymantics e repr, Symantics repr) => () -> e (repr ((Int->Int)->Int))
test3 () = mlam (return $ \x -> add (app x (int 1)) (int 2))

-- ------------------------------------------------------------------------
-- The monadic version of Symantics.  Note how this is fully generic
-- and works for any 'Symantics repr'
instance (Monad e,Symantics repr) => MSymantics e repr  where
    mint x = return $ int x
    mbool b = return $ bool b

    mlam = liftM (lam)
    mapp = liftM2 (app)

    madd = liftM2 (add)
    mmul = liftM2 (mul)
    mleq = liftM2 (leq)
    -- this might be too strict?
    mif_ = liftM3 (if_)

-- Separate this out, as for some monads, they will not have an instance
-- of Runnable
class (Monad e, MSymantics e repr) => Runnable e repr where
    runExpr :: e (repr a) -> a

-- Use Maybe and R for running some explicit tests
instance Runnable Maybe R where
    runExpr = unR . fromJust

runExpr1 f = (runExpr :: Maybe (R a) -> a) (f ())
t1 = runExpr1 test1
t2 = runExpr1 test2 5

-----------------------------------------------------------------------

class StackSymantics st where
    pushi  :: Int -> st s (Int,s) a
    sadd   :: st (Int,(Int,s)) (Int,s) a
    sapply :: st (a->b, (a, s)) (b,s) c
    pushf  :: (a->b) -> st s (a->b,s) c

data Stack s1 s2 a = Stack { runStack :: s1 -> s2 }

liftv :: x -> Stack s (x,s) a
liftv n = Stack (\s -> (n, s))
lift2 :: (x -> y -> z) -> Stack (x,(y,s)) (z,s) a
lift2 f = Stack(\(x,(y,s)) -> (f x y, s))

push   = liftv
pop    = Stack (\(_,s) -> s)
swap   = Stack (\(a, (b, s)) -> (b, (a,s)))

instance StackSymantics Stack where
    pushi i = push i
    pushf f = push f
    sadd    = lift2 (+)
    sapply  = Stack (\(f,(x,s)) -> (f x, s))
