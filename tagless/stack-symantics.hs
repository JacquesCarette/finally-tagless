{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}

-- A stack machine as an instance of restricted Symantics 
-- (restricted to arithmetics at present)

module Forth where

import Prelude hiding ((>>=), (>>), return, fail, drop)
import qualified Prelude

-- Abstract Stack machine
class StackMachine stk where
    push :: Show a => a -> stk s -> stk (a,s)
    sadd :: stk (Int,(Int,s)) -> stk (Int,s)
    smul :: stk (Int,(Int,s)) -> stk (Int,s)
    sleq :: Ord a => stk (a,(a,s)) -> stk (Bool,s)
    swap :: stk (a,(b,s)) -> stk (b,(a,s))
    drop :: stk (a,s) -> stk s
    sapp :: stk ((a->b),(a,s)) -> stk (b,s)

(>>) = flip (.)

newtype R a = R a

-- An interepreter
instance StackMachine R where
    push x = \ (R s) -> R (x,s)
    sadd   = \ (R (x,(y,s))) -> R (x+y,s)
    smul   = \ (R (x,(y,s))) -> R (x*y,s)
    sleq   = \ (R (x,(y,s))) -> R (x<=y,s)
    swap   = \ (R (x,(y,s))) -> R (y,(x,s))
    drop   = \ (R (x,s)) -> R s
    sapp   = \ (R (f,(a,s))) -> R ((f a), s)

runR m = let R r = m (R ()) in r

-- `Compiler'
newtype C a = C String
liftC f (C x) = C $ f x

instance StackMachine C where
    push x = liftC (++ (" push " ++ show x))
    sadd   = liftC (++ " add")
    smul   = liftC (++ " mul")
    sleq   = liftC (++ " <=")
    swap   = liftC (++ " swap")
    drop   = liftC (++ " drop")
    sapp   = liftC (++ " apply")

runC m = let C r = m (C "") in r

tst1 = push 2 >>
       push 3 >>
       push 7 >>
       sadd >>
       sadd

tst1R = runR tst1
tst1C = runC tst1

-- Type error
-- tstadd = runR add

tst2 = push 2 >>
       push 3 >>
       sadd    >>
       push "hello" >>
       swap >>
       push 7 >>
       sadd >>
       swap >>
       drop

tst2R = runR tst2
tst2C = runC tst2


-- From our paper, taken verbatim
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

newtype RR c a = RR{unRR :: forall s. c s -> c (a,s)}

instance StackMachine c => Symantics (RR c) where
    int x  = RR (push x)
    bool x = RR (push x)
    add x y = RR (sadd . unRR y . unRR x)
    leq x y = RR (sleq . unRR x . unRR y) -- note the flip!
    mul x y = RR (smul . unRR y . unRR x)
    app f x = RR (sapp . unRR f . unRR x)

runRRR m = runR (unRR m)
runRRC m = runC (unRR m)

test3 = (int 1 `add` int 2) `add` (int 3 `add` int 4)
test3R = runRRR test3
-- (10,())
test3C = runRRC test3
-- " push 1 push 2 add push 3 push 4 add add"

test4 = (int 1 `add` int 2) `mul` (int 3 `add` int 4) `leq` (int 9)
test4R = runRRR test4
-- (False,())
test4C = runRRC test4
-- " push 1 push 2 add push 3 push 4 add mul push 9 <="
