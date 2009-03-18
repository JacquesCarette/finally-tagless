{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSignatures, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
  FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- A typed stack machine: typed PostScript

module StackMachine where

import Prelude hiding ((>>), drop)
import qualified Prelude

data N = N deriving Show
data A = A deriving Show		-- bottom of the register file

-- Abstract Stack machine with stack stk and an accumulator (register)
-- file with the base A
class StackMachine stk where
    accu  :: stk A			-- Must be the neutral element
    xmit  :: stk (a,s) -> stk t -> (stk s, stk (a,t))
	     -- This isn't actually needed: Symantics needs to apply
	     -- a type function/representation change
	     -- This is to merely emulate the non-parameteric data-type
    smap  :: (a->b) -> stk (a,A) -> stk (b,A)

class StackMachine fr => Forth fr where
    push :: Show a => a -> fr N -> fr (a,N)
    sadd :: fr (Int,(Int,N)) -> fr (Int,N)
    smul :: fr (Int,(Int,N)) -> fr (Int,N)
    lequ :: fr (Int,(Int,N)) -> fr (Bool,N)
    swap :: fr (a,(b,N)) -> fr (b,(a,N))
    drop :: fr (a,N) -> fr N
    dup  :: fr (a,N) -> fr (a,(a,N))
    idx2 :: fr (a,(b,(c,N))) -> fr (c,(a,(b,(c,N))))
    lamb :: (fr s1 -> fr s2) -> fr N -> fr (fr s1 -> fr s2,N) -- like push
    appl :: fr (fr s1 -> fr s2,s1) -> fr s2
    ifte':: fr (fr s1 -> fr s2, (fr s1 -> fr s2, (Bool, s1))) -> fr s2


class StackZip s1 s2 s | s1 s2 -> s, s s2 -> s1 where
    sz_up :: StackMachine fr =>
	     fr s1 -> fr s2 -> fr s	-- Put accum s2 on the top of s1
    sz_dn :: StackMachine fr =>
	     fr s  -> (fr s1, fr s2)    -- Remove the top to the accumulator

instance StackZip s1 A s1 where
    sz_up x _ = x			-- fr A is ignorable
    sz_dn x = (x,accu)

instance StackZip s1 s2s s' => StackZip s1 (s2,s2s) (s2,s') where
    sz_up x a = let (s2s,s2) = xmit a accu
		    s' = sz_up x s2s
		in snd $ xmit s2 s'
    sz_dn x =   let (s',s2) = xmit x accu
		    (s1,s2s) = sz_dn s'
		in (s1, snd $ xmit s2 s2s)


class StackZip' s2 s | s -> s2, s2 -> s where
    sz_up' :: StackMachine fr =>
	     fr N -> fr s2 -> fr s	-- Put accum s2 on the top of N
    sz_dn' :: StackMachine fr =>
	     fr s  -> (fr N, fr s2)    -- Remove the top to the accumulator

instance StackZip' A N where
    sz_up' x _ = x			-- fr A is ignorable
    sz_dn' x = (x,accu)

instance StackZip' s2s s' => StackZip' (s2,s2s) (s2,s') where
    sz_up' x a = let (s2s,s2) = xmit a accu
		     s' = sz_up' x s2s
		 in snd $ xmit s2 s'
    sz_dn' x =   let (s',s2) = xmit x accu
		     (s1,s2s) = sz_dn' s'
		 in (s1, snd $ xmit s2 s2s)

-- We consider type N to be sort of a type variable
-- The class below does the unification
class Compose s1a s1b s2a s2b sa sb | s1a s1b s2a s2b -> sa sb  where
    (>>) :: StackMachine fr =>
	    (fr s1a -> fr s1b) -> (fr s2a -> fr s2b) -> (fr sa -> fr sb)

instance Compose s1 N N s2   s1 s2 where
    (>>) = flip (.)

-- s1a --> s1  and N --> s2b
-- Put the stack s2B on top of s1
instance (StackZip' s1A (s, s1b),
	  StackZip' s2s s1b,
	  StackZip' s2B s2b,
	  StackZip (s, s1b) s2B sb) 
    => Compose s1a (s,s1b) N s2b  s1a sb where
    f1 >> f2 = \s1a -> let s1 = f1 s1a
		           (n1,s1A) = sz_dn' s1
			   s2b   = f2 n1
			   (n2,s2B) = sz_dn' s2b
			   s1' = sz_up' n2 s1A
		       in sz_up s1' s2B

-- s1a --> N  and s2a --> s2b 
-- Put s2a underneath of s1a, so s1 is s1a on the top of s2a
instance (StackZip (s,s2a) s1A s1,
	  StackZip' s2A (s,s2a),
	  StackZip' s2s s2a,
	  StackZip' s1A s1a) 
    => Compose s1a N (s,s2a) s2b  s1 s2b where
    f1 >> f2 = \s1 ->  let (s1',s1A) = sz_dn s1
			   (n1,s2A)  = sz_dn' s1'
			   s1a = sz_up' n1 s1A
			   n2  = f1 s1a
		       in f2 $ sz_up' n2 s2A

stype :: s -> fr s
stype = undefined

stypefn :: s1 -> s2 -> fr s1 -> fr s2
stypefn = undefined

-- s1a --> (a,s1b)  and (b,s2a) --> s2b
-- Here, 'a' and 'b' must match-up 
instance (TypeCast (a,A) (b,A),
	  Compose s1a s1b s2a s2b s1 s2,
	  StackZip' s1'A s1',
	  StackZip s1' s1A s1,
	  StackZip' s1A s1a) 
    => Compose s1a (a,s1b) (b,s2a) s2b  s1 s2 where
    f1 >> f2 = \s1 ->  let (s1',s1A) = sz_dn s1
			   (n1,s1'A) = sz_dn' s1'
			   s1a       = sz_up' n1 s1A
			   (s1b,a)   = xmit (f1 s1a) accu
			   b         = typeCast a
			   f1' s1a'  = s1b where
					   _ = s1a' `asTypeOf` 
					            (stype (undefined::s1a))
			   f2' s2a   = f2 (snd (xmit b s2a))
		       in (f1' >> f2') s1


class TypeCast   a b   | a -> b, b->a   where typeCast   :: f a -> f b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->f a-> f b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->f a-> f b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

newtype R a = R a

-- An interepreter
instance StackMachine R where
    accu = R A
    xmit (R (a,s)) (R t) = (R s, R (a,t))
    smap f (R (x,A)) = R (f x,A)

instance Forth R where
    push x = \ _ -> R (x,N)
    sadd   = \ (R (x,(y,_))) -> R (x+y,N)
    smul   = \ (R (x,(y,_))) -> R (x*y,N)
    lequ   = \ (R (x,(y,_))) -> R (y<=x,N)
    swap   = \ (R (x,(y,_))) -> R (y,(x,N))
    drop   = \ (R (x,_)) -> R N
    dup    = \ (R (x,_)) -> R (x,(x,N))
    idx2   = \ (R (a,(b,(c,N)))) -> R (c,(a,(b,(c,N))))
    lamb f = \ _ -> R (f,N)
    appl   = \ (R (f,s)) -> f (R s)
    ifte'  = \ (R (bf,(bt,(tst,s)))) -> if tst then bt (R s) else bf (R s)
  
runR m = let R r = m (R N) in r


tst1 = push (2::Int) >>
       push (3::Int) >>
       push (7::Int) >>
       sadd >>
       sadd
-- inferred type: tst1 :: (StackMachine fr acc, Forth fr) => fr N -> fr (Int, N)

tst1R = runR tst1


-- Type error
--    Couldn't match expected type `N'
--           against inferred type `(Int, (Int, N))'
-- tstadd = runR sadd


tst2 = push (2::Int) >>
       push (3::Int) >>
       sadd    >>
       push "hello" >>
       swap >>
       push (7::Int) >>
       sadd >>
       swap >>
       drop
-- Inferred type
-- tst2 :: (StackMachine fr acc, Forth fr) => fr N -> fr (Int, N)


tst2R = runR tst2


tst3 = dup >>				-- b b a
       idx2 				-- a b b a

{-
tst3 :: (StackMachine fr, Forth fr) =>
        fr (b, (c, N)) -> fr (c, (b, (b, (c, N))))
-}

-- we should actually make automatic computation of lub
-- and replace ifte' with the real ifte that makes sure
-- the two lambdas have the same type, or adds coercions
-- We skip this for now and let the user worry about this.

-- Ensure that stack has enough elements. Useful in functions,
-- if we would like to assign them a less general type

id1 :: StackMachine fr => fr (a,s) -> fr (a,s)
id1 = id

id2 :: StackMachine fr => fr (a,(b,s)) -> fr (a,(b,s))
id2 = id

id3 :: StackMachine fr => fr (a,(b,(c,s))) -> fr (a,(b,(c,s)))
id3 = id


tst4 = swap >> swap
-- tst4 :: (Forth fr) => fr (a, (b, N)) -> fr (a, (b, N))

st1 = push (1::Int) >>
      push (5::Int) >>
      push (3::Int) >>
      push True >>
      lamb (swap >> drop) >>
      appl

st1R = runR st1 -- (True,(5,(1,N)))

st2 = push (1::Int) >>
      push (5::Int) >>
      push (3::Int) >>
      push True >>
      lamb (swap >> drop) >>
      lamb (swap >> swap >> drop)  >>
      ifte'

st2R = runR st2 -- (3,(1,N))

smax = dup >>				-- b b a
       idx2 >> 				-- a b b a
       lequ >>				-- (b<=a) b a
       lamb (id2 >> drop) >>
       lamb (swap >> drop) >>
       ifte'
-- smax :: (StackMachine fr, Forth fr) => fr (Int, (Int, N)) -> fr (Int, N)

tsmax1 = push 1 >> push 2 >> smax
tsmax2 = push 2 >> push 1 >> smax

smaxl  = lamb smax

tsmax3 = push 2 >> push 1 >> smaxl >> appl


tsmax1R = runR tsmax1 -- (2,N)

tsmax3R = runR tsmax3 -- (2,N)


-- `Compiler'
newtype C a = C String
liftC f (C x) = C $ f x


instance StackMachine C where
    accu = C ""				-- Neutral element
    xmit (C s) (C t) = (C s, C t)
    smap f (C x) = C x

instance Forth C where
    push x = liftC (++ (" " ++ show x))
    sadd   = liftC (++ " add")
    smul   = liftC (++ " mul")
    swap   = liftC (++ " exch")
    drop   = liftC (++ " pop")
    lequ   = liftC (++ " le")
    dup    = liftC (++ " dup")
    idx2   = liftC (++ " 2 index")
    lamb f = liftC (++ " {" ++ runC' f ++ "}")
    appl   = liftC (++ " exec")
    ifte'  = liftC (++ " ifelse")

runC' m = let C r = m (C "") in r
runC m = runC' m ++ " =" -- to show the result

tst1C = runC tst1

tsmax1C = runC tsmax1 -- (2,N)

tsmax3C = runC tsmax3 -- (2,N)

-- copy2 a b -> a b a b


{-
GS>2 3 7 add add =
12
GS>2 3 add (hello) exch 7 add exch pop =
12
GS>/max {dup 2 index le {pop} {exch pop} ifelse} def
GS>1 5 max =
5
GS>5 3 max =
5
GS>5 5 max =
5
GS>/f {exch dup 0 le {pop pop 1} {dup -1 add 3 -1 roll exec mul} ifelse} def
GS>/fix {dup /fix cvx 2 array astore cvx exch exec} def
GS>1 {f} fix ==
1
GS>0 {f} fix ==
1
GS>2 {f} fix ==
2
GS>3 {f} fix ==
6
GS>5 {f} fix ==
120
GS>
-}
