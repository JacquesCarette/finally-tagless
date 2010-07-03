{-# LANGUAGE RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module HPattern5 where

import Prelude hiding (uncurry, catch, fail, succ, any)
import Control.Monad hiding (fail)
import Control.Monad.Identity hiding (fail)
import Curry

class (Functor m, MonadPlus m) => PMCMonad m where
    extract :: m a -> a

instance PMCMonad Maybe where
    extract (Just x)  = x
    extract Nothing   = error "match"

-- This is an implementation of (monadic) sequence transformers; the
-- sequences are here explicitly implemented as nested tuples.
--
-- This 'encodes' the action of matching as a transformation on sequences,
-- which stand in for the set of bindings that the pattern makes.
type SeqTran vec vec' = MonadPlus m => vec -> m vec'

nil :: SeqTran vec vec
nil = return

one :: a -> SeqTran vec (a,vec)
one v = \ac -> return (v,ac)

(#) :: SeqTran vec' vec'' -> SeqTran vec vec' -> SeqTran vec vec'' 
m # n = \ac -> do 
                 r <- n ac
                 s <- m r
                 return s

fail :: SeqTran vec vec'
fail = \ac -> mzero

catch :: SeqTran vec vec' -> SeqTran vec vec' -> SeqTran vec vec'
m `catch` n = \ac -> (m ac) `mplus` (n ac)

-- The actual basic pattern combinators

data Pattern a vec vec' = Pattern
    { curryP :: forall ans. UncurryFn vec ans -> UncurryFn vec' ans
    , seqtrP :: a -> SeqTran vec vec'
    }

var :: Pattern a vec (a,vec)
var = Pattern
    { curryP = succ
    , seqtrP = one
    }

cst :: Eq a => a -> Pattern a vec vec
cst v' = Pattern
    { curryP = id
    , seqtrP = \v -> if v == v' then nil else fail
    }

pair :: Pattern a vec' vec'' -> Pattern b vec vec' -> Pattern (a,b) vec vec''
pair p q = Pattern
    { curryP = curryP p . curryP q
    , seqtrP = \v -> seqtrP p (fst v) # seqtrP q (snd v)
    }
    
-- In the sense of PMC, a Case is what we call a 'matching'
-- 'a' is the type of input variable, m ans is [[alpha]]^M
type Case a m ans = (PMCMonad m) => a -> m ans

-- This is the 'ground' case of |=> in PMC, ie where the pattern
-- is expected to have all its components 'swallowed' by the
-- expression 'k'
infix 3 ->>
(->>) :: Pattern a Void vec -> Curry vec ans -> Case a m ans
p ->> k = \v -> fmap (uncurry (curryP p zero) k) (seqtrP p v void)

infixl 2 |||
(|||) :: Case a m ans -> Case a m ans -> Case a m ans
c1 ||| c2 = \v -> (c1 v) `mplus` (c2 v)

-- mmatch for monadic match; to really use this, one needs to specify
-- what monad 'm' to use.
mmatch :: PMCMonad m => a -> Case a m ans -> ans
mmatch v cs = extract (cs v)

-- To deal with ambiguity, define match as being over Maybe
match = mmatch :: a -> Case a Maybe ans -> ans

-- This does not really exist in PMC per se, but is roughly
-- {p |=> |k|}, where k is an 'expression' (in HOAS), ie a lambda
-- expression!
infix 3 |->
(|->) :: Pattern a Void vec -> Curry vec ans -> (a -> ans)
p |-> k = \v -> match v $ p ->> k

-- From PMC, 'result extraction' (into the ambient 'e' monad)
mextract :: PMCMonad m => Case a m ans -> (a -> ans)
mextract = flip mmatch
-- for convenience
hextract = flip match

-- We also need 'feed'; note how we cannot use it as we do not
-- have any way to generate higher-order Case right now
into :: PMCMonad m => a -> (Case a m (b->ans)) -> Case b m ans
x `into` c = \v -> do r <- c x
                      return $ r v

-- Lifting from the expression world to a match
-- (but not a pattern?) would require a function of type
--     lift :: Curry vec ans -> Case a m ans
-- but that would require magically transforming vec -> a.
-- If we try to furnish the right functions, like
--     lift :: Curry vec ans -> (UncurryFn Void ans -> UncurryFn vec ans) 
--                           -> Case a m ans
-- but that still doesn't work as it's still not apparent to Haskell
-- that those vec's are the same

-- A bunch of additional patterns and pattern combinators
-- unfortunately, this still needs an argument to force things.
none :: Pattern a vec vec
none = Pattern
    { curryP = id
    , seqtrP = \v -> fail
    }

-- We can perform result extraction on none to get the PMC notion of fail
mnone :: Case a m ans
mnone = none ->> (undefined::a)

infixl 4 \/
(\/) :: Pattern a vec vec' -> Pattern a vec vec' -> Pattern a vec vec'
p \/ q = Pattern
    { curryP = curryP p
    , seqtrP = \v -> seqtrP p v `catch` seqtrP q v
    }


any :: Pattern a vec vec
any = Pattern
    { curryP = id
    , seqtrP = \v -> nil
    }

infixl 5 /\
(/\) :: Pattern a vec' vec'' -> Pattern a vec vec' -> Pattern a vec vec''
p /\ q = Pattern
    { curryP = curryP p . curryP q
    , seqtrP = \v -> seqtrP p v # seqtrP q v
    }
    
infix 3 ?
(?) :: Pattern a Void Void -> a -> Bool
p ? v = match v $ p ->> True ||| any ->> False


is :: (a -> Bool) -> Pattern a vec vec
is p = Pattern
    { curryP = id
    , seqtrP = \v -> if p v then nil else fail
    }

-- for lists (or any other algebraic type), must create these explicitly.  Ugh.

lnil :: Pattern [a] vec vec
lnil = Pattern
    { curryP = id
    , seqtrP = \v -> if null v then nil else fail
    }

infixr 5 .:
(.:) :: Pattern a vec' vec'' -> Pattern [a] vec vec' -> Pattern [a] vec vec''
p .: q = Pattern 
    { curryP = curryP p . curryP q
    , seqtrP = \v -> if null v then fail else seqtrP p (head v) # seqtrP q (tail v)
    }
