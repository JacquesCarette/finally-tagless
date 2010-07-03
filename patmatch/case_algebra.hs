{-# LANGUAGE MultiParamTypeClasses #-}
-- these extra are for testing only, not the code
{-# LANGUAGE NoMonomorphismRestriction #-}

-- This really reproduces Mark Tullsen's "First Class Patterns"
-- but wrapped in a newtype for reuse.  Removing the newtype makes
-- everything identical to that paper.
module Case_algebra where

import Prelude hiding (fail)
import Control.Monad hiding (fail)

-- In the sense of PMC, a Case is what we call a 'matching'
-- 'a' is the type of input variable, m ans is [[alpha]]^M
newtype Case a m ans = Case (a -> m ans)

-- Now we can directly define various combinators that are a
-- combination of patterns and pattern-matching rolled into one

var :: Monad m => Case a m a
var = Case $ return

-- The undefined here essentially means that the results from this
-- should never be looked at.
const :: (Eq a, MonadPlus m) => a -> Case a m b
const x = Case $ \y -> if x == y then return undefined else mzero

fail :: MonadPlus m => Case a m b
fail = Case $ \_ -> mzero

-- or
infixl 2 |||
(|||) :: MonadPlus m => Case a m ans -> Case a m ans -> Case a m ans
(Case c1) ||| (Case c2) = Case $ \v -> (c1 v) `mplus` (c2 v)

-- useful to have a flipped version of this too
infixl 2 ||.
(||.) :: MonadPlus m => Case a m ans -> Case a m ans -> Case a m ans
(||.) = flip (|||)

-- for tuples
pair :: Monad m => Case a m a1 -> Case b m a2 -> Case (a,b) m (a1,a2)
pair (Case p1) (Case p2) = 
    Case $ \v -> do r1 <- p1 (fst v)
                    r2 <- p2 (snd v)
                    return (r1,r2)

-- We also need 'feed'; note how we cannot use it as we do not
-- have any way to generate higher-order Case right now
into :: Monad m => a -> (Case a m (b->ans)) -> Case b m ans
x `into` (Case c) = Case $ \v -> do r <- c x
                                    return $ r v

