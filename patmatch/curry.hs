{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

module Curry where

import Prelude hiding (uncurry, succ)

data Void
void = undefined :: Void

-- At first, this seems like pure plumbing for (un)currying from 
-- sequences <-> functions.  But it also turns out to be the encoding of
-- 'expressions'!  
--
type family   Curry vec     ans
type instance Curry Void    ans = ans
type instance Curry (a,vec) ans = a -> Curry vec ans

type UncurryFn vec ans = Curry vec ans -> vec -> ans

zero :: UncurryFn Void ans
zero f = \_ -> f

succ :: UncurryFn vec ans -> UncurryFn (a,vec) ans
-- ie (Curry vec ans -> vec -> ans) -> (a -> Curry vec ans) -> (a,vec) -> ans
succ n f = \(x,xs) -> n (f x) xs

uncurry :: UncurryFn vec ans -> Curry vec ans -> vec -> ans
uncurry n f = n f
