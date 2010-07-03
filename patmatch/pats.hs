{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts #-}

module Pats where

import Prelude hiding (uncurry)
import Control.Monad hiding (fail)
import HPattern5
import Curry
import qualified Case_algebra as C

class PatSym pat where
    pvar :: pat a vec (a,vec)
    pcst :: Eq a => a -> pat a vec vec
    ppair :: pat a vec' vec'' -> pat b vec vec' -> pat (a,b) vec vec''

class PatSym dpat => DecPatSym dpat where
    -- allow deconstruction too; these are somewhat restrictive
    -- and should be revisited
    pcurry :: dpat a vec vec' -> (UncurryFn vec ans ->
                                  UncurryFn vec' ans)
    pseqtr :: dpat a vec vec' -> (a -> SeqTran vec vec')

instance PatSym Pattern where
    pvar = var
    pcst = cst
    ppair = pair

instance DecPatSym Pattern where
    pcurry = curryP
    pseqtr = seqtrP

class CaseSym mat where
    mvar :: Monad m => mat a m a
    mcst :: (MonadPlus m, Eq a) => a -> mat a m b
    mpair :: Monad m => mat a m c -> mat b m d -> mat (a,b) m (c,d)

instance CaseSym C.Case where
    mvar = C.var
    mcst = C.const
    mpair = C.pair

-- It is important to note that a 'Matching' here is a full match, ie
-- it requires that one starts from an empty stack.  Note how this cannot
-- be made an instance of CaseSym!  See next function however.
data Matching a m ans = forall vec. Monad m =>
    PP { pat :: Pattern a Void vec
       , cur :: Curry vec ans } 

-- We can take any matching and make a 'Case' out of it.  This is because
-- a Matching really is a type-correct way of combining a pattern and 
-- something which swallows bindings (in our case, a Curry, ie a function)
mtoc :: Matching a m ans -> Case a m ans
mtoc (PP {pat = p, cur = f}) = p ->> f

-- We can define a ->>>, like ->> but over any (deconstructible) pattern
(->>>) :: DecPatSym pat => 
    pat a Void vec -> Curry vec ans -> Case a m ans
p ->>> f = \v -> fmap (uncurry (pcurry p zero) f) (pseqtr p v void)
