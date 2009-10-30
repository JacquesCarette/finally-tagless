{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Polymorphic EDSL in tagless final style, using 1-typeclass style
-- Inspired by the conversation with Tom Schrijvers and Dominic Orchard,
-- October 2009.

module Poly1 where

-- The one and only type-class ever needed

class Apply label r | label -> r where
    apply:: label -> r

data Const (sem :: * -> *) a = Const
data Add   (sem :: * -> *) a = Add

constant :: forall sem a. Apply (Const sem a) (a -> sem a) =>
	    (a -> sem a)
constant = apply (Const :: Const sem a)

add :: forall sem a. Apply (Add sem a) (sem a -> sem a -> sem a) =>
       sem a -> sem a -> sem a
add = apply (Add :: Add sem a)

-- One interpreter
newtype Eval a = E { eval :: a }

instance Num a => Apply (Const Eval a) (a -> Eval a) where
    apply _ = E

instance Num a => Apply (Add Eval a) (Eval a -> Eval a -> Eval a) where
    apply _ = \e1 e2 -> E $ eval e1 + eval e2


t1 = add (add (constant 1) (constant 2)) (constant 3)


-- The Show interpreter
newtype S a = S { sh :: String }

instance Show a => Apply (Const S a) (a -> S a) where
    apply _ = S . show

instance Apply (Add S a) (S a -> S a -> S a) where
    apply _ = \e1 e2 -> S $ sh e1 ++ sh e2

-- The eval interpreter is polymorphic over numbers: it can handle
-- any possible numbers
t1r :: Int
t1r = eval t1
-- 6

t1d :: Double
t1d = eval t1
-- 6.0

t1s = sh t1
-- "123"
