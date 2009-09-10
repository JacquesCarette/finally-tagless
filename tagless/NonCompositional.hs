{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

-- How to express non-compositional computations in the tagles style.
-- The example has been suggested by Jeremy Yallop, August 2009.

module NonCompositional where

import Control.Monad

class Expr exp where
    lit :: Int -> exp Int
    add :: exp Int -> exp Int -> exp Int
    tup :: exp a -> exp b -> exp (a,b)

newtype Value a = V { eval :: a }

instance Expr Value where
    lit v   = V v
    add m n = V (eval m + eval n)
    tup l r = V (eval l, eval r)

type Exp a = forall exp. Expr exp => exp a

evaluate :: Exp a -> a
evaluate e = eval e

add_ :: Exp Int -> Exp Int -> Exp Int
add_ m n = add m n

class Expr' exp where
    lit' :: Int -> exp Int
    add' :: Exp' Int -> Exp' Int -> exp Int
    tup' :: Exp' a -> Exp' b -> exp (a,b)

type Exp' a = forall exp. Expr' exp => exp a

instance Expr' Value where
    lit' x = V x
    add' m n = V (eval m + eval n)
    tup' l r = V (eval l, eval r)

newtype IsAdd a = IA { isAdd :: Bool }
 
instance Expr' IsAdd where
    lit' _   = IA False
    add' _ _ = IA True
    tup' _ _ = IA False

newtype Value' a = V' { eval' :: a }
 
instance Expr' Value' where
     lit' x = V' x
     add' m n = V' (if isAdd n
                     then 0
                     else eval' m + eval' n)
     tup' l r = V' (eval' l, eval' r)

test1' = tup' (add' (lit' 4) (lit' 5)) 
	      (add' (lit' 1) (add' (lit' 2) (lit' 3)))

test1 = tup (add (lit 4) (lit 5)) 
	    (add (lit 1) (add (lit 2) (lit 3)))

newtype Conv a = Conv{unConv :: Exp' a}

instance Expr Conv where
    lit x = Conv $ lit' x
    add x y = Conv $ add' (unConv x) (unConv y)
    tup x y = Conv $ tup' (unConv x) (unConv y)

test1'r = eval' test1'
-- (9,0)

test1r = evaluate test1
-- (9,6)

test1r'' = eval' $ unConv test1
-- (9,0)


newtype Monad m => MExp m a = MExp{unM:: m a}

instance Monad m => Expr (MExp m) where
    lit v   = MExp (return v)
    add m n = MExp (liftM2 (+) (unM m) (unM n))
    tup l r = MExp (liftM2 (,) (unM l) (unM r))

data ValueM a = VM' a Bool

instance Monad ValueM where
    return v = VM' v False
    VM' x isadd >>= f = let VM' v' isadd' = f x in VM' v' (isadd || isadd')

setVM flag (VM' x _) = VM' x flag

instance Expr ValueM where
    lit = return 
    add m n = setVM True $ case n of
			     VM' _ True -> VM' 0 True
			     _ -> liftM2 (+) m n
    tup m n = liftM2 (,) m n

test1rV = case test1 of VM' x _ -> x

