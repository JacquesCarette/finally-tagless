{-# OPTIONS -fglasgow-exts #-}

-- Interpreter, Compiler, Partial Evaluator

module Incope where

{-
  The language is simply-typed lambda-calculus with fixpoint,
  integers and comparison

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | Add ie1 ie2 |
  IFEQ ie1 ie2 e-then e-else
  
  The language just expressive enough for the Gibonacci function.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.
-}

-- This class defines syntax (and its instances, semantics) of our language

class Symantics repr res | repr -> res where
    int :: Int -> repr Int		-- int literal
    
    -- lam and app are now `monadic' and need a `join'
    -- lam :: (repr a -> repr b) -> repr (a->b)
    lam :: (repr a -> repr b) -> repr (repr a -> repr b)
    -- app :: repr (a->b) -> repr a -> repr b
    app :: repr (repr a -> repr b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a

    add :: repr Int -> repr Int -> repr Int
    ifeq :: repr Int -> repr Int -> repr a -> repr a -> repr a

    comp :: repr a -> res a

test1 () = add (int 1) (int 2)
test2 () = lam (\x -> add x x)

testgib () = lam (\x -> lam (\y ->
		  fix (\self -> lam (\n ->
		      ifeq n (int 0) x
			(ifeq n (int 1) y
			 (add (app self (add n (int (-1))))
			      (app self (add n (int (-2))))))))))


testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)

-- ------------------------------------------------------------------------
-- The interpreter
-- It is typed, tagless interpreter: R is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only

newtype R a = R a deriving Show
unR (R x) = x
asR :: R x -> R x; asR = id

instance Symantics R R where
    int x = R x

    lam f = R f
    app e1 e2 = (unR e1) e2
    fix f = R( fx (unR . f . R)) where fx f = f (fx f)

    add e1 e2 = R( (unR e1) + (unR e2) )
    ifeq ie1 ie2 et ee = R( if (unR ie1) == (unR ie2) then unR et else unR ee )

    comp = id

itest1 = unR (comp . asR . test1 $ ())
itest2 = unR (comp . asR . test2 $ ())
itest3 = unR (comp . asR . testgib1 $ ())


-- ------------------------------------------------------------------------
-- The compiler
-- We compile to GADT, to be understood as a typed assembly language
-- (typed bytecode). The GADT does _not_ use the higher-order abstract
-- syntax. We could have used template Haskell. Alas, its expressions
-- are untyped.
-- Note how ByteCode represents MetaOCaml's `code'. Thus the compiler
-- below neatly maps to the MetaOCaml (with no GADTs).
-- Note how teh compiler never raises any exception and matches no tags
-- (no generated code has any tags)

data ByteCode r t where
    Var :: Int -> ByteCode r t		-- variables identified by numbers
    Lam :: Int -> ByteCode r t2 -> ByteCode r (r t1-> r t2)
    App :: ByteCode r (r t1-> r t2) -> ByteCode r t1  -> ByteCode r t2
    Fix :: Int -> ByteCode r t -> ByteCode r t
    INT :: Int -> ByteCode r Int
    Add :: ByteCode r Int -> ByteCode r Int -> ByteCode r Int
    IFEQ :: ByteCode r Int -> ByteCode r Int -> ByteCode r t -> ByteCode r t ->
	    ByteCode r t


instance Show (ByteCode r t) where
    show (Var n) = "V" ++ show n
    show (Lam n b) = "(\\V" ++ show n ++ " -> " ++ show b ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Fix n b) = "(fix\\V" ++ show n ++ " " ++ show b ++ ")"
    show (INT n) = show n
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (IFEQ ie1 ie2 et ee)
	= "(if " ++ show ie1 ++ " == " ++ show ie2  ++
	  " then " ++ show et ++ " else " ++ show ee ++ ")"


-- Int is the variable counter
-- for allocation of fresh variables
newtype C t = C (Int -> (ByteCode C t, Int)) 
unC (C t) vc0 = t vc0
asC :: C x -> C x; asC = id

instance Symantics C (ByteCode C) where
    int x = C(\vc -> (INT x, vc))

    lam f = C(\vc -> let v = vc
	                 var = C(\vc -> (Var v, vc))
	                 (body,vc') = unC (f var) (succ vc)
	             in (Lam v body, vc'))
    app e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
		             (e2b,vc2) = unC e2 vc1
		         in (App e1b e2b,vc2))

    fix f = C(\vc -> let v = vc
	                 var = C(\vc -> (Var v, vc))
	                 (body,vc') = unC (f var) (succ vc)
	             in (Fix v body, vc'))

    add e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
		             (e2b,vc2) = unC e2 vc1
		         in (Add e1b e2b,vc2))

    ifeq ie1 ie2 et ee = C(\vc -> let (ie1b,vc1) = unC ie1 vc
		                      (ie2b,vc2) = unC ie2 vc1
		                      (etb,vc3)  = unC et  vc2
		                      (eeb,vc4)  = unC ee  vc3
		         in (IFEQ ie1b ie2b etb eeb,vc4))

    comp repr = fst $ unC repr 0

ctest1 = comp . asC . test1 $ ()
ctest2 = comp . asC . test2 $ ()
ctest3 = comp . asC . testgib1 $ ()

-- ------------------------------------------------------------------------
-- The partial evaluator


data P t = V t (C t) | E (C t)
asP :: P x -> P x; asP = id

abstr :: P t -> C t
abstr (V _ e) = e
abstr (E e) = e


instance Symantics P ByteCode where
    int x = V x (int x)


e :: P a -> P b

P (P a -> P b) 
V (P a -> P b) (C (P a -> P b))

C (C a -> C b)


V (a->b)
V (\x -> e (V x 
    lam e = V e (C (\vc -> xxx))
	    where 
	    c1 = (lam (abstr . e . E)) -- C (C a -> C b)

--    lam e = V e (lam (abstr . e . E))
{-
    app e1 e2 = case e1 of
		 (V n1 _) -> n1 e2
		 _ -> E $ app (abstr e1) (abstr e2)
-}

    add e1 e2 = case (e1,e2) of
		 (V n1 _,V n2 _) -> V nr (int nr) where nr = n1 + n2
		 _ -> E $ add (abstr e1) (abstr e2)

    ifeq ie1 ie2 et ee = case (ie1,ie2) of
			  (V n1 _, V n2 _) -> if n1==n2 then et else ee
			  _ -> E $ ifeq (abstr ie1) (abstr ie2)
			                (abstr et) (abstr ee)


{-
    fix :: (repr a -> repr a) -> repr a

-}

    comp = comp . abstr

ptest1 = comp . asP . test1 $ ()
-}
