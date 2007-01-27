{-# OPTIONS -fglasgow-exts #-}

-- Tagless Interpreter and Compiler

module Inco where

{-
  The language is simply-typed lambda-calculus with fixpoint,
  integers and comparison.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | Add ie1 ie2 |
  IFEQ ie1 ie2 e-then e-else
  
  The language is just expressive enough for the Gibonacci function.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.

  The implementation below is *almost* possible in OCaml. Indeed,
  one can emulate Haskell typeclasses using explicit dictionary passing.
  The dictionary will by a polymorphic (rank-2) record -- but it is OK,
  OCaml permits it. Alas, The typeclass below uses higher-order type
  constructors: repr and res are of a kind * -> *. OCaml does not support
  higher-order polymorphism.

-}

-- This class defines syntax (and its instances, semantics) of our language

class Symantics repr res | repr -> res where
    int :: Int -> repr Int		-- int literal
    
    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b
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

    lam f = R (unR . f . R)
    app e1 e2 = R( (unR e1) (unR e2) )
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
-- Note how the compiler never raises any exception and matches no tags
-- (no generated code has any tags)

data ByteCode t where
    Var :: Int -> ByteCode t		-- variables identified by numbers
    Lam :: Int -> ByteCode t2 -> ByteCode (t1->t2)
    App :: ByteCode (t1->t2) -> ByteCode t1  -> ByteCode t2
    Fix :: Int -> ByteCode t -> ByteCode t
    INT :: Int -> ByteCode Int
    Add :: ByteCode Int -> ByteCode Int -> ByteCode Int
    IFEQ :: ByteCode Int -> ByteCode Int -> ByteCode t -> ByteCode t ->
	    ByteCode t


instance Show (ByteCode t) where
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
newtype C t = C (Int -> (ByteCode t, Int)) 
unC (C t) vc0 = t vc0
asC :: C x -> C x; asC = id

instance Symantics C ByteCode where
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
-- Compiler into the CPS bytecode
-- The same as above, only in CPS. For example, `int x'
-- will be compiled into Lam 0 (App (Var 0) (INT x)).


{-
Walid wants to see a typechecher: given an untyped term
such as AST (or language in the regular AST) convert it into our term.
Say, suppose we have

data Ue = UInt Int | UAdd Ue Ue | UApp Ue Ue | ULam Typ (Ue -> Ue) ...
data Typ = TInt | TA Typ Typ

write a function 
 typecheck :: Symatics repr res => Ue -> (forall a. res a -> w) -> w

the function can raise an error if the typecheking fails.

Note that ULam has the type annotation on the bound variable. We need
to write a typechecker rather than the type inferencer.
-}
 
