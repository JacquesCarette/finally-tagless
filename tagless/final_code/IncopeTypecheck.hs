{-# OPTIONS -fglasgow-exts -W #-}

-- Interpreter, Compiler, Partial Evaluator 
-- Typechecking into Symantics terms

-- The code demonstrates conversion of untyped terms into 
-- typed terms that can be interpreted by various typed interpreters. 
-- Typechecking happens only once, regardless of the number of 
-- interpretations of the term. Although the typechecking function is
-- necessarily partial (source terms may be ill-typed), we guarantee
-- the resulting term can be interpreted without any pattern-match errors.
-- Furthermore, these assurances are _syntactically_ apparent.

-- That outcome is not trivial. Indeed, suppose we have a function
-- tc :: UExp -> Gamma -> DynTerm repr, where UExp represents an 
-- untyped expression (the result of parsing the source language)
-- and DynTerm represents a typed expression of _some_ type, 
-- interpreted by _any_ interpreter 'repr'.
--
-- For example, 
--    tc (UAdd (UInt 1) (UInt 2)) []
-- should give 
--    DynTerm (add (int 1) (int 2))
-- And it does, see the test dt1 below.
--
-- The trouble comes when we type-check a function, for example,
--    UL "x" (UInt 100) (UAdd (UVar "x") (UInt 1))
-- (here, (UInt 100) is a `type annotation', meaning that the bound variable
-- "x" must have the type Int. All bound variables in the source language
-- must be type annotated. The restriction is lifted if we implement
-- the full HM typechecker with inference).
-- The result of typechecking the function should be something like
--    DynTerm (lam (\x -> ...))
-- where ... is the result of type-checking the body assuming that "x"
-- has the type Int. 
--
-- Naively, we would write
--   DynTerm (lam (\x -> unDynTerm $ tc (UAdd (UVar "x") (UInt 1)) [("x",x)]))
-- Although that approach works, it is horribly inefficient because
-- the typechecking of the function body, 
--   tc (UAdd (UVar "x") (UInt 1)) [("x",x)]
-- will be run _every_ time our function is applied. We have the interpretation
-- overhead at the typechecking stage. We have, however, a far more serious
-- problem: the function |tc| cannot be total. Indeed, an untyped expression
-- UExp may be ill-typed, and so |tc| should not produce any corresponding
-- DynTerm (as it just doesn't exist). The naive approach thus compromises
-- our totality guarantee: the interpretation of a type-checked term raises
-- no (pattern-match) error or any such exception.
--
-- We are left in a quandary then: what should be the result of typechecking
-- the body of our function, (UAdd (UVar "x") (UInt 1))
-- It should be something like (DynTerm (add ??? (int 1))). But what
-- to put in place of ??? This the the body of a function, not applied to
-- anything, so we do not have any real value for the argument. We
-- seem to need a way to _refer to_ an argument without requiring its value.
-- We need a _future stage_ argument. At present stage, when typechecking
-- lambda, we do not need to know the value of that future stage argument.
-- But we do need to know the type of that future value, now. Furthermore,
-- we need a guarantee that this future value will always be replaced with
-- a real value of the same type. Always! And that fact has to be syntactically
-- apparent! It seems to type-check into a higher-order abstract syntax,
-- we need staging!
--
-- This code shows that we can accomplish our task, typechecking into
-- a patently error-free HOAS terms, without any staging, GADTs or typeclasses
-- with functional dependencies. Of all extensions to Haskell98,
-- we only use existentials and Typeable (the latter being expressible in
-- Haskell98 with existentials, see many papers on type-safe casts).
-- This work bears great similarity to our PEPM08 paper on translating
-- away staging. Not only in spirit, but also in technique: using functions
-- to represent _hypothetical_ terms (terms subject to hypotheses)
-- and using conversions to represent weakening of the hypothetical
-- environment. Connections to logic, specifically Deduction Theorem and
-- explicit structural rules, must be apparent.
--
-- Coming back to our example, the typechecking of (UInt 1) gives
-- DynTerm (int 1), in the empty environment. Since the type-checking env
-- includes "x" of the type int, we must weaken the result, giving us
-- DynTerm (lam (\x -> int 1)). (UVar "x") in the env [("x",int)]
-- is translated to DynTerm (lam (\x -> x)). We now need to "add"
-- the two results. First, we weaken the "add" function from the empty
-- environment to the current environment, [("x",int)]. We obtain
-- lam (\u -> lam (\v -> lam (\x -> add (u x) (v x)))). This is the
-- _total_ term. To make the body of the function, we add the applications:
-- DynTerm (
--          lam (\u -> lam (\v -> lam (\x -> add (u x) (v x))))
--          `app` (lam (\x -> x))
--          `app` (lam (\x -> int 1)))
-- This is the result of typechecking _the_ body of the function,
-- (UAdd (UVar "x") (UInt 1)) in the environment [("x",int)].
-- The result contains many administrative redices -- but they are all
-- *total*. The result includes only total operations; it is
-- _syntactically_ patent that interpreting this term in any interpreter
-- (where additions and applications are total) will also be total and
-- raise no errors whatsoever.
-- Also not surprisingly, the result of typechecking of the _body_ of the
-- function happens do be the same as that of typechecking the whole
-- function: Deduction Theorem!
--
-- Our translation includes many administrative redices. 
-- We need cut-elimination!

module IncopeTypecheck where

import Incope
import Data.Typeable
import Control.Monad
import Control.Monad.Error
import Text.Show.Functions

-- Representing parsed AST terms
--
-- The following |DynTerm repr| is the result of the parser/typechecker
-- that reads object terms from a string or a file.
-- The type of the term is abstracted over; however, the parser does not commit
-- to any interpretation of that term. As the following shows, we can
-- interpret the same `dynamic' term in multiple ways.
-- This test was inspired by the discussion with Adam Chlipala
-- on Lambda-the-Ultimate on Sep 4-5, 2007.

-- The Show constraint here is so that we could see the result of the
-- computation...
data DynTerm repr = forall a. (Show a, Typeable a) => DynTerm (repr a)


-- A sample `dynamic' object term
dynterm1 () = DynTerm (lam (\x -> x) `app` (bool True))

-- Examples of several interpretations, using R, L, C, and P interpreters
dynterm_R_show = case dynterm1 () of DynTerm t -> show $ compR t
-- "True"
dynterm_L_show = case dynterm1 () of DynTerm t -> show $ compL t
-- "3"
dynterm_C_show = case dynterm1 () of DynTerm t -> show $ compC t
-- "((\\V0 -> V0) True)"
dynterm_P_show = case dynterm1 () of DynTerm t -> show $ compP t
-- "True"

-- Typechecking
-- Representing untyped terms (the AST produced by the parser, for example)
-- Whenever we need types (to annotate a bound variable), we use
-- a closed expression, whose (inferred) type serves as the needed annotation
data UExp = UInt Int | UBool Bool | UAdd UExp UExp | UIf UExp UExp UExp
	  | UVar VarName
	  | UL VarName UExp UExp -- UL var vart body
                                 -- vart is an UExp whose type is the type
                                 -- of var
	  | UApp UExp UExp
	  deriving Show

{-
 Top-down vs bottom-up type-checking.
 To typecheck the body of lambda-expressions, we need hypothetical reasoning:
 we typecheck the body of the lambda on assumption that the bound
 variable has the given type (given as an annotation in the lambda-form).
 We represent typechecked hypothetical expression as a function. For example,
 when typechecking
   (UL "x" (UInt 1) (UL "y" (UInt 1) (UAdd (UVar "x") (UVar "y"))))
 (UVar "x") will be represented as 
   DynTerm (lam (\x -> lam (\y -> x)))
 and (UAdd (UVar "x") (UVar "y")) will be represented as 
   DynTerm (lam (\x -> lam (\y -> 
     add ((lam (\x -> lam (\y -> x))) `app` x `app` y)
         ((lam (\x -> lam (\y -> y))) `app` x `app` y))))

 The representation has obvious overhead (administrative beta-redices).
 However, it is obviously total and stuck-free. It is syntactically patent
 that there will be no error when accessing the environment at run-time.
 We are assured that the environment shall have the needed data of the
 right types.
     
 We implement below the top-down typechecking algorithm: that is, the function
 typecheck receives a term to typecheck and the environment Gamma, describing
 the names of bound variables in scope. We could have used the bottom-up
 approach, where each DynTerm carries with it Gamma, the list of variables
 (hypotheses) it needs. When typechecking (UAdd e1 e2), we would typecheck
 e1 and e2, find the environments needed by both e1 and e2 and will have to
 _weaken_ them (reconcile). The bottom-up approach is greatly reminiscent
 of environment coercions in our PEPM2008 paper on translating away staging.

-}


ap2 f m1 m2    = do v1 <- m1; v2 <- m2; f v1 v2
ap3 f m1 m2 m3 = do v1 <- m1; v2 <- m2; v3 <- m3; f v1 v2 v3

type Gamma = [(VarName,TypeRep)]
type VarName = String

-- Typechecking is obviously partial
typecheck :: forall repr. Symantics repr => 
	     UExp -> Gamma -> Either String (DynTerm repr)
typecheck (UInt x)  g = return . weaken g $ DynTerm (int x)
typecheck (UBool x) g = return . weaken g $ DynTerm (bool x)

typecheck (UVar v) g 
    | (g',((v,vt):g'')) <- break (\ (v',_) -> v == v') g,
       DynTerm argt <- reflect vt
  = return . weaken g' . weaken_under g'' $
			(DynTerm (lam (\x -> x `asTypeOf` argt)))
typecheck (UVar v ) g = fail $ "unbound var: " ++ v

typecheck (UAdd e1 e2) g = ap2 (tadd g) (typecheck e1 g) (typecheck e2 g)
 where
 tadd [] (DynTerm e1) (DynTerm e2) | Just e1' <- gcast e1,
                                     Just e2' <- gcast e2
				    = return $ DynTerm (add e1' e2')
 tadd [] _ _ = fail "Add: args are not int exp"
 tadd g d1 d2 = typecheck_app (weaken_add g) d1 >>= \f -> typecheck_app f d2

typecheck (UL v vt body) g
  | Right ((DynTerm vtt)::DynTerm repr) <- typecheck vt []
  = typecheck body (g ++ [(v,typerep vtt)])
typecheck (UL v vt body) g = fail $ unwords ["term used for annotating lambda",
					     "is ill-typed",show vt]

typecheck e@(UApp e1 e2) g = ap2 (tapp g) (typecheck e1 g) (typecheck e2 g)
 where
 tapp [] d1 d2 = typecheck_app d1 d2
 tapp g d1@(DynTerm d1') d2
  | Just tf <- typerep_without_hyp d1' g,
    Just wapp <- weaken_app g tf
  = typecheck_app wapp d1 >>= \f -> typecheck_app f d2
 tapp _ _ _ = fail $ "Bad app: " ++ show e


{-
typecheck e@(UIf e1 e2 e3) g = 
    ap3 (tif g) (typecheck e1 g) (typecheck e2 g) (typecheck e3 g)
 where
 tif [] (DynTerm e1) (DynTerm e2) (DynTerm e3) |
  Just e1' <- gcast e1, Just e3' <- gcast e3
	   = return $ DynTerm (if_ e1' e2 e3')
 tif _ _ _ = fail $ "Bad If: " ++ show e
-}

-- Typecheck the application of two types terms in the empty env
typecheck_app :: Symantics repr => 
		 DynTerm repr -> DynTerm repr -> Either String (DynTerm repr)
typecheck_app (DynTerm e1) (DynTerm e2) | 
       let tfun = typerep e1,
       let targ = typerep e2,
       Just tres <- funResultTy tfun targ,
       DynTerm b <- reflect tres,
       Just e1' <- gcast e1
     = return $ DynTerm (app e1' e2 `asTypeOf` b)
typecheck_app (DynTerm e1) (DynTerm e2) = 
    fail $ unwords ["Bad App, of types ",show (typerep e1),"and",
		    show (typerep e2)]

{-
missing
    fix :: (repr a -> repr a) -> repr a
    if_ :: repr Bool -> repr a -> repr a -> repr a

    mul :: repr Int -> repr Int -> repr Int
    leq :: repr Int -> repr Int -> repr Bool
They are all analogous to the above and left as an exercise to the reader.
-}


-- Weaken the term: given the term and Gamma, return the hypothetical
-- term with all the extra hypotheses of Gamma
-- That is, given a term d and the env [("x",typx),("y",typy),...]
-- return lam (\x -> lam (\y -> ... d))
weaken :: Symantics repr => Gamma -> DynTerm repr -> DynTerm repr
weaken [] d = d
weaken ((_,typ):g) d 
  | DynTerm xt <- reflect typ,
    DynTerm d' <- weaken g d
  = let mkfun :: repr a -> repr (a ->b); mkfun = undefined
    in DynTerm (lam (\x -> d') `asTypeOf` mkfun xt)


-- Given a functional term lam (\u -> t) and the 
-- env [("x",typx),("y",typy),...]
-- return lam (\u -> lam (\x -> lam (\y -> ... t)))
-- Note mkfun below is a logical rule!
weaken_under :: Symantics repr => Gamma -> DynTerm repr -> DynTerm repr
weaken_under [] d = d
weaken_under ((_,typ):g) d
  | DynTerm xt  <- reflect typ,
    DynTerm d   <- weaken_under g d,
    Just (DynTerm u,DynTerm t) <- reflect_fn . typerep $ d,
    Just d' <- gcast d
  = let mkfun :: repr a -> repr b -> repr c -> repr (a -> b -> c)
	mkfun = undefined
    in DynTerm (lam (\u -> lam (\x -> app d' u)) `asTypeOf` 
		    (mkfun u xt t))

-- Given a function with two arguments
-- lam (\u -> lam (\v -> t u v))
-- and the environment [("x",typx),("y",typy),...],
-- weaken the arguments of the function, returning
-- lam (\u -> lam (\v -> lam(\x -> lam(\y -> ... t (u x y ...) (v x y ...)))))
weaken_arg2 :: Symantics repr => Gamma -> DynTerm repr -> DynTerm repr
weaken_arg2 [] d = d
weaken_arg2 ((_,typ):g) d 
  | DynTerm xt  <- reflect typ,
    DynTerm d   <- weaken_arg2 g d,
    Just (DynTerm u,DynTerm tu) <- reflect_fn . typerep $ d,
    Just (DynTerm v,DynTerm t)  <- reflect_fn . typerep $ tu,
    Just d' <- gcast d
  = let mkfun :: repr u -> repr v -> repr x -> repr t ->
		 repr ((x->u) -> (x->v) -> x -> t)
	mkfun = undefined
    in DynTerm (lam (\u -> lam (\v -> 
		   lam (\x -> (d' `app` (u `app` x)) `app` (v `app` x))))
		`asTypeOf` (mkfun u v xt t))

weaken_add :: Symantics repr => Gamma -> DynTerm repr
weaken_add [] = DynTerm (lam (\x -> lam (\y -> add x y)))
weaken_add g = weaken_arg2 g (weaken_add [])

weaken_app :: Symantics repr => Gamma -> TypeRep -> Maybe (DynTerm repr)
weaken_app [] tf
  | Just (DynTerm tx,DynTerm tb) <- reflect_fn tf
  = let mkfun :: repr x -> repr b -> repr ((x -> b) -> x -> b)
	mkfun = undefined
    in Just $ DynTerm (lam (\x -> lam (\y -> app x y)) 
		       `asTypeOf` (mkfun tx tb))
weaken_app [] _ = Nothing
weaken_app g tf = liftM (weaken_arg2 g) (weaken_app [] tf)


-- Auxiliary functions

typerep :: forall a repr. Typeable a => repr a -> TypeRep
typerep _ = typeOf (undefined::a)

-- Check to see if a term represents a function. If so,
-- return terms that witness the type of the argument and the body
reflect_fn :: TypeRep -> Maybe (DynTerm repr, DynTerm repr)
reflect_fn tfun
   | (con,[arg1,arg2]) <- splitTyConApp tfun, con == arrowTyCon
   = Just (reflect arg1, reflect arg2)
reflect_fn _ = Nothing

arrowTyCon = typeRepTyCon (typeOf (undefined::Int->Int))

typerep_without_hyp :: forall a repr. Typeable a => 
		       repr a -> Gamma -> Maybe TypeRep
typerep_without_hyp _ [] = Just $ typeOf (undefined::a)
typerep_without_hyp t (_:g) 
  | Just tr <- typerep_without_hyp t g,
    Just (_,DynTerm tr) <- reflect_fn tr
  = Just (typerep tr)
typerep_without_hyp _ _ = Nothing


-- reflect typerep to a type (witness)
reflect :: forall repr. TypeRep -> DynTerm repr
reflect x | x == typeOf (undefined::Int) = DynTerm (undefined::repr Int)
reflect x | x == typeOf (undefined::Bool) = DynTerm (undefined::repr Bool)
reflect x | Just (DynTerm e1, DynTerm e2) <- reflect_fn x
   = let mkfun :: repr a -> repr b -> repr (a->b); mkfun = undefined
     in DynTerm (mkfun e1 e2)


-- ------------------------------------------------------------------------
--		Tests

tc :: Symantics repr => UExp -> DynTerm repr
tc exp = either error id (typecheck exp [])

rShow (DynTerm t) = show $ compR t
lShow (DynTerm t) = show $ compL t
cShow (DynTerm t) = show $ compC t
pShow (DynTerm t) = show $ compP t


dt1:: Symantics repr => DynTerm repr
dt1 = tc (UAdd (UInt 1) (UInt 2))
dt1rs = rShow $ dt1 -- "3"
dt1rc = cShow $ dt1 -- "(1 + 2)"

dt2:: Symantics repr => DynTerm repr
dt2 = tc (UIf (UBool True) (UAdd (UInt 1) (UInt 2)) (UInt 3))
dt2rs = rShow $ dt2  -- "3"
dt2rc = cShow $ dt2  -- "(if True then (1 + 2) else 3)"

dt3:: Symantics repr => DynTerm repr
dt3 = tc (UIf (UBool True) (UAdd (UInt 1) (UInt 2)) (UBool False))
dt3rs = rShow $ dt3 -- type error (raised by tc, but reported lazily here)


-- Higher-order tests
dt4:: Symantics repr => DynTerm repr
dt4 = tc (UL "x" (UInt 1) (UL "y" (UBool True) (UL "z" (UInt 1) (UVar "y"))))
dt4rc = cShow $ dt4
 -- "(\\V0 -> (\\V1 -> (\\V2 -> ((\\V3 -> V3) V1))))"
 -- not pretty but total

dt41:: Symantics repr => DynTerm repr
dt41 = tc (UL "x" (UInt 1) (UL "y" (UBool True) (UL "z" (UInt 1) (UVar "x"))))
dt41rc = cShow $ dt41
-- "(\\V0 -> (\\V1 -> ((\\V2 -> (\\V3 -> ((\\V4 -> V4) V2))) V0)))"

-- The example discussed in the title comments
dt5:: Symantics repr => DynTerm repr
dt5 = tc (UL "x" (UInt 100) (UAdd (UVar "x") (UInt 1)))
dt5rc = cShow $ dt5
-- "(((\\V0 -> (\\V1 -> 
--    (\\V2 -> (((\\V3 -> (\\V4 -> (V3 + V4))) (V0 V2)) (V1 V2))))) 
--   (\\V5 -> V5)) (\\V6 -> 1))"
-- V2 corresponds to x, all other are administrative vars...
dt5rp = pShow $ dt5
-- "(\\V0 -> (V0 + 1))"
-- partial evaluator removed all administrative redices...

-- Making a deliberate mistake...
dt6:: Symantics repr => DynTerm repr
dt6 = tc (UL "x" (UBool True) (UAdd (UVar "x") (UInt 1)))
dt6rc = cShow $ dt6 -- forcing the evaluation of dt6
-- "*** Exception: Bad App, 
-- of types  (Bool -> Int) -> (Bool -> Int) -> Bool -> Int and Bool -> Bool
-- Error messages of course leave much to be desired...


-- Untyped Church Numeral 2
exp_n2 = (UL "f" (UL "x" (UInt 100) (UInt 100))
            (UL "x" (UInt 100)
	      (UApp (UVar "f") (UApp (UVar "f") (UVar "x")))))

dt7:: Symantics repr => DynTerm repr
dt7 = tc exp_n2
dt7rc = cShow $ dt7 -- too big to show
dt7rp = pShow $ dt7 -- "(\\V0 -> (\\V1 -> (V0 (V0 V1))))"

dt8:: Symantics repr => DynTerm repr
dt8 = tc (UApp (UApp exp_n2 (UL "x" (UInt 100) (UAdd (UVar "x") (UVar "x"))))
            (UInt 1))
dt8rc = cShow $ dt8 -- too big to show
dt8rp = pShow $ dt8 -- "4"

