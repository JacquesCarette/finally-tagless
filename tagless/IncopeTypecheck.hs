{-# OPTIONS -fglasgow-exts -W #-}

-- Interpreter, Compiler, Partial Evaluator 
-- Typechecking into Symantics terms

-- The code demonstrates conversion of untyped terms into 
-- typed terms that can be interpreted by various typed interpreters. 
-- Typechecking happens only once, regardless of the number of 
-- interpretations of the term. Although typechecking function is
-- necessarily partial (source terms may be ill-typed), the resulting
-- terms are assured to be intertible without pattern-match errors.
-- Furthermore, these assurances are _syntactically_ apparent.

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
-- an closed expression, whose (inferred) type serves as the needed annotation
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
 However, it is obviouslt total and stuck-free. It is syntactically patent
 that there will be no error when accessing the environment at run-time.
 We are assured that the environment shall have the needed data of the
 right types.
     
 We implement below the top-down typechecking algorithm: that is, teh function
 typecheck receives a term to typecheck and the environment Gamma, describing
 the names of bound variables in scope. We could have used the bottom-up
 approach, where each DynTerm carries with it Gamma, the list of variables
 (hypotheses) it needs. When typechecking (UAdd e1 e2), we would typecheck
 e1 and e2, find the environments needed by both e1 and e2 and will have to
 _weaken_ them (reconcile). The bottom-up approach is greatly reminiscent
 of environment coercions in our PEPM2008 paper on translating away staging.

-}

-- Typechecking is obviously partial
-- Error messages could have been better, but I'm hungry...

ap2 f m1 m2    = do v1 <- m1; v2 <- m2; f v1 v2
ap3 f m1 m2 m3 = do v1 <- m1; v2 <- m2; v3 <- m3; f v1 v2 v3

type Gamma = [(VarName,TypeRep)]
type VarName = String

typecheck :: forall repr. Symantics repr => 
	     UExp -> Gamma -> Either String (DynTerm repr)
typecheck (UInt x)  g = return . weaken g $ DynTerm (int x)
typecheck (UBool x) g = return . weaken g $ DynTerm (bool x)
typecheck (UVar v) g | (g',((v,vt):g'')) <- break (\ (v',_) -> v == v') g,
		       DynTerm argt <- reflect vt
  = return $ weaken g' (weaken_under g''
			(DynTerm (lam (\x -> x `asTypeOf` argt))))
typecheck (UVar v ) g = fail $ "unbound var: " ++ v
typecheck (UAdd e1 e2) g = ap2 (tadd g) (typecheck e1 g) (typecheck e2 g)
 where
 tadd [] (DynTerm e1) (DynTerm e2) | Just e1' <- gcast e1,
                                     Just e2' <- gcast e2
				    = return $ DynTerm (add e1' e2')
 tadd [] _ _ = fail "Add: args are not int exp"
 tadd g d1 d2 = typecheck_app (weaken_add g) d1 >>= \f -> typecheck_app f d2
typecheck (UL v vt body) g | 
  Right ((DynTerm vtt)::DynTerm repr) <- typecheck vt []
  = typecheck body ((v,typerep vtt):g)
typecheck (UL v vt body) g = fail $ unwords ["term used for annotating lambda",
					     "is ill-typed",show vt]





{-
typecheck e@(UIf e1 e2 e3) = 
    ap3 tif (typecheck e1) (typecheck e2) (typecheck e3)
 where
 tif (DynTerm e1) (DynTerm e2) (DynTerm e3) |
  Just e1' <- gcast e1, Just e3' <- gcast e3
	   = return $ DynTerm (if_ e1' e2 e3')
 tif _ _ _ = fail $ "Bad If: " ++ show e
typecheck e@(UApp e1 e2) = ap2 tapp (typecheck e1) (typecheck e2)
 where
 tapp (DynTerm e1) (DynTerm e2) | let tfun = typerep e1,
				  let targ = typerep e2,
				  Just tres <- funResultTy tfun targ,
				  DynTerm b <- reflect tres,
				  Just e1' <- gcast e1
       = return $ DynTerm (app e1' e2 `asTypeOf` b)
 tapp _ _ = fail $ "Bad App: " ++ show e
typecheck (UK e1 e2) = ap2 tuk (typecheck e1) (typecheck e2)
 where
 mkfun :: Symantics repr => repr a -> repr b -> repr (a -> b -> a)
 mkfun _ _ = lam (\x -> lam (\y -> x))
 tuk (DynTerm e1) (DynTerm e2) = return $ DynTerm (mkfun e1 e2)

-}

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
    fail $ unwords ["Bad App, of types ",show (typerep e1),show (typerep e2)]



-- Weaken the term: given the term and Gamma, return the hypothetical
-- term with all the extra hypotheses of Gamma
weaken :: Symantics repr => Gamma -> DynTerm repr -> DynTerm repr
weaken [] d = d
weaken ((_,typ):g) d = case (weaken g d,reflect typ) of
      (DynTerm d',DynTerm argt) ->
	  let mkfun :: repr a -> repr (a ->b); mkfun = undefined
	  in DynTerm (lam (\x -> d') `asTypeOf` mkfun argt)


weaken_under :: Symantics repr => Gamma -> DynTerm repr -> DynTerm repr
weaken_under [] d = d
weaken_under ((_,typ):g) d = case (weaken_under g d,reflect typ) of
      (e@(DynTerm d),DynTerm argt) | 
			   Just (DynTerm xt,DynTerm b) <- reflect_fn e,
			   Just d' <- gcast d ->
	  let mkfun :: repr a -> repr b -> repr c -> repr (a -> b -> c)
	      mkfun = undefined
	  in DynTerm (lam (\x -> lam (\y -> app d' x)) `asTypeOf` 
		      (mkfun xt argt b))

{-
weaken_under2 :: Symantics repr => Gamma -> 
		DynTerm repr -> DynTerm repr
weaken_under2 [] d = d
weaken_under2 ((_,typ):g) d = case (weaken_under2 g d,reflect typ) of
      (e@(DynTerm d),DynTerm argt) | 
			 Just (DynTerm xt,bx) <- reflect_fn e,
			 Just (DynTerm yt,b) <- reflect_fn bx,
			 Just d' <- gcast d ->
	  let mkfun :: repr a -> repr b -> repr c -> repr d -> 
		       repr (a -> b -> c -> d)
	      mkfun = undefined
	  in DynTerm (lam (\x -> lam (\y -> lam (\z -> d' x)) `asTypeOf` 
		      (mkfun xt argt b))
 
-}

typerep :: forall a repr. Typeable a => repr a -> TypeRep
typerep _ = typeOf (undefined::a)

weaken_add :: Symantics repr => Gamma -> DynTerm repr
weaken_add [] = DynTerm (lam (\x -> lam (\y -> add x y)))

reflect_fn :: DynTerm repr -> Maybe (DynTerm repr, DynTerm repr)
reflect_fn (DynTerm d) |
   let tfun = typerep d,
   (con,[arg1,arg2]) <- splitTyConApp tfun, con == arrowTyCon =
   Just (reflect arg1, reflect arg2)
reflect_fn _ = Nothing
  

{-
hyp_app2 :: Gamma -> 
	    (DynTerm repr -> DynTerm repr -> Either String (DynTerm repr)) ->
	    DynTerm repr -> DynTerm repr -> Either String (DynTerm repr)
hyp_app2 [] f d1 d2 = f d1 d2
hyp_app2 ((_,typ):g) f (DynTerm d1) (DynTerm d2)
	 | Just d1' <- gcast d1
	DynTerm (lam (\x -> 


(\a -> \b -> x)
(\a -> \b -> y)

lam (\x -> lam (\y -> add x y))


l1 (\x -> inc x) (\x -> z)
l1 = lam (\f -> lam (\arg -> lam (\e1 -> (f e1) (arg e1))))

(\h -> x) -> (\h -> y) -> (\h -> r)

add1 = lam (\x -> lam (\y -> lam (\e1 -> add (x e1) (y e1))))

add1 `app` t1 `app` t2
-}



-- typerep_dt ::  DynTerm repr -> TypeRep
-- typerep_dt (DynTerm dt) = typerep dt

-- reflect typerep to a type (witness)
reflect :: forall repr. TypeRep -> DynTerm repr
reflect x | x == typeOf (undefined::Int) = DynTerm (undefined::repr Int)
reflect x | x == typeOf (undefined::Bool) = DynTerm (undefined::repr Bool)
reflect x | (con,[arg1,arg2]) <- splitTyConApp x, con == arrowTyCon =
  case (reflect arg1, reflect arg2) of
    (DynTerm e1, DynTerm e2) -> 
	let mkfun :: repr a -> repr b -> repr (a->b); mkfun = undefined
        in DynTerm (mkfun e1 e2)

arrowTyCon = typeRepTyCon (typeOf (undefined::Int->Int))

-- A special case of reflect, to reflect a function ta -> tb
reflect_fun :: forall repr. TypeRep -> TypeRep -> DynTerm repr
reflect_fun ta tb = reflect (mkFunTy ta tb)


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



{-
missing
    fix :: (repr a -> repr a) -> repr a

    mul :: repr Int -> repr Int -> repr Int
    leq :: repr Int -> repr Int -> repr Bool
-}

-- Higher-order tests
dt4:: Symantics repr => DynTerm repr
dt4 = tc (UL "x" (UInt 1) (UL "y" (UBool True) (UL "z" (UInt 1) (UVar "y"))))
dt4rc = cShow $ dt4
 -- "(\\V0 -> (\\V1 -> (\\V2 -> ((\\V3 -> V3) V1))))"
 -- not pretty but total

{-
ut = UK (UInt 1) (UBool True) -- K specialized to Int->Bool->Int
ut2 = UK ut (UBool True)      -- K whose 1st arg is of type (Int->Bool->Int)

-- Higher-order tests
dt4:: Symantics repr => DynTerm repr
dt4 = tc ((ut `UApp` (UInt 10)) `UApp` (UBool False))
dt4rc = cShow $ dt4  -- "(((\\V0 -> (\\V1 -> V0)) 10) False)"

dt5:: Symantics repr => DynTerm repr
dt5 = tc (((ut2 `UApp` ut) `UApp` (UBool False)) `UApp` (UInt 20))
dt5rl = lShow $ dt5  -- 9
dt5rc = cShow $ dt5  
  -- "((((\\V0 -> (\\V1 -> V0)) (\\V2 -> (\\V3 -> V2))) False) 20)"
dt5rp = pShow $ dt5  -- "(\\V0 -> 20)"

dt6:: Symantics repr => DynTerm repr
dt6 = tc (((ut2 `UApp` ut2) `UApp` (UBool False)) `UApp` (UInt 20))
dt6rl = lShow $ dt6
-- "*** Exception: Bad App: UApp (UK (UK (UInt 1) (UBool True)) (UBool True)) (UK (UK (UInt 1) (UBool True)) (UBool True))

-- Again, the error message could be better, but I am getting hungry...
-}