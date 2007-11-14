{-# OPTIONS -fglasgow-exts -W #-}

-- Interpreter, Compiler, Partial Evaluator 
-- Typechecking into Symantics terms

module IncopeTypecheck where

import Incope
import Data.Typeable
import Control.Monad
import Control.Monad.Error
import Text.Show.Functions

-- Representing parsed AST terms
--
-- The following DynTerm repr could well be the result of the
-- parser/typechecker (created along the lines of `typing dynamic typing')
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
-- Representing unparsed terms
data UExp = UInt Int | UBool Bool | UAdd UExp UExp | UIf UExp UExp UExp
	  | UApp UExp UExp
	    -- functions are represented via combinators
	  | UK UExp UExp -- K combinator, args give the types of args
	    -- more combinators needed...
	  deriving Show

-- Typechecking is obviously partial
-- Error messages could have been better, but I'm hungry...

ap2 f m1 m2    = do v1 <- m1; v2 <- m2; f v1 v2
ap3 f m1 m2 m3 = do v1 <- m1; v2 <- m2; v3 <- m3; f v1 v2 v3

typecheck :: Symantics repr => UExp -> Either String (DynTerm repr)
typecheck (UInt x)  = return $ DynTerm (int x)
typecheck (UBool x) = return $ DynTerm (bool x)
typecheck (UAdd e1 e2) = ap2 tadd (typecheck e1) (typecheck e2)
 where
 tadd (DynTerm e1) (DynTerm e2) | Just e1' <- gcast e1,
                                  Just e2' <- gcast e2
				    = return $ DynTerm (add e1' e2')
 tadd _ _ = fail "Add: args are not int exp"
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

typerep :: forall a repr. Typeable a => repr a -> TypeRep
typerep _ = typeOf (undefined::a)

-- reflect typerep to a type (witness)
reflect :: forall repr. TypeRep -> DynTerm repr
reflect x | x == typeOf (undefined::Int) = DynTerm (undefined::repr Int)
reflect x | x == typeOf (undefined::Bool) = DynTerm (undefined::repr Bool)
reflect x | (con,[arg1,arg2]) <- splitTyConApp x,
	    con == typeRepTyCon (typeOf (undefined::Int->Int)) =
  case (reflect arg1, reflect arg2) of
    (DynTerm e1, DynTerm e2) -> 
	let mkfun :: repr a -> repr b -> repr (a->b); mkfun = undefined
        in DynTerm (mkfun e1 e2)

tc :: Symantics repr => UExp -> DynTerm repr
tc exp = either error id (typecheck exp)

rShow (DynTerm t) = show $ compR t
lShow (DynTerm t) = show $ compL t
cShow (DynTerm t) = show $ compC t


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
    lam :: (repr a -> repr b) -> repr (a->b)
    fix :: (repr a -> repr a) -> repr a

    mul :: repr Int -> repr Int -> repr Int
    leq :: repr Int -> repr Int -> repr Bool
-}

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

dt6:: Symantics repr => DynTerm repr
dt6 = tc (((ut2 `UApp` ut2) `UApp` (UBool False)) `UApp` (UInt 20))
dt6rl = lShow $ dt6
-- "*** Exception: Bad App: UApp (UK (UK (UInt 1) (UBool True)) (UBool True)) (UK (UK (UInt 1) (UBool True)) (UBool True))

-- Again, the error message could be better, but I am getting hungry...
