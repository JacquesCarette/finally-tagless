{-# LANGUAGE GADTs, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -W #-}

-- Interpreter, Compiler, Partial Evaluator
-- based on the ``tagless final'' encoding.
-- Original code by
--   Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
-- with maybe modifications by Jacques Carette

module Futamura1 where

import Data.Maybe (fromJust)
import Control.Applicative
import qualified Data.Function as F

class Applicative f => Liftable1 f where
  pull :: (f a -> f b) -> f (a -> b)

{-
  The language is simply-typed lambda-calculus with fixpoint,
  integers, booleans and comparison: essentially, PCF.

  For ease of programming, we add tuples, unit, equality,
  (binary) sum, 
  and lists as well as tuple and list projections.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.  For list, we provide a discriminator, which is 
  sufficient to implement other (total) functions that consume 
  lists).
-}

-- This class defines syntax (and its instances, semantics) of our language
-- This class is Haskell98!
class Symantics repr where
    int  :: Int  -> repr Int              -- int literal
    bool :: Bool -> repr Bool             -- bool literal

    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a

    add :: repr Int  -> repr Int -> repr Int
    sub :: repr Int  -> repr Int -> repr Int
    mul :: repr Int  -> repr Int -> repr Int
    leq :: repr Int  -> repr Int -> repr Bool
    eql :: repr Int  -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

    unit :: repr ()
    pair :: repr a  -> repr b -> repr (a,b)
    pfst  :: repr (a,b) -> repr a
    psnd  :: repr (a,b) -> repr b

--    lcons :: repr a -> repr [a] -> repr [a]

    left :: repr a -> repr (Either a b)
    right :: repr b -> repr (Either a b)
    elimOr :: repr (a -> c) -> repr (b -> c) -> repr (Either a b) -> repr c

test1 :: Symantics repr => () -> repr Int
test1 () = add (int 1) (int 2)
test2 () = lam (\x -> add x x)
test3 () = lam (\x -> add (app x (int 1)) (int 2))

testgib () = lam (\x -> lam (\y ->
                  fix (\self -> lam (\n ->
                      if_ (leq n (int 0)) x
                        (if_ (leq n (int 1)) y
                         (add (app self (add n (int (-1))))
                              (app self (add n (int (-2))))))))))

testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)
testgib2 () = lam (\x -> (lam (\y ->app (app (app (testgib ()) x) y) (int 5))))

testpowfix () = lam (\x ->
                      fix (\self -> lam (\n ->
                        if_ (leq n (int 0)) (int 1)
                            (mul x (app self (add n (int (-1))))))))
testpowfix7 () = lam (\x -> app (app (testpowfix ()) x) (int 7))

if' :: Bool -> a -> a -> a
if' eb et el = if eb then et else el

-- ------------------------------------------------------------------------
-- The interpreter
-- It is a typed, tagless interpreter: R is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only

newtype R a = R a deriving Show
unR (R x) = x

instance Functor R where
    fmap f (R x) = R (f x)

instance Applicative R where
    pure = R
    R f <*> R a = R (f a)

instance Liftable1 R where
    pull f = R (unR . f . R)

instance Symantics R where
    int = pure
    bool = pure

    lam = pull
    fix f = liftA F.fix (pull f)
    app = (<*>)

    add = liftA2 (+)
    sub = liftA2 (-)
    mul = liftA2 (*)
    leq = liftA2 (<=)
    eql = liftA2 (==)
    if_ = liftA3 (if')

    unit = pure ()
    pair = liftA2 (,)
    pfst = liftA fst
    psnd = liftA snd

    left = liftA Left
    right = liftA Right
    elimOr = liftA3 either

compR = unR

mkitest f = compR (f ())


itest1 = mkitest test1
itest2 = mkitest test2
itest3 = mkitest test3

itestgib  = mkitest testgib
itestgib1 = mkitest testgib1
itestgib2 = mkitest testgib2

itestpw   = mkitest testpowfix
itestpw7  = mkitest testpowfix7
itestpw72 = mkitest (\() -> app (testpowfix7 ()) (int 2))

{-
The expression "R (unR . f . R)" aka "pull" _looks_ like tag 
introduction and elimination.
But the function unR is *total*. There is no run-time error
is possible at all -- and this fact is fully apparent to the
compiler.
Note the corresponding code in incope.ml:
  let int (x:int) = x
  let add e1 e2 = e1 + e2

  let lam f = f
No tags at all...
-}

-- ------------------------------------------------------------------------
-- Another interpreter: it interprets each term to give its size
-- (the number of constructors)
-- It is a typed, tagless interpreter: L is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only.
-- This interpreter is also total: it determines the size of the term
-- even if the term itself is divergent.

newtype L a = L Int deriving Show
unL (L x) = x

instance Functor L where
    fmap _ (L x) = L (x + 1) -- builtin app costs 1

instance Applicative L where
    pure _ = L 1
    L f <*> L a = L (f + a) -- a pure app costs nothing
          -- else liftA2 costs "too much"

instance Liftable1 L where
    pull f = L ((unL . f . L $ 0) + 1)

-- some useful abbreviations
unitL = L 1
oneL e1 = L( (unL e1) + 1)
twoL e1 e2 = L( (unL e1) + (unL e2) + 1)

instance Symantics L where
    int = pure
    bool = pure

    lam = pull
    app = (<*>)
    fix f = liftA F.fix (pull f)

    add = liftA2 (+)
    sub = liftA2 (-)
    mul = liftA2 (*)
    leq = liftA2 (<=)
    eql = liftA2 (==)
    if_ = liftA3 (if')

    unit = pure ()
    pair = liftA2 ((,))
    pfst = liftA fst
    psnd = liftA snd

    left = liftA Left
    right = liftA Right
    elimOr = liftA3 either

compL = unL

ltest1 = compL . test1 $ ()
ltest2 = compL . test2 $ ()
ltest3 = compL . test3 $ ()


ltestgib  = compL . testgib  $ ()
ltestgib1 = compL . testgib1 $ () -- 19
ltestgib2 = compL . testgib2 $ ()

ltestpw   = compL . testpowfix $ ()
ltestpw7  = compL . testpowfix7 $ ()
ltestpw72 = compL (app (testpowfix7 ()) (int 2)) -- 14

-- ------------------------------------------------------------------------
-- The compiler
-- We compile to GADT, to be understood as a typed assembly language
-- (typed bytecode). The GADT does _not_ use the higher-order abstract
-- syntax. We could have used template Haskell. Alas, its expressions
-- are untyped.
-- ByteCode represents MetaOCaml's `code'. Thus the compiler
-- below neatly maps to MetaOCaml (with no GADTs).
-- Like MetaOCaml `code', we never pattern-match on ByteCode!
-- The compiler never raises any exception and matches no tags:
-- generated code has no tags at all.

-- The LIFT bytecode operation is used only as an optimization for the
-- partial evaluator, to be able to 'lift' computations.
-- it corresponds to a CSP in MetaOCaml. 

data ByteCode t where
    Var :: Int -> ByteCode t                -- variables identified by numbers
    Lam :: Int -> ByteCode t2 -> ByteCode (t1->t2)
    App :: ByteCode (t1->t2) -> ByteCode t1  -> ByteCode t2
    Fix :: Int -> ByteCode t -> ByteCode t
    INT :: Int -> ByteCode Int
    BOOL:: Bool -> ByteCode Bool
    Add :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Sub :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Mul :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Leq :: ByteCode Int -> ByteCode Int -> ByteCode Bool
    Eql :: ByteCode t1 -> ByteCode t1 -> ByteCode Bool
    IF  :: ByteCode Bool -> ByteCode t -> ByteCode t -> ByteCode t
    LIFT :: t -> ByteCode t                 -- Used only in PE 
    UNIT :: ByteCode ()
    Pair :: ByteCode t1 -> ByteCode t2 -> ByteCode (t1,t2)
    Pfst :: ByteCode (t1,t2) -> ByteCode t1
    Psnd :: ByteCode (t1,t2) -> ByteCode t2
    Lcons :: ByteCode t1 -> ByteCode [t1] -> ByteCode [t1]
    ELeft :: ByteCode t1 -> ByteCode (Either t1 t2)
    ERight :: ByteCode t2 -> ByteCode (Either t1 t2)
    ElimOr :: ByteCode (t1 -> t3) -> ByteCode (t2 -> t3) -> ByteCode (Either t1 t2) -> ByteCode t3

-- helpers
bp :: String -> String
bp x = "(" ++ x ++ ")"
mid :: (Show a, Show b) => String -> a -> b -> String
mid s e1 e2 = show e1 ++ s ++ show e2

instance Show (ByteCode t) where
    show (Var n) = "V" ++ show n
    show (Lam n b) = "(\\V" ++ (mid " -> " n b) ++ ")"
    show (App e1 e2) = bp $ mid " " e1 e2
    show (Fix n b) = "(fix\\V" ++ (mid " " n b) ++ ")"
    show (INT n) = show n
    show (BOOL b) = show b
    show (Add e1 e2) = bp $ mid " + " e1 e2 
    show (Sub e1 e2) = bp $ mid " - " e1 e2 
    show (Mul e1 e2) = bp $ mid " * " e1 e2 
    show (Leq e1 e2) = bp $ mid " <= " e1 e2 
    show (Eql e1 e2) = bp $ mid " == " e1 e2 
    show (IF be et ee)
        = "(if " ++ show be ++ 
          " then " ++ show et ++ " else " ++ show ee ++ ")"
    show (LIFT _) = "<csp>"
    show (UNIT) = "() "
    show (Pair e1 e2) = bp $ mid ", " e1 e2
    show (Pfst e1)  = "(fst " ++ show e1 ++ ")"
    show (Psnd e1)  = "(snd " ++ show e1 ++ ")"
    show (Lcons e1 e2) = bp $ mid ": " e1 e2
    show (ELeft e1) = "(left" ++ show e1 ++ ")"
    show (ERight e1) = "(right" ++ show e1 ++ ")"
    show (ElimOr l r m) = "(either " ++ show l ++ " " ++ show r ++ " " ++ show m ++ ")"

-- Int is the variable counter
-- for allocation of fresh variables
newtype C t = C (Int -> (ByteCode t, Int)) 
unC (C x) = x

-- helper
toC x = C(\vc -> (x, vc))
oneC f e1 = C(\vc -> let (e1b,vc1) = unC e1 vc
                     in (f e1b ,vc1))
twoC f e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                            (e2b,vc2) = unC e2 vc1
                         in (f e1b e2b,vc2))

instance Symantics C where
    int  = toC . INT
    bool = toC . BOOL

    lam f = C(\vc -> let v = vc
                         var = C(\vc -> (Var v, vc))
                         (body,vc') = unC (f var) (succ vc)
                     in (Lam v body, vc'))
    fix f = C(\vc -> let v = vc
                         var = C(\vc -> (Var v, vc))
                         (body,vc') = unC (f var) (succ vc)
                     in (Fix v body, vc'))
    app = twoC App

    add = twoC Add
    sub = twoC Sub
    mul = twoC Mul
    leq = twoC Leq
    eql = twoC Eql
    if_ be et ee = C(\vc -> let (beb,vc1) = unC be vc
                                (etb,vc2)  = unC et vc1
                                (eeb,vc3)  = unC ee vc2
                         in (IF beb etb eeb,vc3))

    unit   = C(\vc -> (UNIT, vc))
    pair = twoC Pair

    pfst = oneC Pfst
    psnd = oneC Psnd
    left = oneC ELeft
    right = oneC ERight
    elimOr l r m = C(\vc -> let (bl, vc1) = unC l vc
                                (br, vc2) = unC r vc1
                                (bm, vc3) = unC m vc2
                            in (ElimOr bl br bm, vc3))

compC repr = fst $ unC repr 0

ctest1 = compC . test1 $ ()
ctest2 = compC . test2 $ ()
ctest3 = compC . test3 $ ()

ctestgib  = compC . testgib  $ ()
ctestgib1 = compC . testgib1 $ ()
ctestgib2 = compC . testgib2 $ ()

ctestpw   = compC . testpowfix $ ()
ctestpw7  = compC . testpowfix7 $ ()
ctestpw72 = compC  (app (testpowfix7 ()) (int 2))

-- ------------------------------------------------------------------------
-- The partial evaluator: the combination of the interpreter (R) and
-- the compiler (C).

-- Generalized Semantics, with an inductive type map

class Symantics2 rep where
    zint  :: Int  -> rep Int Int              -- int literal
    zbool :: Bool -> rep Bool Bool            -- bool literal

    zlam :: (rep sa da -> rep sb db) -> rep (rep sa da -> rep sb db) (da->db)
    zapp :: rep (rep sa da -> rep sb db) (da->db) -> rep sa da -> rep sb db
    zfix :: (rep sa da -> rep sa da) -> rep sa da

    zadd :: rep Int Int -> rep Int Int -> rep Int Int
    zmul :: rep Int Int -> rep Int Int -> rep Int Int
    zleq :: rep Int Int -> rep Int Int -> rep Bool Bool
    zeql :: rep Int Int -> rep Int Int -> rep Bool Bool
    zif_ :: rep Bool Bool -> rep s d -> rep s d -> rep s d

    zunit :: rep () ()                        -- unit literal
    zpair :: rep a a -> rep b b -> rep (a,b) (a,b)
    zfst  :: rep (a,b) (a,b) -> rep a a
    zsnd  :: rep (a,b) (a,b) -> rep b b
    zleft :: rep a a -> rep (Either a b) (Either a b)
    zright :: rep b b -> rep (Either a b) (Either a b)

-- Unlike in Incope.hs, this P explicitly refers to C and R, and isn't
-- polymorphic.  This can have a deleterious effect on the static part!
data P static dynamic = 
    P { dynamic :: C dynamic, static :: Maybe (R static) }
zE dynamic = P dynamic Nothing

-- takes a 'static cast' and a static and dynamic binary operation
-- works for results at directly embedable types, i.e. static'
-- must be a known ground type.
ztwo :: (static' -> P static' dynamic') -> 
        (R s1 -> R s2 -> R static') -> 
        (C d1 -> C d2 -> C dynamic') ->
        (P s1 d1) -> (P s2 d2) ->
        (P static' dynamic')
ztwo cast op_s op_d = zop
  where
    zop (P _ (Just n1)) (P _ (Just n2)) = cast . unR $ op_s n1 n2
    zop (P n1 _       ) (P n2 _       ) = zE (op_d n1 n2)
    
-- cast from static to P -- CSP
zcast x = P (toC . LIFT . unR $ x) (Just x)

-- Similar to ztwo, but uses polymorphic lift
ztwoP :: (R s1 -> R s2 -> R res) -> (C d1 -> C d2 -> C res) ->
         (P s1 d1) -> (P s2 d2) -> (P res res)
ztwoP op_s op_d = zop
  where
    zop (P _ (Just n1)) (P _ (Just n2)) = zcast (op_s n1 n2)
    zop (P n1 _       ) (P n2 _       ) = zE (op_d n1 n2)
    
-- And a unary version
zoneP :: (R s1 -> R res) -> (C d1 -> C res) ->
         (P s1 d1) -> (P res res)
zoneP op_s op_d = zop
  where
    zop (P _ (Just n1)) = zcast (op_s n1)
    zop (P n1 _       ) = zE (op_d n1)
    
instance Symantics2 P where
    zint x  = P (int x) (Just (int x))
    zbool b = P (bool b) (Just (bool b))

    zadd = ztwo zint add add -- note that the 2 'add' are at different types!
    zmul = ztwo zint mul mul
    zleq = ztwo zbool leq leq
    zeql = ztwo zbool eql eql
    zif_ (P _ (Just b1)) et ee = if unR b1 then et else ee
    zif_ (P be _       ) et ee = zE (if_ be (dynamic et) (dynamic ee))

    zlam f = P (lam (dynamic . f . zE)) (Just (R f))
    zapp (P _ (Just f)) x = (unR f) x
    zapp (P f _       ) x = zE (app f (dynamic x))
    zfix f = case f (zfix f) of
          (P _ (Just s)) -> P dfix (Just s)
          _              -> P dfix Nothing
       where dfix = fix (dynamic . f . zE)

    zunit = P (unit) (Just unit)
    zpair = ztwoP pair pair
    zfst = zoneP pfst pfst
    zsnd = zoneP psnd psnd
    zleft = zoneP left left
    zright = zoneP right right

-- unit to suppress the monomorphism restriction

ztestgib () = zlam (\x -> zlam (\y ->
                zfix (\self -> zlam (\n ->
                    zif_ (zleq n (zint 0)) x
                      (zif_ (zleq n (zint 1)) y
                       (zadd (zapp self (zadd n (zint (-1))))
                             (zapp self (zadd n (zint (-2))))))))))

ztestgib5 () = zlam (\x -> zlam (\y ->
    zapp (zapp (zapp (ztestgib ()) x) y) (zint 5)))

ztestgib1 () = zapp (zapp (zapp (ztestgib ()) (zint 1)) (zint 1)) (zint 5)

-- The result of PE can be interpreted using R or C

testR_ztestgib1 = compR (fromJust (static (ztestgib1 ()))) -- 8
testC_ztestgib1 = compC (dynamic (ztestgib1 ())) -- 8

testC_ztestgib5 = compC (dynamic (ztestgib5 ()))
-- (\V0 -> (\V1 -> ((((V1 + V0) + V1) + (V1 + V0)) + ((V1 + V0) + V1))))

-- Every instance of Symantics can be injected into Symantics2
newtype S12 repr s d = S12{unS12:: repr d}

oneS f (S12 e1) = S12 $ f e1
twoS f (S12 e1) (S12 e2) = S12 $ f e1 e2

instance Symantics repr => Symantics2 (S12 repr) where
    zint x  = S12 $ int x
    zbool b = S12 $ bool b

    zadd = twoS add
    zmul = twoS mul
    zleq = twoS leq
    zeql = twoS eql
    zif_ (S12 e) (S12 e1) (S12 e2) = S12 $ if_ e e1 e2

    zlam f = S12 $ lam (unS12 . f . S12 )
    zfix f = S12 $ fix (unS12 . f . S12 )
    zapp = twoS app

    zunit = S12 (unit)
    zpair = twoS pair
    zfst = oneS pfst
    zsnd = oneS psnd
    zleft = oneS left
    zright = oneS right

testR_zztestgib1 = compR $ unS12 $ ztestgib1 () -- 8
testL_zztestgib1 = compL $ unS12 $ ztestgib1 () -- 23
testC_zztestgib1 = compC $ unS12 $ ztestgib1 () -- big code

ztestpowfix () = zlam (\x ->
                      zfix (\self -> zlam (\n ->
                        zif_ (zleq n (zint 0)) (zint 1)
                            (zmul x (zapp self (zadd n (zint (-1))))))))
ztestpowfix7 () = zlam (\x -> zapp (zapp (ztestpowfix ()) x) (zint 7))

-- We can interpret ztespowerfix directly, using R, L or C
testR_zztestpw7 = (compR $ unS12 $ ztestpowfix7 ()) 2 -- 128
testL_zztestpw7 = compL $ unS12 $ ztestpowfix7 () -- 15
testC_zztestpw7 = compC $ unS12 $ ztestpowfix7 ()
{-
  (\V0 -> (((\V1 -> 
    (fix\V2 (\V3 -> (if (V3 <= 0) then 1 else (V1 * (V2 (V3 + -1))))))) V0) 7))
-}

testC_ztestpw7 = compC $ dynamic $ ztestpowfix7 ()
-- (\V0 -> (V0 * (V0 * (V0 * (V0 * (V0 * (V0 * (V0 * 1))))))))

-- ------------------------------------------------------------------------
-- The HOAS bytecode compiler
-- We compile to GADT, to be understood as a typed assembly language
-- (typed bytecode). 
-- One may argue that this bytecode still faithfully represents
-- MetaOCaml's `code': this is because the function
-- 'a code -> 'b code is trivially convertible to ('a->'b) code.
-- Also, HOAS bytecode certainly seems to match better with MetaOCaml's
-- syntax for functions.
-- Like MetaOCaml `code', we never pattern-match on HByteCode!
-- Furthermore, the compiler never raises any exception and matches no tags:
-- generated code has no tags.

-- The HVar bytecode operation is used only during evaluation
-- it corresponds to a CSP in MetaOCaml.

data HByteCode t where
    HVar :: t -> HByteCode t                -- essentially, the Lift operation
    HLam :: (HByteCode t1 -> HByteCode t2) -> HByteCode (t1->t2)
    HApp :: HByteCode (t1->t2) -> HByteCode t1  -> HByteCode t2
    HFix :: (HByteCode t -> HByteCode t) -> HByteCode t
    HINT :: Int -> HByteCode Int
    HBOOL:: Bool -> HByteCode Bool
    HAdd :: HByteCode Int -> HByteCode Int -> HByteCode Int
    HSub :: HByteCode Int -> HByteCode Int -> HByteCode Int
    HMul :: HByteCode Int -> HByteCode Int -> HByteCode Int
    HLeq :: HByteCode Int -> HByteCode Int -> HByteCode Bool
    HEql :: Eq t1 => HByteCode t1  -> HByteCode t1  -> HByteCode Bool
    HIF  :: HByteCode Bool -> HByteCode t -> HByteCode t -> HByteCode t
    HUNIT :: HByteCode ()
    HPair :: HByteCode t1 -> HByteCode t2 -> HByteCode (t1,t2)
    HFst :: HByteCode (t1,t2) -> HByteCode t1
    HSnd :: HByteCode (t1,t2) -> HByteCode t2
    HLeft :: HByteCode t1 -> HByteCode (Either t1 t2)
    HRight :: HByteCode t2 -> HByteCode (Either t1 t2)
    HElimOr :: HByteCode (t1 -> t3) -> HByteCode (t2 -> t3) -> HByteCode (Either t1 t2) -> HByteCode t3

-- Showing HOAS has always been problematic... We just skip it for now...

-- An evaluator for the ByteCode: the virtual machine
-- It is total (modulo potential non-termination in fix)
-- No exceptions are to be raised, and no pattern-match failure
-- may occur. All pattern-matching is _syntactically_, patently complete.
eval :: HByteCode t -> t
eval (HVar v) = v
eval (HLam b) = \x -> eval (b (HVar x))
eval (HApp e1 e2) = (eval e1) (eval e2)
eval (HFix f) = eval (f (HFix f))
eval (HINT n) = n
eval (HBOOL n) = n
eval (HAdd e1 e2) = eval e1 + eval e2
eval (HSub e1 e2) = eval e1 - eval e2
eval (HMul e1 e2) = eval e1 * eval e2
eval (HLeq e1 e2) = eval e1 <= eval e2
eval (HEql e1 e2) = eval e1 == eval e2
eval (HIF be et ee) = if (eval be) then eval et else eval ee
eval (HUNIT) = ()
eval (HPair e1 e2) = (eval e1, eval e2)
eval (HFst e1) = fst (eval e1)
eval (HSnd e1) = snd (eval e1)
eval (HLeft e1) = Left (eval e1)
eval (HRight e1) = Right (eval e1)
eval (HElimOr l r m) = either (eval l) (eval r) (eval m)

instance Symantics HByteCode where
    int  = HINT
    bool = HBOOL

    lam  = HLam
    app  = HApp

    fix  = HFix
    add  = HAdd
    sub  = HSub
    mul  = HMul
    leq  = HLeq
    eql  = HEql
    if_  = HIF

    unit = HUNIT
    pair = HPair
    pfst = HFst
    psnd = HSnd
    left = HLeft
    right = HRight
    elimOr = HElimOr

compH :: HByteCode t -> HByteCode t
compH = id 

htest1 = compH . test1 $ ()
htest1r = eval . test1 $ ()
htest2 = compH . test2 $ ()
htest2r = eval . test2 $ ()
htestgib1  = compH . testgib1 $ ()
htestgib1r = eval . testgib1 $ ()
