{-# LANGUAGE TypeFamilies, TypeOperators, Rank2Types, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# OPTIONS_GHC -W #-}

module Exper where

import qualified Data.Function as F
import Language.Haskell.TH

data IntT
data BoolT
data UnitT
data a :-> b
infixr 5 :->
data Pair a b
data List a
data EitherT a b

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
class Symantics repr where
    int  :: Int  -> repr IntT              -- int literal
    bool :: Bool -> repr BoolT             -- bool literal

    lam :: (repr a -> repr b) -> repr (a :-> b)
    app :: repr (a :-> b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a

    add :: repr IntT  -> repr IntT -> repr IntT
    sub :: repr IntT  -> repr IntT -> repr IntT
    mul :: repr IntT  -> repr IntT -> repr IntT
    leq :: repr IntT  -> repr IntT -> repr BoolT
    eql :: repr IntT  -> repr IntT -> repr BoolT
    if_ :: repr BoolT -> repr a -> repr a -> repr a

    unit :: repr UnitT
    pair :: repr a  -> repr b -> repr (Pair a b)
    pfst  :: repr (Pair a b) -> repr a
    psnd  :: repr (Pair a b) -> repr b

    lnil :: repr (List a)
    lcons :: repr a -> repr (List a) -> repr (List a)
    lfoldr :: repr (a :-> b :-> b) -> repr b -> repr (List a) -> repr b
    lmap :: repr (a :-> b) -> repr (List a) -> repr (List b)

    left :: repr a -> repr (EitherT a b)
    right :: repr b -> repr (EitherT a b)
    elimOr :: repr (a :-> c) -> repr (b :-> c) -> repr (EitherT a b) -> repr c

test1 :: Symantics repr => () -> repr IntT
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

testpair () = lam(\p ->
                  fix (\self -> lam (\n ->
                      if_ (leq n (int 0)) (pfst p)
                        (if_ (leq n (int 1)) (psnd p)
                         (add (app self (add n (int (-1))))
                              (app self (add n (int (-2)))))))))

btoe () = lam(\x -> if_ x (left unit) (right unit))

if' :: Bool -> a -> a -> a
if' eb et el = if eb then et else el

type family Sem a :: *
type instance Sem IntT = Int
type instance Sem BoolT = Bool
type instance Sem UnitT = ()
type instance Sem (a :-> b) = Sem a -> Sem b
type instance Sem (Pair a b) = (Sem a, Sem b)
type instance Sem (List a) = [Sem a]
type instance Sem (EitherT a b) = Either (Sem a) (Sem b)

class App f where
  lift0 :: Sem a -> f a
  lift1 :: (Sem a -> Sem b) -> f a -> f b
  lift2 :: (Sem a -> Sem b -> Sem c) -> f a -> f b -> f c
  lift3 :: (Sem a -> Sem b -> Sem c -> Sem d) -> f a -> f b -> f c -> f d
  pull  :: (f a -> f b) -> f (a :-> b)

-- ------------------------------------------------------------------------
-- The interpreter
-- It is a typed, tagless interpreter: R is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only

newtype R a = R (Sem a)
unR (R x) = x

instance App R where
  lift0 = R
  lift1 f (R a) = R $ f a
  lift2 f (R a) (R b) = R $ f a b
  lift3 f (R a) (R b) (R c) = R $ f a b c
  pull f = R (unR . f . R)

instance Symantics R where
    int = lift0
    bool = lift0

    lam = pull
    fix = lift1 F.fix . pull
    app = lift2 ($)

    add = lift2 (+)
    sub = lift2 (-)
    mul = lift2 (*)
    leq = lift2 (<=)
    eql = lift2 (==)
    if_ = lift3 (if')

    unit = lift0 ()
    pair = lift2 (,)
    pfst = lift1 fst
    psnd = lift1 snd

    lnil = lift0 []
    lcons = lift2 (:) 
    lfoldr = lift3 (foldr)
    lmap = lift2 (map)

    left = lift1 Left
    right = lift1 Right
    elimOr = lift3 either

mkitest f = unR (f ())


itest1 = mkitest test1
itest2 = mkitest test2
itest3 = mkitest test3

itestgib  = mkitest testgib
itestgib1 = mkitest testgib1
itestgib2 = mkitest testgib2

itestpw   = mkitest testpowfix
itestpw7  = mkitest testpowfix7
itestpw72 = mkitest (\() -> app (testpowfix7 ()) (int 2))

-- ------------------------------------------------------------------------
-- Another interpreter: it interprets each term to give its size
-- (the number of constructors)
-- It is a typed, tagless interpreter: L is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only.
-- This interpreter is also total: it determines the size of the term
-- even if the term itself is divergent.

newtype L a = L (Sem IntT) deriving Show
unL (L x) = x

instance App L where
  lift0 _ = L 1
  lift1 _ (L a) = L $ a + 1
  lift2 _ (L a) (L b) = L $ a + b + 1
  lift3 _ (L a) (L b) (L c) = L $ a + b + c + 1
  pull f = L ((unL . f . L $ 0) + 1)

instance Symantics L where
    int = lift0
    bool = lift0

    lam = pull
    fix = lift1 F.fix . pull

    app = lift2 ($)

    add = lift2 (+)
    sub = lift2 (-)
    mul = lift2 (*)
    leq = lift2 (<=)
    eql = lift2 (==)
    if_ = lift3 (if')

    unit = lift0 ()
    pair = lift2 (,)
    pfst = lift1 fst
    psnd = lift1 snd

    lnil = lift0 []
    lcons = lift2 (:) 
    lfoldr = lift3 (foldr)
    lmap = lift2 (map)

    left = lift1 Left
    right = lift1 Right
    elimOr = lift3 either

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

-- Compiler a la TH
newtype C a = C ExpQ
unC (C x) = x

clift0 x = C [| x |]
clift1 :: ExpQ -> C t -> C a
clift1 g (C x) = C $ do f <- g
                        tx <- x
                        return $ AppE f tx
clift2 g (C x) (C y) = C $ do f <- g
                              tx <- x
                              ty <- y
                              return $ AppE (AppE f tx) ty
clift3 g (C a) (C b) (C c) = C $ 
    do f <- g
       ta <- a
       tb <- b
       tc <- c
       return $ AppE (AppE (AppE f ta) tb) tc 
    
cpull :: (C a -> C b) -> C (a :-> b)
cpull f = C [| \x -> $((unC . f . C) [| x |] ) |]

instance Symantics C where
    int = clift0
    bool = clift0
    lam = cpull
    app = clift2 [| ($) |]
    fix = clift1 [| F.fix |] . cpull
    add = clift2 [| (+) |]
    sub = clift2 [| (-) |]
    mul = clift2 [| (*) |]
    leq = clift2 [| (<=) |]
    eql = clift2 [| (==) |]
    if_ = clift3 [| if' |]
    pair = clift2 [| ((,)) |]
    pfst = clift1 [| fst |]
    psnd = clift1 [| snd |]
    left = clift1 [| Left |]
    right = clift1 [| Right |]
    elimOr = clift3 [| either |]
    lnil = C [| [] |]
    lcons = clift2 [| (:) |]
    lfoldr = clift3 [| (foldr) |]
    lmap = clift2 [| map |]
    unit = C [| () |]
{-
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
    HNil :: HByteCode [t1]
    HCons :: HByteCode t1 -> HByteCode [t1] -> HByteCode [t1]
    HFoldr :: HByteCode (a -> b -> b) -> HByteCode b -> HByteCode [a] ->
              HByteCode b
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
eval (HNil) = []
eval (HCons x xs) = (eval x) : (eval xs)
eval (HFoldr f u l) = foldr (eval f) (eval u) (eval l)
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

    lnil = HNil
    lcons = HCons
    lfoldr = HFoldr

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

-}
