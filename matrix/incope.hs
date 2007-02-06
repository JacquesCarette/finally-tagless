{-# OPTIONS -fglasgow-exts #-}

-- Interpreter, Compiler, Partial Evaluator

module Incope where

-- import qualified Data.Map as Map
-- import Data.Maybe

{-
  The language is simply-typed lambda-calculus with fixpoint,
  integers, booleans and comparison.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | B Bool | Add ie1 ie2 |
  IF b e-then e-else
  
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
    int :: Int -> repr Int                -- int literal
    bool :: Bool -> repr Bool             -- bool literal
    
    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b
    rec :: repr a -> repr (a -> a) -> repr (Int -> a)
    fix :: (repr a -> repr a) -> repr a

    add :: repr Int -> repr Int -> repr Int
    if_ :: repr Bool -> repr a -> repr a -> repr a
    eql :: Eq a => repr a -> repr a -> repr Bool

    comp :: repr a -> res a

test1 () = add (int 1) (int 2)
test2 () = lam (\x -> add x x)

testgib () = lam (\x -> lam (\y ->
                  fix (\self -> lam (\n ->
                      if_ (eql n (int 0)) x
                        (if_ (eql n (int 1)) y
                         (add (app self (add n (int (-1))))
                              (app self (add n (int (-2))))))))))

testgib' () = lam (\x -> lam (\y -> lam (\n ->
              app (app (rec (lam (\c -> app (app c x) y))
                            (lam (\m -> lam (\c -> app m (lam (\a -> lam (\b ->
                                        app (app c b) (add a b))))))))
                       n)
                  (lam (\a -> lam (\b -> a))))))

testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)

testgib1' () = app (app (app (testgib' ()) (int 1)) (int 1)) (int 5)

-- ------------------------------------------------------------------------
-- The interpreter
-- It is typed, tagless interpreter: R is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only

-- Note that everything going on in the interpreter is straight out of
-- "Boxes go Bananas" by Washburn and Weirich (intended or otherwise)
newtype R a = R a deriving Show
unR (R x) = x
asR :: R x -> R x; asR = id

instance Symantics R R where
    int x = R x
    bool b = R b

    lam f = R (unR . f . R)
    app e1 e2 = R( (unR e1) (unR e2) )
    fix f = R( fx (unR . f . R)) where fx f = f (fx f)
    rec z s = R (\n -> if n <= 0 then unR z else unR s (unR (rec z s) (pred n)))

    add e1 e2 = R( (unR e1) + (unR e2) )
    eql e1 e2 = R( (unR e1) == (unR e2) )
    if_ be et ee = R( if (unR be) then unR et else unR ee )

    comp = id

mkitest f = unR (comp. asR . f $ ())

itest1 = mkitest test1
itest2 = mkitest test2
itest3 = mkitest testgib1


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
    Var :: Int -> ByteCode t                -- variables identified by numbers
    Lam :: Int -> ByteCode t2 -> ByteCode (t1->t2)
    App :: ByteCode (t1->t2) -> ByteCode t1  -> ByteCode t2
    Fix :: Int -> ByteCode t -> ByteCode t
    INT :: Int -> ByteCode Int
    BOOL :: Bool -> ByteCode Bool
    Add :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Eql :: ByteCode t1 -> ByteCode t1 -> ByteCode Bool
    IF :: ByteCode Bool -> ByteCode t -> ByteCode t -> ByteCode t
instance Show (ByteCode t) where
    show (Var n) = "V" ++ show n
    show (Lam n b) = "(\\V" ++ show n ++ " -> " ++ show b ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Fix n b) = "(fix\\V" ++ show n ++ " " ++ show b ++ ")"
    show (INT n) = show n
    show (BOOL b) = show b
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Eql e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (IF be et ee)
        = "(if " ++ show be ++ 
          " then " ++ show et ++ " else " ++ show ee ++ ")"

-- An evaluator for the ByteCode
-- built using a one-step evaluator with an explicit environment
-- which does not work :-(  because I have no experience with GADTs

-- type Env = forall t.Map.Map Int (ByteCode t)
-- eval1 :: ByteCode a -> Env -> ByteCode a
-- eval1 (Var n) e = fromJust $ Map.lookup n e -- yuck!
-- eval1 x@(Lam _ _) e = x
-- eval1 (App (Lam n b) x) e = let e2 = Map.insert n x e in eval1 b e2
-- eval1 (Fix n b) e = 
-- eval1 x@(INT n) _ = x
-- eval1 (Add e1 e2) e = let x1 = eval1 e1 e
--                           x2 = eval1 e2 e
--                        in INT (x1 + x2)
-- eval1 (IF be et ee) e = 

-- Int is the variable counter
-- for allocation of fresh variables
newtype C t = C (Int -> (ByteCode t, Int)) 
unC (C t) vc0 = t vc0
asC :: C x -> C x; asC = id

instance Symantics C ByteCode where
    int x = C(\vc -> (INT x, vc))
    bool b = C(\vc -> (BOOL b, vc))

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

    rec z s = C(\vc -> let f = vc
                           func = C(\vc -> (Var f, vc))
                           vc1 = succ vc
                           n = vc1
                           num = C(\vc -> (Var n, vc))
                           vc2 = succ vc1
                           (zc, vc3) = unC z vc2
                           (sc, vc4) = unC s vc3
                       in (Fix f (Lam n (IF (Eql (Var n) (INT 0)) zc (App sc (App (Var f) (Var n))))), vc4))
    -- ccshan TODO: change ifeq to ifle so that we can compile rec correctly

    add e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                             (e2b,vc2) = unC e2 vc1
                         in (Add e1b e2b,vc2))

    eql e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                             (e2b,vc2) = unC e2 vc1
                         in (Eql e1b e2b,vc2))

    if_ be et ee = C(\vc -> let (beb,vc1) = unC be vc
                                (etb,vc2)  = unC et vc1
                                (eeb,vc3)  = unC ee vc2
                         in (IF beb etb eeb,vc3))

    comp repr = fst $ unC repr 0

ctest1 = comp . asC . test1 $ ()
ctest2 = comp . asC . test2 $ ()
ctest3 = comp . asC . testgib1 $ ()

-- ------------------------------------------------------------------------
-- The partial evaluator

-- We need no Lift byte-code instruction: no parametric CSP. That is great!

-- The code below is NOT parametric. We could have used type-classes
-- instead of GADTs. Ken noted that the code below could be re-functionalized,
-- and so could in fact be translated into MetaOCaml.
-- Or, if we observe the correspondence between GADT and MetaOCaml <code>,
-- perhaps we can translate to MetaOCaml with an additional layer of code?

data P t where
    VI :: Int -> P Int                -- Possibly static (base type)
    VB :: Bool -> P Bool
    VF :: (P a -> P b) -> P (a->b)
    E  :: C t -> P t                  -- Purely dynamic

asP :: P t -> P t; asP = id

abstr :: P t -> C t
abstr (VI i) = int i
abstr (VF f) = lam (abstr . f . E)
abstr (E x) = x

instance Symantics P ByteCode where
    int x = VI x
    bool b = VB b

    -- lam :: (repr a -> repr b) -> repr (a->b)
    lam = VF
    -- app :: repr (a->b) -> repr a -> repr b
    app ef ea = case ef of
                        VF f -> f ea
                        E  f -> E (app f (abstr ea))

    -- fix :: (repr a -> repr a) -> repr a
    -- For now, to avoid divergence at the PE stage, we residualize
    -- actually, we unroll the fixpoint exactly once, and then
    -- residualize
    fix f = f (E (fix (abstr . f . E)))

    rec z s = VF (\n -> case n of
        VI n -> let f n | n <= 0    = z
                        | otherwise = case s of VF s -> s (f (pred n))
                                                E s -> E (app s (abstr (f (pred n))))
                in f n
        E n -> E (app (rec (abstr z) (abstr s)) n))

    add e1 e2 = case (e1,e2) of
                 (VI n1,VI n2) -> VI nr where nr = n1 + n2
                 _ -> E $ add (abstr e1) (abstr e2)

    eql e1 e2 = case (e1,e2) of
                 (VI n1,VI n2) -> VB nr where nr = n1 == n2
                 (VB n1,VB n2) -> VB nr where nr = n1 == n2
                 _ -> E $ eql (abstr e1) (abstr e2)

    if_ be et ee = case be of
                        (VB b1) -> if b1 then et else ee
                        _ -> E $ if_ (abstr be) (abstr et) (abstr ee)

    comp = comp . abstr

ptest1 = comp . asP . test1 $ ()
ptest2 = comp . asP . test2 $ ()
-- Compare the output with ctest3. The partial evaluation should be
-- evident!
ptest3 = comp . asP . testgib1 $ ()
-- Compare the output with ptest3. The full partial evaluation should be
-- evident!
ptest3' = comp . asP . testgib1' $ ()
