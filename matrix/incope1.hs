{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- Interpreter, Compiler, Partial Evaluator

module Incope1 where

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

class Symantics ar a b ci ca cb cab | 
                ar -> ci, ar a -> ca, ar b -> cb, ar a b -> cab  where
    int :: Int -> ar ci Int                -- int literal
    add :: ar ci Int -> ar ci Int -> ar ci Int
    ifeq:: ar ci Int -> ar ci Int -> ar ca a -> ar ca a -> ar ca a

    lam :: (ar ca a -> ar cb b) -> ar cab (a->b)
    app :: ar cab (a->b) -> ar ca a -> ar cb b
    fix :: (ar ca a -> ar ca a) -> ar ca a


-- The following `projection' function is specific to repr.
-- It is like `run' of the monad
--    comp :: repr a -> something a

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

-- Note that everything going on here is straight out of
-- "Boxes go Bananas" by Washburn and Weirich (intended or otherwise)
newtype R t a = R a deriving Show
unR (R x) = x


instance Symantics R a b () () () () where
    int x = R x
    add e1 e2 = R( (unR e1) + (unR e2) )
    ifeq ie1 ie2 et ee = R( if (unR ie1) == (unR ie2) then unR et else unR ee )

    lam f = R (unR . f . R)
    app e1 e2 = R( (unR e1) (unR e2) )
    fix f = R( fx (unR . f . R)) where fx f = f (fx f)

mkitest f = unR (f $ ())

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
newtype C cr t = C (Int -> (ByteCode t, Int)) 
unC (C t) vc0 = t vc0

instance Symantics C a b () () () () where
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

compC repr = fst $ unC repr 0

ctest1 = compC . test1 $ ()
ctest2 = compC . test2 $ ()
ctest3 = compC . testgib1 $ ()


-- ------------------------------------------------------------------------
-- The partial evaluator

-- We need no Lift byte-code instruction: no parametric CSP. That is great!

-- The code below is NOT parametric. We could have used type-classes
-- instead of GADTs. Ken noted that the code below could be re-functionalized,
-- and so could in fact be translated into MetaOCaml.
-- Or, if we observe the correspondence between GADT and MetaOCaml <code>,
-- perhaps we can translate to MetaOCaml with an additional layer of code?

data P cr t = PV cr | PE (C () t)

newtype PVS t = PVS t
newtype PFII  = PFII (PVS Int -> PVS Int)


class PRep t crep | t -> crep where
  abstr :: P crep t -> C () t

instance PRep Int (PVS Int) where
    abstr (PV (PVS x)) = int x
    abstr (PE x) = x


instance PRep (Int->Int) (PFII) where
    -- abstr (PVL x) = int x
    abstr (PE x) = x

{-
data P t where
    VI :: Int -> P Int                -- Possibly static (base type)
    VF :: (P a -> P b) -> P (a->b)
    E  :: C t -> P t                  -- Purely dynamic

abstr :: P t -> C t
abstr (VI i) = int i
abstr (VF f) = lam (abstr . f . E)
abstr (E x) = x
-}



instance (PRep a ca, PRep b cb, PRep (a->b) cab)
    => Symantics P a b (PVS Int) ca cb cab where
    int = PV . PVS
{-
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

    add e1 e2 = case (e1,e2) of
                 (VI n1,VI n2) -> VI nr where nr = n1 + n2
                 _ -> E $ add (abstr e1) (abstr e2)

    ifeq ie1 ie2 et ee = case (ie1,ie2) of
                          (VI n1, VI n2) -> if n1==n2 then et else ee
                          _ -> E $ ifeq (abstr ie1) (abstr ie2)
                                        (abstr et) (abstr ee)
-}
compP = compC . abstr

ptest1 = compP . test1 $ ()
{-
ptest2 = compP . test2 $ ()
-- Compare the output with ctest3. The partial evaluation should be
-- evident!
ptest3 = compP . testgib1 $ ()
-}
