{-# OPTIONS -fglasgow-exts #-}

-- Interpreter, Compiler, Partial Evaluator

module Incope where

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
  constructors: repr is of a kind * -> *. OCaml does not support
  higher-order polymorphism.

-}

-- This class defines syntax (and its instances, semantics) of our language
-- This class is Haskell98!
-- The Typeable constraint is for the sake of ByteCode evaluator
class Symantics repr where
    int :: Int -> repr Int                -- int literal
    bool :: Bool -> repr Bool             -- bool literal

    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b
    rec :: repr a -> repr (a -> a) -> repr (Int -> a)
    fix :: (repr a -> repr a) -> repr a

    add :: repr Int -> repr Int -> repr Int
    if_ :: repr Bool -> repr a -> repr a -> repr a
    eql :: Eq a => repr a -> repr a -> repr Bool

-- The following `projection' function is specific to repr.
-- It is like `run' of the monad
--    comp :: repr a -> something a

test1 () = add (int 1) (int 2)
test2 () = lam (\x -> add x x)
test2' () = lam (\x -> add (x (int 1)) (int 2))

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

instance Symantics R where
    int x = R x
    bool b = R b

    lam f = R (unR . f . R)
    app e1 e2 = R( (unR e1) (unR e2) )
    fix f = R( fx (unR . f . R)) where fx f = f (fx f)
    rec z s = R (\n -> if n <= 0 then unR z else unR s (unR (rec z s) (pred n)))

    add e1 e2 = R( (unR e1) + (unR e2) )
    eql e1 e2 = R( (unR e1) == (unR e2) )
    if_ be et ee = R( if (unR be) then unR et else unR ee )

compR = unR

mkitest f = compR (f ())

itest1 = mkitest test1
itest2 = mkitest test2
itest3 = mkitest testgib1

{-
The expression "R (unR . f . R)" _looks_ like tag introduction and
elimination.
But. the function unR is *total*. There is no run-time error
is possible at all -- and this fact is fully apparent to the
compiler.
Note the corresponding code in incope.ml:
  let int (x:int) = x
  let add e1 e2 = e1 + e2

  let lam f = f
No tags at all...
-}


-- ------------------------------------------------------------------------
-- The compiler
-- We compile to GADT, to be understood as a typed assembly language
-- (typed bytecode). The GADT does _not_ use the higher-order abstract
-- syntax. We could have used template Haskell. Alas, its expressions
-- are untyped.
-- Note how ByteCode represents MetaOCaml's `code'. Thus the compiler
-- below neatly maps to the MetaOCaml (with no GADTs).
-- Also note, that like MetaOCaml `code', we never pattern-match on
-- ByteCode!
-- Note how the compiler never raises any exception and matches no tags
-- (no generated code has any tags)

-- The LIFT bytecode operation is used only dyring evaluation
-- it correponds to a CSP in MetaOCaml.

data ByteCode t where
    Var :: Int -> ByteCode t                -- variables identified by numbers
    Lam :: Int -> ByteCode t2 -> ByteCode (t1->t2)
    App :: ByteCode (t1->t2) -> ByteCode t1  -> ByteCode t2
    Fix :: Int -> ByteCode t -> ByteCode t
    INT :: Int -> ByteCode Int
    BOOL:: Bool -> ByteCode Bool
    Add :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Eql :: ByteCode t1 -> ByteCode t1 -> ByteCode Bool
    IF  :: ByteCode Bool -> ByteCode t -> ByteCode t -> ByteCode t
    LIFT :: t -> ByteCode t                 -- Used only for eval

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

-- An evaluator for the ByteCode: the virtual machine
-- The evaluator is partial: if we attempt to evaluate an open code
-- (a variable not found in the env), we will get an error.
-- We use Dynamic to `assure' that variables are properly typed,
-- that is, that the variable name (counter) witnesses its type.
-- A better assurance is to use STRef. Strange correspondence:
-- mutation and lambda application. The essence of both is sharing.
{-
type BEnv = [(Int,Dynamic)]

eval :: Typeable t => BEnv -> ByteCode t -> t
eval env (Var n) | Just dv <- lookup n env,
		   Just v  <- fromDynamic dv = v
eval env v@Var{} = error $ "Open code? variable not found: " ++ show v
eval env (Lam v b) = \x -> eval ((v,toDyn x):env) b
eval env (App e1 e2) = (eval env e1) (eval env e2)
-- eval1 (Fix n b) e = 
eval env (INT n) = n
eval env (BOOL n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
-- eval1 (IF be et ee) e = 
-}


-- Int is the variable counter
-- for allocation of fresh variables
newtype C t = C (Int -> (ByteCode t, Int)) 
unC (C t) vc0 = t vc0

instance Symantics C where
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

data P t where
    VI :: Int -> P Int                -- Possibly static (base type)
    VB :: Bool -> P Bool
    VF :: (P a -> P b) -> P (a->b)
    E  :: C t -> P t                  -- Purely dynamic

abstr :: P t -> C t
abstr (VI i) = int i
abstr (VB b) = bool b
abstr (VF f) = lam (abstr . f . E)
abstr (E x) = x

instance Symantics P where
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

compP = compC . abstr

ptest1 = compP . test1 $ ()
ptest2 = compP . test2 $ ()
-- Compare the output with ctest3. The partial evaluation should be
-- evident!
ptest3 = compP . testgib1 $ ()
-- Compare the output with ptest3. The full partial evaluation should be
-- evident!
ptest3' = compP . testgib1' $ ()


{- GADTs, although work, are still not quite satsifactory:
let us look at the following lines in incope.hs

    app ef ea = case ef of
                        VF f -> f ea
                        E  f -> E (app f (abstr ea))
The datatype has four constructors, VI, VB, VF and E. How come we used
only two of them in the above expression. It is obvious, one may say,
that the constructors VI and VB just can't occur, because of the
typing consideration. This is obvious to us -- but it is not obvious
to the compiler. The case matching above does look *partial*. That
seems like a tenuous point -- but it is precisely the main point of
the whole tag elimination approach. In a typed tagged interpreter
there are lots of pattern-matching that looks partial but in reality
total. The ultimate goal is to make this totality syntactically
apparent.
-}


-- ------------------------------------------------------------------------
-- A partial evaluator that does not use GADTs (except in using C)

-- Ken thinks that, without GADTs, it's not possible to explain in Haskell or
-- MetaOCaml how PE is an instance of Symantics.  But we can still explain PE
-- by itself.  In some sense we're just observing that Asai's code type-checks
-- in Hindley-Milner as soon as we deforest the static code representation.

data Rep dynamic static = Rep { dynamic :: C dynamic, static :: Maybe static }

zE dynamic = Rep dynamic Nothing

zint x = Rep (int x) (Just x)
zbool b = Rep (bool b) (Just b)
zlam f = Rep (lam (dynamic . f . zE)) (Just f)
zapp (Rep _ (Just f)) = f
zapp (Rep f _       ) = zE . app f . dynamic
zfix f = f (zE (fix (dynamic . f . zE)))
zrec z s = zlam (\n -> case n of
    Rep _ (Just n) -> let f n | n <= 0    = z
                              | otherwise = case s of Rep _ (Just s) -> s (f (pred n))
                                                      Rep s _ -> zE (app s (dynamic (f (pred n))))
                      in f n
    Rep n _ -> zE (app (rec (dynamic z) (dynamic s)) n))
zadd (Rep _ (Just n1)) (Rep _ (Just n2)) = zint (n1 + n2)
zadd (Rep n1 _       ) (Rep n2 _       ) = zE (add n1 n2)
zeql (Rep _ (Just n1)) (Rep _ (Just n2)) = zbool (n1 == n2)
zeql (Rep n1 _       ) (Rep n2 _       ) = zE (eql n1 n2)
zif_ (Rep _ (Just b1)) et ee = if b1 then et else ee
zif_ (Rep be _       ) et ee = zE (if_ be (dynamic et) (dynamic ee))

ztestgib = zlam (\x -> zlam (\y ->
                zfix (\self -> zlam (\n ->
                    zif_ (zeql n (zint 0)) x
                      (zif_ (zeql n (zint 1)) y
                       (zadd (zapp self (zadd n (zint (-1))))
                             (zapp self (zadd n (zint (-2))))))))))

ztestgib1 = zapp (zapp (zapp ztestgib (zint 1)) (zint 1)) (zint 5)

-- "show ptest3 == show (compC (dynamic ztestgib1))" is True

ztestgib' = zlam (\x -> zlam (\y -> zlam (\n ->
            zapp (zapp (zrec (zlam (\c -> zapp (zapp c x) y))
                             (zlam (\m -> zlam (\c -> zapp m (zlam (\a -> zlam (\b ->
                                          zapp (zapp c b) (zadd a b))))))))
                       n)
                 (zlam (\a -> zlam (\b -> a))))))

ztestgib1' = zapp (zapp (zapp ztestgib' (zint 1)) (zint 1)) (zint 5)

-- "show ptest3' == show (compC (dynamic ztestgib1'))" is True

