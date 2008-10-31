{-# LANGUAGE GADTs, TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS_GHC -W #-}

-- Interpreter, Compiler, Partial Evaluator

module Incope where

{-
  The language is simply-typed lambda-calculus with fixpoint,
  integers, booleans and comparison: essentially, PCF.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | B Bool | Add ie1 ie2 | Mul ie1 ie2 | Leq ie1 ie2 |
  IF b e-then e-else
  
  The language is just expressive enough for the Gibonacci function
  and the power function.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.
-}

-- This class defines syntax (and its instances, semantics) of our language
-- This class is Haskell98!
class Symantics repr where
    int :: Int -> repr Int                -- int literal
    bool :: Bool -> repr Bool             -- bool literal

    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a

    add :: repr Int -> repr Int -> repr Int
    mul :: repr Int -> repr Int -> repr Int
    if_ :: repr Bool -> repr a -> repr a -> repr a
    leq :: repr Int -> repr Int -> repr Bool

-- The following `projection' function is specific to repr.
-- It is like `run' of the monad
--    comp :: repr a -> something a

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

-- ------------------------------------------------------------------------
-- The interpreter
-- It is a typed, tagless interpreter: R is not a tag. The interpreter
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

    add e1 e2 = R( (unR e1) + (unR e2) )
    mul e1 e2 = R( (unR e1) * (unR e2) )
    leq e1 e2 = R( (unR e1) <= (unR e2) )
    if_ be et ee = R( if (unR be) then unR et else unR ee )

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
-- Another interpreter: it interprets each term to give its size
-- (the number of constructors)
-- It is a typed, tagless interpreter: L is not a tag. The interpreter
-- never gets stuck, because it evaluates typed terms only.
-- This interpreter is also total: it determines the size of the term
-- even if the term itself is divergent.

newtype L a = L Int deriving Show
unL (L x) = x

instance Symantics L where
    int _  = L 1
    bool _ = L 1

    lam f = L( unL (f (L 0)) + 1 )
    app e1 e2 = L( unL e1 + unL e2 + 1 )
    fix f = L( unL (f (L 0)) + 1 )

    add e1 e2 = L( unL e1 + unL e2 + 1 )
    mul e1 e2 = L( unL e1 + unL e2 + 1 )
    leq e1 e2 = L( unL e1 + unL e2 + 1 )
    if_ be et ee = L( unL be +  unL et + unL ee  + 1 )

compL = unL

ltest1 = compL . test1 $ ()
ltest2 = compL . test2 $ ()
ltest3 = compL . test3 $ ()


ltestgib  = compL . testgib  $ ()
ltestgib1 = compL . testgib1 $ () -- 23
ltestgib2 = compL . testgib2 $ ()

ltestpw   = compL . testpowfix $ ()
ltestpw7  = compL . testpowfix7 $ ()
ltestpw72 = compL (app (testpowfix7 ()) (int 2)) -- 17



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

-- The LIFT bytecode operation is used only during evaluation and for fmap
-- it corresponds to a CSP in MetaOCaml.

data ByteCode t where
    Var :: Int -> ByteCode t                -- variables identified by numbers
    Lam :: Int -> ByteCode t2 -> ByteCode (t1->t2)
    App :: ByteCode (t1->t2) -> ByteCode t1  -> ByteCode t2
    Fix :: Int -> ByteCode t -> ByteCode t
    INT :: Int -> ByteCode Int
    BOOL:: Bool -> ByteCode Bool
    Add :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Mul :: ByteCode Int -> ByteCode Int -> ByteCode Int
    Leq :: ByteCode t1 -> ByteCode t1 -> ByteCode Bool
    IF  :: ByteCode Bool -> ByteCode t -> ByteCode t -> ByteCode t
    LIFT :: t -> ByteCode t                 -- Used only for eval and fmap

instance Show (ByteCode t) where
    show (Var n) = "V" ++ show n
    show (Lam n b) = "(\\V" ++ show n ++ " -> " ++ show b ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Fix n b) = "(fix\\V" ++ show n ++ " " ++ show b ++ ")"
    show (INT n) = show n
    show (BOOL b) = show b
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (Leq e1 e2) = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
    show (IF be et ee)
        = "(if " ++ show be ++ 
          " then " ++ show et ++ " else " ++ show ee ++ ")"

-- Int is the variable counter
-- for allocation of fresh variables
newtype C t = C (Int -> (ByteCode t, Int)) 
unC (C t) vc0 = t vc0

{-
instance Functor C where
    fmap f = app (C(\vc -> (LIFT f, vc)))
-}

instance Symantics C where
    int x  = C(\vc -> (INT x, vc))
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

    add e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                             (e2b,vc2) = unC e2 vc1
                         in (Add e1b e2b,vc2))

    mul e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                             (e2b,vc2) = unC e2 vc1
                         in (Mul e1b e2b,vc2))

    leq e1 e2 = C(\vc -> let (e1b,vc1) = unC e1 vc
                             (e2b,vc2) = unC e2 vc1
                         in (Leq e1b e2b,vc2))

    if_ be et ee = C(\vc -> let (beb,vc1) = unC be vc
                                (etb,vc2)  = unC et vc1
                                (eeb,vc3)  = unC ee vc2
                         in (IF beb etb eeb,vc3))

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
-- We need no Lift byte-code instruction: no parametric CSP. That is great!

-- First attempt: works for the first-order fragment of our language,
-- but stumbles on the higher-order fragment.

data P1 t = P1 (Maybe (R t)) (C t)

abstr1 :: P1 t -> C t
abstr1 (P1 _ dyn) = dyn

instance Symantics P1 where
    int  x = P1 (Just (int x)) (int x)
    bool b = P1 (Just (bool b)) (bool b)
    add (P1 (Just n1) _) (P1 (Just n2) _) = int (unR (add n1 n2))
    add e1 e2 = P1 Nothing (add (abstr1 e1) (abstr1 e2))
    mul (P1 (Just n1) _) (P1 (Just n2) _) = int (unR (mul n1 n2))
    mul e1 e2 = P1 Nothing (mul (abstr1 e1) (abstr1 e2))
    leq (P1 (Just n1) _) (P1 (Just n2) _) = bool (unR (leq n1 n2))
    leq e1 e2 = P1 Nothing (leq (abstr1 e1) (abstr1 e2))
    if_ (P1 (Just s) _) et ef = if unR s then et else ef
    if_ eb et ef = P1 Nothing (if_ (abstr1 eb) (abstr1 et) (abstr1 ef))

-- But the problem occurs when we try to implement lam. The result
-- of (lam f) must be P1 Nothing _ or P1 (Just _) _. Alas, we won't know
-- which is which until we apply the function 'f' to a particular
-- P1 value. 


-- The code below is NOT parametric. We could have used type-classes
-- instead of GADTs (see incope1.hs, where we did just that). 
-- See also the GADT-less solution below.

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

{-
instance Functor P where
    fmap f = E . fmap f . abstr
-}


instance Symantics P where
    int x  = VI x
    bool b = VB b

    -- lam :: (repr a -> repr b) -> repr (a->b)
    lam = VF
    -- app :: repr (a->b) -> repr a -> repr b
    app (VF f) ea = f ea
    app (E f)  ea = E (app f (abstr ea))

    -- fix :: (repr a -> repr a) -> repr a
    {- use to:
    -- For now, to avoid divergence at the PE stage, we residualize
    -- actually, we unroll the fixpoint exactly once, and then
    -- residualize
    fix f = f (E (fix (abstr . f . E)))
    -}
    -- Now, we just go all the way (see Jacques' point)
    -- provided `fixing' produces static results...

    fix f = pfix f -- need this charade for GADTs sake

    add (VI 0) e = e
    add e (VI 0) = e
    add (VI n1) (VI n2) = VI (n1 + n2)
    add e1 e2 = E (add (abstr e1) (abstr e2))
    mul e@(VI 0) _ = e
    mul _ e@(VI 0) = e
    mul (VI 1) e = e
    mul e (VI 1) = e
    mul (VI n1) (VI n2) = VI (n1 * n2)
    mul e1 e2 = E (mul (abstr e1) (abstr e2))

    leq (VI n1) (VI n2) = VB (n1 <= n2)
    leq e1 e2 = E (leq (abstr e1) (abstr e2))

    if_ (VB b1) et ee = if b1 then et else ee
    if_ be      et ee = E (if_ (abstr be) (abstr et) (abstr ee))

-- we need this signature to bind 'a'. That's why it can't be a method
pfix :: forall a. (P a -> P a) -> P a
pfix f = res where
 res:: P a
 res = case f res  of
	      E _  -> E (fix (abstr . f . E))
	      VF g -> VF (\x -> 
			  case x of
			          E cde -> E (app (fix (abstr . f . E)) cde)
			          x     -> g x)

compP = compC . abstr

ptest1 = compP . test1 $ ()
ptest2 = compP . test2 $ ()
ptest3 = compP . test3 $ ()

-- Compare the output with ctestgib1. The partial evaluation should be
-- evident!
ptestgib  = compP . testgib  $ ()
ptestgib1 = compP . testgib1 $ ()
ptestgib2 = compP . testgib2 $ ()


ptestpw   = compP . testpowfix $ ()
ptestpw7  = compP . testpowfix7 $ ()
ptestpw72 = compP  (app (testpowfix7 ()) (int 2))


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
-- MetaOCaml how PE is an instance of Symantics (unless Symantics is 
-- generalized to carry both `static' and `dynamic' parts). But we can 
-- still explain PE by itself. In some sense we're just observing 
-- that Asai's code type-checks in Hindley-Milner as soon as we
-- deforest the static code representation.

data Rep repr dynamic static = 
    Rep { dynamic :: repr dynamic, static :: Maybe static }

zE dynamic = Rep dynamic Nothing

zint x = Rep (int x) (Just x)
zbool b = Rep (bool b) (Just b)
zlam f = Rep (lam (dynamic . f . zE)) (Just f)
zapp (Rep _ (Just f)) = f
zapp (Rep f _       ) = zE . app f . dynamic
zadd (Rep _ (Just n1)) (Rep _ (Just n2)) = zint (n1 + n2)
zadd (Rep n1 _       ) (Rep n2 _       ) = zE (add n1 n2)
zmul (Rep _ (Just n1)) (Rep _ (Just n2)) = zint (n1 * n2)
zmul (Rep n1 _       ) (Rep n2 _       ) = zE (mul n1 n2)
zleq (Rep _ (Just n1)) (Rep _ (Just n2)) = zbool (n1 <= n2)
zleq (Rep n1 _       ) (Rep n2 _       ) = zE (leq n1 n2)
zif_ (Rep _ (Just b1)) et ee = if b1 then et else ee
zif_ (Rep be _       ) et ee = zE (if_ be (dynamic et) (dynamic ee))

zfix f = case f (zfix f)  of
	  Rep _ (Just g) -> Rep dfix
			        (Just (\x -> 
				       case x of
			                Rep cde Nothing -> zE (app dfix cde)
			                x     -> g x))
	  Rep _ Nothing -> zE dfix
 where dfix = fix (dynamic . f . zE)

-- unit to supress the monomorphism restriction

ztestgib () = zlam (\x -> zlam (\y ->
                zfix (\self -> zlam (\n ->
                    zif_ (zleq n (zint 0)) x
                      (zif_ (zleq n (zint 1)) y
                       (zadd (zapp self (zadd n (zint (-1))))
                             (zapp self (zadd n (zint (-2))))))))))

ztestgib5 () = zlam (\x -> zlam (\y ->
	zapp (zapp (zapp (ztestgib ()) x) y) (zint 5)))

ztestgib1 () = zapp (zapp (zapp (ztestgib ()) (zint 1)) (zint 1)) (zint 5)

-- The result of PE can be interpreted is several ways, as usual

testR_ztestgib1 = compR (dynamic (ztestgib1 ())) -- 8
testL_ztestgib1 = compL (dynamic (ztestgib1 ())) -- 1
testC_ztestgib1 = compC (dynamic (ztestgib1 ())) -- 8

testR_ztestgib5 = compR (dynamic (ztestgib5 ())) 1 1 -- 8
testL_ztestgib5 = compL (dynamic (ztestgib5 ())) -- 9
testC_ztestgib5 = compC (dynamic (ztestgib5 ()))
-- (\V0 -> (\V1 -> ((((V1 + V0) + V1) + (V1 + V0)) + ((V1 + V0) + V1))))


-- ------------------------------------------------------------------------
-- The HOAS bytecode compiler
-- We compile to GADT, to be understood as a typed assembly language
-- (typed bytecode). 
-- One may argue that this bytecode still faithfully represents
-- MetaOCaml's `code': this is because the function
-- 'a code -> 'b code is trivially convertible to ('a->'b) code.
-- Also, HOAS bytecode certainly seems to match better with MetaOCaml's
-- syntax for functions.
-- Also note, that like MetaOCaml `code', we never pattern-match on
-- HByteCode!
-- Note how the compiler never raises any exception and matches no tags
-- (no generated code has any tags)

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
    HMul :: HByteCode Int -> HByteCode Int -> HByteCode Int
    HLeq :: HByteCode Int -> HByteCode Int -> HByteCode Bool
    HIF  :: HByteCode Bool -> HByteCode t -> HByteCode t -> HByteCode t

{- Showing HOAS has always been problematic... We just skip it for now...

instance Show (HByteCode t) where
    show (HVar n) = "V" ++ show n
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
-}

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
eval (HMul e1 e2) = eval e1 * eval e2
eval (HLeq e1 e2) = eval e1 <= eval e2
eval (HIF be et ee) = if (eval be) then eval et else eval ee


instance Symantics HByteCode where
    int  = HINT
    bool = HBOOL

    lam  = HLam
    app  = HApp

    fix  = HFix
    add  = HAdd
    mul  = HMul
    leq  = HLeq
    if_  = HIF

compH :: HByteCode t -> HByteCode t
compH = id 

htest1 = compH . test1 $ ()
htest1r = eval . test1 $ ()
htest2 = compH . test2 $ ()
htest2r = eval . test2 $ ()
htestgib1  = compH . testgib1 $ ()
htestgib1r = eval . testgib1 $ ()

