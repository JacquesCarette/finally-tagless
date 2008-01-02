{-# OPTIONS -fglasgow-exts -W #-}

-- Relating Final and Initial Typed Tagless representations
-- Based on the simplified code from the paper by
--   Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan

module InFin where

{-
  The language is simply-typed lambda-calculus with fixpoint,
  integers, booleans and comparison.

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

mkrtest f = compR (f ())


rtest1 = mkrtest test1
rtest2 = mkrtest test2
rtest3 = mkrtest test3

rtestgib  = mkrtest testgib
rtestgib1 = mkrtest testgib1
rtestgib2 = mkrtest testgib2

rtestpw   = mkrtest testpowfix
rtestpw7  = mkrtest testpowfix7
rtestpw72 = mkrtest (\() -> app (testpowfix7 ()) (int 2))

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

data IR h t where
    Var  :: h t -> IR h t
    INT  :: Int -> IR h Int
    BOOL :: Bool -> IR h Bool
    Lam  :: (IR h t1 -> IR h t2) -> IR h (t1->t2)
    App  :: IR h (t1->t2) -> IR h t1  -> IR h t2
    Fix  :: (IR h t -> IR h t) -> IR h t
    Add  :: IR h Int -> IR h Int -> IR h Int
    Mul  :: IR h Int -> IR h Int -> IR h Int
    Leq  :: IR h Int -> IR h Int -> IR h Bool
    IF   :: IR h Bool -> IR h t -> IR h t -> IR h t


-- An evaluator for IR: the virtual machine
-- It is total (modulo potential non-termination in fix)
-- No exceptions are to be raised, and no pattern-match failure
-- may occur. All pattern-matching is _syntactically_, patently complete.
evalI :: IR R t -> t
evalI (Var v) = unR v
evalI (Lam b) = \x -> evalI (b . Var . R $ x)
evalI (App e1 e2) = (evalI e1) (evalI e2)
evalI (Fix f) = evalI (f (Fix f))
evalI (INT n)  = n
evalI (BOOL n) = n
evalI (Add e1 e2) = evalI e1 + evalI e2
evalI (Mul e1 e2) = evalI e1 * evalI e2
evalI (Leq e1 e2) = evalI e1 <= evalI e2
evalI (IF be et ee) = if (evalI be) then evalI et else evalI ee


{- Showing HOAS has always been problematic... We just skip it for now...
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
-}




-- Conversion from the Final to the Initial representations

instance Symantics (IR h) where
    int  = INT
    bool = BOOL

    lam  = Lam
    app  = App

    fix  = Fix
    add  = Add
    mul  = Mul
    leq  = Leq
    if_  = IF

compI :: IR h t -> IR h t
compI = id 

itest1  = compI . test1 $ ()
itest1r = evalI . test1 $ () -- 3
itest2  = compI . test2 $ ()
itest2r = evalI . test2 $ ()
itest3  = compI . test3 $ ()
itest3r = (evalI . test3 $ ()) (+ 5) -- 8

itestgib1  = compI . testgib1 $ () 
itestgib1r = evalI . testgib1 $ () -- 8, or gib 5

itestpw   = compI . testpowfix $ ()
itestpw7  = compI . testpowfix7 $ ()
itestpw72 = evalI (app (testpowfix7 ()) (int 2)) -- 128


-- Converter from the Initial to the Final representations
itf :: Symantics repr => IR repr t -> repr t
itf (INT n)  = int n
itf (BOOL n) = bool n
itf (Add e1 e2) = add (itf e1) (itf e2)
itf (Mul e1 e2) = mul (itf e1) (itf e2)
itf (Leq e1 e2) = leq (itf e1) (itf e2)
itf (IF be et ee) = if_ (itf be) (itf et) (itf ee)
itf (Var v) = v
itf (Lam b) = lam(\x -> itf (b (Var x)))
itf (App e1 e2) = app (itf e1) (itf e2)
itf (Fix b) = fix(\x -> itf (b (Var x)))

-- Veryfying Initial -> Final -> Initial
-- ifi :: IR (IR h) t -> IR h t
ifi ir = compI . itf $ ir
ifi_12i = (evalI (Lam (\x -> App x (INT 1)))) (+6) -- 7
ifi_12r = (evalI (ifi (Lam (\x -> App x (INT 1))))) (+6) -- 7

ifi_gib1i = evalI (testgib1 ())       -- 8
ifi_gib1r = evalI (ifi (testgib1 ())) -- 8


-- Veryfying Final -> Initial -> Final
-- fif :: (Symantics h) => IR h t -> h t
fif fr = itf . compI $ fr

fif_gib1i = compR (testgib1 ())       -- 8
fif_gib1r = compR (fif (testgib1 ())) -- 8

