{-# OPTIONS -fglasgow-exts -W #-}

-- Experimenting with Self-interperter in the tagless-final style

module IncoSelf where

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

import Incope

-- ---------------------------------------------------------------------------
-- Self-interpretation

twice_inc_3_e () =
    -- The evaluation context in the following four lines is a self-interpreter
    -- of the object language, encoded in the metalanguage.
    let lam_ = lam (\f -> f) in
    let app_ = lam (\f -> lam (\x -> app f x)) in
    let add_ = lam (\m -> lam (\n -> add m n)) in
    let int_ = lam (\i -> i) in
    -- The term in the following three lines is the object term
    --      let twice_ f x = f (f x) in let inc_ n = n + 1 in twice_ inc_ 3
    -- encoded in the object language then encoded in the metalanguage.
    let twice_ = app lam_ (lam (\f -> 
	   app lam_ (lam (\x -> app (app app_ f) (app (app app_ f) x))))) in
    let inc_ = app lam_ (lam (\n -> app (app add_ n) (app int_ (int 1)))) in
    app (app app_ (app (app app_ twice_) inc_)) (app int_ (int 3))
test_inc3e_r = compR $ twice_inc_3_e ()
test_inc3e_c = compC $ twice_inc_3_e ()
test_inc3e_p = compP $ twice_inc_3_e ()
-- to verify the Prop 5
test_inc3p = compP $ (app (app
                       (lam (\twice -> lam (\inc_ -> 
                                              app (app twice inc_) (int 3))))
                       (lam (\f -> lam (\x -> app f (app f x)))))
                       (lam (\n -> add n (int 1))))
-- 5

-- This is for computing *size*.
twice_inc_3_se () =
    -- The evaluation context in the following four lines is a self-interpreter
    -- of the object language, encoded in the metalanguage.
    let lam_ = lam (\f -> add (int 1) (app f (int 0)) ) in
    let app_ = lam (\f -> lam (\x -> add (int 1) (add f x))) in
    let add_ = lam (\m -> lam (\n -> add (int 1) (add m n))) in
    let int_ = lam (\_ -> int 1) in
    -- The term in the following three lines is the object term
    --      let twice_ f x = f (f x) in let inc_ n = n + 1 in twice_ inc_ 3
    -- encoded in the object language then encoded in the metalanguage.
    let twice_ = app lam_ (lam (\f -> 
	   app lam_ (lam (\x -> app (app app_ f) (app (app app_ f) x))))) in
    let inc_ = app lam_ (lam (\n -> app (app add_ n) (app int_ (int 1)))) in
    app (app app_ (app (app app_ twice_) inc_)) (app int_ (int 3))
test_inc3se_r = compR $ twice_inc_3_se ()
test_inc3se_c = compC $ twice_inc_3_se ()
test_inc3se_p = compP $ twice_inc_3_se ()
-- 10

twice_inc_3_ee () =
    -- The evaluation context in the following four lines is a self-interpreter
    -- of the object language, encoded in the metalanguage.
    let lam_ = lam (\f -> f) in
    let app_ = lam (\f -> lam (\x -> app f x)) in
    let add_ = lam (\m -> lam (\n -> add m n)) in
    let int_ = lam (\i -> i) in
    -- The evaluation context in the following four lines is a self-interpreters
    -- of the object language (the same one as above), encoded in the object
    -- language then encoded in the metalanguage.  We use two underscores in
    -- the variable names only because Haskell's "let" is "letrec".
    let lam__ = app lam_ (lam (\f -> f)) in
    let app__ = app lam_ (lam (\f -> app lam_ (lam (\x -> app (app app_ f) x)))) in
    let add__ = app lam_ (lam (\m -> app lam_ (lam (\n -> app (app add_ m) n)))) in
    let int__ = app lam_ (lam (\i -> app int_ i)) in
    -- Rename *__ back to *_.
    let lam_ = lam__ in
    let app_ = app__ in
    let add_ = add__ in
    let int_ = int__ in
    -- The term in the following three lines is the object term
    --      let twice_ f x = f (f x) in let inc_ n = n + 1 in twice_ inc_ 3
    -- encoded in the object language then encoded in the metalanguage.
    let twice_ = app lam_ (lam (\f -> app lam_ (lam (\x -> app (app app_ f) (app (app app_ f) x))))) in
    let inc_ = app lam_ (lam (\n -> app (app add_ n) (app int_ (int 1)))) in
    app (app app_ (app (app app_ twice_) inc_)) (app int_ (int 3))

test_inc3ee_r = compR $ twice_inc_3_ee ()
test_inc3ee_c = compC $ twice_inc_3_ee ()
test_inc3ee_p = compP $ twice_inc_3_ee ()
-- *Incope> compC (dynamic (twice_inc_3_ee ()))
-- 5

-- test_inc3e_c and test_inc3ee_c are most illustrative.

-- Implement a term that is a lambda term rather than a ground term
-- this is test3 above.
test3_e () =
    -- The evaluation context in the following four lines is a self-interpreter
    -- of the object language, encoded in the metalanguage.
    let lam_ = lam (\f -> f) in
    let app_ = lam (\f -> lam (\x -> app f x)) in
    let add_ = lam (\m -> lam (\n -> add m n)) in
    let int_ = lam (\i -> i) in
    -- The term in the following three lines is the object term
    -- let test3_ = \x -> (x 1) + 2
    -- encoded in the object language then encoded in the metalanguage.
    app lam_ (lam (\x -> (app 
			   (app add_
			    (app (app app_ x) (app int_ (int 1))))
			   (app int_ (int 2)))))
test_t3_r = compR $ test3_e ()
test_t3_c = compC $ test3_e ()
test_t3_p = compP $ test3_e ()
-- to verify the Prop 5
test_t3p = compP $ lam (\x -> (add (app x (int 1)) (int 2)))


{- And now something harder: powfix
testpowfix () = lam (\x ->
                      fix (\self -> lam (\n ->
                        if_ (leq n (int 0)) (int 1)
                            (mul x (app self (add n (int (-1))))))))
at the same time, do
testpowfix7 () = lam (\x -> app (app (testpowfix ()) x) (int 7))
-}
testpowfix_e () =
    -- The evaluation context in the following lines is a self-interpreter
    -- of the object language, encoded in the metalanguage.
    let lam_ = lam (\f -> f) in
    let app_ = lam (\f -> lam (\x -> app f x)) in
    let add_ = lam (\m -> lam (\n -> add m n)) in
    let mul_ = lam (\m -> lam (\n -> mul m n)) in
    let leq_ = lam (\m -> lam (\n -> leq m n)) in
    let if__ = lam (\be -> lam (\ee -> lam (\te -> if_ be ee te))) in
    let int_ = lam (\i -> i) in
    let fix_ = lam (\f -> fix (\self -> app f self)) in
    -- The term in the following three lines is the object term
    -- testpowfix above
    -- encoded in the object language then encoded in the metalanguage.
    -- That is seriously not pretty, but it works!
    let tpf = app lam_ (lam (\x ->
          app fix_ (lam (\self -> app lam_ (lam (\n ->
             (app (app (app if__ (app (app leq_ n) (app int_ (int 0))))
                (app int_ (int 1)))
                (app (app mul_ x) (app (app app_ self)
                     (app (app add_ n) (app int_ (int (-1))))))))))))) in  
    let tpf7 = app lam_ (lam (\x ->
          app (app app_ (app (app app_ tpf) x)) (app int_ (int 7)))) in
    (tpf, tpf7)

test_pf_r = compR.fst $ testpowfix_e ()
test_pf_c = compC.fst $ testpowfix_e ()
test_pf_p = compP.fst $ testpowfix_e ()
test_pf7_r = compR.snd $ testpowfix_e ()
test_pf7_c = compC.snd $ testpowfix_e ()
test_pf7_p = compP.snd $ testpowfix_e ()
-- to verify the Prop 5
test_pfp = compP $ testpowfix ()
test_pf7p = compP $ testpowfix7 ()

-- start encoding some of Ken's ideas on a self-interpreter
-- Comment (Jacques): I have left this in here because this is
-- what we would really like to do, but it doesn't quite work
-- because of polymorphism issues.
an_ep :: (Int  -> repr Int)
      -> (Bool -> repr Bool)
      -> (repr Int -> repr Int -> repr Int)  -- add
      -> (repr Int -> repr Int -> repr Int)  -- mul
      -> (forall a b. repr (a->b) -> repr a -> repr b)             -- app
      -> (repr Int -> repr Int -> repr Bool) -- leq
      -> (forall a. repr Bool -> repr a  -> repr a  -> repr a)     -- if
      -> (forall a b. (repr a -> repr b) -> repr (a->b))	   -- lam
      -> (forall a. (repr a -> repr a) -> repr a)	           -- fix
      -> repr (Int -> Int -> Int)
an_ep = (\ _int _bool _add _mul _app _leq _if_ _lam _fix -> 
            _lam (\x ->
                  _fix (\self -> _lam (\n ->
                    _if_ (_leq n (_int 0)) (_int 1)
                        (_mul x (_app self (_add n (_int (-1)))))))))


test_epr = compR (an_ep int bool add mul app leq if_ lam fix)
test_epc = compC (an_ep int bool add mul app leq if_ lam fix)
test_epp = compP (an_ep int bool add mul app leq if_ lam fix)

{-
  The self-interpreter seems to want this instance to exist
  or at least it did at one point!   And that was usually 
  a sign of a bug, so this should not be allowed in, even
  though it is a rather fun instance.
instance Functor ((->) t) where
    fmap f = \y -> f . y

instance Symantics ((->) t) where
    int x = \y -> x
    bool b = \y -> b

    lam f = \t -> \a -> let ta = const a in (f ta) t
    app e1 e2 = \t -> (e1 t) (e2 t)

    add e1 e2 = \y -> (e1 y) + (e2 y)
    mul e1 e2 = \y -> (e1 y) * (e2 y)
    leq e1 e2 = \y -> (e1 y) <= (e2 y)
    if_ be et ee = \y -> if (be y) then (et y) else (ee y)
-}

-- A plain interpreter, with the right type
interp :: (Symantics repr) => 
         ((Int -> repr Int)
      -> (Bool -> repr Bool)
      -> (repr Int -> repr Int -> repr Int)  -- add
      -> (repr Int -> repr Int -> repr Int)  -- mul
      -> (forall a b. repr (a->b) -> repr a -> repr b)             -- app
      -> (repr Int -> repr Int -> repr Bool) -- leq
      -> (forall a. repr Bool -> repr a  -> repr a  -> repr a)     -- if
      -> (forall a b. (repr a -> repr b) -> repr (a->b))	   -- lam
      -> (forall a. (repr a -> repr a) -> repr a)              -- fix
      -> a ) -> a
interp prog = (prog int bool add mul app leq if_ lam fix)

type Foo c = (Symantics repr) =>
         (Int -> repr Int)
      -> (Bool -> repr Bool)
      -> (repr Int -> repr Int -> repr Int)  -- add
      -> (repr Int -> repr Int -> repr Int)  -- mul
      -> (forall a b. repr (a->b) -> repr a -> repr b)             -- app
      -> (repr Int -> repr Int -> repr Bool) -- leq
      -> (forall a. repr Bool -> repr a  -> repr a  -> repr a)     -- if
      -> (forall a b. (repr a -> repr b) -> repr (a->b))	   -- lam
      -> (forall a. (repr a -> repr a) -> repr a)              -- fix
      -> repr c
-- simple tests, but they need signatures else they don't work
int1 :: Foo Int
int1 = \ _int _bool _add _mul _app _leq _if_ _lam _fix -> 
    _add (_int 1) (_int 2)
int2 :: Foo (Int -> Int)
int2 = \ _int _bool _add _mul _app _leq _if_ _lam _fix -> 
    _lam (\x -> _add x x)
int3 :: Foo ((Int -> Int) -> Int)
int3 = \ _int _bool _add _mul _app _leq _if_ _lam _fix -> 
    _lam (\x -> _add (_app x (_int 1)) (_int 2))

-- monomorphism restriction means we need signatures
t_int1 :: Symantics repr => repr Int
t_int1 = interp int1
t_int2 :: Symantics repr => repr (Int -> Int)
t_int2 = interp int2
t_int3 :: Symantics repr => repr ((Int -> Int) -> Int)
t_int3 = interp int3

{-
class Self repr where
    lift :: a -> repr a

sinterp :: (Symantics repr, Self repr) => 
      repr (
         (Int -> repr Int)
      -> (Bool -> repr Bool)
      -> (repr Int -> repr Int -> repr Int)  -- add
      -> (repr Int -> repr Int -> repr Int)  -- mul
      -> (forall a b. repr (a->b) -> repr a -> repr b)             -- app
      -> (repr Int -> repr Int -> repr Bool) -- leq
      -> (forall a. repr Bool -> repr a  -> repr a  -> repr a)     -- if
      -> (forall a b. (repr a -> repr b) -> repr (a->b))	   -- lam
      -> (forall a. (repr a -> repr a) -> repr a)	           -- fix
      -> a)
      -> repr a
sinterp prog = 
       app (app (app (app (app (app (app (app (app prog
       (lam (\x -> lift x)))
       (lam (\b -> lift b)))
       (lift add ))
       (lift mul))
       (lift (\f x -> app f x)))
       (lift leq))
       (lift (\be te ee -> if_ be te ee)))
       (lift (\f -> lam f)))
       (lift (\f -> fix f))

type SFoo c = (Symantics repr, Self repr) => repr (
         (Int -> repr Int)
      -> (Bool -> repr Bool)
      -> (repr Int -> repr Int -> repr Int)  -- add
      -> (repr Int -> repr Int -> repr Int)  -- mul
      -> (forall a b. repr (a->b) -> repr a -> repr b)             -- app
      -> (repr Int -> repr Int -> repr Bool) -- leq
      -> (forall a. repr Bool -> repr a  -> repr a  -> repr a)     -- if
      -> (forall a b. (repr a -> repr b) -> repr (a->b))	   -- lam
      -> (forall a. (repr a -> repr a) -> repr a)             -- fix
      -> repr c)
testi :: SFoo Int
testi = lift (\ _int _bool _add _mul _app _leq _if_ _lam _fix -> 
     _add (int 1) (int 3))
-}

{-  This is so tentalizingly close!
i1 :: (Symantics repr, Self repr) => repr Int
i1 = app (lam sinterp) (testi)
i1r = compR i1 -- 3
i1c = compC i1
i1p = compP i1 -- 3
-}

{-
si prog = 
     lam (\_int ->
     lam (\_bool ->
     lam (\_add ->
     lam (\_mul ->
     lam (\_leq ->
     lam (\_if_ ->
     lam (\_lam ->
     lam (\_app ->
     lam (\_fix ->
       app (app (app (app (app (app (app (app (app prog
       _int) _bool) _add) _mul) _leq) _if_) _lam) _app) _fix
     )))))))))

-- i2 prog = (si interp) prog
-}


-- The following doesn't work because "repr" and "forall a." quite reasonably
-- do not commute
-- Use CSP to encode the object language in the object language?

open_lam :: (Symantics repr, Functor repr) 
	                     => repr (forall a b. (r a -> r b) -> r (a -> b))
                             -> repr (            (r a -> r b) -> r (a -> b))
open_lam = fmap id

open_app :: (Symantics repr, Functor repr)
	                     => repr (forall a b. r (a -> b) -> (r a -> r b))
                             -> repr (            r (a -> b) -> (r a -> r b))
open_app = fmap id

open_csp :: (Symantics repr, Functor repr)
	                     => repr (forall a. a -> r a)
                             -> repr (          a -> r a)
open_csp = fmap id

twice_encoded :: (Symantics repr, Functor repr) => repr
    ((forall a b. (r a -> r b) -> r (a -> b)) ->
     (forall a b. r (a -> b) -> (r a -> r b)) ->
     (forall a. a -> r a) ->
     r ((c -> c)  -> (c -> c)))
twice_encoded =
    lam (\_lam ->
    lam (\_app ->
    lam (\_csp ->
    app (open_lam _lam)
        (lam (\f -> app (open_lam _lam)
                        (lam (\x -> app (app (open_app _app) f)
                                        (app (app (open_app _app) f) x))))))))

open_encoded :: (Symantics repr, Functor repr)
    => repr (forall r. (forall a b. (r a -> r b) -> r (a -> b)) ->
                       (forall a b. r (a -> b) -> (r a -> r b)) ->
                       (forall a. a -> r a) ->
                       r c)
    -> repr (          (forall a b. (r a -> r b) -> r (a -> b)) ->
                       (forall a b. r (a -> b) -> (r a -> r b)) ->
                       (forall a. a -> r a) ->
                       r c)
open_encoded = fmap id

-- Need repr to not just be covariant but also "continuous" for universal intro
{-
self_interp :: (Symantics repr) => repr
    ((forall r.
      (forall a b. (r a -> r b) -> r (a -> b)) ->
      (forall a b. r (a -> b) -> (r a -> r b)) ->
      (forall a b. (a -> b) -> (r a-> r b)) ->
      r c)
     -> c)
self_interp = lam (\exp_encoded ->
    app (open_encoded exp_encoded)
-}


newtype RR r a = RR{unRR::r a} deriving Functor

instance Symantics r => Symantics (RR r) where
    int  = RR . int
    bool = RR . bool

    lam f = RR (lam (unRR . f . RR))
    app e1 e2 = RR( app (unRR e1) (unRR e2) )
    fix f = RR(fix (unRR . f . RR))

    add e1 e2 = RR( add (unRR e1) (unRR e2) )
    mul e1 e2 = RR( mul (unRR e1) (unRR e2) )
    leq e1 e2 = RR( leq (unRR e1) (unRR e2) )
    if_ be et ee = RR( if_ (unRR be) (unRR et) (unRR ee) )


-- test3_1 and test3_2 have the same signature!
test3_1 :: (Symantics repr) => repr ((Int->Int)->Int)
test3_1 = lam (\x -> add (app x (int 1)) (int 2))

test3_2 :: (Symantics repr) => repr ((Int->Int)->Int)
test3_2 = unRR (test3_1)

test3_3 :: (Symantics repr) => repr ((Int->Int)->Int)
test3_3 = unRR (test3_1)


-- ------------------------------------------------------------------------
-- A rather unsatisfactory attempt at the problem of deriving an 
-- eta-transformer. It works only in the simplest case (test3 below)
-- and doesn't work for nested lambda
--
-- Another idea, not tried below, is to represent a Hypothetical
-- expression (Hyp below) as Hyp (a->b) where 'a' is the hypothesis.
-- So, we may be able to represent
-- \x -> e1 + e2 as (\x -> e1) `liftedplus` (\x -> e2)
-- \x -> e1 e2 as (\x -> e1) `liftedapp` (\x->e2)
-- The hypothesis can be discharged when we examine e1 or e2
-- (e.g., when e1 is a constant, we can note the fact it does not
-- depend on the hypothesis). This approach better works in Twelf.
-- Still, it has trouble for nested lambdas.

-- For related work, see the paper ''Free variable Types''
-- by Edwin Westbrook, on Aaron Stump's home page.


data RC r s = RCHyp | RCHRed (r s) | RCReal
data RH r s a = RH (RC r s) (r a)

hdyn (RH _ x) = x

-- instance Symantics r => Symantics (RH r) where
hint x  = RH RCReal (int x)
hbool x = RH RCReal (bool x)

h1add RCReal RCReal = RCReal
h1add _ _ = RCHyp
hadd (RH ec1 e1) (RH ec2 e2) = RH (h1add ec1 ec2) (add e1 e2)

h1app RCReal _ RCReal = RCReal
h1app RCReal e1 RCHyp = RCHRed e1
h1app _ _ _ = RCHyp

happ (RH ec1 e1) (RH ec2 e2) = RH (h1app ec1 e1 ec2) (app e1 e2)

hlam f = check_eta_red (f (RH RCHyp undefined)) -- this is fixable
 where
  to_r f = lam (hdyn . f . (RH RCReal))
  check_eta_red (RH RCHyp _)      = RH RCHyp (to_r f)
  check_eta_red (RH RCReal _)     = RH RCReal (to_r f)
  check_eta_red (RH (RCHRed e) _) = RH RCReal e


{-
htest1 () = hlam (\x -> (hlam (\y -> (hint 1))) `happ` (hint 2))
htest1r = compC . hdyn $ htest1 ()

htest2 () = hlam (\x -> (hlam (\y -> x)) `happ` (hint 2))
htest2r = compC . hdyn $ htest2 ()

htest3 () = hlam (\x -> (hlam (\y -> (hint 1))) `happ` x)
htest3r = compC . hdyn $ htest3 ()

htest31 () = hlam (\x -> (hlam (\y -> x)) `happ` x)
htest31r = compC . hdyn $ htest31 ()

-- nested lambdas aren't handled yet. Need to also propagate
haddtest () = hlam (\x -> hlam (\y -> hadd x y))
htest4 () = hlam (\x -> hlam (\y -> (haddtest () `happ` y) `happ` x))
htest4r = compC . hdyn $ htest4 ()

htest5 () = hlam (\x -> hlam (\y -> (haddtest () `happ` x) `happ` y))
htest5r = compC . hdyn $ htest5 ()

htest6 () = hlam (\x -> hlam (\y -> (haddtest () `happ` y) `happ` y))
htest6r = compC . hdyn $ htest6 ()

htest7 () = hlam (\x -> hlam (\y -> (haddtest () `happ` x) `happ` x))
htest7r = compC . hdyn $ htest7 ()
-}
