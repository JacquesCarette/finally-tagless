{-# LANGUAGE GADTs, NoMonomorphismRestriction, RankNTypes #-}

-- GADT and (generalized) Church encoding
-- Understanding 
--    Patricia Johann and Neil Ghani:
--    Foundations for Structured Programming with GADTs, POPL2008
-- and relating to the typed tagless final approach

module GADTChurch where

import Prelude hiding (const)
import Data.Functor.Identity

-- Example 2 (p 10) from Johann and Ghani's paper

data Term a where
   Const :: a -> Term a
   Pair ::  Term b -> Term c -> Term (b,c)
   App   :: Term (b -> a) -> Term b -> Term a

-- sample term

ti1 = App (Const fst) (Pair (Const True) (Const 'a'))

-- Sample evaluator
evalI :: Term a -> a
evalI (Const x)  = x
evalI (Pair x y) = (evalI x, evalI y)
evalI (App f x)  = (evalI f) (evalI x)

ti1_v = evalI ti1
-- True


-- Tagless final representation (Haskell98)

class Symantics repr where
    const :: a -> repr a
    pair  :: repr a -> repr b -> repr (a,b)
    app   :: repr (b->a) -> repr b -> repr a

-- Sample term
-- (obtained from ti1 by applying Emacs' downcase-region)
tf1 :: Symantics repr => repr Bool  -- inferred
tf1 = app (const fst) (pair (const True) (const 'a'))


-- Sample evaluator
newtype R a = R{unR :: a}  -- (R a) operationally is the same as (a)

instance Symantics R where
    const = R
    pair (R x) (R y) = R (x,y)
    app  (R x) (R y) = R (x y)

evalF = unR

tf1_v = evalF tf1
-- True


-- Why GADT Term and Symantics are in bijection

-- Initial (GADT) -> Tagless Final

evalIF :: Symantics repr => Nat Term repr
evalIF (Const x)  = const x
evalIF (Pair x y) = pair (evalIF x) (evalIF y)
evalIF (App x y)  = app (evalIF x) (evalIF y)

-- Evaluating ti1 by first transforming it to the final
ti1_fv = evalF (evalIF ti1)
-- True


-- Tagless Final -> Initial (GADT)

instance Symantics Term where
    const = Const
    pair  = Pair
    app   = App

evalFI :: Term a -> Term a
evalFI = id

-- Evaluating tf1 by first transforming it to the initial
tf1_iv = evalI (evalFI tf1)
-- True

-- Deriving Church encoding

-- The dictionary associated with the class Symantics

data SYMDict repr = 
  SYMDict{dict_const :: forall a. a -> repr a,
	  dict_pair  :: forall a b. repr a -> repr b -> repr (a,b),
	  dict_app   :: forall a b. repr (b -> a) -> repr b -> repr a}

-- the sample term with the explicit dictionary
-- Church-encoding?

-- te1 :: SYMDict repr -> repr Bool
te1 dict = app' (const' fst) (pair' (const' True) (const' 'a'))
 where
 SYMDict {dict_const = const', dict_pair = pair', dict_app = app'} = dict


-- Dictionary for the Final->GADT interpreter

dict_fi = SYMDict{dict_const = Const, dict_pair = Pair, dict_app = App}

-- Evaluating te1 by first transforming it to the initial
te1_iv = evalI (te1 dict_fi)
-- True

-- Added by Jacques below

-- Dictionary for GADT -> Final interpreter
dict_GADT = SYMDict{dict_const = const, dict_pair = pair, dict_app = app}

type Nat f g = forall a. f a -> g a
foldTerm :: SYMDict f -> Nat Term f
foldTerm dict = \z ->
    case z of
      (Const x)  -> const' x
      (Pair x y) -> pair' (foldTerm dict x) (foldTerm dict y)
      (App x y)  -> app' (foldTerm dict x) (foldTerm dict y)
 where
 SYMDict {dict_const = const', dict_pair = pair', dict_app = app'} = dict

tt1 = foldTerm dict_fi ti1
-- Term Bool

-- we can compose:
foldTerm' :: SYMDict f -> Nat Term f
foldTerm' dict t = foldTerm dict (evalFI t)

-- their foldTerm
foldTerm'' ::
  (forall a. a -> f a) ->
  (forall a b. f a -> f b -> f (a,b)) ->
  (forall a b. f (b -> a) -> f b -> f a) -> 
  Nat Term f
foldTerm'' c p a (Const x) = c x
foldTerm'' c p a (Pair x y) = p (foldTerm'' c p a x) (foldTerm'' c p a y)
foldTerm'' c p a (App x y) = a (foldTerm'' c p a x) (foldTerm'' c p a y)

-- so that we can work on Symantics too:
tf1' :: Symantics repr => repr Bool -- inferred
tf1' = foldTerm dict_GADT tf1

buildTerm :: (forall f.
  (forall a. a -> f a) ->
  (forall a b. f a -> f b -> f (a,b)) ->
  (forall a b. f (b -> a) -> f b -> f a) -> 
    Nat c f) -> Nat c Term
buildTerm g = g const' pair' app'
 where
 SYMDict {dict_const = const', dict_pair = pair', dict_app = app'} = dict_fi

-- this is "the same as", but with Term and repr exchanged:
buildTerm' :: Symantics repr => (forall f. SYMDict f -> Nat Term f) -> Nat Term repr
buildTerm' t = t dict_GADT

-- but buildTerm' does NOT "build a term" at all!  It builds an evalIF
-- function.  But out of what?  Out of a function which builds an *evaluator*
-- out of a dictionary.  i.e. out of foldTerm...
eval2 :: Symantics repr => Nat Term repr
eval2 = buildTerm' foldTerm
x = eval2 ti1

eval3 :: Nat Term Term -- best we can do, but is really "Nat repr Term"
eval3 = buildTerm foldTerm''
y = eval3 tf1
