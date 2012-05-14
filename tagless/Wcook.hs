{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}
module Wcook where
import Futamura1 (Symantics(..), compC, testpowfix7)

------------------------------------------------------------------------
-- Using associated types
------------------------------------------------------------------------

data family Static a :: (* -> *) -> *
newtype instance Static Int         repr = SInt   Int
newtype instance Static Bool        repr = SBool  Bool
newtype instance Static (a->b)      repr = SArrow (repr a -> repr b)
data    instance Static ()          repr
data    instance Static (a,b)       repr = SPair (Static a repr) (Static b repr)
                                         | SFst  (Static a repr)
                                         | SSnd  (Static b repr)
data    instance Static [a]          repr = SList [Static a repr]
data    instance Static (Either a b) repr = SLeft (Static a repr) | SRight (Static b repr)
   -- should be able to handle partially static too!

data P c a = P { dynamic :: c a, static :: Maybe (Static a (P c)) }

pdyn dynamic = P dynamic Nothing

instance (Symantics c) => Symantics (P c) where
    int  x = P (int  x) (Just (SInt  x))
    bool x = P (bool x) (Just (SBool x))

    add (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = int (n1 + n2)
    add (P n1 _) (P n2 _) = pdyn (add n1 n2)
    sub (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = int (n1 - n2)
    sub (P n1 _) (P n2 _) = pdyn (sub n1 n2)
    mul (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = int (n1 * n2)
    mul (P n1 _) (P n2 _) = pdyn (mul n1 n2)
    leq (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = bool (n1 <= n2)
    leq (P n1 _) (P n2 _) = pdyn (leq n1 n2)
    eql (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = bool (n1 == n2)
    eql (P n1 _) (P n2 _) = pdyn (eql n1 n2)
    if_ (P _ (Just (SBool b1))) et ee = if b1 then et else ee
    if_ (P be _) et ee = pdyn (if_ be (dynamic et) (dynamic ee))

    lam f = P (lam (dynamic . f . pdyn)) (Just (SArrow f))
    app (P _ (Just (SArrow f))) = f
    app (P f _) = pdyn . app f . dynamic
    fix f = case f (fix f) of
              (P _ (Just s)) -> P dfix (Just s)
              _              -> P dfix Nothing
       where dfix = fix (dynamic . f . pdyn)

    unit = P unit Nothing
    pair (P d1 Nothing  ) (P d2 Nothing  ) = P (pair d1 d2) Nothing
    pair (P d1 (Just s1)) (P d2 (Just s2)) = P (pair d1 d2) (Just (SPair s1 s2))
    pair (P d1 (Just s1)) (P d2 Nothing  ) = P (pair d1 d2) (Just (SFst s1))
    pair (P d1 Nothing  ) (P d2 (Just s2)) = P (pair d1 d2) (Just (SSnd s2))
    pfst (P d (Just (SPair s _))) = P (pfst d) (Just s)
    pfst (P d (Just (SFst  s  ))) = P (pfst d) (Just s)
    pfst (P d _                 ) = P (pfst d) Nothing
    psnd (P d (Just (SPair _ s))) = P (psnd d) (Just s)
    psnd (P d (Just (SSnd    s))) = P (psnd d) (Just s)
    psnd (P d _                 ) = P (psnd d) Nothing

    lcons (P da (Just a)) (P dl (Just (SList x)))    = P (lcons da dl) (Just (SList (a:x)))
    lcons (P a _) (P l _)                            = pdyn (lcons a l)
    left (P d (Just s)) = P (left d) (Just (SLeft s))
    left (P d _       ) = P (left d) Nothing
    right (P d (Just s)) = P (right d) (Just (SRight s))
    right (P d _       ) = P (right d) Nothing
    elimOr (P dl (Just l)) (P dr _) (P dm (Just (SLeft x))) = P (elimOr dl dr dm) (Just . l $ x)

-- Test repeated partial evaluation
test = compC (dynamic (dynamic (testpowfix7 ())))

-- Test pairs
testPair = compC (dynamic (app (lam (\p -> add (pfst p) (psnd p))) (pair (int 3) (int 4))))

------------------------------------------------------------------------
-- Using associated type synonyms
------------------------------------------------------------------------

data Void
data Or a b = Both a b | Fst a | Snd b
type Lst a = [a]

type family Static' a (repr :: (* -> *))
type instance Static' Int    repr = Int
type instance Static' Bool   repr = Bool
type instance Static' (a->b) repr = (repr a -> repr b)
type instance Static' ()     repr = Void
type instance Static' (a,b)  repr = Or (Static' a repr) (Static' b repr)
type instance Static' [a]    repr = Lst (Static' a repr) -- static list
type instance Static' (Either a b) repr = Either (Static' a repr) (Static' b repr)

data P' c a = P' { dynamic' :: c a, static' :: Maybe (Static' a (P' c)) }

pdyn' dynamic' = P' dynamic' Nothing

instance (Symantics c) => Symantics (P' c) where
    int  x = P' (int  x) (Just x)
    bool x = P' (bool x) (Just x)

    add (P' _ (Just n1)) (P' _ (Just n2)) = int (n1 + n2)
    add (P' n1 _) (P' n2 _) = pdyn' (add n1 n2)
    sub (P' _ (Just n1)) (P' _ (Just n2)) = int (n1 - n2)
    sub (P' n1 _) (P' n2 _) = pdyn' (sub n1 n2)
    mul (P' _ (Just n1)) (P' _ (Just n2)) = int (n1 * n2)
    mul (P' n1 _) (P' n2 _) = pdyn' (mul n1 n2)
    leq (P' _ (Just n1)) (P' _ (Just n2)) = bool (n1 <= n2)
    leq (P' n1 _) (P' n2 _) = pdyn' (leq n1 n2)
    eql (P' _ (Just n1)) (P' _ (Just n2)) = bool (n1 == n2)
    eql (P' n1 _) (P' n2 _) = pdyn' (eql n1 n2)
    if_ (P' _ (Just b1)) et ee = if b1 then et else ee
    if_ (P' be _) et ee = pdyn' (if_ be (dynamic' et) (dynamic' ee))

    lam f = P' (lam (dynamic' . f . pdyn')) (Just f)
    app (P' _ (Just f)) = f
    app (P' f _) = pdyn' . app f . dynamic'
    fix f = case f (fix f) of
              (P' _ (Just s)) -> P' dfix (Just s)
              _              -> P' dfix Nothing
       where dfix = fix (dynamic' . f . pdyn')

    unit = P' unit Nothing
    pair (P' d1 Nothing  ) (P' d2 Nothing  ) = P' (pair d1 d2) Nothing
    pair (P' d1 (Just s1)) (P' d2 (Just s2)) = P' (pair d1 d2) (Just (Both s1 s2))
    pair (P' d1 (Just s1)) (P' d2 Nothing  ) = P' (pair d1 d2) (Just (Fst s1))
    pair (P' d1 Nothing  ) (P' d2 (Just s2)) = P' (pair d1 d2) (Just (Snd s2))
    pfst (P' d (Just (Both s _))) = P' (pfst d) (Just s)
    pfst (P' d (Just (Fst  s  ))) = P' (pfst d) (Just s)
    pfst (P' d _                ) = P' (pfst d) Nothing
    psnd (P' d (Just (Both _ s))) = P' (psnd d) (Just s)
    psnd (P' d (Just (Snd    s))) = P' (psnd d) (Just s)
    psnd (P' d _                ) = P' (psnd d) Nothing

    lcons (P' d1 (Just a)) (P' d2 (Just l)) = P' (lcons d1 d2) (Just (a:l))
    lcons (P' d1 Nothing ) (P' d2 _)        = P' (lcons d1 d2) Nothing

    left (P' d1 (Just s)) = P' (left d1) (Just (Left s))
    right (P' d1 (Just s)) = P' (right d1) (Just (Right s))
-- Test repeated partial evaluation
test' = compC (dynamic' (dynamic' (testpowfix7 ())))

-- Test pairs
testPair' = compC (dynamic' (app (lam (\p -> add (pfst p) (psnd p))) (pair (int 3) (int 4))))
