{-# LANGUAGE TypeFamilies #-}
module Family where
import Incope (Symantics(..), compC, testpowfix7)

data family Static a :: (* -> *) -> *
newtype instance Static Int    repr = SInt   Int
newtype instance Static Bool   repr = SBool  Bool
newtype instance Static (a->b) repr = SArrow (repr a -> repr b)

data P c a = P { dynamic :: c a, static :: Maybe (Static a (P c)) }

pdyn dynamic = P dynamic Nothing

instance (Symantics c) => Symantics (P c) where
    int  x = P (int  x) (Just (SInt  x))
    bool x = P (bool x) (Just (SBool x))

    add (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = int (n1 + n2)
    add (P n1 _) (P n2 _) = pdyn (add n1 n2)
    mul (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = int (n1 * n2)
    mul (P n1 _) (P n2 _) = pdyn (mul n1 n2)
    leq (P _ (Just (SInt n1))) (P _ (Just (SInt n2))) = bool (n1 <= n2)
    leq (P n1 _) (P n2 _) = pdyn (leq n1 n2)
    if_ (P _ (Just (SBool b1))) et ee = if b1 then et else ee
    if_ (P be _) et ee = pdyn (if_ be (dynamic et) (dynamic ee))

    lam f = P (lam (dynamic . f . pdyn)) (Just (SArrow f))
    app (P _ (Just (SArrow f))) = f
    app (P f _) = pdyn . app f . dynamic
    fix f = case f (fix f) of
              (P _ (Just s)) -> P dfix (Just s)
              _              -> P dfix Nothing
       where dfix = fix (dynamic . f . pdyn)

-- Test repeated partial evaluation
test = compC (dynamic (dynamic (testpowfix7 ())))
