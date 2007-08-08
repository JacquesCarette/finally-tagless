{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}

module ACG where

class Symantics e t | e -> t, t -> e where
    john :: e
    mary :: e
    everyone :: (e -> t) -> t
    someone :: (e -> t) -> t
    saw :: e -> e -> t

instance Symantics String String where
    john = "John"
    mary = "Mary"
    everyone f = f "everyone"
    someone f = f "someone"
    saw obj subj = subj ++ " saw " ++ obj

data Thing = John | Mary

instance Symantics Thing Bool where
    john = John
    mary = Mary
    everyone f = f John && f Mary
    someone f = f John || f Mary
    saw Mary John = True
    saw John Mary = True
    saw _ _ = False

test1 = saw mary john
test2 = someone (\x -> everyone (\y -> saw y x))
test3 = everyone (\y -> someone (\x -> saw y x))
