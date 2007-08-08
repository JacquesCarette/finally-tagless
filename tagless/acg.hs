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

-- the above would make a nice blog post no?
-- maybe more: should show to Chris and Makoto other people why in here...
--
data LamT = Var String | AppL LamT LamT | AppR LamT LamT | Lam String LamT
  deriving Show

-- the next step is to have a typed LC and define symantics for types
--
instance Symantics LamT LamT where
    john = Var "John"
    mary = Var "Mary"
    saw subj obj = AppL subj (AppR obj (Var "saw"))
    everyone f = AppR (Var "everyone") (Lam "xx" (f (Var "xx")))
    someone f  = AppR (Var "someone")  (Lam "yy" (f (Var "yy")))

-- of course, need gensym ("everyone saw everyone")
-- write an interpreter from LamT to Bool...
{-
You're writing a compiler, right?
Btw, partial evaluation = using so-called meaning postulates (axioms
such as "if someone manages to do something, they do it") to simplify
the compilation output

-}

{-
*ACG> test1 :: Bool
True
*ACG> test1 :: String
"John saw Mary"
*ACG> test2 :: Bool
False
*ACG> test2 :: String
"someone saw everyone"
*ACG> test3 :: Bool
True
*ACG> test3 :: String
"someone saw everyone"
*ACG> (test2 :: Bool) == test3
False
*ACG> (test2 :: String) == test3
True
-}
