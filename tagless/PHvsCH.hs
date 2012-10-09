-- Language implementation showdown:
-- PHOAS vs. Coquand-Huet
-- Philip Wadler, 9 Oct 2012

{-# LANGUAGE Rank2Types, GADTs #-}

module PHvsCH where

-- DeBruijn: ExpDB

type Id = String

data ExpDB where
  VarDB :: Int -> ExpDB
  LamDB :: Id -> ExpDB -> ExpDB
  AppDB :: ExpDB -> ExpDB -> ExpDB
  deriving (Eq,Show)

-- PHOAS: ExpPH

data ExPH x a where
  VarPH :: x a -> ExPH x a
  LamPH :: Id -> (x a -> ExPH x b) -> ExPH x (a -> b)
  AppPH :: ExPH x (a -> b) -> ExPH x a -> ExPH x b

newtype ExpPH a = Hide { reveal :: forall x. ExPH x a }

-- Coquand-Huet: ExpCH

class ExCH exp where
  lamCH :: Id -> (exp a -> exp b) -> exp (a -> b)
  appCH :: exp (a -> b) -> exp a -> exp b

newtype ExpCH a = ExpCH { expCH :: forall exp. ExCH exp => exp a }

-- evalPH

newtype I a = I { unI :: a }

evalPH :: ExpPH a -> a
evalPH (Hide e)  =  h e
  where
  h :: ExPH I a -> a
  h (VarPH x)    =  unI x
  h (LamPH _ f)  =  h . f . I
  h (AppPH a b)  =  h a (h b)

-- evalCH

newtype Ev a = Ev { ev :: a }

instance ExCH Ev where
  lamCH _ f  =  Ev (ev . f . Ev)
  appCH d e  =  Ev (ev d (ev e))

evalCH :: ExpCH a -> a
evalCH (ExpCH e)  =  ev e

-- fromPHtoDB

newtype K a b = K { unK :: a }

fromPHtoDB :: ExpPH a -> ExpDB
fromPHtoDB (Hide e)  =  h 0 e
  where
  h :: Int -> ExPH (K Int) a -> ExpDB
  h w (VarPH v)    =  VarDB (w - unK v - 1)
  h w (LamPH u f)  =  LamDB u (h (w+1) (f (K w)))
  h w (AppPH a b)  =  AppDB (h w a) (h w b)

-- fromCHtoDB

newtype ToDB a = ToDB { toDB :: Int -> ExpDB }

instance ExCH ToDB where
  lamCH u f  =  ToDB (\w -> LamDB u (toDB (f (ToDB (\v -> VarDB (v-w-1)))) (w+1)))
  appCH d e  =  ToDB (\w -> AppDB (toDB d w) (toDB e w))

fromCHtoDB :: ExpCH a -> ExpDB
fromCHtoDB (ExpCH e)  =  toDB e 0
  
-- fromCHtoPH

instance ExCH (ExPH x) where
  lamCH u f  =  LamPH u (f . VarPH)
  appCH d e  =  AppPH d e

fromCHtoPH :: ExpCH a -> ExpPH a
fromCHtoPH (ExpCH e)  =  Hide e

-- fromPHtoCH

fromPHtoCH :: ExpPH a -> ExpCH a
fromPHtoCH (Hide e) = ExpCH (h e)
  where
  h :: ExCH exp => ExPH exp a -> exp a
  h (VarPH x)    =  x
  h (LamPH u f)  =  lamCH u (h . f)
  h (AppPH a b)  =  appCH (h a) (h b)

-- test data, PH

fourPH :: ExpPH ((a -> a) -> a -> a)
fourPH =
  let
    zeroPH = LamPH "f" (\f -> LamPH "x" (\x -> VarPH x))
    succPH = LamPH "n" (\n -> LamPH "f" (\f -> LamPH "x" (\x ->
             VarPH f `AppPH` VarPH x)))
    plusPH = LamPH "m" (\m -> LamPH "n" (\n -> LamPH "f" (\f -> LamPH "x" (\x ->
             VarPH m `AppPH` VarPH f `AppPH`
               (VarPH n `AppPH` VarPH f `AppPH` VarPH x)))))
    onePH  = succPH `AppPH` zeroPH
    twoPH  = plusPH `AppPH` onePH `AppPH` onePH
    fourPH = plusPH `AppPH` twoPH `AppPH` twoPH
  in
    Hide fourPH

-- test data, CH

fourCH :: ExpCH ((a -> a) -> a -> a)
fourCH =
  ExpCH (
    let
      zeroCH = lamCH "f" (\f -> lamCH "x" (\x -> x))
      succCH = lamCH "n" (\n -> lamCH "f" (\f -> lamCH "x" (\x ->
               f `appCH` x)))
      plusCH = lamCH "m" (\m -> lamCH "n" (\n -> lamCH "f" (\f -> lamCH "x" (\x ->
               m `appCH` f `appCH` (n `appCH` f `appCH` x)))))
      oneCH  = succCH `appCH` zeroCH
      twoCH  = plusCH `appCH` oneCH `appCH` oneCH
      fourCH = plusCH `appCH` twoCH `appCH` twoCH
    in
      fourCH)

-- test

main :: Bool
main = evalPH fourPH (+1) 0 == 4 &&
       evalCH fourCH (+1) 0 == 4 &&
       fromPHtoDB fourPH == fromPHtoDB (fromCHtoPH fourCH) &&
       fromCHtoDB fourCH == fromCHtoDB (fromPHtoCH fourPH)
