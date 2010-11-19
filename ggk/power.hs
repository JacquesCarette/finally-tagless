{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

-- (generic) power function.

module Power where

import qualified Staged as S
import qualified Algebra as A
import qualified Language.Haskell.TH as TH

myshow (S.Now b) = putStrLn $ show b
myshow (S.Later b) = TH.runQ (S.c b) >>= putStrLn . TH.pprint

-- Generic power function over any MultiplicativeMonoid where the carrier
-- type is 'Lift'able.

power :: (A.MultiplicativeMonoid (S.Staged r)) => 
    Int -> S.Staged r -> S.Staged r
power 0 _ = A.one
power n x | n>0 = A.mul x (power (n-1) x)


x = power 5 (S.Now (3::Int))
y = power 5  $ (S.Later $ S.lift_atom [| 3 |] :: S.Staged Int)
