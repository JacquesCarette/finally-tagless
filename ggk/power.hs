
-- (generic) power function.

module Power where

import qualified Staged as S
import qualified Algebra as A

-- Generic power function over any MultiplicativeMonoid where the carrier
-- type is 'Lift'able.

power :: A.LMM r => Int -> S.Staged r -> S.Staged r
power 0 _ = A.one
power n x | n>0 = A.mul x (power (n-1) x)


x = power 5 (S.Now (3::Int))
