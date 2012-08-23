{-# LANGUAGE FlexibleContexts #-}
module GE where

import Data.HList
import StateCPS
import CodeRep

data UpdateKind = FractionFree | DivisionBased

mo_extend :: HExtend (LVPair l1 v) l l => l1 -> v -> StateCPS l () ()
mo_extend ip v =
  do s <- fetch
     store (ip .=. v .*. s)

mo_lookup :: HasField l r b => l -> StateCPS r w b
mo_lookup ip =
  do s <- fetch
     return $ s .!. ip

data Rank r = Rank (r Int)

decl = do rdecl <- retN (liftRef idx_zero)
          mo_extend Rank rdecl
          return rdecl
