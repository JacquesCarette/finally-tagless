module Iters where

import StateCPS
import CodeRep

row_iter b c low high get body d = loopM low high newbody d
  where
    newbody j = unS $ do bjc <- retN (get b j c)
                         body j bjc

col_iter b r low high get body d = loopM low high newbody d
  where
    newbody k = unS $ do brk <- return $ get b r k
                         body k brk       

