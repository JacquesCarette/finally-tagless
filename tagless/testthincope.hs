{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -W #-}

-- Test for TH version of incope
--   Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan

module Testthincope where

import Thincope

-- For the interpreter
itest1 = mkitest test1
itest2 = mkitest test2
itest3 = mkitest test3

itestgib  = mkitest testgib
itestgib1 = mkitest testgib1
itestgib2 = mkitest testgib2

itestpw   = mkitest testpowfix
itestpw7  = mkitest testpowfix7
itestpw72 = mkitest (\() -> app (testpowfix7 ()) (int 2))

-- For the L length interpreter
ltest1 = compL . test1 $ ()
ltest2 = compL . test2 $ ()
ltest3 = compL . test3 $ ()


ltestgib  = compL . testgib  $ ()
ltestgib1 = compL . testgib1 $ () -- 23
ltestgib2 = compL . testgib2 $ ()

ltestpw   = compL . testpowfix $ ()
ltestpw7  = compL . testpowfix7 $ ()
ltestpw72 = compL (app (testpowfix7 ()) (int 2)) -- 17

-- For the TH compiler
ctest1 = $(compC . test1 $ ())
ctest2 = $(compC . test2 $ ())
ctest3 = $(compC . test3 $ ())

ctestgib  = $(compC . testgib $ ())
ctestgib1 = $(compC . testgib1 $ ())
ctestgib2 = $(compC . testgib2 $ ())

ctestpw   = $(compC . testpowfix $ () )
ctestpw7  = $(compC . testpowfix7 $ () )
ctestpw72 = $(compC  (app (testpowfix7 ()) (int 2)))

{- the 'other' pe tests
ptest1 = compP . test1 $ ()
ptest2 = compP . test2 $ ()
ptest3 = compP . test3 $ ()

-- Compare the output with ctestgib1. The partial evaluation should be
-- evident!
ptestgib  = compP . testgib  $ ()
ptestgib1 = compP . testgib1 $ ()
ptestgib2 = compP . testgib2 $ ()


ptestpw   = compP . testpowfix $ ()
ptestpw7  = compP . testpowfix7 $ ()
ptestpw72 = compP  (app (testpowfix7 ()) (int 2))
-}

-- unit to suppress the monomorphism restriction

ztestgib () = zlam (\x -> zlam (\y ->
                zfix (\self -> zlam (\n ->
                    zif_ (zleq n (zint 0)) x
                      (zif_ (zleq n (zint 1)) y
                       (zadd (zapp self (zadd n (zint (-1))))
                             (zapp self (zadd n (zint (-2))))))))))

ztestgib5 () = zlam (\x -> zlam (\y ->
	zapp (zapp (zapp (ztestgib ()) x) y) (zint 5)))

ztestgib1 () = zapp (zapp (zapp (ztestgib ()) (zint 1)) (zint 1)) (zint 5)

-- The result of PE can be interpreted is several ways, as usual

testR_ztestgib1 = compR (dynamic (ztestgib1 ())) -- 8
testL_ztestgib1 = compL (dynamic (ztestgib1 ())) -- 1
testC_ztestgib1 = compC (dynamic (ztestgib1 ())) -- 8

testR_ztestgib5 = compR (dynamic (ztestgib5 ())) 1 1 -- 8
testL_ztestgib5 = compL (dynamic (ztestgib5 ())) -- 9
testC_ztestgib5 = compC (dynamic (ztestgib5 ()))
-- (\V0 -> (\V1 -> ((((V1 + V0) + V1) + (V1 + V0)) + ((V1 + V0) + V1))))

testR_zztestgib1 = compR $ unS12 $ ztestgib1 () -- 8
testL_zztestgib1 = compL $ unS12 $ ztestgib1 () -- 23
testC_zztestgib1 = compC $ unS12 $ ztestgib1 () -- big code


ztestpowfix () = zlam (\x ->
                      zfix (\self -> zlam (\n ->
                        zif_ (zleq n (zint 0)) (zint 1)
                            (zmul x (zapp self (zadd n (zint (-1))))))))
ztestpowfix7 () = zlam (\x -> zapp (zapp (ztestpowfix ()) x) (zint 7))

-- We can interpret ztespowerfix directly, using R, L or C
testR_zztestpw7 = (compR $ unS12 $ ztestpowfix7 ()) 2 -- 128
testL_zztestpw7 = compL $ unS12 $ ztestpowfix7 () -- 15
testC_zztestpw7 = compC $ unS12 $ ztestpowfix7 ()
{-
  (\V0 -> (((\V1 -> 
    (fix\V2 (\V3 -> (if (V3 <= 0) then 1 else (V1 * (V2 (V3 + -1))))))) V0) 7))
-}

-- Or we can partially evaluate ztestpowfix7, and then interpret,
-- using various interpreters
testR_ztestpw7 = (compR $ dynamic $ ztestpowfix7 ()) 2 -- 128
testL_ztestpw7 = compL $ dynamic $ ztestpowfix7 () -- 9
testC_ztestpw7 = compC $ dynamic $ ztestpowfix7 ()
-- (\V0 -> (V0 * (V0 * (V0 * (V0 * (V0 * (V0 * (V0 * 1))))))))
