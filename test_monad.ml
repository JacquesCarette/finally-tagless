(* name:          test_monad.ml
 * synopsis:      simple test frame for monadic syntax sugar
 * author:        Lydia E. van Dijk
 * last revision: Wed Oct 29 09:57:45 UTC 2008
 * ocaml version: 3.11
 *
 * Copyright (C) 2006-2008  J. Carette, L. E. van Dijk, O. Kiselyov
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


let test_simple_expression _ =
  Utest.expect_pass
    "simple expression"
    (fun () ->
       let simple_expression () = perform true in
         simple_expression ())


let test_one_element_list _ =
  Utest.expect_pass
    "one-element list"
    (fun () ->
       let one_element_list () =
         let bind ma fmb = fun state ->
           let result, newstate = ma state in fmb result newstate
         and update newval = fun state -> state, newval
         and fetch = fun state -> state, state in
         let multiplier factor =
           perform
             result <-- fetch;
             update (factor * result)
         in
           multiplier 2 3 in
         one_element_list () = (3, 6))


let test_two_element_list _ =
  Utest.expect_pass
    "two-element list"
    (fun () ->
       let two_element_list () =
         let return x = [x]
         and bind xs f = List.concat (List.map f xs) in
           perform
             a <-- [1; 2; 3];
             b <-- [3; 4; 5];
             return (a + b)
       in
         two_element_list () = [4; 5; 6; 5; 6; 7; 6; 7; 8])


let test_gcd _ =
  Utest.expect_pass
    "gcd"
    (fun () ->
       let gcd a b =
         let get_x s = s, fst s
         and get_y s = s, snd s
         and set_x x' s = (x', snd s), ()
         and set_y y' s = (fst s, y'), ()
         and return a s = a, s
         and bind f0 f1 s0 = let s1, a = f0 s0 in f1 a s1
         and run x f = fst (f () x) in
         let (>>=) = bind in
         let rec monadic_gcd a_unit =
           perform
             x <-- get_x;
             y <-- get_y;
             if x = y then return x
             else if x < y then set_y (y - x) >>= monadic_gcd
             else set_x (x - y) >>= monadic_gcd
         in
           run (a, b) monadic_gcd
       in
         gcd 52_326 59_643 = 27)


(**********************************************************************)

let () =
  let results =
    Utest.run_tests
      Utest.PrintFailedTests
      [test_simple_expression;
       test_one_element_list;
       test_two_element_list;
       test_gcd]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 ||
         results.Utest.unresolved <> 0
       then 1
       else 0)
