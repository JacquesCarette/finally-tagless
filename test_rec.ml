(* name:          test_rec.ml
 * synopsis:      test recursive bindings of "pa_monad"
 * author:        Lydia E. van Dijk
 * last revision: Wed Oct 29 09:59:12 UTC 2008
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


(* standalone top-level bindings *)

let bind a f = f a
let return a = a


(* tests *)

let test_ones _ =
  Utest.expect_pass
    "ones"
    (fun () ->
       let ones = perform rec xs <-- 1 :: xs; return xs in
         (* I would like to advise against testing
          * all elements of [ones].  ;-)  - lvd *)
         List.nth ones 0 = 1 && List.nth ones 1 = 1)


(* Richard Bird's famous "repMin" example *)

type 'a tree =
    Leaf of 'a
  | Branch of 'a tree * 'a tree


(* 2-pass solution *)
let replace_min_two_pass a_tree =
  let rec replace = function
      Leaf _x, min -> Leaf min
    | Branch (left, right), min ->
        Branch (replace (left, min), replace (right, min))
  and minval = function
      Leaf x -> x
    | Branch (left, right) -> Pervasives.min (minval left) (minval right)
  in
    replace (a_tree, minval a_tree)


(* 1-pass solution *)
let replace_min_one_pass a_tree =
  let rec rp_min = function
      Leaf x, min -> Leaf min, x
    | Branch (left, right), min ->
        let left', left_min   = rp_min (left, min)
        and right', right_min = rp_min (right, min)
        in
          (Branch (left', right'),
           lazy (Pervasives.min
                   (Lazy.force_val left_min)
                   (Lazy.force_val right_min)))
  in
  let rec x = lazy (rp_min (a_tree, m))
  and     t = lazy (fst (Lazy.force_val x))
  and     m = lazy (Lazy.force_val (snd (Lazy.force_val x)))
  in
    Lazy.force_val t


let monadic_replace_min_one_pass a_tree =
  let rec rp_min = function
      Leaf x, min ->
        perform
          (* In Haskell we would use the IOMonad here.  In OCaml
           * all the fuzz is just ridiculous.  - lvd
           *     print_int (Lazy.force_val x);
           *     print_newline (); *)
          return (Leaf min, x)
    | Branch (left, right), min ->
        perform
          (left', left_min)   <-- rp_min (left, min);
          (right', right_min) <-- rp_min (right, min);
          return (Branch (left', right'),
                  lazy (Pervasives.min
                          (Lazy.force_val left_min)
                          (Lazy.force_val right_min)))
  in
    perform
      rec x <-- lazy (rp_min (a_tree, m))
      and t <-- lazy (fst (Lazy.force_val x))
      and m <-- lazy (Lazy.force_val (snd (Lazy.force_val x)));
      return Lazy.force_val t


(*
 * tree functions
 *)

let fold a_function an_initial_value a_tree =
  let rec traverse an_accumulator = function
      Leaf x -> a_function an_accumulator x
    | Branch (left, right) ->
        traverse (traverse an_accumulator left) right
  in
    traverse an_initial_value a_tree


let for_all a_predicate_function a_tree =
  fold
    (fun a x -> a && a_predicate_function x)
    true
    a_tree


let string_of_tree a_tree =
  fold
    (fun a x -> a ^ "    " ^ string_of_int x)
    ""
    a_tree


type 'a replace_fixture = {
  min: 'a;
  tree: 'a tree;
  lazy_tree: 'a Lazy.t tree
}


let replace_setup () =
  {min = 3;
   tree =
      Branch (Branch (Leaf 24, Leaf 32),
              Branch ((Leaf 3), Branch ((Leaf 14),
                                        Branch ((Leaf 32), (Leaf 8)))));
   lazy_tree =
      Branch (Branch (Leaf (lazy 24), Leaf (lazy 32)),
              Branch ((Leaf (lazy 3)), Branch ((Leaf (lazy 14)),
                                               Branch ((Leaf (lazy 32)), (Leaf (lazy 8))))))}


let test_replace_min_two_pass a_fixture =
  Utest.expect_pass
    "replace with minimum (2-pass)"
    (fun () ->
       for_all
         (fun x -> x = a_fixture.min)
         (replace_min_two_pass a_fixture.tree))


let test_replace_min_one_pass a_fixture =
  Utest.expect_pass
    "replace with minimum (1-pass)"
    (fun () ->
       for_all
         (fun x -> Lazy.force_val x = a_fixture.min)
         (replace_min_one_pass a_fixture.lazy_tree))


let test_monadic_replace_min_one_pass a_fixture =
  Utest.expect_pass
    "replace with minimum (1-pass, monadic version)"
    (fun () ->
       for_all
         (fun x -> Lazy.force_val x = a_fixture.min)
         (monadic_replace_min_one_pass a_fixture.lazy_tree))


(**********************************************************************)

let first_of_lazy_four x = match Lazy.force_val x with x1, _, _, _ -> x1
let second_of_lazy_four x = match Lazy.force_val x with _, x2, _, _ -> x2
let third_of_lazy_four x = match Lazy.force_val x with _, _, x3, _ -> x3


let deviation a_list_of_lazy_floats =
  let rec dev (lst, n, sum, avg) = function
      [] -> lst, lazy n, lazy sum, avg
    | x :: xs ->
        let x' = Lazy.force_val x in
          dev
            (lazy (x' -. Lazy.force_val avg) :: lst,
             succ n,
             sum +. x',
             avg)
            xs in
  let rec x = lazy (dev ([], 0, 0.0, avg) a_list_of_lazy_floats)
  and   lst = lazy (first_of_lazy_four x)
  and     n = lazy (Lazy.force_val (second_of_lazy_four x))
  and   sum = lazy (Lazy.force_val (third_of_lazy_four x))
  and   avg = lazy (Lazy.force_val sum /. float_of_int (Lazy.force_val n))
  in
    Lazy.force_val lst


let monadic_deviation a_list_of_lazy_floats =
  let rec dev (lst, n, sum, avg) = function
      [] -> perform return (lst, lazy n, lazy sum, avg)
    | x :: xs ->
        perform
          x' <-- Lazy.force_val x;
          return (dev
                    (lazy (x' -. Lazy.force_val avg) :: lst,
                     succ n,
                     sum +. x',
                     avg)
                    xs)
  in
    perform
      rec   x <-- lazy (dev ([], 0, 0.0, avg) a_list_of_lazy_floats)
      and lst <-- lazy (first_of_lazy_four x)
      and   n <-- lazy (Lazy.force_val (second_of_lazy_four x))
      and sum <-- lazy (Lazy.force_val (third_of_lazy_four x))
      and avg <-- lazy (Lazy.force_val sum /. float_of_int (Lazy.force_val n));
      return Lazy.force_val lst


type deviation_fixture = {
  eager_list: float list;
  lazy_list: float Lazy.t list
}


let deviation_setup () =
  {eager_list = [10.0; 0.0; -10.0];
   lazy_list = [lazy 10.0; lazy 20.0; lazy 30.0]}


let test_deviation_one_pass a_fixture =
  Utest.expect_pass
    "deviation (1-pass)"
    (fun () ->
       List.for_all
         (fun (x, x') -> Lazy.force_val x = x')
         (List.combine (deviation a_fixture.lazy_list) a_fixture.eager_list))


let test_monadic_deviation_one_pass a_fixture =
  Utest.expect_pass
    "deviation (1-pass, monadic version)"
    (fun () ->
       List.for_all
         (fun (x, x') -> Lazy.force_val x = x')
         (List.combine (monadic_deviation a_fixture.lazy_list) a_fixture.eager_list))


(**********************************************************************)

let () =
  let results =
    Utest.run_tests
      Utest.PrintFailedTests
      [test_ones;
       Utest.eval_with_functional_fixture replace_setup test_replace_min_two_pass;
       Utest.eval_with_functional_fixture replace_setup test_replace_min_one_pass;
       Utest.eval_with_functional_fixture replace_setup test_monadic_replace_min_one_pass;
       Utest.eval_with_functional_fixture deviation_setup test_deviation_one_pass;
       Utest.eval_with_functional_fixture deviation_setup test_monadic_deviation_one_pass]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 ||
         results.Utest.unresolved <> 0
       then 1
       else 0)
