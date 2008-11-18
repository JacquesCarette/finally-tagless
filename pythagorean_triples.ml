(* name:          pythagorean_triples.ml
 * synopsis:      Test non-determinism monad in the simplest possible implementation.
 * authors:       Jacques Carette and Oleg Kiselyov,
 * last revision: Wed Oct 29 09:44:35 UTC 2008
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


module Nondet:
sig
  type 'a stream =
      Nil
    | Cons of 'a * (unit -> 'a stream)
    | InC of (unit -> 'a stream)

  val ret: 'a -> unit -> 'a stream
  val mplus: (unit -> 'a stream) -> (unit -> 'a stream) -> unit -> 'a stream
  val bind: (unit -> 'a stream) -> ('a -> unit -> 'b stream) -> unit -> 'b stream
  val guard: bool -> unit -> unit stream
end
  =
struct
  type 'a stream =
      Nil
    | Cons of 'a * (unit -> 'a stream)
    | InC of (unit -> 'a stream)

  let mfail = fun () -> Nil

  let ret a = fun () -> Cons (a, mfail)

  (* actually, interleave: a fair disjunction with breadth-first search *)
  let rec mplus a b =
    fun () ->
      match a () with
          Nil -> InC b
        | InC a ->
            begin
              match b () with
		  Nil -> InC a
		| InC b -> InC (mplus a b)
		| Cons (b1, b2) -> Cons (b1, mplus a b2)
            end
        | Cons (a1, a2) -> Cons (a1, mplus b a2)

  (* a fair conjunction *)
  let rec bind m f =
    fun () ->
      match m () with
          Nil -> mfail ()
        | InC a -> InC (bind a f)
        | Cons (a, b) -> mplus (f a) (bind b f) ()

  let guard a_condition =
    if a_condition then ret () else mfail
end


let rec run n m =
  if n = 0 then []
  else
    match m () with
        Nondet.Nil -> []
      | Nondet.InC a -> run n a
      | Nondet.Cons (a, b) -> a :: run (pred n) b


let pythagorean_triples a_count =
  let rec number () =
    Nondet.InC
      begin
        Nondet.mplus
          (Nondet.ret 0)
          (perform with Nondet.bind in
             n <-- number;
             Nondet.ret (succ n))
      end in
  let test =
    perform with Nondet.bind in
      i <-- number;
      Nondet.guard (i > 0);
      j <-- number;
      Nondet.guard (j > 0);
      k <-- number;
      Nondet.guard (k > 0);
      (* Just to illustrate the `let' form within perform *)
      let predicate n = n * n = j * j + k * k in
        Nondet.guard (predicate i);
        Nondet.ret (i, j, k)
  in
    run a_count test


let test_pythagorean_triples _ =
  Utest.expect_pass
    "pythagorean triples"
    (fun () ->
       pythagorean_triples 10 =
        [( 5,  4, 3); ( 5, 3,  4);
         (10,  8, 6); (10, 6,  8);
         (13, 12, 5); (13, 5, 12);
         (15, 12, 9); (15, 9, 12);
         (17, 15, 8); (17, 8, 15)])


(**********************************************************************)

let () =
  let results = Utest.run_tests Utest.PrintFailedTests [test_pythagorean_triples]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 ||
         results.Utest.unresolved <> 0
       then 1
       else 0)
