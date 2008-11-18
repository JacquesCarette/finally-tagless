(* name:          test_syntax.ml
 * synopsis:      test the syntax extension "pa_monad"
 * author:        Lydia E. van Dijk
 * last revision: Wed Oct 29 10:00:45 UTC 2008
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


(* module *)

module IdentityMonad:
sig
  val bind: int -> (int -> int) -> int
  val ret: int -> int
  val failwith: string -> int
end
  =
struct
  let bind a f = f a
  let ret a = a
  let failwith s = print_endline ("failed with " ^ s); 0
end

module NestedIdentityMonad = struct module M = IdentityMonad end


(* standalone top-level bindings *)

let bind a f = f a
let ret a = a
let fail s = print_endline ("failed with " ^ s); 0


(* tests *)

let test_id_int _ =
  Utest.expect_pass
    "identity (int)"
    (fun () -> 1 = perform ret 1)


let test_seq_int _ =
  Utest.expect_pass
    "sequence (int)"
    (fun () -> 1 = perform Pervasives.flush_all (); 1)


let test_binding_int _ =
  Utest.expect_pass
    "binding (int)"
    (fun () -> 1 = perform x <-- 1; x)


let test_let_int _ =
  Utest.expect_pass
    "let (int)"
    (fun () -> 1 = perform let x = 1 in x)


let test_binding_let_int _ =
  Utest.expect_pass
    "binding let (int)"
    (fun () -> 1 = perform let x = 1 in x <-- x; x)


let test_id_char _ =
  Utest.expect_pass
    "identity (char)"
    (fun () -> '1' = perform ret '1')


let test_seq_char _ =
  Utest.expect_pass
    "sequence (char)"
    (fun () -> '1' = perform Pervasives.flush_all (); '1')


let test_binding_char _ =
  Utest.expect_pass
    "binding (char)"
    (fun () -> '1' = perform x <-- '1'; x)


let test_let_char _ =
  Utest.expect_pass
    "let (char)"
    (fun () -> '1' = perform let x = '1' in x)


let test_id_string _ =
  Utest.expect_pass
    "identity (string)"
    (fun () -> "1" = perform ret "1")


let test_seq_string _ =
  Utest.expect_pass
    "sequence (string)"
    (fun () -> "1" = perform Pervasives.flush_all (); "1")


let test_binding_string _ =
  Utest.expect_pass
    "binding (string)"
    (fun () -> "1" = perform x <-- "1"; x)


let test_let_string _ =
  Utest.expect_pass
    "let (string)"
    (fun () -> "1" = perform let x = "1" in x)


let test_unit _ =
  Utest.expect_pass
    "unit"
    (fun () -> 1 = perform () <-- (); 1)


let test_extra_parenthesis _ =
  Utest.expect_pass
    "extra parenthesis"
    (fun () -> 1 = perform (((x))) <-- 1; x)


let test_wildcard _ =
  Utest.expect_pass
    "wildcard"
    (fun () -> 1 = perform _ <-- 99; 1)


let test_tuple _ =
  Utest.expect_pass
    "tuple"
    (fun () -> 6 = perform (x, y, z) <-- 1, 2, 3; x + y + z)


(* This one does not work (because we have no aliases yet):
 * let _ =
 *   perform
 *     ((x, y, z) as tuple) <-- 1, 2, 3;
 *     x + y + z (* - : int = 6 *) *)


let test_type_restriction _ =
  Utest.expect_pass
    "type restriction"
    (fun () -> 1 = perform (x: int) <-- 1; x)


let test_advanced_let _ =
  Utest.expect_pass
    "advanced let"
    (fun () ->
       120 = perform
               let module M = IdentityMonad in
               let rec fact n = if n < 1 then 1 else n * (fact (n - 1)) in
               M.ret (fact 5))


module Rec = struct type t = {x: int; y: int; z: int} end

let test_record _ =
  Utest.expect_pass
    "record"
    (fun () ->
       6 =
        perform
          {Rec.x = x'; y = y'; z = z'} <-- {Rec.x = 1; y = 2; z = 3};
          x' + y' + z')


(* "with" form *)

let test_binding_with_form_unqualified_name _ =
  Utest.expect_pass
    "binding (\"with\" form - unqualified name) (1)"
    (fun () -> 1 = perform with bind in x <-- 1; x)


let test_binding_with_form_unqualified_name' _ =
  Utest.expect_pass
    "binding (\"with\" form - unqualified name) (2)"
    (fun () -> perform with bind in x <-- 1; x = 1)


let test_binding_with_form_qualified_name _ =
  Utest.expect_pass
    "binding (\"with\" form - qualified name) (1)"
    (fun () -> 1 = perform with IdentityMonad.bind in x <-- 1; x)


let test_binding_with_form_qualified_name' _ =
  Utest.expect_pass
    "binding (\"with\" form - qualified name) (2)"
    (fun () -> (perform with IdentityMonad.bind in x <-- 1; x) = 1) (* parenthesis! *)


let test_binding_with_form_expression _ =
  Utest.expect_pass
    "binding (\"with\" form - general expression) (1)"
    (fun () -> 1 = perform with fun a f -> f a in x <-- 1; x)


let test_binding_with_form_expression' _ =
  Utest.expect_pass
    "binding (\"with\" form - general expression) (2)"
    (fun () -> perform with fun a f -> f a in x <-- 1; x = 1)


(* "with module" form *)

let test_binding_module_form _ =
  Utest.expect_pass
    "binding (\"with module\" form - module name) (1)"
    (fun () -> 1 = perform with module IdentityMonad in x <-- 1; x)


let test_binding_module_form' _ =
  Utest.expect_pass
    "binding (\"with module\" form - module name) (2)"
    (fun () -> (perform with module IdentityMonad in x <-- 1; x) = 1) (* parenthesis! *)


let test_binding_module_form_nested _ =
  Utest.expect_pass
    "binding (\"with module\" form - nested module name) (1)"
    (fun () -> 1 = perform with module NestedIdentityMonad.M in x <-- 1; x)


let test_binding_module_form_nested' _ =
  Utest.expect_pass
    "binding (\"with module\" form - nested module name) (2)"
    (fun () -> (perform with module NestedIdentityMonad.M in x <-- 1; x) = 1) (* parenthesis! *)

(* Refutable Patterns *)

let test_refutable_int _ =
  Utest.expect_pass
    "refutable (int)"
    (fun () -> 1 = perform 99 <-- 99; 1)


let test_refutable_int_extra_parenthesis _ =
  Utest.expect_pass
    "refutable (int) with extra parenthesis"
    (fun () -> 1 = perform (((99))) <-- 99; 1)


let test_refutable_tuple _ =
  Utest.expect_pass
    "refutable (tuple)"
    (fun () -> 6 = perform (x, 2, z) <-- 1, 2, 3; x + 2 + z)


let test_refutable_record _ =
  Utest.expect_pass
    "refutable (record)"
    (fun () ->
       6 =
        perform
          {Rec.x = x'; y = 2; z = z'} <-- {Rec.x = 1; y = 2; z = 3};
          x' + 2 + z')


(* This one does not work (because the preprocessor does not have the
 * information on the number of elements in an enumeration, only a
 * syntactic form):
 *     type single = Foo
 *     let _ = perform Foo <-- Foo; 1 *)


type multiple = Bar | Baz


let test_refutable_enumeration _ =
  Utest.expect_pass
    "refutable (enumeration)"
    (fun () -> 1 = perform Bar <-- Bar; 1)


(* Refutable Patterns and "with"-"and" form *)

let test_refutable_with_form_unqualified_names _ =
  Utest.expect_pass
    "refutable (\"with\" form - unqualified names)"
    (fun () -> 1 = perform with bind and fail in 99 <-- 99; 1)


let test_refutable_with_form_unqualified_names' _ =
  Utest.expect_pass
    "refutable (\"with\" form - unqualified names)"
    (fun () -> (perform with bind and fail in 99 <-- 99; 1) = 1) (* parenthesis! *)


let test_refutable_with_form_module _ =
  Utest.expect_pass
    "refutable (\"with\" form - module name)"
    (fun () -> 1 = perform with module IdentityMonad in 99 <-- 99; 1)


let test_refutable_with_form_module' _ =
  Utest.expect_pass
    "refutable (\"with\" form - module name)"
    (fun () -> (perform with module IdentityMonad in 99 <-- 99; 1) = 1) (* parenthesis! *)


let test_refutable_match_failure _ =
  Utest.expect_exception
    "refutable match failure"
    (Failure "pattern match")
    (fun () -> 0 = perform 100 <-- 99; 1)


(**********************************************************************)

let () =
  let results =
    Utest.run_tests
      Utest.PrintFailedTests
      [test_id_int;
       test_seq_int;
       test_binding_int;
       test_let_int;
       test_binding_let_int;
       test_id_char;
       test_seq_char;
       test_binding_char;
       test_let_char;
       test_id_string;
       test_seq_string;
       test_binding_string;
       test_let_string;
       test_unit;
       test_extra_parenthesis;
       test_wildcard;
       test_tuple;
       test_type_restriction;
       test_advanced_let;
       test_record;
       test_binding_with_form_unqualified_name;
       test_binding_with_form_unqualified_name';
       test_binding_with_form_qualified_name;
       test_binding_with_form_qualified_name';
       test_binding_with_form_expression;
       test_binding_with_form_expression';
       test_binding_module_form;
       test_binding_module_form';
       test_binding_module_form_nested;
       test_binding_module_form_nested';
       test_refutable_int;
       test_refutable_int_extra_parenthesis;
       test_refutable_tuple;
       test_refutable_record;
       test_refutable_enumeration;
       test_refutable_with_form_unqualified_names;
       test_refutable_with_form_unqualified_names';
       test_refutable_with_form_module;
       test_refutable_with_form_module';
       test_refutable_match_failure]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 ||
         results.Utest.unresolved <> 0
       then 1
       else 0)
