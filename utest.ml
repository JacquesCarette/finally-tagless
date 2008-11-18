(* name:          utest.ml
 * synopsis:      simple unit-test framework
 * author:        Lydia E. van Dijk
 * last revision: Thu Nov 13 09:54:11 UTC 2008
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


type expected_test_outcome =
    ExpectPass
  | ExpectFail
  | ExpectException of exn


type test_outcome =
    Pass
  | Fail
  | UPass
  | XFail
  | Unresolved of string


type test =
    TestCase of string * expected_test_outcome * (unit -> bool)


type test_results = {
  total: int;
  passed: int;
  failed: int;
  upassed: int;
  xfailed: int;
  unresolved: int
}


(* Creation of Testcases *)

let testcase a_test_title an_expected_outcome a_test_function =
  TestCase (a_test_title, an_expected_outcome, a_test_function)


let expect_pass a_test_title a_test_function =
  testcase a_test_title ExpectPass a_test_function


let expect_fail a_test_title a_test_function =
  testcase a_test_title ExpectFail a_test_function


let expect_exception a_test_title an_exception a_test_function =
  testcase a_test_title (ExpectException an_exception) a_test_function


(* Running of Tests *)

exception InconsistentFixture


let eval_with_imperative_fixture a_setup_function a_test_function a_teardown_function () =
  let fixture = a_setup_function () in
  let result = a_test_function fixture in
    a_teardown_function fixture;
    result


let eval_with_functional_fixture a_setup_function a_test_function () =
  a_test_function (a_setup_function ())


let run_single_testcase an_expected_outcome a_test_function a_fixture =
  let does_raise_exception f =
    try f a_fixture; None with x -> Some x
  in
    match an_expected_outcome with
        ExpectPass ->
          begin
            try if a_test_function a_fixture then Pass else Fail
            with any_exception -> Unresolved (Printexc.to_string any_exception)
          end
      | ExpectFail ->
          begin
            try if a_test_function a_fixture then UPass else XFail
            with any_exception -> Unresolved (Printexc.to_string any_exception)
          end
      | ExpectException x ->
          begin
            match does_raise_exception a_test_function with
                None -> Fail
              | Some x' ->
                  if Printexc.to_string x = Printexc.to_string x'
                  then Pass
                  else Fail
          end


type verbosity =
    PrintNothing
  | PrintFailedTests
  | PrintTestTotals
  | PrintAllTests


let (<=~) a_level another_level =
  let int_of_verbosity = function
      PrintNothing -> 0
    | PrintFailedTests -> 1
    | PrintTestTotals -> 2
    | PrintAllTests -> 3
  in
    int_of_verbosity a_level <= int_of_verbosity another_level


let run_tests a_verbosity_level a_list_of_tests =
  let results =
    List.fold_left
      (fun a x ->
         let TestCase (title, expect, test) = x () in
           match run_single_testcase expect test () with
               Pass ->
                 if PrintAllTests <=~ a_verbosity_level then
                   print_endline ("PASS: " ^ title);
                 {a with total = succ a.total; passed = succ a.passed}
             | Fail ->
                 if PrintFailedTests <=~ a_verbosity_level then
                   print_endline ("FAIL: " ^ title);
                 {a with total = succ a.total; failed = succ a.failed}
             | UPass ->
                 if PrintAllTests <=~ a_verbosity_level then
                   print_endline ("UPASS: " ^ title);
                 {a with total = succ a.total; upassed = succ a.upassed}
             | XFail ->
                 if PrintAllTests <=~ a_verbosity_level then
                   print_endline ("XFAIL: " ^ title);
                 {a with total = succ a.total; xfailed = succ a.xfailed}
             | Unresolved s ->
                 if PrintFailedTests <=~ a_verbosity_level then
                   print_endline ("UNRESOLVED: " ^ title ^ " (exception: " ^ s ^ ")");
                 {a with total = succ a.total; unresolved = succ a.unresolved})
      {total = 0; passed = 0; failed = 0; upassed = 0; xfailed = 0; unresolved = 0}
      a_list_of_tests
  in
    if PrintTestTotals <=~ a_verbosity_level then
      print_string
        (List.fold_left
           (fun a (category, count) ->
              a ^ "# of " ^ category ^ "   " ^ string_of_int count ^ "\n")
           ""
           ["testcases attempted ", results.total;
            "expected passes     ", results.passed;
            "expected failures   ", results.xfailed;
            "unexpected passes   ", results.upassed;
            "unexpected failures ", results.failed;
            "unresolved testcases", results.unresolved]);
    results
