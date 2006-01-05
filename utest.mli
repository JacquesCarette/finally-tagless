(* name:          utest.mli
 * synopsis:      simple unit-test framework
 * author:        Lydia E. van Dijk
 * last revision: Sat Dec 31 16:11:53 UTC 2005
 * ocaml version: 3.09.0 *)


type expected_test_outcome =
    ExpectPass
  | ExpectFail
  | ExpectException of exn


type test_outcome =
    Pass
  | Fail
  | UPass
  | XFail
  | Unresolved


type test_results = {
  total: int;
  passed: int;
  failed: int;
  upassed: int;
  xfailed: int;
  unresolved: int
}


val testcase: string -> expected_test_outcome -> (unit -> bool) -> string * test_outcome
val expect_pass: string -> (unit -> bool) -> string * test_outcome
val expect_fail: string -> (unit -> bool) -> string * test_outcome
val expect_exception: string -> exn -> (unit -> bool) -> string * test_outcome
val run_tests: verbose:bool -> (unit -> string * test_outcome) list -> test_results
