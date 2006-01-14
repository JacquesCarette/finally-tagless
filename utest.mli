(* name:          utest.mli
 * synopsis:      simple unit-test framework
 * author:        Lydia E. van Dijk
 * last revision: Fri Jan 13 10:13:01 UTC 2006
 * ocaml version: 3.09.0 *)


(** {3 Simple Unit-Test Framework}

    [Utest] supplies a simpe framwork for performing unit-tests in an
    Xtreme Programming style.  It has been influenced by Greg (Gnu
    REGression testing) and Maas-Maarten Zeeman's oUnit. *)


(** The outcome of a test case as the writer of the test suite expects
    it to be. *)
type expected_test_outcome =
    ExpectPass                          (** We expect to pass the test. *)
  | ExpectFail                          (** We expect to fail the test. *)
  | ExpectException of exn              (** We expect that the given exception will be raised. *)


(** The actual outcome of a test case after it has been run. *)
type test_outcome =
    Pass                                (** We expected passing and we passsed. *)
  | Fail                                (** We expected passing but we failed. *)
  | UPass                               (** We expected failing but we did succeed. *)
  | XFail                               (** We expected failed and we failed. *)
  | Unresolved                          (** An unexpected exception occurred. *)


(** A test itself. *)
type test =
    TestCase of string * expected_test_outcome * (unit -> bool)


(** Results of some tests. *)
type test_results = {
  total: int;                           (** Total number of test cases attempted *)
  passed: int;                          (** Number of passed tessts *)
  failed: int;                          (** Number of failed tessts *)
  upassed: int;                         (** Number of unexpectedly passed tests *)
  xfailed: int;                         (** Number of expectedly failed tests *)
  unresolved: int                       (** Number of unresolved tests *)
}


(** [testcase a_test_title an_expected_outcome a_test_function]

    Create a single testcase of [a_test_function] with [a_test_title]
    and [an_expected_outcome].

    Note that this is a "low-level" function and the two convenience
    functions {!Utest.expect_pass} and {!Utest.expect_fail} allow for
    a terser definition of a test. *)
val testcase: string -> expected_test_outcome -> (unit -> bool) -> test


(** [expect_pass a_test_title a_test_function]

    Create a testcase of [a_test_function] with [a_test_title] that is
    expected to succeed. *)
val expect_pass: string -> (unit -> bool) -> test


(** [expect_fail a_test_title a_test_function]

    Create a testcase of [a_test_function] with [a_test_title] that is
    expected to fail. *)
val expect_fail: string -> (unit -> bool) -> test


(** [expect_exception a_test_title an_exception a_test_function]

    Create a testcase of [a_test_function] with [a_test_title] that is
    expected to raise [an_exception]. *)
val expect_exception: string -> exn -> (unit -> bool) -> test


(** This exception is for the cenvenience of the user.  Raise it if
    the test data itself is inconsistent. *)
exception InconsistentFixture


(** [eval_with_imperative_fixture a_setup_function a_test_function a_teardown_function]

    Evaluate [a_test_function] by passing the result of
    [a_setup_function].  After [a_test_function] completes, pass the
    result of [a_setup_function] to [a_teardown_function].

    This is for example useful of [a_test_function] need the handles
    of some open files.  In this case [a_setup_function] would open
    the files and pass the handle (the fixture).
    [a_teardown_function] closes the files after [a_test_function]
    completes.*)
val eval_with_imperative_fixture: (unit -> 'fix) -> ('fix -> test) -> ('fix -> unit) -> unit -> test


(** [eval_with_functional_fixture a_setup_function a_test_function]

    Evaluate [a_test_function] by passing the result of
    [a_setup_function].

    This is for example useful if several test functions should be
    tested with the same data set. *)
val eval_with_functional_fixture: (unit -> 'fix) -> ('fix -> test) -> unit -> test


(** [run_tests ~verbose a_list_of_tests]

    Run all tests in [a_list_of_tests].  The [verbose] flag controls
    whetherthe function prints each test result or just the totals. *)
val run_tests: verbose:bool -> (unit -> test) list -> test_results
