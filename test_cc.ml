(* name:          test_cc.ml
 * synopsis:      test the delimited continuation monad
 * author:        Oleg Kiselyov
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


open Cc


let abortP p e = shiftP p (fun _ -> e)


let test1 _ =
  Utest.expect_pass
  "return and bind"
  (fun () -> 5 = run (perform x <-- return 1; return (x + 4)))


let test2 _ =
  Utest.expect_pass
  "prompt, return and bind"
  (fun () -> 9 = run (perform
      p <-- new_prompt ();
      x <-- pushP p (pushP p (return 5));
                         return (x + 4)))

let test3 _ =
  Utest.expect_pass
  "abort (1)"
  (fun () -> 9 = run (perform
     p <-- new_prompt ();
     x <-- pushP p (perform
                      y <-- abortP p (return 5); return (y + 6));
     return (x + 4)))


let test3' _ =
  Utest.expect_pass
  "abort (2)"
  (fun () -> 9 = run (perform
     p <-- new_prompt ();
     x <-- pushP p (pushP p (perform
                      y <-- abortP p (return 5); return (y + 6)));
     return (x + 4)))


let test3'' _ =
  Utest.expect_pass
  "abort (3)"
  (fun () -> 27 = run (perform
     p <-- new_prompt ();
     let c = pushP p (perform
                        y <-- abortP p (return 5); return (y + 6)) in
     x <-- pushP p (perform
                      v1 <-- c;
                      v2 <-- abortP p (return 7);
                      return (v2 + 10));
     return (x + 20)))


let test3''' _ =
  Utest.expect_exception
  "abort (4)"
  (Failure "no prompt set")
  (fun () -> -1 = run (perform
     p <-- new_prompt ();
     let c = pushP p (perform
                        y <-- abortP p (return 5); return (y + 6)) in
     x <-- pushP p (perform
                      v1 <-- c;
                      v2 <-- abortP p (return 7);
                      return (v2 + 10));
     x <-- abortP p (return 9);
     return (x + 20)))


let test5 _ =
  Utest.expect_pass
  "The Standard shift test"
  (fun () -> 117 = run (perform
     p0 <-- new_prompt ();
     p1 <-- new_prompt ();
     x <-- pushP p0 (perform
                      v1 <-- shiftP p0 (fun sk ->
                                 perform v3 <-- sk (sk (return 3));
                                         return (v3 + 100));
                      return (v1 + 2));
     return (x + 10)))


let test5' _ =
  Utest.expect_pass
  "The Standard shift test (2)"
  (fun () -> 115 = run (perform
     p0 <-- new_prompt ();
     p1 <-- new_prompt ();
     x <-- pushP p0 (perform
                      v1 <-- shiftP p0 (fun sk ->
                                 perform v3 <-- (sk (return 3));
                                         return (v3 + 100));
                      return (v1 + 2));
     return (x + 10)))


let test5'' _ =
  Utest.expect_pass
  "The Standard shift test (3)"
  (fun () -> 115 = run (perform
     p0 <-- new_prompt ();
     p1 <-- new_prompt ();
     x <-- pushP p0 (perform
                      v1 <-- shiftP p0 (fun sk ->
                                 perform v3 <-- sk
                                     (pushP p1 (sk (abortP p1 (return 3))));
                                         return (v3 + 100));
                      return (v1 + 2));
     return (x + 10)))


let test6 _ =
  Utest.expect_pass
  "Multiple prompts"
  (fun () -> 15 = run (perform
     p1 <-- new_prompt ();
     p2 <-- new_prompt ();
     let pushtwice sk = sk (sk (return 3)) in
     x <-- pushP p1 (pushP p2  (perform
                      v1 <-- shiftP p1 pushtwice;
                      return (v1 + 1)));
     return (x + 10)))


let test7 _ =
  Utest.expect_pass
  "The most stringent test: capturing prompts in subcontinuations"
  (fun () -> 135 = run (perform
     p1 <-- new_prompt ();
     p2 <-- new_prompt ();
     p3 <-- new_prompt ();
     let pushtwice sk = sk (sk (shiftP p2 (fun sk2 -> sk2 (sk2 (return 3)))))
     in
     x <-- pushP p1 (perform
             v1 <-- pushP p2 (perform
                                v2 <-- pushP p3 (shiftP p1 pushtwice);
                                return (v2 + 10));
             return (v1 + 1));
     return (x + 100)))


let testls _ =
  Utest.expect_pass
  "Ken's `ls' test"
  (fun () -> ["a"] = run (perform
     p <-- new_prompt ();
     pushP p (perform
       xv <-- shiftP p (fun f -> perform
                                    t <-- f (return []);
                                    return ("a"::t));
       shiftP p (fun _ -> return xv))))


(**********************************************************************)

let all_tests =
  [
   test1;
   test2;
   test3;
   test3';
   test3'';
   test3''';
   test5;
   test5';
   test5'';
   test6;
   test7;
   testls;
  ]


let () =
  let results =
    Utest.run_tests Utest.PrintFailedTests all_tests
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 ||
         results.Utest.unresolved <> 0
       then 1
       else 0)
