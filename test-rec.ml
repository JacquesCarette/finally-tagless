(* name:          test-rec.ml
 * synopsis:      test recursive bindings of "pa_monad"
 * author:        Lydia E. Van Dijk
 * last revision: Wed Jan  4 09:02:45 UTC 2006
 * ocaml version: 3.09.0 *)


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
            all elements of [ones].  ;-)  - lvd *)
         List.nth ones 0 = 1 && List.nth ones 1 = 1)


(* Richard Bird's famous example
   See also: http://www.cse.ogi.edu/PacSoft/projects/rmb/repMin.html
 *)

type 'a tree =
    Leaf of 'a
  | Branch of 'a tree * 'a tree


let rec rp_min: 'a tree * 'a -> 'a tree * 'a = function
    Leaf x, min -> Leaf min, x
  | Branch (left, right), min ->
      let left', left_min = rp_min (left, min)
      and right', right_min = rp_min (right, min)
      in
        Branch (left', right'), Pervasives.min left_min right_min


(* http://groups.google.com/group/comp.lang.ml/browse_thread/thread/e1e2bbb9cb25591c/266106904b3d3708?lnk=st&q=recursive+value+fixpoint&rnum=6#266106904b3d3708 *)


let replace_min a_tree =
  (* (1) This is an almost literal transciption of the Haskell
         original.  It does not work, for OCaml is an eager language.
         {[
             let rec (t, m) = rp_min (a_tree, m)
         ]}
     (2) The next expression at least compiles
         {[
             let rec x: ('a tree * 'a) Lazy.t = lazy (rp_min (Lazy.force_val x))
         ]}
         but of course raises exception [Undefined], for [x] is
         undefined.  Note that it is not allowed to say
         {[
             let rec (t, m) = ...
         ]}
         as is explained in Sec. 7.3 of the Manual; only names are
         allowed on the left-hand side of a [let rec] form.
     (3) This one looks a bit more promising as one might believe that
         some variables could be initialized correctly.
         {[
             let rec t:  int tree Lazy.t    = lazy (fst (Lazy.force_val x))
             and m: int Lazy.t              = lazy (snd (Lazy.force_val x))
             and x: (int tree * int) Lazy.t = lazy (rp_min (Lazy.force_val t, Lazy.force_val m))
         ]}
         The variable [x] is nothing more than a container for [t] and
         [m].
  *)
  a_tree                                (* WRONG - but compiles *)


let fold a_function an_initial_value a_tree =
  let rec traverse an_accumulator = function
      Leaf x -> a_function an_accumulator x
    | Branch (left, right) -> traverse (traverse an_accumulator left) right
  in
    traverse an_initial_value a_tree


let string_of_tree a_tree =
  fold
    (fun a x -> a ^ "    " ^ string_of_int x)
    ""
    a_tree


let my_tree =
  Branch (Branch (Leaf 24, Leaf 32),
          Branch ((Leaf 3), Branch ((Leaf 14),
                                    Branch ((Leaf 32), (Leaf 8)))))


let (_: unit) =
  let min_tree = replace_min my_tree in
    print_endline "replace_min:";
    print_endline (string_of_tree my_tree);
    print_endline (string_of_tree min_tree)


let test_replace_min _ =
  Utest.expect_fail
    "replace with minimum"
    (fun () -> fold (fun a x -> a && x = 3) true (replace_min my_tree))


(**********************************************************************)

let (_: unit) =
  let results =
    Utest.run_tests
      ~verbose:true
      [test_ones;
       test_replace_min]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 || results.Utest.unresolved <> 0 then 1 else 0)
