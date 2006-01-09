(* name:          test-rec.ml
 * synopsis:      test recursive bindings of "pa_monad"
 * author:        Lydia E. Van Dijk
 * last revision: Sun Jan  8 08:22:09 UTC 2006
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


(* 2-pass solution *)
let replace_min_two_pass a_tree =
  let rec replace = function
      Leaf _x, min -> Leaf min
    | Branch (left, right), min -> Branch (replace (left, min), replace (right, min))
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
           lazy (Pervasives.min (Lazy.force_val left_min) (Lazy.force_val right_min)))
  in
  let rec x = lazy (rp_min (a_tree, m))
  and     t = lazy (fst (Lazy.force_val x))
  and     m = lazy (Lazy.force_val (snd (Lazy.force_val x)))
  in
    Lazy.force_val t


let bind a f = f a
let return a = a

let monadic_replace_min_one_pass a_tree =
  let rec rp_min = function
      Leaf x, min ->
        perform
          print_int (Lazy.force_val x);
          print_newline ();
          return (Leaf min, x)
    | Branch (left, right), min ->
        perform
          (left', left_min)   <-- rp_min (left, min);
          (right', right_min) <-- rp_min (right, min);
          return (Branch (left', right'),
                  lazy (Pervasives.min (Lazy.force_val left_min) (Lazy.force_val right_min)))
  in
    (*
    perform rec
      x <-- lazy (rp_min (a_tree, m));
      t <-- lazy (fst (Lazy.force_val x));
      m <-- lazy (Lazy.force_val (snd (Lazy.force_val x)));
      return Lazy.force_val t
      *)
    ()


(*
 * tree functions
 *)

let fold a_function an_initial_value a_tree =
  let rec traverse an_accumulator = function
      Leaf x -> a_function an_accumulator x
    | Branch (left, right) -> traverse (traverse an_accumulator left) right
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


let test_replace_min_two_pass _ =
  Utest.expect_pass
    "replace with minimum (2-pass)"
    (fun () ->
       let t =
         Branch (Branch (Leaf 24, Leaf 32),
                 Branch ((Leaf 3), Branch ((Leaf 14),
                                           Branch ((Leaf 32), (Leaf 8)))))
       in
         for_all (fun x -> x = 3) (replace_min_two_pass t))


let test_replace_min_one_pass _ =
  Utest.expect_pass
    "replace with minimum (1-pass)"
    (fun () ->
       let t =
         Branch (Branch (Leaf (lazy 24), Leaf (lazy 32)),
                 Branch ((Leaf (lazy 3)), Branch ((Leaf (lazy 14)),
                                                  Branch ((Leaf (lazy 32)), (Leaf (lazy 8))))))
       in
         for_all (fun x -> Lazy.force_val x = 3) (replace_min_one_pass t))


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
    (*
    perform rec
      x   <-- lazy (dev ([], 0, 0.0, avg) a_list_of_lazy_floats);
      lst <-- lazy (first_of_lazy_four x);
      n   <-- lazy (Lazy.force_val (second_of_lazy_four x));
      sum <-- lazy (Lazy.force_val (third_of_lazy_four x));
      avg <-- lazy (Lazy.force_val sum /. float_of_int (Lazy.force_val n));
      return Lazy.force_val lst
    *)
    ()


let test_deviation_one_pass _ =
  Utest.expect_pass
    "deviation (1-pass)"
    (fun () ->
       let t = [lazy 10.0; lazy 20.0; lazy 30.0]
       and result = [10.0; 0.0; -10.0] in
         List.for_all
           (fun (x, x') -> Lazy.force_val x = x')
           (List.combine (deviation t) result))


(**********************************************************************)

let (_: unit) =
  let results =
    Utest.run_tests
      ~verbose:true
      [test_ones;
       test_replace_min_two_pass;
       test_replace_min_one_pass;
       test_deviation_one_pass]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 || results.Utest.unresolved <> 0 then 1 else 0)
