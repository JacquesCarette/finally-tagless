(* name:          test-monad.ml
 * synopsis:      simple test frame for monadic syntax sugar
 * author:        Lydia E. van Dijk
 * last revision: Sun Dec 11 08:49:21 UTC 2005
 * ocaml version: 3.09.0 *)


let test_simple_expression () = perform true

let test_one_element_list () =
  let bind ma fmb = fun state -> let result, newstate = ma state in fmb result newstate
  and update newval = fun state -> state, newval
  and fetch = fun state -> state, state in
    let multiplier factor =
      perform
        result <-- fetch;
        update (factor * result)
    in
      multiplier 2 3

let test_two_element_list () =
  let return x = [x]
  and bind xs f = List.concat (List.map f xs) in
    perform
      a <-- [1; 2; 3];
      b <-- [3; 4; 5];
      return (a + b)


let test_with_gcd a b =
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



(* main *)

let all_tests () =
  test_simple_expression () &&
  test_one_element_list () = (3, 6) &&
  test_two_element_list () = [4; 5; 6; 5; 6; 7; 6; 7; 8] &&
  test_with_gcd 52326 59643 = 27


let (_: unit) =
  match all_tests () with
      true -> exit 0
    | false -> exit 1

