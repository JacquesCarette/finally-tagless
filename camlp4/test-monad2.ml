(* name:          test-monad.ml
 * synopsis:      simple tests for monadic syntax sugar
 * based on work of:        Lydia E. van Dijk
 * ocaml version: 3.08.0 *)


let test_simple_expression () =
    let ret x = x in
  mdo { mret true }


let test_one_element_list () =
  let ret x = x
  and bind ma fmb = fun state -> let result, newstate = ma state in fmb result newstate
  and update newval = fun state -> state, newval
  and fetch = fun state -> state, state in
    let multiplier factor =
      mdo { result <-- fetch ;
            mret (update (factor * result)) } in
        multiplier 2 3


let test_two_element_list () =
  let ret x = [x]
  and bind xs f = List.concat (List.map f xs) in
    mdo { a <-- [1; 2; 3] ;
          b <-- [3; 4; 5] ;
          mret a+b }


let _ =
    let ret x = x and bind a f = f a in
  mdo { _ <-- 1+2 ; 
        mret 2 }

(* main *)

let all_tests () =
  test_simple_expression () &&
  test_one_element_list () = (3, 6) &&
  test_two_element_list () = [4; 5; 6; 5; 6; 7; 6; 7; 8] 


let (_: unit) =
  match all_tests () with
      true -> exit 0
    | false -> exit 1
