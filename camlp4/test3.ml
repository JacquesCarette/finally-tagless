(* name:          test-monad.ml
 * synopsis:      simple tests for monadic syntax sugar
 * based on work of:        Lydia E. van Dijk
 * ocaml version: 3.08.0 *)


let test_simple_expression () =
    let ret x = x in
  mdo { mret true }

let brt : ('a, int) code = brackets 0 ;;

(*
let test_brackets =
  let ret x = x in
    mdo { y <-- .< 0 >. ;
          mret y }
*)

(* main *)

(* 
let (_: unit) =
  match all_tests () with
      true -> exit 0
    | false -> exit 1
*)
