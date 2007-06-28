(* name:          test-monad.ml
 * synopsis:      simple tests for monadic syntax sugar
 * based on work of:        Lydia E. van Dijk
 * ocaml version: 3.08.0 *)


let test_simple_expression () =
    let ret x = x in
  perform  ret true 

let brt : ('a, int) code = .< 0 >. ;;

let test_brackets =
  let ret x = x and bind x f = f x in
    perform  y <-- .< 0 >. ;
          ret y 

let test_escape =
  let x = .< 1 >. in
    .< .~x >.
