(* name:          test-monad.ml
 * synopsis:      simple tests for monadic syntax sugar
 * based on work of:        Lydia E. van Dijk
 * $Id$
 * ocaml version: 3.08.0 *)


let test_simple_expression () =
    let ret x = x in
  mdo { ret true }

let brt = .< 0 >. ;;

let test_brackets () =
  let ret x = x and bind x f = x in
    mdo { y <-- .< 0 >. ;
          ret y }

let test_state1 () =
  let ret x = fun state -> (x,state)
  and bind ma fmb = fun state -> let result, newstate = ma state in 
                                 fmb result newstate
  and update newval = fun state -> state, newval
  and fetch = fun state -> state, state in
    let multiplier factor =
      mdo { result <-- fetch ;
	    update (factor * result);
	    ret result } in
        multiplier 2 3

let test_state2 () =
  let ret x = fun state -> (x,state)
  and bind ma fmb = fun state -> let result, newstate = ma state in 
                                 fmb result newstate
  and update newval = fun state -> state, newval
  and fetch = fun state -> state, state in
    let multiplier factor =
      mdo { result <-- fetch ;
	    _ <-- update (factor * result);
	    fetch } in
        multiplier 2 3


let test_state21 () =
  let ret x = fun state -> (x,state)
  and bind ma fmb = fun state -> let result, newstate = ma state in 
                                 fmb result newstate
  and update newval = fun state -> state, newval
  and fetch = fun state -> state, state in
    let multiplier factor =
      mdo { result <-- fetch ;
	    let updater f = update (f * result) end;
	    _ <-- updater factor;
	    fetch } in
        multiplier 2 3

let test_two_element_list () =
  let ret x = [x]
  and bind xs f = List.concat (List.map f xs) in
    mdo { a <-- [1; 2; 3] ;
          b <-- [3; 4; 5] ;
          ret (a-b) } (* negation is not commutative, and more illustrative*)

(* State monad with a single-threaded updateable state *)
class state_cl (iv:int) istate =
   object (s:'s)
     val mutable v = iv
     val mutable state = istate
     method set_v newv = (v <- newv; s)
     method mfetch = (v <- state; s)
     method mupdate nv = (v <- state; state <- nv; s)
     method bind (fn:int->'s) = fn v
     method result = (v,state)
   end

let test_state3 () =
  let the_state = new state_cl 0 3 in
  let ret = the_state#set_v in
  let fetch = the_state#mfetch in
  let update = the_state#mupdate in
  let multiplier factor =
      odo { result <-- fetch ;
	    _ <-- (update (factor * result));
	    ret result } in
        (multiplier 2) # result


let simple_mdo () =
    let ret x = x and bind a f = f a in
  mdo { _ <-- 1+2 ;
        ret 2 }

let simple_let_mdo () =
    let ret x = x and bind a f = f a in
  mdo { let a = 1 and b = 2 end;
        _ <-- a+b ;
        ret b }

(* main *)

let all_tests () =
  .! (test_brackets ()) = 0 &&
  test_simple_expression () &&
  test_state1 ()  = (3, 6) &&
  test_state2 ()  = (6, 6) &&
  test_state21 () = (6, 6) &&
  test_state3 ()  = (3, 6) &&
  test_two_element_list () = [-2; -3; -4; -1; -2; -3; 0; -1; -2] &&
  simple_mdo () = 2 &&
  simple_let_mdo () = 2


(* *)
let (_: unit) =
  match all_tests () with
      true -> exit 0
    | false -> exit 1
