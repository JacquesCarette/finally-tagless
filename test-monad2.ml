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
	    let updater f = update (f * result) in ;
	    x <-- updater factor;
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
  mdo { let a = 1 and b = 2 in;
        _ <-- a+b ;
        ret b }

(* test non-determinism monad, the simplest possible implementation *)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream) 
                     | InC of (unit -> 'a stream)
let test_nondet () =
  let mfail = fun () -> Nil in
  let ret a = fun () -> Cons (a,mfail) in
  (* actually, interleave: a fair disjunction with breadth-first search*)
  let rec mplus a b = fun () -> match a () with
                  | Nil -> InC b
		  | InC a -> (match b () with
		    | Nil -> InC a
		    | InC b -> InC (mplus a b)
		    | Cons (b1,b2) -> Cons (b1, (mplus a b2)))
                  | Cons (a1,a2) -> Cons (a1,(mplus b a2)) in
  (* a fair conjunction *)
  let rec bind m f = fun () -> match m () with
                  | Nil -> mfail ()
		  | InC a -> InC (bind a f)
                  | Cons (a,b) -> mplus (f a) (bind b f) () in
  let guard be = if be then ret () else mfail in
  let rec run n m = if n = 0 then [] else
                match m () with
		| Nil -> []
		| InC a -> run n a
		| Cons (a,b) -> (a::run (n-1) b)
  in
  let rec numb () = InC (mplus (ret 0) (mdo { n <-- numb; ret (n+1) })) in
  (* Don't try this in Prolog or in Haskell's MonadPlus! *)
  let tst = mdo {
                  i <-- numb;
                  guard (i>0);
                  j <-- numb;
                  guard (j>0);
                  k <-- numb;
                  guard (k>0);
                  (* Just to illustrate the `let' form within mdo *)
                  let test x = x*x = j*j + k*k in;
                  guard (test i);
		  ret (i,j,k)
                } 
  in run 7 tst
;;

let _ = List.map (fun (i,j,k) -> Printf.printf "(%d %d %d) " i j k) (test_nondet ())
;;
		      
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
  simple_let_mdo () = 2 &&
  test_nondet () = [(5,4,3); (5,3,4); (10,8,6); 
		    (10,6,8); (13,12,5); (13,5,12); (15,12,9)]


(* *)
let (_: unit) =
  match all_tests () with
      true -> exit 0
    | false -> exit 1
