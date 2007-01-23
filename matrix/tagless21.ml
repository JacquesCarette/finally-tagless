(* Tagless staged interpreter *)
(* The language is simply typed lambda-calculus with booleans and
   other base types, the fixpoint, and the higher-order abstract syntax.
   type exp =  A of exp * exp | L of (exp -> exp) | Fix of (exp -> exp)
             | If b t e 
             | B of bool | I of int
              
*)


(* implementation *)
(* base *)
let app e1 e2 = .<.~e1 .~e2>.;;
let lam e     = .<fun x -> .~(e .<x>.)>.;;
let fix e     = .<let rec f = .~(e .<f>.) in f>.;;


(* extensions *)
(* booleans *)
let b (bv:bool) = .<bv>.;;
let band b1 b2  = .<.~b1 && .~b2>. ;;
let bor b1 b2   = .<.~b1 || .~b2>. ;;
let bnot b1     = .<not .~b1>. ;;

(* integers *)
let i (i:int) = .<i>.;;
let (++) i j = .< .~i + .~j >. ;;
let ( ** ) i j = .< .~i  * .~j  >. ;;
let ( -- ) i j = .< .~i - .~j >. ;;
let ( // ) i j = .< .~i  / .~j >. ;;
let ( === ) i j = .< .~i  = .~j >. ;;

(* if *)
let tif eb et ee = .<if .~eb then .~et else .~ee>.;;


(* pairs *)
let p a b   = .< (.~a, .~b) >. ;;
let pfst p  = .< fst .~p >. ;;
let psnd p  = .< snd .~p >. ;;

(* references *)
let rref a = .< ref .~a >. ;;
let (!!) a  = .< !(.~a) >. ;;
let ( := ) aref b  = .< .~aref := .~b >. ;;

(* sequencing *)
let seq a b = .< (.~a ; .~b ) >. ;;

(* even let! *)
let nlet v f = .< let x = .~v in .~(f .<x>.) >. ;;

(* tests *)
let t1 = lam (fun x -> x);;

let t2 = app (lam (fun x -> x)) (b true);;

let t3 = lam (fun x -> lam (fun y -> x));;

let t4 = app (lam (fun x -> lam (fun y -> x))) (lam (fun x -> x));;

let t5 = (app 
	    (app (lam (fun x -> (lam (fun y -> (app (app x (b true)) y)))))
               (lam (fun x -> (lam (fun y -> y)))))
	    (b false));;


(* the following are exprected errors *)
(* and are commented out for testing 
let t6 = (app 
	    (app (lam (lam (app (app (varS varZ) (b true)) varZ)))
               (lam varZ))
	    (b false)) ();;
let t6 = (app 
	    (app (lam (fun x -> (lam (fun y -> (app (app x (b true)) y)))))
               (lam (fun x -> x)))
	    (b false));;

let t6' = lam (fun x -> (app x x)) ;; 

*)

(* a few new tests *)
let t7 = (i 5 ++ i 6) ;;
let t8 = nlet (i 5 ++ i 6) (fun x -> (x ** i 3));;

(* Factorial *)
let fact = fix (fun self ->
              lam (fun n ->
		tif (n === (i 0)) (i 1)
		    (n ** (app self (n -- (i 1))))));;
(*
 val fact : ('a, int -> int) code =
  .<let rec f_1 = fun x_2 -> if (x_2 = 0) then 
   1 else (x_2 * (f_1 (x_2 - 1))) in
  f_1>.
*)

let tfact1 = .!(app fact (i 5));;
let tfact2 = (.! fact) 5;;
(* 120 *)
