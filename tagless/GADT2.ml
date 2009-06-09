(* GADT in OCaml with *safe* Obj.magic
   We implement GADT classically, as a data type with type equality
   constraints. OCaml makes us jump through extra hoops for the sake of
   existentials.
   
   Our example is tagless initial interpreters: simply typed lambda-calculus
   with constants, as GADTs. We write terms once and can interpret them
   many times. We show two different interpreters.
*)

(* The essence of GADT: the witness of type equality 
   It is fully safe, meaning never leads to segmentation faults.
*)
module EQ = struct
  type ('a,'b) eq = Refl of 'a option ref * 'b option ref

  let refl () = let r = ref None in Refl (r,r)

  let symm : ('a,'b) eq -> ('b,'a) eq = function 
      Refl (x,y) -> Refl (y,x)


  let apply_eq : ('a,'b) eq -> 'a -> 'b = function
      Refl (rx,ry) -> fun x ->
        rx := Some x;
        match !ry with
	| Some y -> rx := None; y
	|     _  -> failwith "Impossible"

  let apply2 = fun eq -> apply_eq (symm eq)
end;;

(* The following is a more optimal implementation

(* Here is the principal component, the kernel of trust.
   Only this module uses Obj.magic. One has to be careful here.
*)
module EQ : sig 
  type ('a,'b) eq
  val refl : ('a,'a) eq
  val symm : ('a,'b) eq -> ('b,'a) eq
  val apply_eq : ('a,'b) eq -> 'a -> 'b
  module EQF1(X:sig type 'a r end) : sig
    val feq : ('a,'b) eq -> ('a X.r, 'b X.r) eq
  end
end = struct
  type ('a,'b) eq = Refl
  let refl = Refl
  let symm Refl = Refl
  let apply_eq Refl = Obj.magic
  module EQF1(X:sig type 'a r end) = struct
    let feq Refl = Refl			(* Just like in Agda *)
  end
end;;
*)

open EQ;;

(* simply-typed lambda-calculator with constants *)
type 'a exp =
    {ef_int: 'w. ((('a,int) eq -> int -> 'w) -> 'w) option;
     ef_inc: 'w. ((('a,int -> int) eq -> 'w) -> 'w) option;
     ef_prd: 'w. ((('a,int -> int) eq -> 'w) -> 'w) option;
     ef_lft: 'w. ((('a,'a) eq -> 'a -> 'w) -> 'w) option;
     ef_lam: 'w. (('a,'w) lam_k -> 'w) option;
     ef_app: 'w. (('a,'w) app_k -> 'w) option;}
      (* The standard encoding of existentials *)
and ('a,'w) lam_k = 
    {lam_k : 'u 'v. ('a, ('u -> 'v)) eq * ('u exp -> 'v exp) -> 'w}
and ('a,'w) app_k = 
    {app_k : 'u 'v. ('a, 'v) eq * ('u -> 'v) exp * 'u exp -> 'w};;

let e_err = {ef_int  = None;
	     ef_inc = None;
	     ef_prd = None;
	     ef_lft = None;
	     ef_lam = None;
	     ef_app = None;};;


(* smart constructors *)
let e_int x = {e_err with ef_int = Some (fun k -> k (refl ()) x)};;
(* val e_int : int -> int exp = <fun> *)

let e_inc   = {e_err with ef_inc = Some (fun k -> k (refl ()))};;
(* val e_inc : (int -> int) exp = *)

let e_prd   = {e_err with ef_prd = Some (fun k -> k (refl ()))};;
(* val e_prd : (int -> int) exp = *)

let e_lft x = {e_err with ef_lft = Some (fun k -> k (refl ()) x)};;
(* val e_lft : 'a -> 'a exp = <fun> *)

let e_lam f = {e_err with ef_lam = Some (fun k -> k.lam_k (refl (),f))};;
(* val e_lam : ('a exp -> 'b exp) -> ('a -> 'b) exp = <fun> *)

let e_app f x = {e_err with ef_app = Some (fun k -> k.app_k (refl (),f,x))};;
(* val e_app : ('a -> 'b) exp -> 'a exp -> 'b exp = <fun> *)

let e_ran = {e_err with ef_inc = Some (fun k -> k (refl ())); 
                        ef_prd = Some (fun k -> k (refl ()))}

(* Test expressions *)
(* Note their inferred types given in comments *)
let test1 = e_app e_inc (e_int 1);;
(* val test1 : int exp = *)

let test2 = e_lam (fun x -> e_app e_inc (e_app e_inc x));;
(* val test2 : (int -> int) exp = *)

let test3 = e_lam (fun f -> e_lam (fun x -> e_app f x));;
(* val test3 : (('_a -> '_b) -> '_a -> '_b) exp = *)

let test4 = e_app (e_app test3 test2) (e_int 3);;
(* val test4 : int exp = *)

let test5 = e_lam (fun x -> e_app e_ran x);;
(* val test5 : (int -> int) exp = *)

(* Two interpreters. The need polymorphic recursion -- as is
   typical with GADTs*)

(* The first interpreter is the evaluator *)

type eval_sig = {ev: 'a. 'a exp -> 'a};;
let rec eval = {ev = 
 function 
   | {ef_inc = Some k1; ef_prd = Some k2} -> 
       let f = fun k g -> k (fun eq -> apply_eq (symm eq) g) in
       if Random.bool () then f k1 succ else f k2 pred
   | {ef_int = Some k} -> k apply2
   | {ef_inc = Some k} -> k (fun eq -> apply2 eq succ)
   | {ef_prd = Some k} -> k (fun eq -> apply2 eq pred)
   | {ef_lft = Some k} -> k apply2
   | {ef_lam = Some k} -> 
       k {lam_k = 
	  fun (eq,f) -> apply2 eq (fun x -> eval.ev (f (e_lft x)))}
   | {ef_app = Some k} -> 
       k {app_k = 
	  fun (eq,f,x) -> apply2 eq ((eval.ev f) (eval.ev x))}
	      };;

let test1_ev = eval.ev test1;; (* int = 2 *)
let test2_ev = eval.ev test2;; (* int -> int = <fun> *)
let test3_ev = eval.ev test3;; (* (int -> int) -> int -> int = <fun> *)
let test4_ev = eval.ev test4;; (* int = 5 *)

(* A different interpreter: counts the number of constructors *)
type size_sig = {sv: 'a. 'a exp -> int};;
let rec size = {sv =
 function 
   | {ef_lam = Some k} -> 
       k {lam_k = fun (eq,f) -> size.sv (f e_err) + 1}
   | {ef_app = Some k} -> 
       k {app_k = fun (eq,f,x) -> (size.sv f) + (size.sv x) + 1}
   | _ -> 1
	      };;

let test1_sv = size.sv test1;; (* 3 *)
let test2_sv = size.sv test2;; (* 6 *)
let test3_sv = size.sv test3;; (* 5 *)
let test4_sv = size.sv test4;; (* 14 *)
