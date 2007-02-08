(* Interpreter, Compiler, Partial Evaluator *)

(*
  The language is simply-typed lambda-calculus with fixpoint,
  integers, booleans and comparison.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | B Bool | Add ie1 ie2 |
  IF b e-then e-else
  
  The language is just expressive enough for the Gibonacci function.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.
*)


(* This is used only to extract the result so we can see it nicely.
   Alternatively, we can always extract the result as code,
   especially if we don't use the interpreter.
*)
type ('a,'v) result = RL of 'v | RC of ('a,'v) code;;

(* This class defines syntax (and its instances, semantics) of our language
 *)



module type Symantics = sig
  type ('c,'v) repr
  val int  : int -> ('c,int) repr
  val add  : ('c,int) repr -> ('c,int) repr -> ('c,int) repr
  (* The last two arguments to ifeq are functional terms.
     One of them is applied to the first argument of ifeq.
     The reason for this sharade is to prevent evaluation
     of both arguments of ifeq in a CBV language.
     ifeq must be a syntactic form rather than just a function.
     Or, at least it shouldn't take just (undelayed) terms.
  *)
  val ifeq : ('c,int) repr -> ('c,int) repr ->
             ('c,int->'a) repr -> ('c,int->'a) repr -> ('c,'a) repr 

  val lam : (('c,'a) repr -> ('c,'b) repr) -> ('c,'a->'b) repr
  val app : ('c,'a->'b) repr -> ('c,'a) repr -> ('c,'b) repr
  val fix : (('c,'a->'b) repr -> ('c,'a->'b) repr) -> ('c,'a->'b) repr

  val get_res : ('c,'v) repr -> ('c,'v) result
end
;;


(* Running example *)

module EX(S: Symantics) = struct
 include S

 (* Unit is to prevent monomorphising over 'c *)
 let test1 () = add (int 1) (int 2)
 let test2 () = lam (fun x -> add x x)
 let test3 () = lam (fun x -> add (app x (int 1)) (int 2))

 let testgib () = lam (fun x -> lam (fun y ->
                  fix (fun self -> lam (fun n ->
                      ifeq n (int 0) (lam (fun _ -> x))
                        (lam (fun _ ->
			  (ifeq n (int 1) (lam (fun _ -> y))
			   (lam (fun _ ->
                             (add (app self (add n (int (-1))))
				(app self (add n (int (-2))))))))))))))


 let testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)

 let test1r = get_res (test1 ())
 let test2r = get_res (test2 ())
 let test3r = get_res (test3 ())

 let testgibr = get_res (testgib ())
 let testgib1r = get_res (testgib1 ())
end;;


(* ------------------------------------------------------------------------ *)
(* The interpreter
   It is typed, tagless interpreter: R is not a tag. The interpreter
   never gets stuck, because it evaluates typed terms only
*)
(* Pure interpreter. It is essentially the identity transformer *)
module R = struct
  type ('c,'v) repr = 'v    (* absolutely no wrappers *)
  let int (x:int) = x
  let add e1 e2 = e1 + e2
  let ifeq ie1 ie2 et ee = if ie1 = ie2 then (et ie1) else (ee ie1)

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f n = let rec fx f n = f (fx f) n in fx f n

  let get_res x = RL x
end;;


module EXR = EX(R);;
let itest1 = EXR.test1r;;
let itest2 = EXR.test2r;;
let itest3 = EXR.test3r;;
let itestg = EXR.testgibr;;
let itestg1 = EXR.testgib1r;;


(* ------------------------------------------------------------------------ *)
(* Pure compiler *)

(* Note how the compiler never raises any exception and matches no tags
  (no generated code has any tags)
*)

module C = struct
  type ('c,'v) repr = ('c,'v) code
  let int (x:int) = .<x>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let ifeq ie1 ie2 et ee = 
    .<let i1 = .~ie1 in if i1 = .~ie2 then .~et i1 else .~ee i1>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<fun n -> let rec self n = .~(f .<self>.) n in self n>.

  let get_res x = RC x
end;;

module EXR = EX(C);;

let ctest1 = EXR.test1r;;
let ctest2 = EXR.test2r;;
let ctest3 = EXR.test3r;;
let ctestg = EXR.testgibr;;
let ctestg1 = EXR.testgib1r;;


(*
module P = struct
  type ('c,'v) repr = ('c,'v) code
  let int (x:int) = fun iint -> let x = iint x in .<x>.
  let add e1 e2 = fun iadd -> .<iadd .~e1 .~e2>.
  let ifeq ie1 ie2 et ee = 
    .<let i1 = .~ie1 in if i1 = .~ie2 then .~et i1 else .~ee i1>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<fun n -> let rec self n = .~(f .<self>.) n in self n>.

  let get_res x = RC x
end;;
*)
