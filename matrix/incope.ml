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
  type ('c,'sv,'dv) repr
  val int  : int  -> ('c,int,int) repr
  val bool : bool -> ('c,bool,bool) repr
  val add  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val leq  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,bool,bool) repr
  (* The last two arguments to if are functional terms.
     One of them is applied to unit.
     The reason for this charade is to prevent evaluation
     of both arguments of if_ in a CBV language.
     if_ must be a syntactic form rather than just a function.
     Or, at least it shouldn't take just (undelayed) terms.
  *)
  val if_ : ('c,bool,bool) repr ->
             (unit -> ('c,'sa,'da) repr) ->
             (unit -> ('c,'sa,'da) repr) -> ('c,'sa,'da) repr 

  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr)
    -> ('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr),'da->'db) repr
  val app : ('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr),'da->'db) repr
    -> ('c,'sa,'da) repr -> ('c,'sb,'db) repr
  val fix : (('c,'s,'a->'b) repr -> ('c,'s,'a->'b) repr) -> ('c,'s,'a->'b) repr

  val get_res : ('c,'sv,'dv) repr -> ('c,'dv) result
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
                      if_ (leq n (int 0)) (fun () -> x)
                        (fun () ->
                          (if_ (leq n (int 1)) (fun () -> y)
                           (fun () ->
                             (add (app self (add n (int (-1))))
                                  (app self (add n (int (-2))))))))))))


 let testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)

 let test1r = get_res (test1 ())
 let test2r = get_res (test2 ())
 let test3r = get_res (test3 ())
 let test3r' = get_res (test3 ())

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
  type ('c,'sv,'dv) repr = 'dv    (* absolutely no wrappers *)
  let int (x:int) = x
  let bool (b:bool) = b
  let add e1 e2 = e1 + e2
  let leq x y = x <= y
  let if_ eb et ee = if eb then (et ()) else (ee ())

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
  type ('c,'sv,'dv) repr = ('c,'dv) code
  let int (x:int) = .<x>.
  let bool (b:bool) = .<b>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let leq x y = .< .~x <= .~y >.
  let if_ eb et ee = 
    .<if .~eb then .~(et () ) else .~(ee () )>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<fun n -> let rec self n = .~(f .<self>.) n in self n>.

  let get_res x = RC x
  let cC (x : ('c,'sv,'dv) repr) : ('c,'sv1,'dv) repr = x
end;;

module EXC = EX(C);;

let ctest1 = EXC.test1r;;
let ctest2 = EXC.test2r;;
let ctest3 = EXC.test3r;;
let ctestg = EXC.testgibr;;
let ctestg1 = EXC.testgib1r;;

(* actually run some of the above tests *)
let ctest2' = let res = match (ctest2) with
    | (RL x) -> failwith "not what we want"
    | (RC x) -> (.< .~x 5 >.) 
in .! res;;
let ctest3' = let f = (fun x -> x+17) in
    let res = match ctest3 with
    | (RL x) -> failwith "not what we want"
    | (RC x) -> (.< .~x f >.) 
in .! res;;
let ctestg1' = 
    let res = match ctestg1 with
    | (RL x) -> failwith "not what we want"
    | (RC x) -> x
in .! res;;

(* ------------------------------------------------------------------------ *)
(* Partial evaluator *)
(* Inspired by Ken's solution *)

module P = struct
  type ('c,'sv,'dv) repr = {st: 'sv option; dy: ('c,'dv) code}
  let abstr {dy = x} = x
  let pdyn x = {st = None; dy = x}

  let int  (x:int)  = {st = Some x; dy = .<x>.}
  let bool (x:bool) = {st = Some x; dy = .<x>.}

  let add e1 e2 = match (e1,e2) with
                   ({st = Some n1}, {st = Some n2}) -> int (n1+n2)
                 | _ -> pdyn (C.add (abstr e1) (abstr e2))
  let leq e1 e2 = match (e1,e2) with
                   ({st = Some n1}, {st = Some n2}) -> bool (n1<=n2)
                 | _ -> pdyn (C.leq (abstr e1) (abstr e2))
  let if_ eb et ee = match eb with
                       {st = Some b} -> if b then et () else ee ()
                     | _ -> pdyn (C.if_ (abstr eb) 
                                      (fun () -> abstr (et ()))
                                      (fun () -> abstr (ee ())))

  let lam f = {st = Some f; 
	       dy = C.cC (C.lam (fun x -> abstr (f (pdyn x))))}

  let app ef ea = match ef with
                    {st = Some f} -> f ea
                  | _ -> pdyn (C.app (C.cC (abstr ef)) (abstr ea))
   (*
     For now, to avoid divergence at the PE stage, we residualize
    actually, we unroll the fixpoint exactly once, and then
    residualize
   *)
  let fix f = f (pdyn (C.fix (fun x -> abstr (f (pdyn x)))))

  let get_res x = C.get_res (abstr x)
end;;

module EXP = EX(P);;

let ptest1 = EXP.test1r;;
let ptest2 = EXP.test2r;;
let ptest3 = EXP.test3r;;
let ptestg = EXP.testgibr;;
let ptestg1 = EXP.testgib1r;;

(* That's all folks. It seems to work... *)


(* Remnants of an earlier idea: compile the PE: make a code, which,
when run, will make a PE...
*)


(*
module P = struct
  let abstr = function (RL x) -> .<x>. | RC x -> x
  let int (x:int) = .<RL x>.
  let add e1 e2 = .<
    match (e1,e2) with
      (RL n1, RL n2) -> RL (n1+n2)
    | _              -> RC (C.add (abstr e1) (abstr e2))>.
(*
  let ifeq ie1 ie2 et ee = 
    .<let i1 = .~ie1 in if i1 = .~ie2 then .~et i1 else .~ee i1>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<fun n -> let rec self n = .~(f .<self>.) n in self n>.
  let get_res x = .! x
*)
end;;

*)
