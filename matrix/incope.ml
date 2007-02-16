(* Interpreter, Compiler, Partial Evaluator *)

(*
  The language is simply-typed lambda-calculus with fixpoint,
  integers [plus basic operations], booleans [ditto] and comparison.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | Add ie1 ie2 | Mul ie1 ie2 
  B Bool |
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

(* This class/type defines syntax (and its instances, semantics) 
   of our language
 *)

module type Symantics = sig
  type ('c,'sv,'dv) repr
  val int  : int  -> ('c,int,int) repr
  val bool : bool -> ('c,bool,bool) repr
  val add  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val mul  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val leq  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,bool,bool) repr
  (* could be defined in terms of leq and if_ *)
  val eql  : ('c,'a,'a) repr -> ('c,'a,'a) repr -> ('c,bool,bool) repr
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
  val fix : (('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr) as 's,'da->'db) repr 
             -> ('c,'s,'da->'db) repr)  -> ('c,'s,'da->'db) repr
  val unfold : ('c,'s,'a) repr -> 
               ('c,('c,'s,'a) repr -> ('c,'s,'a) repr,'a->'a) repr -> 
               ('c, ('c,int,int) repr -> ('c,'s,'a) repr, int->'a) repr

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
 let testgib2 () = lam (fun x -> (lam (fun y ->
   app (app (app (testgib ()) x) y) (int 5))))

 let testpowfix () = lam (fun x ->
                      fix (fun self -> lam (fun n ->
                        if_ (leq n (int 0)) (fun () -> int 1)
                            (fun () -> mul x (app self (add n (int (-1))))))))

 (* this pow takes its arguments backwards for now, for ease of PE,
    should be fixed later 
  Can we delete this?
 let testpowfix () = unfold (lam (fun x -> int 1))
                            (lam (fun f -> lam (fun x -> mul x (app f x))))
*)

 let testpowfix7 () = lam (fun x -> app (app (testpowfix ()) x) (int 7))

 let test1r = get_res (test1 ())
 let test2r = get_res (test2 ())
 let test3r = get_res (test3 ())
 let test3r' = get_res (test3 ())

 let testgibr = get_res (testgib ())
 let testgib1r = get_res (testgib1 ())
 let testgib2r = get_res (testgib2 ())

 let testpowfixr = get_res (testpowfix ())
 let testpowfix7r = get_res (testpowfix7 ())

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
  let mul e1 e2 = e1 * e2
  let leq x y = x <= y
  let eql x y = x == y
  let if_ eb et ee = if eb then (et ()) else (ee ())

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f n = let rec fx f n = f (fx f) n in fx f n
  let rec unfold z s = fun n -> if n<=0 then z else s ((unfold z s) (n-1))

  let get_res x = RL x
end;;

module EXR = EX(R);;
let itest1 = EXR.test1r;;
let itest2 = EXR.test2r;;
let itest3 = EXR.test3r;;
let itestg = EXR.testgibr;;
let itestg1 = EXR.testgib1r;;
let itestg2 = EXR.testgib2r;;
let itestp7 = EXR.testpowfix7r;;

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
  let mul e1 e2 = .<.~e1 * .~e2>.
  let leq x y = .< .~x <= .~y >.
  let eql x y = .< .~x == .~y >.
  let if_ eb et ee = 
    .<if .~eb then .~(et () ) else .~(ee () )>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<fun n -> let rec self n = .~(f .<self>.) n in self n>.
  let unfold z s = .<let rec f n = if n <= 0 then .~z else .~s (f (n-1)) in f>.

  let get_res x = RC x
  let dyn_id (x : ('c,'sv,'dv) repr) : ('c,'sv1,'dv) repr = x
end;;

module EXC = EX(C);;

let ctest1 = EXC.test1r;;
let ctest2 = EXC.test2r;;
let ctest3 = EXC.test3r;;
let ctestg = EXC.testgibr;;
let ctestg1 = EXC.testgib1r;;
let ctestg2 = EXC.testgib2r;;
let ctestp7 = EXC.testpowfix7r;;

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

module P =
struct
  type ('c,'sv,'dv) repr = {st: 'sv option; dy: ('c,'dv) code}
  let abstr {dy = x} = x
  let pdyn x = {st = None; dy = x}

  let int  (x:int)  = {st = Some (R.int x); dy = C.int x}
  let bool (x:bool) = {st = Some (R.bool x); dy = C.bool x}

  (* generic build - takes a repr constructor, an interpreter function
     and a compiler function (all binary) and builds a PE version *)
  let build cast f1 f2 = fun e1 -> fun e2 -> match (e1,e2) with
                   ({st = Some n1}, {st = Some n2}) -> cast (f1 n1 n2)
                 | _ -> pdyn (f2 (abstr e1) (abstr e2))
  (* same as 'build' but takes care of the neutral element (e) simplification
     allowed via a monoid structure which is implicitly present *)
  let buildsimp cast e f1 f2 = fun e1 e2 -> match (e1,e2) with
                 | ({st = Some e'}, _) when e = e' -> e2
                 | (_, {st = Some e'}) when e = e' -> e1
                 | ({st = Some n1}, {st = Some n2}) -> cast (f1 n1 n2)
                 | _ -> pdyn (f2 (abstr e1) (abstr e2))

  let add e1 e2 = buildsimp int 0 R.add C.add e1 e2
  let mul e1 e2 = buildsimp int 1 R.mul C.mul e1 e2
  let leq e1 e2 = build bool R.leq C.leq e1 e2
  let eql e1 e2 = build bool R.eql C.eql e1 e2
  let if_ eb et ee = match eb with
                       {st = Some b} -> if b then et () else ee ()
                     | _ -> pdyn (C.if_ (abstr eb) 
                                      (fun () -> abstr (et ()))
                                      (fun () -> abstr (ee ())))

  let lam f = {st = Some f; 
               dy = C.lam (fun x -> abstr (f (pdyn x)))}

  let app ef ea = match ef with
                    {st = Some f} -> f ea
                  | _ -> pdyn (C.app (C.dyn_id (abstr ef)) (abstr ea))

   (*
     For now, to avoid divergence at the PE stage, we residualize
    actually, we unroll the fixpoint exactly once, and then
    residualize
   *)
  let fix f = f (pdyn (C.fix (fun x -> abstr (f (pdyn x)))))
  (* this should allow us controlled unfolding *)
  let unfold z s = lam (function
      | {st = Some n} -> 
              let rec f k = if k<=0 then z else
                                match s with
                                | {st = Some m} -> m (f (k-1))
                                | {dy = y}      -> pdyn (C.app y (abstr (f (k-1))))
              in f n
      | {dy = y}      -> pdyn (C.app (C.unfold (abstr z) (abstr s)) y))

  let get_res x = C.get_res (abstr x)
end;;

module P1 =
struct
  include P
(*
  type ('c,'sv,'dv) repr = {st: 'sv option; dy: ('c,'dv) code}
  let abstr {dy = x} = x
  let pdyn x = {st = None; dy = x}
*)
  let fix f = 
    let fdyn = C.fix (fun x -> abstr (f (pdyn x))) in
    let fdynn e = pdyn (C.app fdyn e.dy) in
    {st = Some (function {st = Some _} as e -> 
                  let rec self n = 
                    (match n.st with Some _ -> app (f (lam self)) n 
                                     | _ ->  fdynn n) in 
                  app (f (lam self)) e
               | e  -> fdynn e);
               dy = fdyn }
end;;


module EXP = EX(P1);;

let ptest1 = EXP.test1r;;
let ptest2 = EXP.test2r;;
let ptest3 = EXP.test3r;;
let ptestg = EXP.testgibr;;
let ptestg1 = EXP.testgib1r;;
let ptestg2 = EXP.testgib2r;;
let ptestp7 = EXP.testpowfix7r;;

(* start encoding some of Ken's ideas on a self-interpreter *)
let apply_to_si_R encoded_e =
    encoded_e R.int R.bool R.add R.app 
        R.mul R.leq R.eql (R.if_) (R.lam) (R.fix) (R.unfold)
    ;;

let apply_to_si_C encoded_e =
    encoded_e C.int C.bool C.add C.app
        C.mul C.leq C.eql C.if_ C.lam C.fix C.unfold
    ;;

let apply_to_si_P encoded_e =
    encoded_e P1.int P1.bool P1.add P1.app
        P1.mul P1.leq P1.eql P1.if_ P1.lam P1.fix P1.unfold
    ;;

let an_e1 = (fun _int -> (fun _bool -> (fun _add -> (fun _app ->
            (fun _mul -> (fun _leq -> (fun _eql -> (fun _if_ ->
            (fun _lam -> (fun _fix -> (fun _unfold ->
            (_add (_int 1) (_int 2))
            ))))))))))) ;;

let an_e2 = (fun _int -> (fun _bool -> (fun _add -> (fun _app ->
            (fun _mul -> (fun _leq -> (fun _eql -> (fun _if_ ->
            (fun _lam -> (fun _fix -> (fun _unfold ->
            _lam (fun x -> _add x x)
            ))))))))))) ;;

let an_ep = (fun _int -> (fun _bool -> (fun _add -> (fun _app ->
            (fun _mul -> (fun _leq -> (fun _eql -> (fun _if_ ->
            (fun _lam -> (fun _fix -> (fun _unfold ->
            _lam (fun x ->
                  _fix (fun self -> _lam (fun n ->
                    _if_ (_leq n (_int 0)) (fun () -> _int 1)
                        (fun () -> _mul x (_app self (_add n (_int (-1))))))))
            ))))))))))) ;;

(* an_e1 - compute 3 three ways *)
let testR1 = apply_to_si_R an_e1 ;;
let testC1 = apply_to_si_C an_e1 ;;
let testP1 = apply_to_si_P an_e1 ;;

(* an_e2 - compute x+x three ways *)
let testR2 = apply_to_si_R an_e2 ;;
let testC2 = apply_to_si_C an_e2 ;;
let testP2 = apply_to_si_P an_e2 ;;

(* an_ep - compute power three ways 
let testRp = apply_to_si_R an_ep ;;
let testCp = apply_to_si_C an_ep ;;
let testPp = apply_to_si_P an_ep ;;
 doesn't type? *)

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
