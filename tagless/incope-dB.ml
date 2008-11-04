(* Interpreter, Compiler, Partial Evaluator *)
(* deBruijn indices *)

(*
  Code accompanying the paper by
    Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
*)

(*
  The language is simply-typed lambda-calculus with fixpoint,
  integers [plus basic operations], booleans [ditto] and comparison.

  Var | Lam | App e e | Fix |
  I Int | Add ie1 ie2 | Mul ie1 ie2 
  B Bool |
  IF b e-then e-else
  
  Var ::= VZ | VS Var

  The language is just expressive enough for the Gibonacci and
  power functions.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.

  Please refer to incope.ml for further explanations and comparison.  
*)


(* This class/type defines syntax (and its instances, semantics) 
   of our language
 *)

(* In the following, we made the repr type to be a bit concrete:
     type ('c,'h,'dv) repr = 'h -> ('c,'dv) vr
   for the sake of partial evaluation. We could have kept ('c,'h,'dv) repr
   abstract. But then we needed to add to the signature
       type h0 = H0
   as the type of the closed environment and
       val close : ('c,h0,'dv) repr -> ('c,'dv) vr
       val hopen : ('c,'dv) vr -> ('c,'h,'dv) repr
       val hopen1 : (('c,'dv1) vr -> ('c,'dv) vr) ->
	            ('c,('c,'dv1) vr * 'h,'dv) repr
*)

module type Symantics = sig
  type ('c,'dv) vr			(* representation of a var or closed *)
					(* code *)
  type ('c,'h,'dv) repr = 'h -> ('c,'dv) vr
  val int  : int  -> ('c,'h,int) repr
  val bool : bool -> ('c,'h,bool) repr
  val add  : ('c,'h,int) repr -> ('c,'h,int) repr -> 
             ('c,'h,int) repr
  val mul  : ('c,'h,int) repr -> ('c,'h,int) repr -> 
             ('c,'h,int) repr
  val leq  : ('c,'h,int) repr -> ('c,'h,int) repr -> 
             ('c,'h,bool) repr
  val if_ : ('c,'h,bool) repr ->
             (unit -> 'x) ->
             (unit -> 'x) -> (('c,'h,'da) repr as 'x)

  val vz  : ('c,('c,'d) vr * 'h,'d) repr
  val vs  : ('c, 'h,'d) repr -> ('c, _ * 'h,'d) repr
  val lam : ('c,('c,'da) vr * 'h,'db) repr -> ('c,'h,'da->'db) repr
  val app  : ('c,'h,'da->'db) repr -> ('c,'h,'da) repr -> ('c,'h,'db) repr
  val fix : ('c, ('c,'da->'db) vr * 'h, 'da->'db) repr -> 
            ('c, 'h, 'da->'db) repr
end;;


(* Running example *)
module EX(S: Symantics) = struct
 open S

 let test1 () = add (int 1) (int 2)
 let test2 () = lam (add vz vz)
 let test3 () = lam (add (app vz (int 1)) (int 2))

 let testgib () = lam (let x = vz in
                  lam (let x = vs x and y = vz in
                  fix (let x = vs x and y = vs y and self = vz in
		  lam (let x = vs x and y = vs y and self = vs self
		       and n = vz in
                       if_ (leq n (int 0)) (fun () -> x)
                        (fun () ->
                          (if_ (leq n (int 1)) (fun () -> y)
                           (fun () ->
                             (add (app self (add n (int (-1))))
                                    (app self (add n (int (-2))))))))))))

 let testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)
 let testgib2 () = lam (lam (let x = vs vz and y = vz in 
                   app (app (app (testgib ()) x) y) (int 5)))

 let testpowfix () = lam (let x = vz in
                     fix (let x = vs x and self = vz in
                     lam (let x = vs x and self = vs self and n = vz in
                        if_ (leq n (int 0)) (fun () -> int 1)
                            (fun () -> mul x (app self (add n (int (-1))))))))

 let testpowfix7 () = lam (app (app (testpowfix ()) vz) (int 7))
 let testpowfix0 () = lam (app (app (testpowfix ()) (int 0)) vz)
end;;



(* ------------------------------------------------------------------------ *)
(* The interpreter
   It is typed, tagless interpreter: R is not a tag. The interpreter
   never gets stuck, because it evaluates typed terms only
*)
(* Pure interpreter. It is essentially the identity transformer *)
module R = struct
  type ('c,'d) vr = 'd			(* variable representation *)
  type ('c,'h,'dv) repr = 'h -> 'dv     (* absolutely no wrappers *)

  type h0 = H0				(* empty environment *)
  let vz   = fun (x,_) -> x
  let vs v = fun (_,h) -> v h

  let int (x:int)   = fun h -> x
  let bool (b:bool) = fun h -> b
  let add e1 e2 = fun h -> e1 h + e2 h
  let mul e1 e2 = fun h -> e1 h * e2 h
  let leq e1 e2 = fun h -> e1 h <= e2 h
  let if_ eb et ee = fun h -> if eb h then (et () h) else (ee () h)

  let lam e = fun h -> fun x -> e (x,h)
  let app e1 e2 = fun h -> (e1 h) (e2 h)
  let fix e = fun h -> let rec self n = e (self,h) n in self

  let runit e = e () H0
end;;

module EXR = EX(R);;

let 3 = R.runit EXR.test1;;
let 4 = R.runit EXR.test2 2;;
let 4 = R.runit EXR.test3 succ;;

let 8 = R.runit EXR.testgib 1 1 5;;
let 8 = R.runit EXR.testgib1;;
let 8 = R.runit EXR.testgib2 1 1;;

let 128 = R.runit EXR.testpowfix 2 7;;
let 128 = R.runit EXR.testpowfix7 2;;
let 0   = R.runit EXR.testpowfix0 2;;


(* ------------------------------------------------------------------------ *)
(* Another interpreter: it interprets each term to give its size
   (the number of constructors)
   The interpreter never gets stuck, because it evaluates typed terms only.
   This interpreter is also total: it determines the size of the term
   even if the term itself is divergent  *)

module L = struct
  type ('c,'h,'dv) repr = 'h -> int    (* absolutely no wrappers *)
  type ('c,'d) vr = int		       (* variable representation *)
  let vz   = fun (x,_) -> x
  let vs v = fun (_,h) -> v h

  let int (x:int) = fun _ -> 1
  let bool (b:bool) = fun _ -> 1
  let add e1 e2 = fun h -> e1 h + e2 h + 1
  let mul e1 e2 = fun h -> e1 h + e2 h + 1
  let leq e1 e2 = fun h -> e1 h + e2 h + 1
  let if_ eb et ee = fun h -> eb h + et () h + ee () h + 1

  let lam e = fun h -> e (0,h) + 1
  let app e1 e2 = fun h -> e1 h + e2 h + 1
  let fix e = fun h -> e (0,h) + 1

  let runit e = e () ()
end;;

module EXL = EX(L);;

let 3 = L.runit EXL.test1;;
let 2 = L.runit EXL.test2;;
let 5 = L.runit EXL.test3;;

let 17  = L.runit EXL.testgib;;
let 23 = L.runit EXL.testgib1;;
let 23 = L.runit EXL.testgib2;;

let 11  = L.runit EXL.testpowfix;;
let 15 = L.runit EXL.testpowfix7;;
let 15 = L.runit EXL.testpowfix0;;


(* ------------------------------------------------------------------------ *)
(* Pure compiler *)

(* Note how the compiler never raises any exception and matches no tags
  (nor generated code has any tags)
*)

module C = struct
  type ('c,'h,'dv) repr = 'h -> ('c,'dv) code

  type h0 = H0				(* empty environment *)
  type ('c,'a) vr = ('c,'a) code	(* variable representation *)
  let vz   = fun (x,_) -> x
  let vs v = fun (_,h) -> v h

  let int (x:int)   = fun h -> .<x>.
  let bool (b:bool) = fun h -> .<b>.
  let add e1 e2 = fun h -> .<.~(e1 h) + .~(e2 h)>.
  let mul e1 e2 = fun h -> .<.~(e1 h) * .~(e2 h)>.
  let leq e1 e2 = fun h -> .<.~(e1 h) <= .~(e2 h)>.
  let if_ eb et ee = fun h ->
    .<if .~(eb h) then .~(et () h) else .~(ee () h)>.

  let lam e = fun h -> .<fun x -> .~(e (.<x>.,h))>.
  let app e1 e2 = fun h -> .<.~(e1 h)  .~(e2 h)>.
  let fix e = fun h -> 
    .<let rec self n = .~(e (.<self>.,h)) n in self>.

  let runit e = e () H0
end;;

module EXC = EX(C);;

let ctest1 = C.runit EXC.test1;;
let ctest2 = C.runit EXC.test2;;
let ctest3 = C.runit EXC.test3;;

let ctestg  = C.runit EXC.testgib;;
let ctestg1  = C.runit EXC.testgib1;;
let 8 = .! (C.runit EXC.testgib1);;
let ctestg2 = C.runit EXC.testgib2;;

let ctestp  = C.runit EXC.testpowfix;;
let ctestp7 = C.runit EXC.testpowfix7;;
let 128 = (.! (C.runit EXC.testpowfix7)) 2;;
let ctestp0 = C.runit EXC.testpowfix0;;

(* ------------------------------------------------------------------------ *)
(* Partial evaluator *)

(* Partial evaluation requires and additional index map for types:
   Hence Symantic2 with an additional type parameter 'sv 
*)
module type Symantics2 = sig
  type ('c,'h,'sv,'dv) repr
  type ('c,'sv,'dv) vr			(* representation for var/closed code *)
  val int  : int  -> ('c,'h,int,int) repr
  val bool : bool -> ('c,'h,bool,bool) repr
  val add  : ('c,'h,int,int) repr -> ('c,'h,int,int) repr -> 
             ('c,'h,int,int) repr
  val mul  : ('c,'h,int,int) repr -> ('c,'h,int,int) repr -> 
             ('c,'h,int,int) repr
  val leq  : ('c,'h,int,int) repr -> ('c,'h,int,int) repr -> 
             ('c,'h,bool,bool) repr

  val if_ : ('c,'h,bool,bool) repr ->
             (unit -> 'x) ->
             (unit -> 'x) -> (('c,'h,'sa,'da) repr as 'x)

  val vz  : ('c,('c,'s,'d) vr * 'h,'s,'d) repr
  val vs  : ('c, 'h,'s,'d) repr -> ('c, _ * 'h,'s,'d) repr
  val lam : ('c,('c,'sa,'da) vr * 'h,'sb,'db) repr -> 
            ('c,'h,('c,'sa,'da) vr -> ('c,'sb,'db) vr,'da->'db) repr
  val app  : ('c,'h,('c,'sa,'da) vr -> ('c,'sb,'db) vr,'da->'db) repr ->
	     ('c,'h,'sa,'da) repr -> ('c,'h,'sb,'db) repr
  val fix : ('c,
	 ('c,(('c, 'sa, 'da) vr -> ('c, 'sb, 'db) vr as 'sx),'da->'db) vr * 'h, 
	     'sx, 'da->'db) repr -> 
            ('c, 'h, 'sx, 'da->'db) repr
end;;

(* Alas, we need a version of our running example for Symantics2 *)
(* The only difference is the replacement of Symantics with Symantics2 *)
(* on the first line. *)
module EX2(S: Symantics2) = struct
 open S

 let test1 () = add (int 1) (int 2)
 let test2 () = lam (add vz vz)
 let test3 () = lam (add (app vz (int 1)) (int 2))

 let testgib () = lam (let x = vz in
                  lam (let x = vs x and y = vz in
                  fix (let x = vs x and y = vs y and self = vz in
		  lam (let x = vs x and y = vs y and self = vs self
		       and n = vz in
                       if_ (leq n (int 0)) (fun () -> x)
                        (fun () ->
                          (if_ (leq n (int 1)) (fun () -> y)
                           (fun () ->
                             (add (app self (add n (int (-1))))
                                    (app self (add n (int (-2))))))))))))

 let testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)
 let testgib2 () = lam (lam (let x = vs vz and y = vz in 
                   app (app (app (testgib ()) x) y) (int 5)))

 let testpowfix () = lam (let x = vz in
                     fix (let x = vs x and self = vz in
                     lam (let x = vs x and self = vs self and n = vz in
                        if_ (leq n (int 0)) (fun () -> int 1)
                            (fun () -> mul x (app self (add n (int (-1))))))))

 let testpowfix7 () = lam (app (app (testpowfix ()) vz) (int 7))
 let testpowfix0 () = lam (app (app (testpowfix ()) (int 0)) vz)
end;;

(* We can trivially lift all of our previous interpreters to the *)
(* Symantics2 signature *)

module S12(S1:Symantics) = struct
  type ('c,'h,'sv,'dv) repr = ('c,'h,'dv) S1.repr
  type ('c,'sv,'dv) vr      = ('c,'dv) S1.vr

  let vz  = S1.vz
  let vs  = S1.vs

  let int  = S1.int
  let bool = S1.bool
  let add  = S1.add
  let mul  = S1.mul
  let leq  = S1.leq
  let if_  = S1.if_

  let lam  = S1.lam
  let app  = S1.app
  let fix  = S1.fix
end;;

(* For illustration, we run the Symantics2-based tests EX2 with
   the interprter R implementing regular Symantics.
*)
module EXR2 = EX2(S12(R));;

let 3 = R.runit EXR2.test1;;
let 4 = R.runit EXR2.test2 2;;
let 4 = R.runit EXR2.test3 succ;;

let 8 = R.runit EXR2.testgib 1 1 5;;
let 8 = R.runit EXR2.testgib1;;
let 8 = R.runit EXR2.testgib2 1 1;;

let 128 = R.runit EXR2.testpowfix 2 7;;
let 128 = R.runit EXR2.testpowfix7 2;;
let 0   = R.runit EXR2.testpowfix0 2;;


(* The partial evaluator is parameterized over the module that does
   future-stage interpertation (so-called `dynamic')
   P(F) produces the instance of Symantics2.

   Why the type repr below must be of the form 
       'h -> {st: 'sv option; dy: ('c,'dv) F.vr}
   For example, why can't we define repr to be a record with the
   (optional) static and the dynamic component, each component
   being a function from h to the closed value.
   The reason is that the dynamic part of the result of (add e1 e2)
   depends on the static part: if both e1 and e2 have the static part,
   we add the numbers and lift the sum in to the dynamic part of the result,
   disregarding the dynamic parts of e1 and e2. To obtain the static part
   of e1 and e2, we need to know h, the variable environment. It follows
   then that h must `scope' over both the static and the dynamic parts.
*)

module P(F:Symantics) = struct
  type ('c,'sv,'dv) vr = {st: 'sv option;
                          dy: ('c,'dv) F.vr}
  type ('c,'h,'sv,'dv) repr = 'h -> ('c,'sv,'dv) vr
  type h0 = H0				(* empty environment *)
  let abstr {dy = x} = x
  let habstr {dy = x} = fun (_:h0) -> x
  let pdyn x = {st = None; dy = x}
  let close x = x H0
  let vz   = fun (x,_) -> x
  let vs v = fun (_,h) -> v h
  let int  (x:int)  = fun h -> {st = Some x;
                                dy = close (F.int x)}
  let bool (x:bool) = fun h -> {st = Some x;
                                dy = close (F.bool x)}
  (* generic build - takes a repr constructor, an interpreter function
     and a compiler function (all binary) and builds a PE version *)
  let build cast f1 f2 (x,y)
      : ('c,'h,'sv,'dv) repr = fun h ->
  match (x h,y h) with
  | {st = Some m}, {st = Some n} -> cast (f1 m n) h
  | e1, e2 -> pdyn (f2 (habstr e1) (habstr e2))
  (* same as 'build' but takes care of the neutral element (e) simplification
     allowed via a monoid structure which is implicitly present *)
  let monoid cast one f1 f2 ((x,y) as ee): ('c,'h,'sv,'dv) repr = fun h ->
  match (x h,y h) with
  | {st = Some e'}, e when e' = one -> e
  | e, {st = Some e'} when e' = one -> e
  | _ -> build cast f1 f2 ee h
  (* same as above but for a ring structure instead of monoid *)
  let ring cast zero one f1 f2 ((x,y) as ee) = fun h -> match (x h, y h) with
  | ({st = Some e'} as e), _ when e' = zero -> e
  | _, ({st = Some e'} as e) when e' = zero -> e
  | _ -> monoid cast one f1 f2 ee h
  let add e1 e2 = monoid int 0 ( + ) (fun e1 e2 -> close(F.add e1 e2)) (e1,e2)
  let mul e1 e2 = ring int 0 1 ( * ) (fun e1 e2 -> close(F.mul e1 e2)) (e1,e2)
  let leq e1 e2 = build bool (<=)  (fun e1 e2 -> close(F.leq e1 e2)) (e1,e2)
  let if_ eb et ee = fun h -> match eb h with
  | {st = Some b} -> if b then et () h else ee () h
  | _ -> pdyn (close(F.if_ (habstr (eb h)) 
	               (fun () -> habstr (et () h))
	               (fun () -> habstr (ee () h))))
  let lam (e : ('c,('c,'sa,'da) vr * 'h,'sb,'db) repr) 
      : ('c,'h,('c,'sa,'da) vr -> ('c,'sb,'db) vr,'da->'db) repr
      = fun h -> {st = Some (fun x -> e (x,h));
		  dy = close(F.lam(fun (x,_) -> abstr (e ({st=None;dy=x},h))))}
  let apph e1 e2 = match e1 with
                  | {st=Some e} -> e e2
		  | _ -> {st = None;
			  dy = close(F.app (habstr e1) (habstr e2))}
  let app (e1:('c,'h,('c,'sa,'da) vr -> ('c,'sb,'db) vr,'da->'db) repr) 
      (e2:('c,'h,'sa,'da) repr) : ('c,'h,'sb,'db) repr 
      = fun h -> apph (e1 h) (e2 h)
  (* Process fix all the way *)
  let fix f = fun h ->
    let fdyn = F.fix (fun (self,_) -> abstr(f (pdyn self,h)))
    in let rec self = function
       | {st = Some _} as e -> apph (f (lam (fun (x,h) -> self x) h,h)) e
       | e -> pdyn (close (F.app fdyn (habstr e)))
    in {st = Some self; dy = close fdyn}
  let runit e = close (e ())
end;;

module PC = P(C);;
module EXP = EX2(PC);;

let ptest1 = PC.runit EXP.test1;; (* {P.st = Some 3; P.dy = .<3>.} *)
let ptest2 = PC.runit EXP.test2;; (* P.dy = .<fun x_1 -> (x_1 + x_1)>. *)
let ptest3 = PC.runit EXP.test3;; (* P.dy = .<fun x_1 -> ((x_1 1) + 2)>. *)

let ptestg  = PC.runit EXP.testgib;;
let ptestg1 = PC.runit EXP.testgib1;; (* {P.st = Some 8; P.dy = .<8>.} *)
let ptestg2 = PC.runit EXP.testgib2;;
(*
 {P.st = Some <fun>;
  P.dy =
  .<fun x_1 ->
    fun x_2 -> ((((x_2 + x_1) + x_2) + (x_2 + x_1)) + ((x_2 + x_1) + x_2))>.}
*)
let ptestg2xxx = let xxx () = PC.runit EXP.testgib2 in (xxx ()).PC.dy;;
(* Generalization fails: seems like a bug with the generalization they
   fixed in OCaml 3.10. Alas, MetaOCaml is stuck at 3.09...
let 8 = (.! ptestg2xxx) 1 1;;
*)

let ptestp  = PC.runit EXP.testpowfix;;
let ptestp7 = PC.runit EXP.testpowfix7;;
(*
{P.st = Some <fun>;
 P.dy = .<fun x_1 -> (x_1 * (x_1 * (x_1 * (x_1 * (x_1 * (x_1 * x_1))))))>.}
*)
(* let 128 = (.! (P.runit EXP.testpowfix7).P.dy) 2;; *)
let ptestp0 = PC.runit EXP.testpowfix0;;
(*
 P.dy =
  .<fun x_1 ->
   ((let rec self_6 =
      fun n_7 -> ((fun x_8 -> if (x_8 <= 0) then 1 else 0) n_7) in
     self_6) x_1)>.
*)
