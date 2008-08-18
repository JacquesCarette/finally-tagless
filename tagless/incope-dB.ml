(* Interpreter, Compiler, Partial Evaluator *)
(* deBruijn indices *)

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
*)


(* This class/type defines syntax (and its instances, semantics) 
   of our language
 *)

module type Symantics = sig
  type ('c,'h,'sv,'dv) repr
  type ('c,'sv,'dv) vr			(* variable representation *)
  val int  : int  -> ('c,'h,int,int) repr
  val bool : bool -> ('c,'h,bool,bool) repr
  val add  : ('c,'h,int,int) repr -> ('c,'h,int,int) repr -> 
             ('c,'h,int,int) repr
  val mul  : ('c,'h,int,int) repr -> ('c,'h,int,int) repr -> 
             ('c,'h,int,int) repr
  val leq  : ('c,'h,int,int) repr -> ('c,'h,int,int) repr -> 
             ('c,'h,bool,bool) repr
  val eql  : ('c,'h,int,int) repr -> ('c,'h,int,int) repr -> 
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
  type ('c,'h,'sv,'dv) repr = 'h -> 'dv    (* absolutely no wrappers *)

  type h0 = H0				(* empty environment *)
  type ('c,'s,'d) vr = 'd		(* variable representation *)
  let vz   = fun (x,_) -> x
  let vs v = fun (_,h) -> v h

  let int (x:int)   = fun h -> x
  let bool (b:bool) = fun h -> b
  let add e1 e2 = fun h -> e1 h + e2 h
  let mul e1 e2 = fun h -> e1 h * e2 h
  let leq e1 e2 = fun h -> e1 h <= e2 h
  let eql e1 e2 = fun h -> e1 h = e2 h
  let if_ eb et ee = fun h -> if eb h then (et () h) else (ee () h)

  let lam e = fun h -> fun x -> e (x,h)
  let app e1 e2 = fun h -> (e1 h) (e2 h)
  let fix e = fun h -> let rec self n = e (self,h) n in self

  let runit e = e () H0
end;;

module EXR = EX(R);;

let itest1 = R.runit EXR.test1;;
let itest2 = R.runit EXR.test2;;
let itest3 = R.runit EXR.test3;;

let itestg  = R.runit EXR.testgib;;
let itestg1 = R.runit EXR.testgib1;; (* 8 *)
let itestg2 = R.runit EXR.testgib2;;

let itestp  = R.runit EXR.testpowfix;;
let itestp7 = R.runit EXR.testpowfix7 2;; (* 128 *)
let itestp0 = R.runit EXR.testpowfix0;;


(* ------------------------------------------------------------------------ *)
(* Another interpreter: it interprets each term to give its size
   (the number of constructors)
   The interpreter never gets stuck, because it evaluates typed terms only.
   This interpreter is also total: it determines the size of the term
   even if the term itself is divergent  *)

module L = struct
  type ('c,'h,'sv,'dv) repr = int    (* absolutely no wrappers *)
  type ('c,'s,'d) vr = int	     (* variable representation *)
  let vz   = 0
  let vs v = 0

  let int (x:int) = 1
  let bool (b:bool) = 1
  let add e1 e2 = e1 + e2 + 1
  let mul e1 e2 = e1 + e2 + 1
  let leq x y = x + y + 1
  let eql x y = x + y + 1
  let if_ eb et ee = eb + et () + ee () + 1

  let lam e = e + 1
  let app e1 e2 = e1 + e2 + 1
  let fix e = e + 1

  let runit e = e ()
end;;

module EXL = EX(L);;

let ltest1 = L.runit EXL.test1;; (* 3 *)
let ltest2 = L.runit EXL.test2;; (* 2 *)
let ltest3 = L.runit EXL.test3;; (* 5 *)

let ltestg  = L.runit EXL.testgib;; (* 17 *)
let ltestg1 = L.runit EXL.testgib1;; (* 23 *)
let ltestg2 = L.runit EXL.testgib2;; (* 23 *)

let ltestp  = L.runit EXL.testpowfix;;  (* 11 *)
let ltestp7 = L.runit EXL.testpowfix7;; (* 15 *)
let ltestp0 = L.runit EXL.testpowfix0;; (* 15 *)


(* ------------------------------------------------------------------------ *)
(* Pure compiler *)

(* Note how the compiler never raises any exception and matches no tags
  (nor generated code has any tags)
*)

module C = struct
  type ('c,'h,'sv,'dv) repr = 'h -> ('c,'dv) code

  type h0 = H0				(* empty environment *)
  type ('c,'s,'a) vr = ('c,'a) code	(* variable representation *)
  let vz   = fun (x,_) -> x
  let vs v = fun (_,h) -> v h

  let int (x:int)   = fun h -> .<x>.
  let bool (b:bool) = fun h -> .<b>.
  let add e1 e2 = fun h -> .<.~(e1 h) + .~(e2 h)>.
  let mul e1 e2 = fun h -> .<.~(e1 h) * .~(e2 h)>.
  let leq e1 e2 = fun h -> .<.~(e1 h) <= .~(e2 h)>.
  let eql e1 e2 = fun h -> .<.~(e1 h) = .~(e2 h)>.
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
let ctestg1r = .! (C.runit EXC.testgib1);; (* 8 *)
let ctestg2 = C.runit EXC.testgib2;;

let ctestp  = C.runit EXC.testpowfix;;
let ctestp7 = C.runit EXC.testpowfix7;;
let ctestp7r = (.! (C.runit EXC.testpowfix7)) 2;; (* 128 *)
let ctestp0 = C.runit EXC.testpowfix0;;

(* ------------------------------------------------------------------------ *)
(* Partial evaluator *)


module P =
struct
  type ('c,'sv,'dv) record = {st: 'sv option;
                              dy: ('c,'dv) code}
  type ('c,'h,'sv,'dv) repr = 'h -> ('c,'sv,'dv) record
  type h0 = H0				(* empty environment *)
  type ('c,'sv,'dv) vr = ('c,'sv,'dv) record (* variable representation *)
  let abstr {dy = x} = x
  let pdyn x = fun h -> {st = None; dy = x}
  let vz   = fun (x,_) -> x
  let vs v = fun (_,h) -> v h
  let int  (x:int)  = fun h -> {st = Some x;
                                dy = .<x>.}
  let bool (x:bool) = fun h -> {st = Some x;
                                dy = .<x>.}
  (* generic build - takes a repr constructor, an interpreter function
     and a compiler function (all binary) and builds a PE version *)
  let build cast f1 f2 (x,y)
      : ('c,'h,'sv,'dv) repr = fun h ->
  match (x h,y h) with
  | {st = Some m}, {st = Some n} -> cast (f1 m n) h
  | e1, e2 -> pdyn (f2 (abstr e1) (abstr e2)) h
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
  let add e1 e2 = monoid int 0 (+) (fun e1 e2 -> .<.~e1 + .~e2>.) (e1,e2)
  let mul e1 e2 = ring int 0 1 ( * ) (fun e1 e2 -> .<.~e1 * .~e2>.) (e1,e2)
  let leq e1 e2 = build bool (<=)  (fun e1 e2 -> .<.~e1 <= .~e2>.) (e1,e2)
  let eql e1 e2 = failwith "eql"
  let if_ eb et ee = fun h -> match eb h with
  | {st = Some b} -> if b then et () h else ee () h
  | _ -> pdyn (.<if .~(abstr (eb h)) then .~(abstr (et () h)) 
                    else .~(abstr (ee () h))>.) h
  let lam (e : ('c,('c,'sa,'da) vr * 'h,'sb,'db) repr) 
      : ('c,'h,('c,'sa,'da) vr -> ('c,'sb,'db) vr,'da->'db) repr
      = fun h -> {st = Some (fun x -> e (x,h));
		  dy = .<fun x -> .~(abstr (e ({st=None;dy= .<x>.},h)))>.}
  let apph e1 e2 = match e1 with
                  | {st=Some e} -> e e2
		  | _ -> {st = None;
			  dy = .<.~(abstr e1) .~(abstr e2)>.}
  let app (e1:('c,'h,('c,'sa,'da) vr -> ('c,'sb,'db) vr,'da->'db) repr) 
      (e2:('c,'h,'sa,'da) repr) : ('c,'h,'sb,'db) repr 
      = fun h -> apph (e1 h) (e2 h)
  (* Process fix all the way *)
  let fix f = fun h ->
    let fdyn = .<let rec self n = .~(abstr (f (pdyn .<self>. h,h))) n in self>.
    in let rec self = function
       | {st = Some _} as e -> apph (f (lam (fun (x,h) -> self x) h,h)) e
       | e -> pdyn (.<.~fdyn .~(abstr e)>.) h
    in {st = Some self; dy = fdyn}
  let runit e = e () H0
end;;

module EXP = EX(P);;

let ptest1 = P.runit EXP.test1;;
let ptest2 = P.runit EXP.test2;;
let ptest3 = P.runit EXP.test3;;

let ptestg  = P.runit EXP.testgib;;
let ptestg1  = P.runit EXP.testgib1;; (* {P.st = Some 8; P.dy = .<8>.} *)
let ptestg2 = P.runit EXP.testgib2;;

let ptestp  = P.runit EXP.testpowfix;;
let ptestp7 = P.runit EXP.testpowfix7;;
let ptestp7r2 = (.! (P.runit EXP.testpowfix7).P.dy) 2;; (* 128 *)
let ptestp0 = P.runit EXP.testpowfix0;;


(*
  let eql e1 e2 = build bool R.eql C.eql (e1,e2)
*)