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
  type ('c,'dv) vr			(* variable representation *)
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

  val vz  : ('c,('c,'a) vr * 'h,'a,'a) repr
  val vs  : ('c, 'h,'sa,'da) repr -> ('c, _ * 'h,'sa,'da) repr
  val lam : ('c,('c,'da) vr * 'h,'sa,'db) repr -> ('c,'h,'sa,'da->'db) repr
  val app  : ('c,'h,_,'da->'db) repr -> ('c,'h,_,'da) repr -> 
             ('c,'h,_,'db) repr
  val fix : ('c,('c,'da->'db) vr * 'h, _, 'da->'db) repr -> 
            ('c, 'h, _, 'da->'db) repr
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
  type ('c,'a) vr = 'a			(* variable representation *)
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
  type ('c,'a) vr = int			(* variable representation *)
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
let ctestg1r = .! (C.runit EXC.testgib1);; (* 8 *)
let ctestg2 = C.runit EXC.testgib2;;

let ctestp  = C.runit EXC.testpowfix;;
let ctestp7 = C.runit EXC.testpowfix7;;
let ctestp7r = (.! (C.runit EXC.testpowfix7)) 2;; (* 128 *)
let ctestp0 = C.runit EXC.testpowfix0;;
