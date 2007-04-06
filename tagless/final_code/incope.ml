(* Interpreter, Compiler, Partial Evaluator *)

(*
  The language is simply-typed lambda-calculus with fixpoint,
  integers [plus basic operations], booleans [ditto] and comparison.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | Add ie1 ie2 | Mul ie1 ie2 
  B Bool |
  IF b e-then e-else
  
  The language is just expressive enough for the Fibonacci and
  power functions.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  The only tags in the PE are staging tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.
*)



(* This class/type defines syntax (and its instances, semantics) 
   of our language
 *)

module type Symantics = sig
  type ('c,+'sv,+'dv) repr
  val int  : int  -> ('c,int,int) repr
  val bool : bool -> ('c,bool,bool) repr
  val add  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val mul  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val leq  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,bool,bool) repr
  (* could be defined in terms of leq and if_ *)
  val eql  : ('c,'sa,'da) repr -> ('c,'sa,'da) repr -> ('c,bool,bool) repr
  (* The last two arguments to [if_] are functional terms.
     One of them is applied to unit.
     The reason for this charade is to prevent evaluation
     of both arguments of if_ in a CBV language.
     if_ must be a syntactic form rather than just a function.
     Or, at least it shouldn't take just (undelayed) terms.
  *)
  val if_ : ('c,bool,bool) repr ->
             (unit -> 'x) ->
             (unit -> 'x) -> (('c,'sa,'da) repr as 'x)

  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x) -> ('c,'x,'da->'db) repr
  val app : ('c,'x,'da->'db) repr -> (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x)
  val fix : ('x -> 'x) -> (('c, ('c,'sa,'da) repr -> ('c,'sb,'db) repr, 'da->'db) repr as 'x)
(*
  val unfold : ('c,'s,'a) repr -> 
               ('c,('c,'s,'a) repr -> ('c,'s,'a) repr,'a->'a) repr -> 
               ('c, ('c,int,int) repr -> ('c,'s,'a) repr, int->'a) repr
*)
(* This is used only to extract the result so we can see it nicely.
   Alternatively, we can always extract the result as code,
   especially if we don't use the interpreter.
  val get_res : ('c,'sv,'dv) repr -> ('c,'sv,'dv) result
*)

end
;;

(* A more explicit version of the above, with explicit polymorphic records.
*)

module type T = sig type ('a, 'b, 'c) repr end;;
module Q(T:T) = struct
  type ('c, 'sv, 'dv) pack = {
   int  : int  -> ('c,int,int) T.repr;
   bool : bool -> ('c,bool,bool) T.repr;
   add  : ('c,int,int) T.repr -> ('c,int,int) T.repr -> ('c,int,int) T.repr;
   mul  : ('c,int,int) T.repr -> ('c,int,int) T.repr -> ('c,int,int) T.repr;
   leq  : ('c,int,int) T.repr -> ('c,int,int) T.repr -> ('c,bool,bool) T.repr;
   eql  : ('c,'sv,'dv) T.repr -> ('c,'sv,'dv) T.repr -> ('c,bool,bool) T.repr;
   if_ : ('c,bool,bool) T.repr ->
             (unit -> ('c,'sv,'dv) T.repr) ->
             (unit -> ('c,'sv,'dv) T.repr) -> ('c,'sv,'dv) T.repr;
   lam : 'sa 'sb 'da 'db . (('c,'sa,'da) T.repr -> ('c,'sb,'db) T.repr)
    -> ('c,(('c,'sa,'da) T.repr -> ('c,'sb,'db) T.repr),'da->'db) T.repr;
   app : 'da 'db 'sa 'sb . 
    ('c,(('c,'sa,'da) T.repr -> ('c,'sb,'db) T.repr),'da->'db) T.repr
    -> ('c,'sa,'da) T.repr -> ('c,'sb,'db) T.repr;
   fix : 'sa 'sb 'da 'db . 
      (('c,(('c,'sa,'da) T.repr -> ('c,'sb,'db) T.repr) as 's,'da->'db) T.repr 
             -> ('c,'s,'da->'db) T.repr)  -> ('c,'s,'da->'db) T.repr
  }
end ;;

(* Running example *)

module EX(S: Symantics) = struct
 open S
 module QQ = Q(S)
 open QQ
 let p = {
     int = S.int; bool = S.bool; add = S.add; mul = S.mul; leq = S.leq; 
     eql = S.eql; if_ = S.if_; lam = S.lam; app = S.app; fix = S.fix}

 let test1 e = e.add (e.int 1) (e.int 2)
 let test2 e = e.lam (fun x -> e.add x x)
 let test3 e = e.lam (fun x -> e.add (e.app x (e.int 1)) (e.int 2))

 let testgib e = e.lam (fun x -> e.lam (fun y ->
                  e.fix (fun self -> e.lam (fun n ->
                      e.if_ (e.leq n (e.int 0)) (fun () -> x)
                        (fun () ->
                          (e.if_ (e.leq n (e.int 1)) (fun () -> y)
                           (fun () ->
                             (e.add (e.app self (e.add n (e.int (-1))))
                                    (e.app self (e.add n (e.int (-2))))))))))))

 let testgib1 e = e.app (e.app (e.app (testgib e) (e.int 1)) (e.int 1)) (e.int 5)
 let testgib2 e = e.lam (fun x -> (e.lam (fun y ->
   e.app (e.app (e.app (testgib e) x) y) (e.int 5))))

 let testpowfix e = e.lam (fun x ->
                      e.fix (fun self -> e.lam (fun n ->
                        e.if_ (e.leq n (e.int 0)) (fun () -> e.int 1)
                            (fun () -> e.mul x (e.app self (e.add n (e.int (-1))))))))

 let testpowfix7 e = e.lam (fun x -> e.app (e.app (testpowfix e) x) (e.int 7))
 let testpowfix0 e = e.lam (fun x -> e.app (e.app (testpowfix e) (e.int 0)) x)

 (* This is not really needed anymore 
 let interp prog =
     p.app (p.app (p.app (p.app (p.app (p.app (p.app (p.app (p.app (p.app prog
       (p.lam (fun (x:('a,int,int) S.repr) -> x)))
       (p.lam (fun (b:('a,bool,bool) S.repr) -> b)))
       (p.lam (fun e1 -> p.lam (fun e2 -> p.add e1 e2))))
       (p.lam (fun e1 -> p.lam (fun e2 -> p.mul e1 e2))))
       (p.lam (fun e1 -> p.lam (fun e2 -> p.leq e1 e2))))
       (p.lam (fun e1 -> p.lam (fun e2 -> p.eql e1 e2))))
       (p.lam (fun eb -> p.lam (fun et -> p.lam (fun ee -> p.if_ eb (fun () ->
           et) (fun () -> ee))))))
       (p.lam (fun f  -> f)))
       (p.lam (fun e1 -> p.lam (fun e2 -> p.app e1 e2))))
       (p.lam (fun f  -> p.lam (fun n -> p.app (p.app (
           p.fix (fun fx -> p.lam (fun f -> p.lam (fun n -> 
              p.app (p.app f (p.app fx f)) n)))) f) n)))

 let test1' e = 
     e.lam (fun int ->
     e.lam (fun bool ->
     e.lam (fun add ->
     e.lam (fun mul ->
     e.lam (fun leq ->
     e.lam (fun eql ->
     e.lam (fun if_ ->
     e.lam (fun lam ->
     e.lam (fun app ->
     e.lam (fun fix ->
         e.app (e.app add (e.app int (e.int 1))) (e.app int (e.int 2))
     ))))))))))
 
 let i1 e = interp (test1' e)

 let test_interp () = p.lam interp
 
 (* self-interpreter application ! *)
 let i2 prog = interp (p.app (test_interp ()) prog)

 (* but it is not quite an interpreter, as
 let i3 e = i2 (test1' e)
    does not work.  Seems like the failure of rank-2 polymorphism again? *)
 *)

 let runit t = t p

 let test1r () = runit test1
 let test2r () = runit test2
 let test3r () = runit test3

 let testgibr () = runit testgib
 let testgib1r () = runit testgib1
 let testgib2r () = runit testgib2

 let testpowfixr () = runit testpowfix
 let testpowfix7r () = runit testpowfix7
 let testpowfix0r () = runit testpowfix0
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
  let eql x y = x = y
  let if_ eb et ee = if eb then (et ()) else (ee ())

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f = let rec self n = f self n in self
end;;

module EXR = EX(R);;

(* ------------------------------------------------------------------------ *)
(* Another interpreter: it interprets each term to give its size
   (the number of constructors)
   It is a typed, tagless interpreter: L is not a tag. The interpreter
   never gets stuck, because it evaluates typed terms only.
   This interpreter is also total: it determines the size of the term
   even if the term itself is divergent  *)

module L = struct
  type ('c,'sv,'dv) repr = int    (* absolutely no wrappers *)
  let int (x:int) = 1
  let bool (b:bool) = 1
  let add e1 e2 = e1 + e2 + 1
  let mul e1 e2 = e1 + e2 + 1
  let leq x y = x + y + 1
  let eql x y = x + y + 1
  let if_ eb et ee = eb + et () + ee () + 1

  let lam f = (f 0) + 1
  let app e1 e2 = e1 + e2 + 1
  let fix f = (f 0) + 1
end;;

module EXL = EX(L);;

(* ------------------------------------------------------------------------ *)
(* Pure compiler *)

(* Note how the compiler never raises any exception and matches no tags
  (nor generated code has any tags)
*)

module C = struct
  type ('c,'sv,'dv) repr = ('c,'dv) code
  let int (x:int) = .<x>.
  let bool (b:bool) = .<b>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let mul e1 e2 = .<.~e1 * .~e2>.
  let leq x y = .< .~x <= .~y >.
  let eql x y = .< .~x = .~y >.
  let if_ eb et ee = 
    .<if .~eb then .~(et () ) else .~(ee () )>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<let rec self n = .~(f .<self>.) n in self>.
end;;

module EXC = EX(C);;

(* ------------------------------------------------------------------------ *)
(* Partial evaluator *)

module P =
struct
  type ('c,'sv,'dv) repr = {st: 'sv option; dy: ('c,'dv) code}
  type ('a,'s,'v) result = RL of 's | RC of ('a,'v) code;;
  let abstr {dy = x} = x
  let pdyn x = {st = None; dy = x}

  let int  (x:int)  = {st = Some (R.int x);
                       dy = C.int x}
  let bool (x:bool) = {st = Some (R.bool x);
                       dy = C.bool x}

  (* generic build - takes a repr constructor, an interpreter function
     and a compiler function (all binary) and builds a PE version *)
  let build cast f1 f2 = function
  | {st = Some m}, {st = Some n} -> cast (f1 m n)
  | e1, e2 -> pdyn (f2 (abstr e1) (abstr e2))
  (* same as 'build' but takes care of the neutral element (e) simplification
     allowed via a monoid structure which is implicitly present *)
  let monoid cast one f1 f2 = function
  | {st = Some e'}, e when e' = one -> e
  | e, {st = Some e'} when e' = one -> e
  | ee -> build cast f1 f2 ee
  (* same as above but for a ring structure instead of monoid *)
  let ring cast zero one f1 f2 = function
  | ({st = Some e'} as e), _ when e' = zero -> e
  | _, ({st = Some e'} as e) when e' = zero -> e
  | ee -> monoid cast one f1 f2 ee

  let add e1 e2 = monoid int 0 R.add C.add (e1,e2)
  let mul e1 e2 = ring int 0 1 R.mul C.mul (e1,e2)
  let leq e1 e2 = build bool R.leq C.leq (e1,e2)
  let eql e1 e2 = build bool R.eql C.eql (e1,e2)
  let if_ eb et ee = match eb with
  | {st = Some b} -> if b then et () else ee ()
  | _ -> pdyn (C.if_ (abstr eb) 
                     (fun () -> abstr (et ()))
                     (fun () -> abstr (ee ())))

  let lam f =
  {st = Some f; 
   dy = C.lam (fun x -> abstr (f (pdyn x)))}

  let app ef ea = match ef with
  | {st = Some f} -> f ea
  | _ -> pdyn (C.app (abstr ef) (abstr ea))

   (*
     For now, to avoid divergence at the PE stage, we residualize.
     Actually, we unroll the fixpoint exactly once, and then
     residualize
   *)
  let fix f = f (pdyn (C.fix (fun x -> abstr (f (pdyn x)))))
  (* this should allow us controlled unfolding *)
  (*
  let unfold z s = lam (function
      | {st = Some n} -> 
              let rec f k = if k<=0 then z else
                                match s with
                                | {st = Some m} -> m (f (k-1))
                                | {dy = y}      -> pdyn (C.app y (abstr (f (k-1))))
              in f n
      | {dy = y}      -> pdyn (C.app (C.unfold (abstr z) (abstr s)) y))
  *)

  let get_res t = match t with
      | {st = (Some y) } -> RL y
      | _                -> RC (abstr t)
end;;

(* Alternatively, we process fix all the way, provided the computation
   is static.
*)
module P1 =
struct
  include P
(*
  type ('c,'sv,'dv) repr = {st: 'sv option; dy: ('c,'dv) code}
  let abstr {dy = x} = x
  let pdyn x = {st = None; dy = x}
*)
  let fix f =
    let fdyn = C.fix (fun x -> abstr (f (pdyn x)))
    in let rec self = function
       | {st = Some _} as e -> app (f (lam self)) e
       | e -> pdyn (C.app fdyn (abstr e))
       in {st = Some self; dy = fdyn}
end;;


module EXP = EX(P1);;

(* all the tests together *)
let itest1 = EXR.test1r ();;
let ctest1 = EXC.test1r ();;
let ptest1 = EXP.test1r ();;
let ltest1 = EXL.test1r ();;

let itest2 = EXR.test2r ();;
let ctest2 = EXC.test2r ();;
let ptest2 = EXP.test2r ();;
let ltest2 = EXL.test2r ();;

let itest3 = EXR.test3r ();;
let ctest3 = EXC.test3r ();;
let ptest3 = EXP.test3r ();;
let ltest3 = EXL.test3r ();;

let itestg = EXR.testgibr ();;
let ctestg = EXC.testgibr ();;
let ptestg = EXP.testgibr ();;
let ltestg = EXL.testgibr ();;

let itestg1 = EXR.testgib1r ();;
let ctestg1 = EXC.testgib1r ();;
let ptestg1 = EXP.testgib1r ();;
let ltestg1 = EXL.testgib1r ();;

let itestg2 = EXR.testgib2r ();;
let ctestg2 = EXC.testgib2r ();;
let ptestg2 = EXP.testgib2r ();;
let ltestg2 = EXL.testgib2r ();;

let itestp7 = EXR.testpowfix7r ();;
let ctestp7 = EXC.testpowfix7r ();;
let ptestp7 = EXP.testpowfix7r ();;
let ltestp7 = EXL.testpowfix7r ();;

let itestp0 = EXR.testpowfix0r ();;
let ctestp0 = EXC.testpowfix0r ();;
let ptestp0 = EXP.testpowfix0r ();;
let ltestp0 = EXL.testpowfix0r ();;

(* ------------------------------------------------------------------------ *)
(* CPS CBN interpreter *)

(* Pure CPS interpreter. *)
(* We make the CPS to be fully polymorphic over the answer type.
   We could have just as well made RCN a functor, parameterized 
   over the answer type (as is common in SML). But because we have 
   higher-rank types in OCaml, we may as well use them.
*)

(* Call-by-name interpreter *)
module RCN = struct
  type ('c,'sv,'dv) repr = {ko: 'w. ('sv -> 'w) -> 'w}
  let int (x:int) = {ko = fun k -> k x}
  let bool (b:bool) = {ko = fun k -> k b}
  let add e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1+v2)))}
  let mul e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1*v2)))}
  let leq e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1 <= v2)))}
  let eql e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1 = v2)))}
  let if_ eb et ee = 
    {ko = fun k -> eb.ko (fun vb -> if vb then (et ()).ko k else (ee ()).ko k)}

  let lam f = {ko = fun k -> k f} (* weird CPS: it's actually a CBN CPS! *)

  let app e1 e2 = {ko = fun k -> e1.ko (fun f -> (f e2).ko k)}
  (* The following is the `imperative' part, dealing with the state *)
  (* because our CPS is CBN, we have to force the evaluation of e2! *)
  let lapp e2 e1 = 
    {ko = fun k -> e2.ko (fun v -> (app (lam e1) {ko = fun k -> k v}).ko k)}

  let fix f = let rec fx f n = app (f (lam (fx f))) n in lam(fx f)

  let get_res x = x.ko (fun v -> v)
end;;

(* Emulating CBV my modifying the interpretation of lam to force
   evaluation of the argument, always.
*)
module RCV = struct
  include RCN
  let lam f = {ko = 
    fun k -> k (fun e -> e.ko (fun v -> f {ko = fun k -> k v}))}
end;;

(* A simpler test *)
module EXS(S: Symantics) = struct
 open S
 let test1 () = app (lam (fun x -> x)) (bool true)
 let testpowfix () = 
   lam (fun x ->fix (fun self -> lam (fun n ->
     if_ (leq n (int 0)) (fun () -> int 1)
         (fun () -> mul x (app self 
                               (add n (int (-1))))))))
 let testpowfix7 = 
    lam (fun x -> app (app (testpowfix ()) x) (int 7))
 let testpowfix72 = 
    app testpowfix7 (int 2)
 (* A test to illustrate the difference between CBV and CBN *)
 let diverg () = app (lam (fun x -> int 1)) 
                     (app (fix (fun self -> self)) (int 2))
end;;

module EXRCN = EXS(RCN);;
let rcntest1 = RCN.get_res (EXRCN.test1 ());;
let rcntestpw = RCN.get_res (EXRCN.testpowfix ());;
let rcntestpw72 = RCN.get_res (EXRCN.testpowfix72);;
let ndiverg = RCN.get_res (EXRCN.diverg ());;

module EXRCV = EXS(RCV);;
let rcvtest1 = RCV.get_res (EXRCV.test1 ());;
let rcvtestpw72 = RCV.get_res (EXRCV.testpowfix72);;
(* diverges
 let vdiverg = RCV.get_res (EXRCV.diverg ());;
*)

module EXSV = EXS(R);;
let rstest1 = EXSV.test1 ();;
let rstestpw72 = EXSV.testpowfix72;;
(* diverges
 let rsdiverg = EXSV.diverg ();;
*)


(* ------------------------------------------------------------------------ *)
(* CPS transformers *)

(* Simplified Symantics modules with no 'sv *)
module type SymS = sig
  type ('c,+'dv) repr
  val int : int  -> ('c,int) repr
  val bool: bool -> ('c,bool) repr
  val add : ('c,int) repr-> ('c,int) repr-> ('c,int) repr
  val mul : ('c,int) repr-> ('c,int) repr-> ('c,int) repr
  val leq : ('c,int) repr-> ('c,int) repr-> ('c,bool) repr
  val if_ : ('c,bool) repr ->
             (unit -> ('c,'da) repr) ->
             (unit -> ('c,'da) repr) -> ('c,'da) repr 
  val lam : (('c,'da) repr -> ('c,'db) repr) 
          -> ('c,'da->'db) repr
  val app : ('c,'da->'db) repr
    -> ('c,'da) repr -> ('c,'db) repr
  val fix : (('c,'da->'db) repr -> ('c,'da->'db) repr) 
            -> ('c,'da->'db) repr
end;;

module RS = struct
  type ('c,'dv) repr = 'dv    (* absolutely no wrappers *)
  let int (x:int) = x
  let bool (b:bool) = b
  let add e1 e2 = e1 + e2
  let mul e1 e2 = e1 * e2
  let leq x y = x <= y
  let eql x y = x = y
  let if_ eb et ee = if eb then (et ()) else (ee ())

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f = let rec self n = f self n in self
end;;

module CS = struct
  type ('c,'dv) repr = ('c,'dv) code
  let int (x:int) = .<x>.
  let bool (b:bool) = .<b>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let mul e1 e2 = .<.~e1 * .~e2>.
  let leq x y = .< .~x <= .~y >.
  let eql x y = .< .~x = .~y >.
  let if_ eb et ee = 
    .<if .~eb then .~(et () ) else .~(ee () )>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<let rec self n = .~(f .<self>.) n in self>.
end;;


module CPST(S: Symantics) = struct
 (*  type w = unit *)
 (*  type ('c,'dv) repr = ('c, ('dv -> w)->w) S.repr *)
  let int i = S.lam (fun k -> S.app k (S.int i))
  let bool b = S.lam (fun k -> S.app k (S.bool b))
  let add e1 e2 = S.lam (fun k ->
    S.app e1 (S.lam (fun v1 ->
    S.app e2 (S.lam (fun v2 -> S.app k (S.add v1 v2))))))
  let mul e1 e2 = S.lam (fun k ->
    S.app e1 (S.lam (fun v1 ->
    S.app e2 (S.lam (fun v2 -> S.app k (S.mul v1 v2))))))
  let leq e1 e2 = S.lam (fun k ->
    S.app e1 (S.lam (fun v1 ->
    S.app e2 (S.lam (fun v2 -> S.app k (S.leq v1 v2))))))
  let if_ ec et ef = S.lam (fun k ->
    S.app ec (S.lam (fun vc ->
    S.if_ vc (fun () -> S.app (et ()) k) (fun () -> S.app (ef ()) k))))
  let lam f = S.lam (fun k -> S.app k (S.lam (fun x ->
    f (S.lam (fun k -> S.app k x)))))
  let app e1 e2 = S.lam (fun k -> 
    S.app e1 (S.lam (fun f ->
    S.app e2 (S.lam (fun v -> S.app (S.app f v) k)))))
  let fix f  = S.fix (fun self -> (f self))
end;;

module T = struct
 module M = CPST(P)
 open M
 let test1 () = app (lam (fun x -> x)) (bool true)
 let test2 () = lam (fun x -> add x (int 1))
 (* let tfix () = app (fix (fun self -> self)) (int 1) *)
 let tif () = if_ (bool true) (fun () -> (add (int 2) (int 1)))
                  (fun () -> int 2)
 (*let tfix1 () = fix (fun self -> lam (fun n -> app self (add n (int 1)))) *)
 let tfix1 () = fix (fun self -> (lam (fun m -> (int 1))))

 let tfix3 () = app (tfix1 ()) (int 2)
 let testpowfix () = 
   lam (fun x ->fix (fun self -> lam (fun n ->
     if_ (leq n (int 0)) (fun () -> int 1)
         (fun () -> mul x (app self 
                               (add n (int (-1))))))))
 let testpowfix7 = 
    lam (fun x -> app (app (testpowfix ()) x) (int 7))
 let testpowfix72 = 
    app testpowfix7 (int 2)
end;;

let ctest1 = T.test1 ();;
(* let ctestfix = T.tfix ();; *)
let ctestif = T.tif ();;
let ctestfix = T.tfix1 ();;
let ctestfix = T.testpowfix ();;
(*
let ctestfix = T.testpowfix7;;
 let ctestfix = T.testpowfix72 (fun x -> x);;
let ctestfix = T.testpowfix72;;
*)


(* Extension of S for an imperative language with a single piece of
   state.

        let x = e1 in e2
        deref ()
        set e (returning the old value of the state)
        The optional 
                begin e1; e2 end
        is just 
                let dummy=e1 in e2

 Since we use higher-order abstract syntax, let x = e1 in e2
 is just lapp e1 (\x -> e2), which is an inverse application.
 Some may call it `bind'.
*)

module type SymSI = sig
  include Symantics
  type state
  val lapp : ('c,'sa,'da) repr -> (('c,'sa,'da) repr -> ('c,'sb,'db) repr)
    ->  ('c,'sb,'db) repr
  val deref : unit -> ('c,state,state) repr
  val set   : ('c,state,state) repr -> ('c,state,state) repr
end;;

(* INT state *)
module EXSI_INT(S: SymSI with type state = int) = struct
  open S

  (* this program corresponds to 
     let v0 = !state in
       state := 2;
       v0 + !state; *)
  let test1 () = lapp (deref ()) (fun v0 -> 
                  lapp (set (int 2)) (fun _ ->
		   add v0 (deref ())))
      (* Here we know the evaluation is left-to-right *)
  (* (state := 2) + !state *)
  let test2 () = add (set (int 2)) (deref ())
      (* imperative power *)
      (* fun x -> let _ = (state := 1) in
           fix (fun self -> fun n ->
               if n<=0 then !state
               else let _ = state := !state * x in self(n-1)) *)
  let pow () = lam (fun x -> lapp (set (int 1)) (fun _ -> 
		  fix (fun self ->
		  lam (fun n ->
		    if_ (leq n (int 0)) (fun () -> deref ())
			(fun () -> 
			  lapp (set (mul (deref ()) x)) (fun _ ->
			    (app self (add n (int (-1))))))))))
  let pow7 () = lam (fun x -> app (app (pow ()) x) (int 7))
  let pow27 () = app (pow7 ()) (int 2)
end;;

(* Pure state passing CPS interpreter. *)
(* We make the CPS to be fully polymorphic over the answer type.
   We could have just as well put the answer type into the ST signature
   below (as common in SML). But because we have higher-rank types
   in OCaml, we may as well use them.
*)
module RCPS(ST: sig type state end) = struct
  type state = ST.state
  type ('c,'sv,'dv) repr = {ko: 'w. ('sv -> state -> 'w) -> state -> 'w}
  type ('c, 'sv, 'dv) result = state -> 'sv
  let int (x:int) = {ko = fun k -> k x}
  let bool (b:bool) = {ko = fun k -> k b}
  let add e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1+v2)))}
  let mul e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1*v2)))}
  let leq e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1 <= v2)))}
  let eql e1 e2 = 
    {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1 = v2)))}
  let if_ eb et ee = 
    {ko = fun k -> eb.ko (fun vb -> if vb then (et ()).ko k else (ee ()).ko k)}

(*
  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr)
    -> ('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr),'da->'db) repr
  val app : ('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr),'da->'db) repr
    -> ('c,'sa,'da) repr -> ('c,'sb,'db) repr
  val fix : (('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr) as 's,'da->'db) repr 
             -> ('c,'s,'da->'db) repr)  -> ('c,'s,'da->'db) repr
*)

  let lam f = {ko = fun k -> k f} (* weird CPS: it's actually a CBN CPS! *)

  let app e1 e2 = {ko = fun k -> e1.ko (fun f -> (f e2).ko k)}

  let fix f = let rec fx f n = app (f (lam (fx f))) n in lam(fx f)

  let get_res x = fun s0 -> x.ko (fun v s -> v) s0
  (* let run x s0 = x.ko (fun v s -> v) s0 *)

  (* The following is the `imperative' part, dealing with the state *)
  (* because our CPS is CBN, we have to force the evaluation of e2! *)
  let lapp e2 e1 = 
    {ko = fun k -> e2.ko (fun v -> (app (lam e1) {ko = fun k -> k v}).ko k)}
  let deref () = {ko = fun k s -> k s s}
  let set e = {ko = fun k -> e.ko (fun v s -> k s v)}
end;;

module RCPSI = RCPS(struct type state = int end);;
module EXPSI = EX(RCPSI);;

(*
let cpsitest1 = (EXPSI.test1r ()) 100;;
let cpsitest2 = (EXPSI.test2r ()) 100;;
let cpsitest3 = (EXPSI.test3r ()) 100;;
let cpsitestg = (EXPSI.testgibr ()) 100;;
let cpsitestg1 = (EXPSI.testgib1r ()) 100;;
let cpsitestg2 = (EXPSI.testgib2r ()) 100;;
let cpsitestp7 = (EXPSI.testpowfix7r ()) 100;;
*)

module EXPSI_INT = EXSI_INT(RCPSI);;
let cpsitesti1 = RCPSI.get_res (EXPSI_INT.test1 ()) 100;; (* 102 *)
let cpsitesti2 = RCPSI.get_res (EXPSI_INT.test2 ()) 100;; (* 102 *)
let cpsipow = RCPSI.get_res (EXPSI_INT.pow ()) 100;;
let cpsipow7 = RCPSI.get_res (EXPSI_INT.pow7 ()) 100;;
let cpsipow27 = RCPSI.get_res (EXPSI_INT.pow27 ()) 100;;

(* Extension of S for an imperative language

        let x = e1 in e2
        newref e
        deref e
        set e1 e2 (returning the old value of e1)
        The optional 
                begin e1; e2 end
        is just 
                let dummy=e1 in e2

 Since we use higher-order abstract syntax, let x = e1 in e2
 is just lapp e1 (\x -> e2), which is an inverse application.

*)
module type SymSP = sig
  include Symantics
  type 'a rf
  val newref : ('c,'sa,'da) repr -> ('c,'sa rf,'da rf) repr 
  val deref : ('c,'sa rf,'da rf) repr -> ('c,'sa,'da) repr
  val setref : ('c,'sa rf,'da rf) repr -> ('c,'sa,'da) repr
              -> ('c,'sa,'da) repr
  val lapp : ('c,'sa,'da) repr ->
    (('c,'sa,'da) repr -> ('c,'sb,'db) repr)
    -> ('c,'sb,'db) repr
end;;

module RR = struct
  include R
  type 'a rf = 'a ref
  let newref v = ref v
  let deref  v = ! v
  let setref v nv = let ov = !v in v := nv; ov
  let lapp e1 e2 = app (lam e2) e1
end;;

module CR = struct
  include C
  type 'a rf = 'a ref
  let newref v = .<ref .~v>.
  let deref  v = .<! (.~v)>.
  let setref v nv = .<let rv = .~v in let ov = !rv in rv := .~nv; ov>.
  let lapp e1 e2 = app (lam e2) e1
end;;

(* Doing PE with references may be problematic. It is not clear what
  to do wif we encounter newref with the static code, but then need
  to residualize. We have to `import' the reference and its contents.
  We skip this for now.
*)
module EXSP(S: SymSP) = struct
  open S

  (* this program corresponds to 
     let v0 = !state in
       state := 2;
       v0 + !state; *)
  let testi1 () = lapp (newref (int 1)) (fun r ->
                  lapp (deref r) (fun v0 -> 
                  lapp (setref r (int 2)) (fun _ ->
		   add v0 (deref r))))
  (* the same but with higher order *)
  let testi2 () = lapp (newref (lam (fun x -> (add x (int 1))))) (fun r ->
                  lapp (deref r) (fun v0 ->
		  lapp (setref r (lam (fun x -> (mul x x)))) (fun _ ->
		    add (app v0 (int 2)) (app (deref r) (int 4)))))
      (* imperative power *)
  let pow () = lam (fun x -> 
               lapp (newref (int 1)) (fun r ->
		  fix (fun self ->
		  lam (fun n ->
		    if_ (leq n (int 0)) (fun () -> deref r)
			(fun () -> 
			  lapp (setref r (mul (deref r) x)) (fun _ ->
			    (app self (add n (int (-1))))))))))
  let pow7 () = lam (fun x -> app (app (pow ()) x) (int 7))
  let pow27 () = app (pow7 ()) (int 2)
end;;

module EXSPR = EXSP(RR);;
let testi1 = EXSPR.testi1 ();;
let testi2 = EXSPR.testi2 ();;
let testi27 = EXSPR.pow27 ();;


module EXSPC = EXSP(CR);;
let testi1 = EXSPC.testi1 ();;
let testi2 = EXSPC.testi2 ();;
let testi27 = EXSPC.pow27 ();;

(*
module type SI = sig
  include S
  val lapp : ('c,'sa,'da) repr ->
    ('c,(('c,'sa,'da) repr -> ('c,'sb,'db) repr),'da->'db) repr
    -> ('c,'sb,'db) repr
  val 
*)

(* The following shows that splitting a type variable doesn't
   help for module subtyping. A split (constrained) type variable
   is less polymorphic, so it won't pass where the signature demands
   unconstrained variable.

module type S1 = sig
  type ('a,'v) rep
  val vi : int -> ('a,int) rep
end;;

module R1 = struct
  type ('a,'v) rep = ('b, ('v->'w) -> 'w) code constraint 'a = 'b * 'w
  let vi (x:int) = .<fun k -> k x>.
end;;

module E1(S:S1) = struct
  let t () = S.vi 1
end;;

module E11 = E1(R1);;
*)
