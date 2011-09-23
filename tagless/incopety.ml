(* Interpreter, Compiler, Partial Evaluator *)

(*
  The language is simply-typed lambda-calculus with fixpoint,
  integers [plus basic operations], booleans [ditto] and comparison.

  Lam hoas_fn | App e e | Fix hoas_fn |
  I Int | Add ie1 ie2 | Mul ie1 ie2 
  B Bool |
  IF b e-then e-else
  
  The language is just expressive enough for the Gibonacci and
  power functions.

  The compiler, the interpreter and the source and target languages
  are *all* typed. The interpreter and the compiler use no tags.
  There is no pattern-match failure possible: the evaluators never
  get stuck.

  What we try to add in this variant is types at a different level.
  We will simulate types by values.  The point here is to show the
  relation between type checking, type inference, etc and 
  interpreters, compilers and (most interestingly) partial evaluation.
  Of course, the main link is abstract interpretation!
*)


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
  val eql  : ('c,'sa,'da) repr -> ('c,'sa,'da) repr -> ('c,bool,bool) repr
  val if_ : ('c,bool,bool) repr ->
             (unit -> 'x) ->
             (unit -> 'x) -> (('c,'sa,'da) repr as 'x)

  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x) -> ('c,'x,'da->'db) repr
  val app : ('c,'x,'da->'db) repr -> (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x)
  val fix : ('x -> 'x) -> (('c, ('c,'sa,'da) repr -> ('c,'sb,'db) repr, 'da->'db) repr as 'x)
end
;;

(* Running example *)

module EX(S: Symantics) = struct
 open S

 let test1 () = S.add (S.int 1) (S.int 2)
 let test2 () = S.lam (fun x -> S.add x x)
 let test2b () = S.lam (fun x -> S.add x (S.int 1))
 let test3 () = S.lam (fun x -> S.add (S.app x (S.int 1)) (S.int 2))
 let test4 () = S.lam (fun x -> S.lam (fun y -> 
     S.if_ (S.leq x (S.int 0)) (fun () -> S.add x y)
                               (fun () -> S.mul x y)))

 let testgib () = S.lam (fun x -> S.lam (fun y ->
                  S.fix (fun self -> S.lam (fun n ->
                      S.if_ (S.leq n (S.int 0)) (fun () -> x)
                        (fun () ->
                          (S.if_ (S.leq n (S.int 1)) (fun () -> y)
                           (fun () ->
                             (S.add (S.app self (S.add n (S.int (-1))))
                                    (S.app self (S.add n (S.int (-2))))))))))))

 let testgib1 () = S.app (S.app (S.app (testgib ()) (S.int 1)) (S.int 1)) (S.int 5)
 let testgib2 () = S.lam (fun x -> (S.lam (fun y ->
   S.app (S.app (S.app (testgib ()) x) y) (S.int 5))))

 let testpowfix () = S.lam (fun x ->
                      S.fix (fun self -> S.lam (fun n ->
                        S.if_ (S.leq n (S.int 0)) (fun () -> S.int 1)
                            (fun () -> S.mul x (S.app self (S.add n (S.int (-1))))))))

 let testpowfix7 () = S.lam (fun x -> S.app (S.app (testpowfix ()) x) (S.int 7))
 let testpowfix0 () = S.lam (fun x -> S.app (S.app (testpowfix ()) (S.int 0)) x)

 let runit t = t ()

 let test1r () = runit test1
 let test2r () = runit test2
 let test2br () = runit test2b
 let test3r () = runit test3
 let test4r () = runit test4

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

(* We need some meta-types that are shared by everyone 
   The most important thing to notice is that this is an _intentional_ 
   type, in that the information contained in a function application 
   is not forgotten.  If it were, then I don't know how to write a
   typechecker in direct style.  *)
type base     = IntT | BoolT 
type allt     = Base of base 
              | AnyT of int
              | Function of (allt * allt)

(* our type representation will be alt above *)
type typerep     = allt

(* Second attempt.  This is part checking, part inference.  We get
   passed a type, and we return either a *specialization* of that type
   [if there is one which will work], or None.  *)
type typecheck2   = typerep -> (typerep option)

(* Unifier.  Convention: negative indices for polymorphic variables are
   "fresh", and will unify with anything.  *)
let unify (t1:allt) (t2:allt) : allt option = 
    let rec unify_with_subs u1 u2 s = match (u1,u2) with
    | (Base IntT, Base IntT)   -> s, Some (Base IntT)
    | (Base BoolT, Base BoolT) -> s, Some (Base BoolT)
    | (AnyT x, y)              -> (x,y)::s, Some y
    | (x, AnyT y)              -> (y,x)::s, Some x
    | (Function(t1,t2), Function(t3,t4)) -> begin
        match unify_with_subs t1 t3 s with
        | (s1,Some z) -> begin match unify_with_subs t2 t4 s1 with
            | (s2, Some zz) -> s2, Some(Function(z, zz))
            | _             -> s, None end
        | _           -> s, None end
    | _                        -> s, None
in snd (unify_with_subs t1 t2 [])

(* This code really ought to be rewritten using pa_monad for the 
   Maybe monad!  Right now, the code is astonishingly ugly as it is
   expanded monadic code, blech.  Later. *)
module RT2 = struct
  type ('c,'sv,'dv) repr = typecheck2

  let int (x:int) = unify (Base IntT)
  let bool (x:bool) = unify (Base BoolT)
  (* No point matching x and y's return since IntT is not specializable *)
  let add x y = fun t -> 
      let x1 = x (Base IntT) and y1 = y (Base IntT) in
      match (x1,y1) with
      | (Some x2, Some y2) -> unify (Base IntT) t
      | _                  -> None
  let mul x y = fun t -> 
      let x1 = x (Base IntT) and y1 = y (Base IntT) in
      match (x1,y1) with
      | (Some x2, Some y2) -> unify (Base IntT) t
      | _                  -> None
  let leq x y = fun t -> 
      let x1 = x (Base IntT) and y1 = y (Base IntT) in
      match (x1,y1) with
      | (Some x2, Some y2) -> unify (Base BoolT) t
      | _                  -> None
  (* But now we can really make this work.  We get actual types out
     of checking the arguments, and we can make sure that what we get
     is in fact equal.  And here we really mean equality! *)
  let eql x y = fun t -> match unify (Base BoolT) t with
      | Some (Base BoolT) -> begin
          let xt = x (AnyT(-1)) and yt = y (AnyT(-1)) in
          match (xt,yt) with
          | (Some t1, Some t2) when t1 = t2 -> Some t
          | _                               -> None
          end
      | _                                   -> None
  (* Here we again have an almost problem: we really have to have that
     the 'then' and 'else' clause are the same types.  But since that
     is the result type, then we can just test it twice, and that will 
     work *)
  let if_ eb et ee = fun t -> match (eb (Base BoolT)) with
      | Some _ -> begin let etc = (et () t) and eec = (ee () t) in 
                  match (etc, eec) with 
                  | (Some t1, Some t2) -> unify t1 t2
                  | _                  -> None end
      | None   -> None
  let lam f = fun t -> match t with
      | Function(x,y) -> let res = f (fun _ -> Some x) y in begin
          match res with
          | Some z -> begin match unify z y with 
              | Some zz -> Some(Function(x, zz))
              | _       -> None end
          | _      -> None end
      | _             -> None
  let app f c = fun t -> let ctyp = c (AnyT(-1)) in
      match ctyp with
      | Some cc -> 
          let res = f(Function(cc,t)) in begin
          match res with
          | Some(Function(x1,x2)) -> Some x2
          | _                     -> None end
      | None -> None
  let fix f = fun t -> match t with
      | Function(x,y) -> let res = f (fun _ -> Some t) t in begin
          match res with
          | Some z -> unify t z
          | None   -> None end
      | _             -> None
end;;

module EXRT2 = EX(RT2);;

(* ------------------------------------------------------------------------ *)
(* Another interpreter: it interprets each term to give its size
   (the number of constructors)
   The interpreter never gets stuck, because it evaluates typed terms only.
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

  let lam f = f 0 + 1
  let app e1 e2 = e1 + e2 + 1
  let fix f = f 0 + 1
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
  (* let unfold z s = .<let rec f n = if n <= 0 then .~z else .~s (f (n-1)) in f>. *)
end;;

module EXC = EX(C);;

(* ------------------------------------------------------------------------ *)
(* Compiler generator 

   This is essentially the same as the compiler, except that it is a functor
   over an interpreter instead of being a separate module.  The only 
   drawback is that Ocaml doesn't do enough inlining, so there is overhead.
   But the advantages of being functorial probably outweight that. *)

module type InterpreterType = Symantics with type ('c,'sv,'dv) repr = 'dv

module CGen(R:InterpreterType) = struct
  type ('c,'sv,'dv) repr = ('c,'dv) code
  let int (x:int) = let y = R.int x in .<y>.
  let bool (b:bool) = let y = R.bool b in .<y>.
  let add e1 e2 = .< R.add .~e1 .~e2 >.
  let mul e1 e2 = .<R.mul .~e1 .~e2>.
  let leq x y = .< R.leq .~x .~y >.
  let eql x y = .< R.eql .~x .~y >.
  let if_ eb et ee = .< R.if_ .~eb (fun _ -> .~(et ())) (fun _ -> .~(ee ())) >.

  let lam f = let y = R.lam f in .< fun x -> .~(y .<x>.) >. 
  let app e1 e2 = .<R.app .~e1 .~e2>.
  let fix f = .<let rec self n = .~(f .<self>.) n in self>.
end;;

module C2 = CGen(R) ;;
module EXC2 = EX(C2) ;;

(* ------------------------------------------------------------------------ *)
(* Partial evaluator *)
(* Inspired by Ken's solution *)

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

(* ---------------------------------------*)
(* Or we can also go functorial *)
module P2(R:InterpreterType) = 
struct
  module C = CGen(R)
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
module P3(R:InterpreterType) =
struct
  include P2(R)
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

module P2A = P2(R);;
module EXP2 = EX(P2A);;

(* all the tests together *)
let itest1 = EXR.test1r ();;
let ctest1 = EXC.test1r ();;
let ptest1 = EXP.test1r ();;
let ltest1 = EXL.test1r ();;
let ttest1 = EXRT2.test1r () (Base IntT);;
(* let ztest1 = EXCT.test1r ();;  *)

let itest2 = EXR.test2r ();;
let ctest2 = EXC.test2r ();;
let ptest2 = EXP.test2r ();;
let ltest2 = EXL.test2r ();;
let ttest2 = EXRT2.test2r () (Function(Base IntT, Base IntT));;
(* let ztest2 = EXCT.test2r ();; *)
let ttest2b = EXRT2.test2br () (Function(Base IntT, Base IntT));;

let itest3 = EXR.test3r ();;
let ctest3 = EXC.test3r ();;
let ptest3 = EXP.test3r ();;
let ltest3 = EXL.test3r ();;
let ttest3 = EXRT2.test3r () (Function(Function(Base IntT, Base IntT),Base IntT));;
(* let ztest3 = EXCT.test3r ();; *)

(* a test of some features without the baggage of fix *)
let ttest4 = EXRT2.test4r () (Function(Base IntT, Function(Base IntT, Base
IntT)));;

let itestg = EXR.testgibr ();;
let ctestg = EXC.testgibr ();;
let ptestg = EXP.testgibr ();;
let ltestg = EXL.testgibr ();;
let ttestg = EXRT2.testgibr () (Function(Base IntT, Function(Base IntT,
Function(Base IntT, Base IntT))));;
(* let ztestg = EXCT.testgibr ();; *)

let itestg1 = EXR.testgib1r ();;
let ctestg1 = EXC.testgib1r ();;
let ptestg1 = EXP.testgib1r ();;
let ltestg1 = EXL.testgib1r ();;
let ttestg1 = EXRT2.testgib1r () (Base IntT);;
(* let ztestg1 = EXCT.testgib1r ();; *)

let itestg2 = EXR.testgib2r ();;
let ctestg2 = EXC.testgib2r ();;
let ptestg2 = EXP.testgib2r ();;
let ltestg2 = EXL.testgib2r ();;
let ttestg2 = EXRT2.testgib2r () (Function(Base IntT, Function(Base IntT, Base
IntT)));;
(* let ztestg2 = EXCT.testgib2r ();; *)

let ttestp = EXRT2.testpowfix () (Function(Base IntT, Function(Base IntT, Base
IntT)));;

let itestp7 = EXR.testpowfix7r ();;
let ctestp7 = EXC.testpowfix7r ();;
let ptestp7 = EXP.testpowfix7r ();;
let ltestp7 = EXL.testpowfix7r ();;
let ttestp7 = EXRT2.testpowfix7r () (Function(Base IntT, Base IntT));;
(* let ztestp7 = EXCT.testpowfix7r ();; *)

let itestp0 = EXR.testpowfix0r ();;
let ctestp0 = EXC.testpowfix0r ();;
let ptestp0 = EXP.testpowfix0r ();;
let ltestp0 = EXL.testpowfix0r ();;
let ttestp0 = EXRT2.testpowfix0r () (Function(Base IntT, Base IntT));;
(* let ztestp0 = EXCT.testpowfix0r ();; *)

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

  let fix f = let rec fx f n = app (f (lam (fx f))) n in lam (fx f)

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
  type ('c,'dv) repr
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
  (* type w = unit
  type ('c,'sv,'dv) repr = ('c, 'sv,('dv -> w)->w) S.repr *)
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
  let eql e1 e2 = S.lam (fun k ->
    S.app e1 (S.lam (fun v1 ->
    S.app e2 (S.lam (fun v2 -> S.app k (S.eql v1 v2))))))
  let if_ ec et ef = S.lam (fun k ->
    S.app ec (S.lam (fun vc ->
    S.if_ vc (fun () -> S.app (et ()) k) (fun () -> S.app (ef ()) k))))
  let lam f = S.lam (fun k -> S.app k (S.lam (fun x ->
    f (S.lam (fun k -> S.app k x)))))
  let app e1 e2 = S.lam (fun k -> 
    S.app e1 (S.lam (fun f ->
    S.app e2 (S.lam (fun v -> S.app (S.app f v) k)))))
  let fix = S.fix
end;;
(*  This fails (when type declarations are commented out above) because
    the type of 'lam' is wrong!  I don't know how to fix it [JC]
    module T4:Symantics = CPST(R);; *)

(* Attempt to make CPST more 'uniform', but it has the same problem as
   the code above -- lam has the wrong type
module CPST2(S: Symantics) = struct
  let ret a = fun k -> S.app k a
  let bind a f = S.app a (S.lam (fun v -> f v))
  let binop g e1 e2 = S.lam (fun k->
                      bind e1 (fun v1 ->
                      bind e2 (fun v2 -> 
                      S.app k (g v1 v2))))

  type ('a,'sv,'dv) repr = ('a, 'sv, ('dv->unit)->unit) S.repr
  let int i = ret (S.int i)
  let bool i = ret (S.bool i)
  let add e1 e2 = binop S.add e1 e2 
  let mul e1 e2 = binop S.mul e1 e2
  let eql e1 e2 = binop S.eql e1 e2
  let leq e1 e2 = binop S.leq e1 e2

  let if_ ec et ef = S.lam (fun k ->
    S.app ec (S.lam (fun vc ->
    S.if_ vc (fun () -> S.app (et ()) k) (fun () -> S.app (ef ()) k))))
  let lam f = S.lam (fun k1 -> 
              S.lam (fun x ->
              bind x (fun z -> f z k1)))
  let app e1 e2 = S.lam (fun k -> 
    S.app e1 (S.lam (fun f ->
    S.app e2 (S.lam (fun v -> S.app (S.app f v) k)))))
  let fix = S.fix
end;;
module TT:Symantics = CPST2(R);;  *)

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


(* Extension of S for an imperative language with a single piece of state.

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
  type 'c states			(* static version of the state *)
  val lapp : ('c,'sa,'da) repr -> (('c,'sa,'da) repr ->
      ('c,'sb,'db) repr) ->  ('c,'sb,'db) repr
  val deref : unit -> ('c,'c states,state) repr
  val set   : ('c,'c states,state) repr -> ('c,'c states,state) repr
end;;
 
(* INT state *)
module EXSI_INT(S: SymSI with type state = int and type 'c states = int) = 
struct
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

(* Second-order, INT->INT state *)
(* The two Symantics arguments are necessary because we cannot write
  S: SymSI with ... 
       type 'c states = ('c,int,int) S.repr -> ('c,int,int) S.repr
 that is, the typing constraints can't refer to the signature being
 defined.
*)
module EXSI_INT_INT(S0: Symantics)
(S: SymSI with type state  = int->int
           and  type 'c states = ('c,int,int) S0.repr ->
               ('c,int,int) S0.repr
           and  type ('c,'sv,'dv) repr = ('c,'sv,'dv) S0.repr) = 
struct
  open S

  (* this program corresponds to 
     let v0 = !state in
       state := (\x -> x * 2);
       v0 10 + !state 10; *)
  let test1 () = lapp (deref ()) (fun v0 -> 
                  lapp (set (lam (fun x -> mul x (int 2)))) (fun _ ->
		   add (app v0 (int 10)) (app (deref ()) (int 10))))
end;;



(* Pure state passing CPS interpreter. *)
(* We make the CPS to be fully polymorphic over the answer type.
   We could have just as well put the answer type into the ST signature
   below (as common in SML). But because we have higher-rank types
   in OCaml, we may as well use them.
*)
module RCPS (ST: sig 
  type state 
  type 'c states 
  type ('c,'sv,'dv) repr = 
      {ko: 'w. ('sv -> 'c states -> 'w) -> 'c states -> 'w}
end) = struct
  include ST
  type ('c, 'sv, 'dv) result = 'c states -> 'sv

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

(* Instantiate for the first-order state *)
module RCPSI = RCPS(struct 
  type state = int
  type 'c states = int
  type ('c,'sv,'dv) repr = 
      {ko: 'w. ('sv -> 'c states -> 'w) -> 'c states -> 'w}
end);;
module EXPSI = EX(RCPSI);;

(*
let cpsitest1 = RCPSI.get_res (EXPSI.test1r ()) 100;;
let cpsitest2 = RCPSI.get_res (EXPSI.test2r ()) 100;;
let cpsitest3 = RCPSI.get_res (EXPSI.test3r ()) 100;;
let cpsitestg = RCPSI.get_res (EXPSI.testgibr ()) 100;;
let cpsitestg1 = RCPSI.get_res (EXPSI.testgib1r ()) 100;;
let cpsitestg2 = RCPSI.get_res (EXPSI.testgib2r ()) 100;;
let cpsitestp7 = RCPSI.get_res (EXPSI.testpowfix7r ()) 100;;
*)

module EXPSI_INT = EXSI_INT(RCPSI);;
let cpsitesti1 = RCPSI.get_res (EXPSI_INT.test1 ()) 100;; (* 102 *)
let cpsitesti2 = RCPSI.get_res (EXPSI_INT.test2 ()) 100;; (* 102 *)
let cpsipow = RCPSI.get_res (EXPSI_INT.pow ()) 100;;
let cpsipow7 = RCPSI.get_res (EXPSI_INT.pow7 ()) 100;;
let cpsipow27 = RCPSI.get_res (EXPSI_INT.pow27 ()) 100;;


(* Instantiate for the second-order state. The structure RCPS2_t must be named.
   We cannot `inline' it and write
  module RCPSII = RCPS(
  type state = int -> int
  type 'c states = ('c,int,int) repr -> ('c,int,int) repr
  and ('c,'sv,'dv) repr = ... end);;
because OCaml will make the type 'c states abstract. If the types are
mutually recursive, the structure should be named rather than anonymous.
*)
module RCPS2_t = struct 
  type state = int -> int
  type 'c states = ('c,int,int) repr -> ('c,int,int) repr
  and ('c,'sv,'dv) repr =
       {ko: 'w. ('sv -> 'c states -> 'w) -> 'c states -> 'w}
end;;


module RCPSII = RCPS(RCPS2_t);;
module EXPSII = EX(RCPSII);;


module EXPSI_INT_INT = EXSI_INT_INT(RCPSII)(RCPSII);;
let cpsitestii1 = RCPSII.get_res (EXPSI_INT_INT.test1 ()) 
                  (fun x -> RCPSII.add (RCPSII.int 2) 
		                        (RCPSII.mul x (RCPSII.int 2)));;
(* 42 *)


(* Extension of S for an imperative language with reference cells
   (which may hold any values, including functional values).

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

 This time, we use reference cells of the meta-language to
 implement reference cells of the source language.
*)
module type SymSP = sig
  include Symantics
  type 'a rf
  type 'a rft
  val newref : ('c,'sa,'da) repr -> ('c,'sa rf,'da rf) repr 
  val deref : ('c,'sa rf,'da rf) repr -> ('c,'sa,'da) repr
  val setref : ('c,'sa rf,'da rf) repr -> ('c,'sa,'da) repr
              -> ('c,'sa,'da) repr
  val lapp : ('c,'sa,'da) repr ->
    (('c,'sa,'da) repr -> ('c,'sb,'db) repr) -> ('c,'sb,'db) repr
end;;

module RR = struct
  include R
  type 'a rf = 'a ref
  type 'a rft
  let newref v = ref v
  let deref  v = ! v
  let setref v nv = let ov = !v in v := nv; ov
  let lapp e1 e2 = app (lam e2) e1
end;;

module CR = struct
  include C
  type 'a rf = 'a ref
  type 'a rft
  let newref v = .<ref .~v>.
  let deref  v = .<! (.~v)>.
  let setref v nv = .<let rv = .~v in let ov = !rv in rv := .~nv; ov>.
  let lapp e1 e2 = app (lam e2) e1
end;;

(* Doing PE with references may be problematic. It is not clear what
  to do if we encounter newref with the static code, but then need
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
  (* the same but with the higher order *)
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
