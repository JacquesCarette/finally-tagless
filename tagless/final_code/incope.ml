(* Interpreter, Compiler, Partial Evaluator *)

(*
  Code accompanying the paper by
    Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
*)

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

(* Running example *)
module EX(S: Symantics) = struct
 open S

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

 let testpowfix7 () = lam (fun x -> app (app (testpowfix ()) x) (int 7))
 let testpowfix0 () = lam (fun x -> app (app (testpowfix ()) (int 0)) x)

 let fact    () = 
   fix (fun self -> lam (fun n ->
     if_ (eql n (int 0))
           (fun () -> (int 1))
           (fun () -> (mul n (app self (add n (int (-1))))))))
 let testfact1   () = app (fact ()) (int 5)

 let fold zero succ = fix (fun self -> lam (fun i ->
     if_ (eql i (int 0))
         (fun () -> zero)
         (fun () -> app succ (app self (add i (int (-1)))))))
 let ack () = fold (lam (fun n -> add n (int 1)))
                    (lam (fun g -> fold (app g (int 1)) g))
 let testack1 () = app (ack ()) (int 2)
 let testack13 () = app (testack1 ()) (int 3)

 (* Ackermann's Higher-order (using Church numerals) *)
 let ackho () = lam (fun m -> lam (fun n -> 
   let succ = lam (fun n -> add n (int 1)) in
   app
     (app
        (app m (lam (fun g -> lam (fun n ->
          app (app n g) (app g (int 1))))))
        succ) n))

 let runit t = t () 

 let test1r () = runit test1
 let test2r () = runit test2
 let test3r () = runit test3

 let testgibr () = runit testgib
 let testgib1r () = runit testgib1
 let testgib2r () = runit testgib2

 let testpowfixr () = runit testpowfix
 let testpowfix7r () = runit testpowfix7
 let testpowfix0r () = runit testpowfix0

 let testfactr ()  = runit testfact1
 let testackr1 ()  = runit testack1
 let testackr13 () = runit testack13
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
(* Partial evaluator *)

(* First attempt: straightforward but quickly runs into trouble of requiring
   polymorphic lift, which we don't have and should not try to get. 
   Indeed, when implementing `add', we can lift a `static' int to
   the `dynamic' int by using C.int. But to write if_, we need to be able
   to lift values of any type...
*)

module P0 = struct
  type ('c,'sv,'dv) repr 
        = S0 of ('c,'sv,'dv) R.repr | D0 of ('c,'sv,'dv) C.repr
  let int (x:int) = S0 (R.int x)
  let bool (x:bool) = S0 (R.bool x)
  let abstrI0 (e:('c,int,int) repr) : ('c,int,int) C.repr =
    match e with
    | S0 e -> C.int e
    | D0 e -> e
  let add e1 e2 = 
    match (e1,e2) with
    | (S0 e1, S0 e2) -> S0 (R.add e1 e2)
    | (e1, e2)       -> D0 (C.add (abstrI0 e1) (abstrI0 e2))
end;;

(* Second attempt: works for the first-order fragment of our language,
   but stumbles on the higher-order fragment.
*)

module P1 : Symantics = struct
  type ('c,'sv,'dv) repr = 
      P1 of ('c,'sv,'dv) R.repr option * ('c,'sv,'dv) C.repr
  let abstr1 (P1 (_,dyn)) = dyn

  let int  (x:int)  = P1 (Some (R.int x), C.int x)
  let bool (x:bool) = P1 (Some (R.bool x),C.bool x)
  let add e1 e2 = 
    match (e1,e2) with
    | (P1 (Some n1,_),P1 (Some n2,_)) -> int (R.add n1 n2)
    | _ -> P1 (None,(C.add (abstr1 e1) (abstr1 e2)))
  let mul e1 e2 = 
    match (e1,e2) with
    | (P1 (Some n1,_),P1 (Some n2,_)) -> int (R.mul n1 n2)
    | _ -> P1 (None,(C.mul (abstr1 e1) (abstr1 e2)))
  let leq e1 e2 = 
    match (e1,e2) with
    | (P1 (Some n1,_),P1 (Some n2,_)) -> bool (R.leq n1 n2)
    | _ -> P1 (None,(C.leq (abstr1 e1) (abstr1 e2)))
  let eql e1 e2 = 
    match (e1,e2) with
    | (P1 (Some n1,_),P1 (Some n2,_)) -> bool (R.eql n1 n2)
    | _ -> P1 (None,(C.eql (abstr1 e1) (abstr1 e2)))
  let if_ = function
    | P1 (Some s,_) -> fun et ee -> if s then et () else ee ()
    | eb -> fun et ee -> P1 (None, C.if_ (abstr1 eb) 
                                   (fun () -> abstr1 (et ()))
                                   (fun () -> abstr1 (ee ())))
  let lam f     = failwith "problem!"
  let fix f     = failwith "problem!"
  let app e1 e2 = failwith "problem!"
end;;

(* But the problem occurs when we try to implement lam. The result
  of (lam f) must be P1 (None,_) or P1 ((Some _),_). Alas, we won't know
  which is which until we apply the function 'f' to a particular
  P1 value. 
*)

(* Final solution *)

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
let 3 = EXR.test1r ();;
let ctest1 = EXC.test1r ();; (* .<(1 + 2)>. *)
let ptest1 = EXP.test1r ();; (* {P1.st = Some 3; P1.dy = .<3>.} *)
let 3 = EXL.test1r ();;

let 20 = EXR.test2r () 10 ;;
let ctest2 = EXC.test2r ();; (* .<fun x_1 -> (x_1 + x_1)>. *)
let ptest2 = EXP.test2r ();; (* P1.dy -- same as above *)
let 2 = EXL.test2r ();;

let 4 = EXR.test3r () succ;;
let ctest3 = EXC.test3r ();; (* .<fun x_1 -> ((x_1 1) + 2)>. *)
let ptest3 = EXP.test3r ();; (* P1.dy -- same as above *)
let 5 = EXL.test3r ();;

let 8 = EXR.testgibr () 1 1 5;;
let ctestg = EXC.testgibr ();; (* big code *)
let ptestg = EXP.testgibr ();; (* likewise big code *)
let 17 = EXL.testgibr ();;

let 8 = EXR.testgib1r ();;
let ctestg1 = EXC.testgib1r ();; (* big code with beta redices *)
let ptestg1 = EXP.testgib1r ();; (* {P1.st = Some 8; P1.dy = .<8>.} *)
let 23 = EXL.testgib1r ();;

let 8 = EXR.testgib2r () 1 1;;
let ctestg2 = EXC.testgib2r ();; (* big code with beta redices *)
let ptestg2 = EXP.testgib2r ();;
(*
   P1.dy =
    .<fun x_1 ->
   fun x_2 -> ((((x_2 + x_1) + x_2) + (x_2 + x_1)) + ((x_2 + x_1) + x_2))>.}
*)
let 23 = EXL.testgib2r ();;

let 128 = EXR.testpowfix7r () 2;;
let ctestp7 = EXC.testpowfix7r ();; (* big code with beta redices *)
let ptestp7 = EXP.testpowfix7r ();;
(* 
 P1.dy = .<fun x_1 -> (x_1 * (x_1 * (x_1 * (x_1 * (x_1 * (x_1 * x_1))))))>.}
*)
let 15 = EXL.testpowfix7r ();;

let 0 = EXR.testpowfix0r () 2;;
let ctestp0 = EXC.testpowfix0r ();; (* big code with beta redices *)
let ptestp0 = EXP.testpowfix0r ();;
(*
    .<fun x_1 ->
   ((let rec self_6 =
      fun n_7 -> ((fun x_8 -> if (x_8 <= 0) then 1 else 0) n_7) in
     self_6) x_1)>.}
*)
let 15 = EXL.testpowfix0r ();;

let 120 = EXR.testfactr ();;
let ctestf0 = EXC.testfactr ();; (* big code with beta redices *)
let ptestf0 = EXP.testfactr ();; (* {P1.st = Some 120; P1.dy = .<120>.} *)
let 12 = EXL.testfactr ();;

let itesta0 = EXR.testackr1 ();;
let 9 = EXR.testackr13 ();;
let ctesta0 = EXC.testackr1 ();;
let ptesta0 = EXP.testackr1 ();;
(*
   P1.dy =
    .<let rec self_34 =
   fun n_35 ->
    ((fun x_36 ->
       if (x_36 = 0) then 3
       else
        ((let rec self_27 =
           fun n_28 ->
            ((fun x_29 ->
               if (x_29 = 0) then 2 else ((self_27 (x_29 + (-1))) + 1)) n_28) in
          self_27) (self_34 (x_36 + (-1))))) n_35) in
  self_34>.}
*)
let ptesta3 = EXP.testackr13 ();; (* {P1.st = Some 9; P1.dy = .<9>.} *)

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
  let fix = S.fix
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
  type 'c states                        (* static version of the state *)
  val lapp : ('c,'sa,'da) repr -> (('c,'sa,'da) repr -> ('c,'sb,'db) repr)
    ->  ('c,'sb,'db) repr
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
           and  type 'c states = ('c,int,int) S0.repr -> ('c,int,int) S0.repr
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
let 102 = RCPSI.get_res (EXPSI_INT.test1 ()) 100;;
let 102 = RCPSI.get_res (EXPSI_INT.test2 ()) 100;;
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
