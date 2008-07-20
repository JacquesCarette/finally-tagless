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
*)



(* This class/type defines syntax (and its instances, semantics) 
   of our language
 *)

module type Symantics = sig
  type ('c,'sv,'dv) repr
  type rat
  val int  : int  -> ('c,int,int) repr
  val bool : bool -> ('c,bool,bool) repr
  val rat  : (int*int) -> ('c,rat,rat) repr
  val add  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val addr : ('c,rat,rat) repr -> ('c,rat,rat) repr -> ('c,rat,rat) repr
  val mul  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val mulr : ('c,rat,rat) repr -> ('c,rat,rat) repr -> ('c,rat,rat) repr
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
 let test1a () = addr (rat (1,2)) (rat (1,3))
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
 let test1ar () = runit test1a
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

(* A few utilities *)
let rec gcd_int i1 i2 = if i2 = 0 then abs i1 else gcd_int i2 (i1 mod i2)
let add_ratio (x1,y1) (x2,y2) = 
    let p = gcd_int y1 y2 in
    if p = 1 then (x1*y2+x2*y1,y1*y2)
    else
        let d1 = y1 / p and d2 = y2 / p in
        let n = x1*d2 + x2*d1 in
        let p' = gcd_int n p in
        (n/p', d1 * (y2/p'))

let mult_ratio (x1,y1) (x2,y2) = 
    let p1 = gcd_int x1 y2 and p2 = gcd_int x2 y1 in
    let (n1,n2,d1,d2) = (x1/p1, x2/p2, y1/p1, y2/p2) in
    (n1*n2, d1*d2)

(* ------------------------------------------------------------------------ *)
(* The interpreter
   It is typed, tagless interpreter: R is not a tag. The interpreter
   never gets stuck, because it evaluates typed terms only
*)
(* Pure interpreter. It is essentially the identity transformer *)
module R = struct
  type ('c,'sv,'dv) repr = 'dv    (* absolutely no wrappers *)
  type rat = int * int
  let int (x:int) = x
  let bool (b:bool) = b
  let rat (x,y) = let p = gcd_int x y in (x/p, y/p)
  let add e1 e2 = e1 + e2
  let addr (x1,y1) (x2,y2) = add_ratio (x1,y1) (x2,y2)
  let mul e1 e2 = e1 * e2
  let mulr = mult_ratio
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
  type rat = int
  let int (x:int) = 1
  let bool (b:bool) = 1
  let rat r = 2
  let add e1 e2 = e1 + e2 + 1
  let addr e1 e2 = e1 + e2 + 1
  let mul e1 e2 = e1 + e2 + 1
  let mulr e1 e2 = e1 + e2 + 1
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
  type rat = int * int
  let int (x:int) = .<x>.
  let bool (b:bool) = .<b>.
  let rat (r:rat) = .<r>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let addr e1 e2 = .<add_ratio .~e1 .~e2>.
  let mul e1 e2 = .<.~e1 * .~e2>.
  let mulr e1 e2 = .<mult_ratio .~e1 .~e2>.
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

(* Final solution, Inspired by Ken's solution *)

module P =
struct
  type ('c,'sv,'dv) repr = {st: 'sv option; dy: ('c,'dv) code}
  type ('a,'s,'v) result = RL of 's | RC of ('a,'v) code;;
  type rat = int*int
  let abstr {dy = x} = x
  let pdyn x = {st = None; dy = x}

  let int  (x:int)  = {st = Some (R.int x);
                       dy = C.int x}
  let bool (x:bool) = {st = Some (R.bool x);
                       dy = C.bool x}
  let rat  r        = {st = Some (R.rat r);
                       dy = C.rat r}


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
  let addr e1 e2 = monoid rat (0,1) R.addr C.addr (e1,e2)
  let mul e1 e2 = ring int 0 1 R.mul C.mul (e1,e2)
  let mulr e1 e2 = ring rat (0,1) (1,1) R.mulr C.mulr (e1,e2)
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

let itestf0 = EXR.testfactr ();;
let ctestf0 = EXC.testfactr ();;
let ptestf0 = EXP.testfactr ();;
let ltestf0 = EXL.testfactr ();;

let itesta0 = EXR.testackr1 ();;
let itesta3 = EXR.testackr13 ();;
let ctesta0 = EXC.testackr1 ();;
let ptesta0 = EXP.testackr1 ();;
let ptesta3 = EXP.testackr13 ();;

let itest1a = EXR.test1ar ();;
let ctest1a = EXC.test1ar ();;
let ptest1a = EXP.test1ar ();;
let ltest1a = EXL.test1ar ();;

