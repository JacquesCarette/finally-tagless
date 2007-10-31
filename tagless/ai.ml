(* Abstract interpretation

  The language is simply-typed lambda-calculus with fixpoint,
  integers [plus basic operations], booleans [ditto] and comparison.

   See the Symantics signature for the exact definition.

 This is the unfinished experiment. It barely works for the first-order
 fragment. For the HO fragment, we need the same technique as in the P
 evaluator of our paper.
 Mainly, see my misgivings about this approach in the message to Jacques,
 and the message about supercompilation (Oct 25, 2007).

*)

module type Symantics = sig
  type ('c,'sv,'dv) repr
  val int  : int  -> ('c,int,int) repr
  val bool : bool -> ('c,bool,bool) repr
  val add  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val mul  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val leq  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,bool,bool) repr
  val if_ : ('c,bool,bool) repr ->
             (unit -> 'x) ->
             (unit -> 'x) -> (('c,'sa,'da) repr as 'x)

  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x) 
    -> ('c,'x,'da->'db) repr
  val app : ('c,'x,'da->'db) repr -> (('c,'sa,'da) repr 
				      -> ('c,'sb,'db) repr as 'x)
  val fix : ('x -> 'x) -> 
    (('c, ('c,'sa,'da) repr -> ('c,'sb,'db) repr, 'da->'db) repr as 'x)
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
(* The interpreter and the compiler, quoted from incope.ml
   See that file for more comments.
*)
module R = struct
  type ('c,'sv,'dv) repr = 'dv    (* absolutely no wrappers *)
  let int (x:int) = x
  let bool (b:bool) = b
  let add e1 e2 = e1 + e2
  let mul e1 e2 = e1 * e2
  let leq x y = x <= y
  let if_ eb et ee = if eb then (et ()) else (ee ())

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f = let rec self n = f self n in self
end;;

module C = struct
  type ('c,'sv,'dv) repr = ('c,'dv) code
  let int (x:int) = .<x>.
  let bool (b:bool) = .<b>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let mul e1 e2 = .<.~e1 * .~e2>.
  let leq x y = .< .~x <= .~y >.
  let if_ eb et ee = 
    .<if .~eb then .~(et () ) else .~(ee () )>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<let rec self n = .~(f .<self>.) n in self>.
end;;

(* ------------------------------------------------------------------------ *)
(* The first abstract interpreter: estimate the sign of integer value *)

(* The abstraction domain: *)
type sign = 
  | SLit of int (* the value is fully known, statically *)
  | SPos	(* positive *)
  | SNeg	(* negative *)
  | SNPos       (* Not positive *)
  | SNNeg       (* Not negative *)
  | SUnknown    (* Unknown of the expression is not of int type *)
;;

let sign_neg = function
  | SLit i -> SLit (-i)
  | SPos   -> SNeg
  | SNeg   -> SPos
  | SNPos  -> SNNeg
  | SNNeg  -> SNPos
  | SUnknown -> SUnknown
;;

let sign_weaken0 = function
  | SLit i -> SLit i
  | SPos   -> SNNeg
  | SNeg   -> SNPos
  | s      -> s
;;


(* This AIS patterned after partial evaluator P of incope.ml *)

module AIS = 
struct
  type ('c,'sv,'dv) repr = {st: sign; dy: ('c,'dv) code}
  let concr {dy = x} = x
  let pdyn x = {st = SUnknown; dy = x}
  let seta av f e1 e2 = {st = av; dy = f (concr e1) (concr e2)}

  let int  (x:int)  = {st = SLit x;   dy = C.int x}
  let bool (x:bool) = {st = SUnknown; dy = C.bool x}

  let add e1 e2 = match (e1,e2) with
  | {st = SLit i1}, {st = SLit i2} -> int (i1 + i2)
  | {st = SLit 0}, e | e, {st = SLit 0} -> e   (* monoid unity *)
  | {st = SLit i}, {st = SPos}  when i > 0 -> seta SPos C.add e1 e2
  | {st = SPos}, {st = SLit i}  when i > 0 -> seta SPos C.add e1 e2
  | {st = SLit i}, {st = SNNeg} when i > 0 -> seta SPos C.add e1 e2
  | {st = SNNeg}, {st = SLit i} when i > 0 -> seta SPos C.add e1 e2
  | {st = SLit i}, {st = SNeg}  when i < 0 -> seta SNeg C.add e1 e2
  | {st = SNeg}, {st = SLit i}  when i < 0 -> seta SNeg C.add e1 e2
  | {st = SLit i}, {st = SNPos} when i < 0 -> seta SNeg C.add e1 e2
  | {st = SNPos}, {st = SLit i} when i < 0 -> seta SNeg C.add e1 e2
  | {st = SPos}, {st = SPos} | {st = SPos}, {st = SNNeg} 
  | {st = SNNeg}, {st = SPos} -> seta SPos C.add e1 e2
  | {st = SNeg}, {st = SNeg} | {st = SNeg}, {st = SNPos} 
  | {st = SNPos}, {st = SNeg} -> seta SNeg C.add e1 e2
  | {st = SNPos}, {st = SNPos} -> seta SNPos C.add e1 e2
  | {st = SNNeg}, {st = SNNeg} -> seta SNNeg C.add e1 e2
  | _, _ -> seta SUnknown C.add e1 e2

  let mul e1 e2 = match (e1,e2) with
  | {st = SLit i1}, {st = SLit i2} -> int (i1 * i2)
  | {st = SLit 1}, e | e, {st = SLit 1} -> e       (* ring unity *)
  | {st = SLit 0}, e | e, {st = SLit 0} -> int 0   (* ring zero *)
  | {st = SLit i}, e when i > 0 -> seta e.st C.mul e1 e2
  | e, {st = SLit i} when i > 0 -> seta e.st C.mul e1 e2
  | {st = SLit i}, e when i < 0 -> seta (sign_neg e.st) C.mul e1 e2
  | e, {st = SLit i} when i < 0 -> seta (sign_neg e.st) C.mul e1 e2
  | {st = SPos}, e -> seta e.st C.mul e1 e2
  | {st = SNeg}, e -> seta (sign_neg e.st) C.mul e1 e2
  | {st = SNNeg}, e -> seta (sign_weaken0 e.st) C.mul e1 e2
  | {st = SNPos}, e -> seta (sign_weaken0 (sign_neg e.st)) C.mul e1 e2
  | _, _ -> seta SUnknown C.mul e1 e2

  let leq e1 e2 = pdyn (C.leq (concr e1) (concr e2))
  let if_ eb et ee =
     pdyn (C.if_ (concr eb) 
                 (fun () -> concr (et ()))
                 (fun () -> concr (ee ())))

  let lam f =
      {st = SUnknown; dy = C.lam (fun x -> concr (f (pdyn x)))}

  let app ef ea = pdyn (C.app (concr ef) (concr ea))

   (*
     For now, to avoid divergence at the PE stage, we residualize.
     Actually, we unroll the fixpoint exactly once, and then
     residualize
   *)
  let fix f = f (pdyn (C.fix (fun x -> concr (f (pdyn x)))))
  (* this should allow us controlled unfolding *)
  (*
  let unfold z s = lam (function
      | {st = Some n} -> 
              let rec f k = if k<=0 then z else
                                match s with
                                | {st = Some m} -> m (f (k-1))
                                | {dy = y}      -> pdyn (C.app y (concr (f (k-1))))
              in f n
      | {dy = y}      -> pdyn (C.app (C.unfold (concr z) (concr s)) y))
  *)

  let get_res t = concr t
end;;

module EXP = EX(AIS);;

let ptest1 = EXP.test1r ();;
let ptest2 = EXP.test2r ();;
let ptest3 = EXP.test3r ();;
let ptestg = EXP.testgibr ();;
let ptestg1 = EXP.testgib1r ();;
let ptestg2 = EXP.testgib2r ();;
let ptestp7 = EXP.testpowfix7r ();;
let ptestp0 = EXP.testpowfix0r ();;
