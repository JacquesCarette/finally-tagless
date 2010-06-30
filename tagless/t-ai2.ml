(*
 In our tagless final approach, try to represent AI

 Unlike the previous version, don't use continuations, but represent things
 directly.

 For information propagation purposes, we still keep suc, pre, and sub,
 as well as having eql being monomorphic.
*)

module type Symantics1 = sig
  type ('c,'dv) repr
  type 'c dint
  type 'c dbool
  type ('c,'da,'db) darr
  val int  : int  -> ('c, 'c dint) repr
  val bool : bool -> ('c, 'c dbool) repr
  val suc  : ('c, 'c dint) repr -> ('c, 'c dint) repr
  val pre  : ('c, 'c dint) repr -> ('c, 'c dint) repr
  val add  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dint) repr
  val sub  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dint) repr
  val mul  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dint) repr
  val leq  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dbool) repr
  val eql  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dbool) repr
  val if_  : ('c, 'c dbool) repr ->
             (unit -> 'x) -> (unit -> 'x) -> (('c, 'da) repr as 'x)

  val lam : (('c, 'da) repr -> ('c, 'db) repr) -> ('c, ('c,'da,'db) darr) repr
  val app : ('c, ('c,'da,'db) darr) repr -> ('c, 'da) repr -> ('c, 'db) repr
  val fix : ('x -> 'x) -> (('c, ('c,'da,'db) darr) repr as 'x)
end;;

(* examples *)
module Ex1(S:Symantics1) = struct
 open S
 let test1 () = add (int 1) (int 2)
 let test2 () = app (app (lam (fun x -> lam (fun y -> 
         if_ (leq x y) (fun () -> y) (fun () -> int 4)))) (int 1)) (int 2)

 let test3 () = lam (fun x -> x)

 let mmul () = fix (fun self -> lam (fun x -> lam (fun y -> 
     if_ (eql x (int 0))                    (fun () -> 
         int 0)                             (fun () ->
         if_ (leq x (int 0))                (fun () ->
             sub (app (app self (suc x)) y) y) (fun () ->
             add y (app (app self (pre x)) y)
                                    )))))

 let test4 () = app (app (mmul ()) (int 0)) (int 3)
 let test5 () = app (app (mmul ()) (int 1)) (int 4)
 let test6 () = app (app (mmul ()) (int (-2))) (int 3)
 let test7 () = app (app (mmul ()) (int 11)) (int 7)
 
 (* Example 3.27 *)
 let test8 () =
     let f = lam (fun x -> if_ (leq x (int 0)) 
                               (fun () -> lam (fun z -> int 25))
                               (fun () -> lam (fun y -> y)))
     in app (app f (int 3)) (int 0)
end;;

(* Pure interpreter. It is essentially the identity transformer *)
module R = struct
  type ('c,'dv) repr = 'dv    (* absolutely no wrappers *)
  type 'c dint = int
  type 'c dbool = bool
  type ('c,'da,'db) darr = 'da -> 'db
  let int (x:int) = x
  let bool (b:bool) = b
  let suc e1 = succ e1
  let pre e2 = pred e2
  let add e1 e2 = e1 + e2
  let sub e1 e2 = e1 - e2
  let mul e1 e2 = e1 * e2
  let leq x y = x <= y
  let eql x y = x = y
  let if_ eb et ee = if eb then (et ()) else (ee ())

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f = let rec self n = f self n in self
end;;

module EXR = Ex1(R);;

(* We will need memoization! *)

let memo : ('a -> 'b) -> ('a -> 'b) =
    let tbl = Hashtbl.create 17 in
    fun f x ->
        try Hashtbl.find tbl x 
        with Not_found ->
            let entry = f x in
            Hashtbl.add tbl x entry;
            entry

module IParity2 = struct
  type 'c dint  = Even | Odd
  type 'c dbool = bool
  type ('c, 'da, 'db) darr = ('da, 'db) Hashtbl.t
  type ('c,'dv) repr = { vals : ('dv, unit) PMap.t }

  let add_ctr (v:'a) (ctr:('c,'a) repr) =
      {vals = PMap.add v () ctr.vals}
  let empty () : ('c,'a) repr = {vals = PMap.create compare}
  let int (x:int) = let v = if x land 1 = 0 then Even else Odd in
      add_ctr v (empty ())
  let bool (b:bool) = add_ctr b (empty ())
  let suc e = match e with
  | Even ->  Odd
  | Odd  ->  Even
  let pre e = match e with
  | Even -> Odd
  | Odd  -> Even
  let add n1 n2 = match (n1, n2) with
  | (Even,Even) | (Odd,Odd)  -> Even
  | (Even,Odd)  | (Odd,Even) -> Odd
  let sub n1 n2 = match (n1, n2) with
  | (Even,Even) | (Odd,Odd)  -> Even
  | (Even,Odd)  | (Odd,Even) -> Odd
  let mul n1 n2 = match (n1, n2) with
  | (Odd,Odd)  -> Odd
  | (Even,_) | (_,Even)  -> Even
  let leq _ _ = empty ()
  let eql x y : ('c, 'a dbool) repr = 
      if (x == y) then empty () else add_ctr false (empty ())
  let if_ eb et ee = 
      if eb then add_ctr (et ()) (empty ())
      else if eb == BFalse then ee ()
      else BUnk

  let lam f = (fun k -> k (memo (fun x -> (f (fun k' -> k' x)))))
  let app e1 e2 = fun k -> e1 (fun f -> e2 (fun x -> (f x) k))

  (* let fix f = let rec self n = f self n in self *)
  let fix f k = k 
     (let self = ref None in 
     self := Some (memo (fun x k'' -> f 
         (match !self with Some s -> fun k' -> k' s) 
         (fun g -> g x k''))); 
     match !self with Some s -> s)
end;;

module EXI2 = Ex1(IParity2) ;;
let print_int s = function
    | IParity2.Even -> Printf.printf "%s = Even\n" s
    | IParity2.Odd  -> Printf.printf "%s = Odd\n" s
let run_int s f = f (print_int s)

module Sign = struct
  type 'c dint  = Pos | Neg | Zero
  type 'c dbool = bool
  type ('c, 'da, 'db) darr = ('da, 'db) frepr
  type ('c,'dv) repr = ('dv -> unit) -> unit
  let int (x:int) = fun k -> k 
      (if x == 0 then Zero else if x<0 then Neg else Pos)
  let bool (b:bool) = fun k -> k b
  let suc e = fun k -> e (function
  | Pos -> k Pos
  | Zero -> k Pos
  | Neg -> k Neg; k Zero)
  let pre e = fun k -> e (function
  | Neg -> k Neg
  | Zero -> k Neg
  | Pos -> k Pos; k Zero)
  let add e1 e2 = fun k -> e1 (fun n1 -> e2 (fun n2 -> 
  match (n1, n2) with
  | (Pos,Pos) | (Pos,Zero) | (Zero,Pos) -> k Pos
  | (Neg,Neg) | (Neg,Zero) | (Zero,Neg) -> k Neg
  | (Zero,Zero) -> k Zero
  | _ -> k Neg; k Zero; k Pos ))
  let sub e1 e2 = fun k -> e1 (fun n1 -> e2 (fun n2 -> 
  match (n1, n2) with
  | (Pos,Pos) -> k Pos; k Zero; k Neg
  | (Pos,Zero) -> k Pos
  | (Pos,Neg) -> k Pos
  | (Zero,Pos) -> k Neg
  | (Zero,Zero) -> k Zero
  | (Zero,Neg) -> k Pos
  | (Neg,Pos) -> k Neg
  | (Neg,Zero) -> k Neg
  | (Neg,Neg) -> k Pos; k Zero; k Neg
  ))
  let mul e1 e2 = fun k -> e1 (fun n1 -> e2 (fun n2 -> 
  match (n1, n2) with
  | (Zero,_)  | (_, Zero) -> k Zero
  | (Pos,Pos) | (Neg,Neg)  -> k Pos
  | (Pos,Neg) | (Neg,Pos)  -> k Neg))
  let leq e1 e2 = fun k -> e1 (fun n1 -> e2 (fun n2 ->
  match (n1, n2) with
  | (Neg, Zero) | (Neg, Pos) | (Zero, Zero) | (Zero,Pos) -> k true
  | (Pos, Zero) | (Zero, Neg) | (Pos,Neg) -> k false
  | (Pos, Pos) | (Neg, Neg) -> k true; k false ))
  let eql e1 e2 = fun k -> e1 (fun x -> e2 (fun y ->
      if ((x=Zero) && (y=Zero)) then k true 
      else
          if (x = y) then (k true; k false) else k false))
  let if_ eb et ee = fun k -> eb (fun b ->
      if b then (et ()) (fun t -> k t) else (ee ()) (fun e -> k e))

  let lam f = (fun k -> k (memo (fun x -> (f (fun k' -> k' x)))))
  let app e1 e2 = fun k -> e1 (fun f -> e2 (fun x -> (f x) k))

  (* let fix f = let rec self n = f self n in self *)
  let fix f k = k 
     (let self = ref None in 
     self := Some (memo (fun x k'' -> f 
         (match !self with Some s -> fun k' -> k' s) 
         (fun g -> g x k''))); 
     match !self with Some s -> s)
end;;


module EXS2 = Ex1(Sign) ;;
let print_sign s = function
    | Sign.Pos -> Printf.printf "%s = Pos\n" s
    | Sign.Neg  -> Printf.printf "%s = Neg\n" s
    | Sign.Zero  -> Printf.printf "%s = Zero\n" s
let run_sign s f = f (print_sign s)

let r1 = 3 == EXR.test1()
let r2 = 2 == EXR.test2()
let r3 = EXR.test3()
let r4 = 0 == EXR.test4()
let r5 = 4 == EXR.test5()
let r6 = (-6) == EXR.test6()
let r7 = 77 == EXR.test7()
let r8 = 0 == EXR.test8()

let j1 = run_int "test1 - parity of 3" (EXI2.test1())
let j2 = run_int "test2 - parity of 2" (EXI2.test2())
let j3 = EXI2.test3()
let j4 = run_int "test4 - parity of 0" (EXI2.test4())
let j5 = run_int "test5 - parity of 4" (EXI2.test5())
let j6 = run_int "test6 - parity of -6" (EXI2.test6())
let j7 = run_int "test7 - parity of 77" (EXI2.test7())
(* let j8 = run_int "test8 - parity of 0" (EXI2.test8()) *)

let k1 = run_sign "test1 - sign of 3" (EXS2.test1())
let k2 = run_sign "test2 - sign of 2" (EXS2.test2())
let k3 = EXS2.test3()
let k4 = run_sign "test4 - sign of 0" (EXS2.test4())
let k5 = run_sign "test5 - sign of 4" (EXS2.test5())
let k6 = run_sign "test6 - sign of -6" (EXS2.test6())
let k7 = run_sign "test7 - sign of 77" (EXS2.test7())
let k8 = run_sign "test7 - sign of 0" (EXS2.test8())
;;
