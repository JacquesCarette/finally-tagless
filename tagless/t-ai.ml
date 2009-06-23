(*
 In our tagless final approach, try to represent AI
*)

module type Symantics1 = sig
  type ('c,'dv) repr
  type 'c dint
  type 'c dbool
  type ('c,'da,'db) darr
  val int  : int  -> ('c, 'c dint) repr
  val bool : bool -> ('c, 'c dbool) repr
  val add  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dint) repr
  val mul  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dint) repr
  val leq  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dbool) repr
  val eql  : ('c, 'a) repr -> ('c, 'a) repr -> ('c, 'c dbool) repr
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
     if_ (eql x (int 0)) 
         (fun () -> int 0)
         (fun () -> (if_ (eql x (int 1)) 
                         (fun () -> y)
                         (fun () -> add y 
                                       (app (app self (add x (int (-1)))) y)))
         ))))

 let test4 () = app (app (mmul ()) (int 0)) (int 3)
 let test5 () = app (app (mmul ()) (int 1)) (int 4)
end;;

(* Pure interpreter. It is essentially the identity transformer *)
module R = struct
  type ('c,'dv) repr = 'dv    (* absolutely no wrappers *)
  type 'c dint = int
  type 'c dbool = bool
  type ('c,'da,'db) darr = 'da -> 'db
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

module EXR = Ex1(R);;

(* The even-odd, abstract interpretation *)

module IParity = struct
  type 'c dint  = Even | Odd
  type 'c dbool = bool
  type ('c, 'da, 'db) darr = 'da option -> 'db option
  type ('c,'dv) repr = 'dv option
  let int (x:int) = Some (if x land 1 = 0 then Even else Odd)
  let bool (b:bool) = Some b
  let add e1 e2 = match (e1, e2) with
  | (Some Even,Some Even) | (Some Odd,Some Odd)  -> Some Even
  | (Some Even,Some Odd)  | (Some Odd,Some Even) -> Some Odd
  | _ -> None
  let mul e1 e2 = match (e1, e2) with
  | (Some Odd,Some Odd)  -> Some Odd
  | (Some Even,_) | (_,Some Even)  -> Some Even
  | _ -> None
  let leq x y = None
  let eql x y = match (x,y) with
  | (Some a, Some b) -> if (a = b) then None else Some false
  | _                -> None
  let if_ eb et ee = match eb with
  | Some b -> if b then et () else ee ()
  | None   -> None

  let app e1 e2 = match (e1,e2) with
  | (Some f, Some _) -> (f e2)
  | _                -> None
  (* What really needs to happen is that lam replaces f with a 
   * memoizing version of f, so that if_ can then actually call each
   * branch safely; as this may be done in the context of fix, someone
   * needs to stop the recursion! *)
  let lam f = Some f
  let fix f = 
    let rec self n = (app (f (Some self)) n)
    in Some self
end;;

module EXI = Ex1(IParity) ;;

let r1 = EXR.test1()
let r2 = EXR.test2()
let r3 = EXR.test3()
let r4 = EXR.test4()
let r5 = EXR.test5()
let i1 = EXI.test1()
let i2 = EXI.test2()
let i3 = match EXI.test3() with
    | Some f -> Some (f (Some IParity.Even), f (Some IParity.Odd))
    | None   -> None
let i4 = EXI.test4()
let i5 = EXI.test5()
;;
