(*
 Representing the Abstract Interpretation Framework  from Jones, Nielson
 paper in our tagless final approach.
 Neil Jones has kindly sent the excerpt, to be referred to as JN
*)

(*
Neil Jones' comments, Wed, 10 Sep 2008.
A couple of relevant pages are attached from the Jones-Nielson survey.
The start "Relation to Scott-style..." is the main point, though the
independent attribute/relational dichotomy is also relevant.

 From the July 21 mail: the treatments of "cond" and "whil" are rather
different:


Standard interpretation:
- "cond" selects ONE branch
- "whil" exits from its fixpoint recursion when the "else" branch
doesn't call "whil"


Even-odd interpretation:
- "cond" computes a lub |_| of BOTH branches
- "whil" iterates until the (entire) store is unchanged

> Could you point out a simple example of an abstract
> interpretation that needs a fixpoint? It would be great if it uses the
> same language as in your book with Nielson...


1. Well, the Even-odd interpretation of "whil" is already iterating to
a fixpoint

2. Constant propagation is anther abstract interpretations that needs
a fixpoint.

3. Further (and semantically trickier, some abstract interpretations
are "backwards" along control flow, e.g., the well-known "live
variable" analysis. Some use greatest and some use least fixpoints,
all motivated by accuracy (and soundness) considerations in compiler
construction.

4.  Your phrase "given more and more precise `seed' " suggests a
greatest fixpoint rather than a least, beginning with a least precise
answer "top" and iterating downwards in the flow lattice.

5. How would you go about proving (or even arguing informally)
correctness of the Even-odd analysis with respect to the precise
semantics?

6. Connecting these requires to reflect the two views of fixpoints:

- "information order" in in Scott domains, ie "higher" is more
completely calculated

- in an abstract value domain, "higher" is a less accurate
description, e.g., to say "top" when actual runs would yield even
values.

*)

module type Symantics = sig
  (* Domain definitions: exactly the same as in JN, p4 *)
  type var = int
  type vl				(* abstract *)
  type sto				(* abstract *)
  type cmd = sto -> sto
  type exp = sto -> vl

  (* Semantic functions, literally the same as on p4 of JN *)
  val assign : var -> exp -> cmd
  val seq    : cmd -> cmd -> cmd
  val cond   : exp -> cmd -> cmd -> cmd
  val whil   : exp -> cmd -> cmd

  (* additional semantic functions for expressions; they are assumed in JN,
     probably because they are the same as those in the earlier part of the
     Jones, Nielson paper.
     We add them so that we could run the examples.
   *)

  val vex : var -> exp  
  val int : int -> exp			(* literals *)
  val add : exp -> exp -> exp
  val leq : exp -> exp -> exp
end;;

(* examples *)

module Ex1(S:Symantics) = struct
 open S
 let vx = 0				(* `declarations' of variables *)
 let vy = 1
 let vr = 100				(* Program result *)
 let x = vex vx
 let y = vex vy
 let res = seq 
            (assign vx (int 1))
            (seq
	      (assign vy (int 2))
	      (cond (leq x y) 
		  (assign vr y) 
		  (assign vr (int 4))))
end;;

module Ex2(S:Symantics) = struct
 open S
 let vx = 0				(* `declarations' of variables *)
 let vy = 1
 let vr = 100				(* Program result *)
 let x = vex vx
 let y = vex vy
 let mul = seq
            (assign vr (int 0))
            (whil (leq (int 1) x)
	       (seq
		  (assign vr (add (vex vr) y))
		  (assign vx (add x (int (-1))))))
 let res1 = seq 
            (assign vx (int 4))
            (seq
	       (assign vy (int 2))
               mul)
 let res2 = seq 
            (assign vx (int 4))
            (seq
	       (assign vy (int 3))
               mul)
end;;


(* The standard interpretation, p4 of JN *)

module IStandard = struct
  type var = int
  type vl  = int				(* Flat CPO *)
  type sto = (var * vl) list			(* a finite map *)
  type cmd = sto -> sto
  type exp = sto -> vl

  let assign v e   = fun s -> (v,e s) :: s
  let seq c1 c2    = fun s -> c2 (c1 s)
  let cond e c1 c2 = fun s -> if e s <> 0 then c1 s else c2 s
  let rec whil e c = fun s -> if e s <> 0 then whil e c (c s) else s

  let vex v = fun s -> List.assoc v s
  let int i = fun s -> i
  let add e1 e2 = fun s -> e1 s + e2 s
  let leq e1 e2 = fun s -> if e1 s <= e2 s then 1 else 0

  let s0 = []
end;;

let test1 = let module M = Ex1(IStandard) in M.res IStandard.s0;;
(*
 Store after evaluating the program
val test1 : IStandard.sto = [(100, 2); (1, 2); (0, 1)]
*)

let test2 = let module M = Ex2(IStandard) in 
 (List.assoc M.vr (M.res1 IStandard.s0),
  List.assoc M.vr (M.res2 IStandard.s0));;
(* 
  (8, 12)
*)

 
(* The even-odd, abstract interpretation, p5 of JN *)

module IParity = struct
  type var = int
  type vl  = Even | Odd | Top
  type sto = (var * vl) list			(* a finite map *)
  type cmd = sto -> sto
  type exp = sto -> vl

  let vl_union = function
    | (x,y) when x = y -> x
    | _ -> Top

  let ext_sto s (vr,vl) =
    (vr,vl) ::
    try  List.remove_assoc vr s with Not_found -> s

  let sto_union s1 s2 =
    let f s (vr,vl) =
      if List.mem_assoc vr s then
	 ext_sto s (vr,vl_union (vl,List.assoc vr s))
      else (vr,vl) :: s
    in
    List.fold_left f s1 s2
         
  let same_sto (s1:sto) (s2:sto) : bool =
    let f (vr1,_) (vr2,_) = vr1 - vr2 in
    List.stable_sort f s1 = List.stable_sort f s2

  let assign v e   = fun s -> ext_sto s (v,e s)
  let seq c1 c2    = fun s -> c2 (c1 s)
  let cond e c1 c2 = fun s -> sto_union (c1 s) (c2 s)
  let rec whil e c = fun s -> let s1 = sto_union s (c s) in
                              if same_sto s s1 then s else whil e c s1

  let vex v = fun s -> List.assoc v s
  let int i = fun s -> if i land 1 = 0 then Even else Odd
  let add e1 e2 = fun s -> match (e1 s, e2 s) with
  | (Even,Even) | (Odd,Odd)  -> Even
  | (Even,Odd)  | (Odd,Even) -> Odd
  | _ -> Top
  let leq e1 e2 = fun s -> Top

  let s0 = []
end;;

let testp1 = let module M = Ex1(IParity) in M.res IParity.s0;;
(*
 Store after evaluating the program
val testp1 : IParity.sto =
  [(0, IParity.Odd); (1, IParity.Even); (100, IParity.Even)]
*)

let test2 = let module M = Ex2(IParity) in 
 (List.assoc M.vr (M.res1 IParity.s0),
  List.assoc M.vr (M.res2 IParity.s0));;
(* 
    val test2 : IParity.vl * IParity.vl = (IParity.Even, IParity.Top)
*)
