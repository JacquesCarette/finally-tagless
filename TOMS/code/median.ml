(* 
Generating decision trees to efficiently find a median of a sequence

Given a sequence of n comparable elements, its median is the m-th
minimum (the minimum of rank m) of the sequence, where the absolute
minimum has rank 0 and m = floor((n-1)/2).

We are given a list of values, and we have to generate code that 
returns the median of the sequence.  To be more precise,
we are to generate a decision tree.  This approach needs no extra
storage, has no loops, and particularly lends itself to an
implementation as a combinational circuit. However, writing such
decision trees is quite challenging even for 5 inputs, especially if
we strive for optimality in terms of the maximum or average depths
of the tree.

The code has to be efficient in terms of the max and average
number of comparsions. The minimal possible decision depth for a
sequence of n elements is n-1. This is the number of comparisons
needed to verify that the input sequence is sorted. Clearly an
algorithm that achieves such lower bound will do poorly in terms of
the maximal depth: The decision tree will always have n! leaves;
therefore, shortening the minimal depth will lengthen the maximal
one. The only sensible optimality criterion is the average or the
max number of comparisons.

*)

open Infra

(* Our domains:
   Our goal is to find the median of a sequence of values of type 'v.
   We assume the total order on that type of values. 
   Because we are to generate decision trees, we need to operate
   on (AST nodes, etc) that represent the run-time values of type 'v.
   ('a,'v) code is such a representation.
*)

type asc
type des

(* ('a,'v,'d) carr is a (lazy) list of sorted values. 'd is the sorting
 order: ascending or descending. 
 The list is lazy and so it has two `cons' operations: 
   CAE (x,l) -- real cons. It means that 'x' is definitely no smaller
     than any element of 'l', if 'd = Asc.
   CAL (x,l) -- lazy cons. The relationship of 'x' regarding the tail of
     the list, 'l', is not yet known.
*)

type ('a,'v,'d) carr =
    Nil 
  | CAL of ('a,'v) code * ('a,'v,'d) carr
  | CAE of ('a,'v) code * ('a,'v,'d) carr
;;

let nil = Nil
let cons x ca  = CAE (x,ca)
let lcons x ca = CAL (x,ca)
(* typecase on asc/des types: dictionaries for the dir typeclass *)
let asc (dummy : ('a,'v,asc) carr) x y = (y,x)
let des (dummy : ('a,'v,des) carr) x y = (x,y)
;;


(* The data type ('c,'e) ecode represents nodes in the decision tree.
   C ((x1,x2),n1,n2) represents the decision comparing x1 with x2,
   and its two outcomes, n1 and n2. The outcome n1 corresponds to
   x1 < x2.
 *)

type ('c,'e) ecode = 
    V of 'e
  | C of ('c * 'c) * ('c,'e) ecode * ('c,'e) ecode
;;

let rec elift f = function 
  | V x -> f x
  | C (cmp,l,r) -> C (cmp,elift f l,elift f r)
;;

(* Force a lazy cons cell. The function returns a decision tree,
   which describes the choices comparing the element 'x' with the
   other elements in the list. We force the lazy cons cell only as much
   as needed to convert it to the eager cell -- but no further.
*)

let rec force dir = function 
  | CAL (x,Nil) -> V (cons x nil)
  | CAL (x,CAE (y,r)) as c -> 
          C ((dir c x y), V (cons x (cons y r)), V (cons y (lcons x r)))
  | CAL (x,(CAL _ as l)) -> elift (fun yr -> force dir (CAL (x,yr)))
	                        (force dir l)
  | e -> V e
;;

(* Force all the way and eliminate all lazy cells *)

let rec force_all dir = function 
  | Nil -> V Nil
  | CAL _ as x -> elift (force_all dir) (force dir x)
  | CAE (x,r) -> elift (fun r' -> V (CAE (x,r'))) (force_all dir r)
;;

(* convert the carr into a code value. That makes it easy to visualize *)
let codify_carr dir vc = 
  let rec cod = function
  | V Nil -> .<[]>.
  | V (CAE (x,r)) -> let c = cod (V r) in .< .~x :: .~c >.
  | C ((x1,x2),l,r) -> .<if .~x1 < .~ x2 then .~(cod l) else .~(cod r)>.
  | V (CAL _) -> assert false
  in cod (elift (force_all dir) vc)
;;


let test1 = codify_carr asc (V (lcons .<1>. nil));;
let test2 = codify_carr asc (V (lcons .<2>. (lcons .<1>. nil)));;
let test3 = codify_carr asc (V (lcons .<3>. (lcons .<2>. (lcons .<1>. nil))));;

(*	

type ('a,'v) farr = ('a,'v) carr * ('a,'v) carr
;;
			    
let rec fins x = function
    V (CANil,CANil) -> V (CAC (x,nil),nil)
  | V ((CAIns (y,ca)),co) -> ins x (cp (ins y carr) co)
  | V ((CAC (c,l)),r) -> C (x,c,(V ((CAC (c,CAIns (x,l))),r)),
			    (V (CAC (c,l)),(CAC 

let rec gen = function
    [x] -> cod nil x nil
    [x1;x2] -> ins x1 (gen [x2])
;;

C (x1,x2,CA (CANil,[|V x1;V x2|],CANil),
		        CA (CANil,[|V x1;V x2|],CANil))
;;

*)
