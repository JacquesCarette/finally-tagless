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

type ('a,'v) carr =
    CANil 
  | CAL of ('a,'v) code * ('a,'v) carr
  | CAE of ('a,'v) code * ('a,'v) carr
;;
type ('a,'v) farr = ('a,'v) carr * ('a,'v) carr
;;

type ('c,'e) ecode = 
    V of 'e
  | C of 'c * 'c * ('c,'e) ecode * ('c,'e) ecode
;;

type d = L | R;;

let nil = CANil;;
let cons x ca  = CAE (x,ca);;
let lcons x ca = CAL (x,ca);;

let rec elift f = function 
  | V x -> f x
  | C (x,y,l,r) -> C (x,y,elift f l,elift f r)
;;

let rec force dir = function 
  | CAL (x,CANil) -> V (CAE (x,CANil))
  | CAL (x,CAE (y,r)) -> C (x,y, V (CAE (x,CAE (y,r))),
			    V (CAE (y,CAL (x,r))))
  | CAL (x,CAL (y,r)) -> elift (fun yr -> force dir (CAL (x,yr)))
	                       (force dir (CAL (y,r)))
  | e -> V e
;;

let rec force_all dir = function 
  | CANil -> V CANil
  | CAL _ as x -> elift (force_all dir) (force dir x)
  | CAE (x,r) -> elift (fun r' -> V (CAE (x,r'))) (force_all dir r)
;;

let codify_carr dir vc = 
  let rec cod = function
  | V CANil -> .<[]>.
  | V (CAE (x,r)) -> let c = cod (V r) in .< .~x :: .~c >.
  | C (x1,x2,l,r) -> .<if .~x1 < .~ x2 then .~(cod l) else .~(cod r)>.
  in cod (elift (force_all dir) vc)
;;


let test1 = codify_carr L (V (lcons .<1>. nil));;
let test2 = codify_carr L (V (lcons .<2>. (lcons .<1>. nil)));;
let test3 = codify_carr L (V (lcons .<3>. (lcons .<2>. (lcons .<1>. nil))));;

(*	
			    
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
