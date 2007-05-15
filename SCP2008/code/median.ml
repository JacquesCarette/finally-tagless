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

See the survey of various methods in
http://ndevilla.free.fr/median/median/median.html ?

It seems none of the methods seem (provably) better than O(n*log(N)), although
Knuth shows the existence of O(n) methods.
*)

(* This algorithm differs from that in median-filt.scm.
   The present algorithm uses essentially an insertion sort whereas
   the former relied on a merge sort.
*)

(* open Infra *)


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


(* A center-view representation for the sorted list
   A combination of two sorted lists: (l,r)
   The list 'l' is sorted in ascending order, and list 'r' is in the descending
   order, and all elements of r are no less than all elements of l.
   Furthermore, we require that either
       length(l) == length(r)       -- Even parity
   or  length(l) == length(r) + 1   -- Odd number of elements
*)

type ('a,'v) farr = 
   | Ev of ('a,'v,asc) carr * ('a,'v,des) carr
   | Od of ('a,'v,asc) carr * ('a,'v,des) carr
;;

(* Insert a new element into the farr list *)

let rec fins x = function
   | Ev (Nil,Nil) -> V (Od (cons x nil,nil))
   | Ev (Nil,_) -> assert false
   | Od (Nil,_) -> assert false
   | Ev ((CAL _ as l),r) -> elift (fun l' -> fins x (Ev (l',r))) (force asc l)
   | Ev (CAE (y,l),r) -> C ((x,y),
                             V (Od (cons y (lcons x l),r)), (* x < y *)
			    (* x >= y, need to compare x with head of r *)
			    let f = function 
			      |	(CAE (x,r)) -> V (Od (cons x (cons y l),r))
			      |	_ -> assert false in
			    (elift f (force des (lcons x r))))
   | Od ((CAL _ as l),r) -> elift (fun l' -> fins x (Od (l',r))) (force asc l)
   | Od (CAE (y,l),r) -> C ((x,y),
                             V (Ev (lcons x l,(cons y r))), (* x < y *)
                             V (Ev (cons y l,(lcons x r))))
;;

(* Pick the median *)

let rec pick = function 
   | Ev (Nil,_) -> assert false
   | Od (Nil,_) -> assert false
   | Ev ((CAL _ as l),r) -> elift (fun l' -> pick (Ev (l',r))) (force asc l)
   | Ev (CAE (x,_),_) -> V x
   | Od ((CAL _ as l),r) -> elift (fun l' -> pick (Od (l',r))) (force asc l)
   | Od (CAE (x,_),_) -> V x
;;


let gen l = List.fold_right (fun x -> elift (fins x)) l (V (Ev (nil,nil)))
;;

(* convert the result into a code value. That makes it easy to visualize *)
let rec codify = function
  | V x -> x
  | C ((x1,x2),l,r) -> .<if .~x1 < .~ x2 then .~(codify l) else .~(codify r)>.
;;

let doit l = codify (elift pick (gen l));;

let testm2 = doit [.<1>.;.<2>.];;
let testm3 = doit [.<1>.;.<2>.;.<3>.];;
let testm4 = doit [.<1>.;.<2>.;.<3>.;.<4>.];;



(* Exhaustive testing *)

let rec iota n = (* 1..n *)
  if n <= 1 then [1] else (iota (pred n)) @ [n]
;;

let rec insert_all x = function
  | [] -> [[x]]
  | (h::t) -> (x::h::t) :: (List.map (fun l -> h::l) (insert_all x t))
;;

let rec permute_all = function
  | [] -> [[]]
  | (h::t) -> List.concat (List.map (insert_all h) (permute_all t))
;;

exception BM of int list * int

let test_med n mf =
  let l = iota n in
  let expected = List.nth l ((n-1)/2) in
  let () = Printf.printf "\ntesting median of 1..%d, which is %d" n expected in
  List.iter (fun l -> let found = mf l in
                      if expected = found then () else
		      raise (BM (l,found)))
    (permute_all l)
;;


let m4c = .<fun [x1;x2;x3;x4] -> .~(doit [.<x1>.;.<x2>.;.<x3>.;.<x4>.])>. in
let m4  = .! m4c in
   test_med 4 m4
;;

