(* 0-CFA in tagless final style, attempt 0 *)

type label = string;;
type varlab = string;;

let gensym = 
  let counter = ref 0 in
  fun () -> incr counter; "g" ^ string_of_int !counter;;

type cache = (label * label) list;;

(* Abstract values and the representation. The variant D
   is like S in our paper
*)
type ('a,'b) av = V of label list | D of 'b;;

type ('a,'b) rep = R of ('a,'b) av * label * cache 
;;

let lab n = function 
  | R (x,l,c) ->  let l' = string_of_int n in
                  R (x,l',(l',l)::c);;

let int (n:int) = let l = gensym () in R (V [l],l,[]);;

let bind m f = List.concat (List.map f m);;

let add e1 e2 = match (e1,e2) with
 (R (V n1s,l1,c1), R (V n2s,l2,c2)) -> 
   R (V (bind n1s (fun n1 -> bind n2s (fun n2 -> [n1 ^ "+" ^ n2]))),
      "", c1 @ c2)
;;

let lam lv f = 
  R ((D (function R (av,l,c) as arg ->
    match f arg with
      R (vr,lr,cr) -> R (vr,lr,(lv,l)::cr))),
     gensym (),[])
;;
     

let app e1 e2 =
 match (e1,e2) with
 | (R (V fs,lf,c1), R (xs,lx,c2)) ->
     R (V (bind fs (fun n1 -> bind [lx] (fun n2 -> [n1 ^ " @ " ^ n2]))),
      "", c1 @ c2)
 | (R (D f,lf,cachef), (R (xs,lx,cachex) as arg)) ->
     let R (rs,lr,cacher) = f arg in
     R (rs,lr,cacher @ cachef)
;;

let test1 = lab 2 (lam "x" (fun x -> lab 1 x));;

let test2 = lab 5 (app test1 (lab 4 (lam "y" (fun y -> lab 3 y))));;

let fid () = lab 2 (lam "x" (fun x -> lab 1 x));;

(* The result matches 0-CFA in the book, moduo the fact that two
   values of fid are created: two invocations of fid ()
*)

let test332 = (lab 8
		 (app 
		    (lab 5 (app (lab 3 (fid ())) (lab 4 (fid ()))))
		    (lab 7 (lam "y" (fun y -> lab 6 y)))));;

(*
let fix f = 
    let newlab = gensym () in
    let fixf = function arg -> app (R (V [newlab],newlab,[])) arg in
    f (R ((D fixf),newlab,[]))
;;
*)

let fix lf f = 
  let table = ref [] in
  let newlab = gensym () in
  let rec fixf (R (_,la,_) as arg) = 
    if List.mem la !table then R (V [],newlab,[])
    else (table := la :: !table;
	  app (f (R ((D fixf),newlab,[]))) arg) in
  f (R ((D fixf),newlab,[]))
;;
   

let tf1 = fix "f" (fun self -> lam "x" (fun x -> app self x));;

(* Example 3.2 The result seems to agree with the analysis on p 143 *)

let test32 = 
 let g = lab 5 (fix "f" (fun self -> lam "x" (fun x ->
   lab 4 (app (lab 1 self) (lab 3 (lam "y" (fun y -> lab 2 y)))))))
 in lab 9 (app (lab 6 g) (lab 8 (lam "y" (fun z -> lab 7 z))))
;;
