(* To run this program:

echo '#use "power.ml" ;;' | LD_LIBRARY_PATH=~/u/cont/caml-shift rlwrap ~/u/cont/caml-shift/ocamltopcc -I ~/u/cont/caml-shift

*)

let rec power1 x = function
    | 0 -> 1
    | n -> x * power1 x (pred n) ;;
let test1 = power1 2 11 ;;

let rec power2 x = function
    | 0 -> .<1>.
    | n -> .<.~x * .~(power2 x (pred n))>. ;;
let test2 = .< fun x -> .~(power2 .<x>. 11) >. ;;

let square3 x = x * x ;;
let rec power3 x = function
    | 0 -> 1
    | 1 -> x
    | n when n mod 2 = 0 -> power3 (square3 x) (n/2)
    | n -> power3 (square3 x) (n/2) * x ;;
let test3 = power3 2 11 ;;

let square4 x = .<.~x * .~x >. ;;
let rec power4 x = function
    | 0 -> .<1>.
    | 1 -> x
    | n when n mod 2 = 0 -> power4 (square4 x) (n/2)
    | n -> .<.~(power4 (square4 x) (n/2)) * .~x>. ;;
let test4 = .< fun x -> .~(power4 .<x>. 11) >. ;;

let square5 x k = .<let r = .~x * .~x in .~(k .<r>.)>. ;;
let rec power5 x k = function
    | 0 -> k .<1>.
    | 1 -> k x
    | n when n mod 2 = 0 -> square5 x (fun s -> power5 s k (n/2))
    | n -> square5 x (fun s -> power5 s (fun r -> k .<.~r * .~x>.) (n/2)) ;;
let test5 = .< fun x -> .~(power5 .<x>. (fun r -> r) 11) >. ;;

open Delimcc ;;
let p = new_prompt () ;;
let reset f = push_prompt p f ;;
let shift f = take_subcont p (fun sk () ->
  push_prompt p (fun () -> (f (fun c ->
    push_prompt p (fun () -> push_subcont sk (fun () -> c)))))) ;;

let square6 x = shift (fun k -> .<let r = .~x * .~x in .~(k .<r>.)>.) ;;
let rec power6 x = function
    | 0 -> .<1>.
    | 1 -> x
    | n when n mod 2 = 0 -> power6 (square6 x) (n/2)
    | n -> .<.~(power6 (square6 x) (n/2)) * .~x>. ;;
let test6 = .< fun x -> .~(reset (fun () -> power6 .<x>. 11)) >. ;;
(*
val test6 : ('_a, int -> int) code =
  .<fun x_1 ->
   let r_2 = (x_1 * x_1) in
   let r_3 = (r_2 * r_2) in let r_4 = (r_3 * r_3) in ((r_4 * r_2) * x_1)>.
*)

(* Careless use of shift/reset for let insertion risks scope extrusion *)
let test7a = .<fun x -> .~(reset (fun () -> .<let y=x+1 in .~(power6 .<y>.
11)>.))>.;;
(*
val test7a : ('_a, int -> int) code =
  .<fun x_2 ->
   let r_4 = (y_3 * y_3) in
   let r_5 = (r_4 * r_4) in
   let r_6 = (r_5 * r_5) in let y_3 = (x_2 + 1) in ((r_6 * r_4) * y_3)>.
*)

(* The type system should force us to write not test7a above but test7b below *)
let test7b = .<fun x -> .~(reset (fun () -> .<let y=x+1 in .~(reset (fun
    () -> power6 .<y>.  11))>.))>.;;
(*
val test7b : ('_a, int -> int) code =
  .<fun x_2 ->
   let y_3 = (x_2 + 1) in
   let r_4 = (y_3 * y_3) in
   let r_5 = (r_4 * r_4) in let r_6 = (r_5 * r_5) in ((r_6 * r_4) * y_3)>.
*)

(* The code generated below crashes. This illustrates the need for multiple
 * delimiters with the same label. *)
let test8a = .<fun x -> let a = [| 1 |] and j = 5 in .~(reset (fun () -> 
    .<if j< 1 then .~(square6 .<a.(j)>.) else 0>.))>.;;
(*
val test8a : ('_a, 'b -> int) code =
  .<fun x_3 ->
   let a_4 = [|1|]
   and j_5 = 5 in
   let r_6 = (a_4.(j_5) * a_4.(j_5)) in if (j_5 < 1) then r_6 else 0>.
*)

(* The type system probably can't force us to write not test8a above but test8b
 * below *)
let test8b = .<fun x -> let a = [| 1 |] and j = 5 in .~(reset (fun () -> 
    .<if j< 1 then .~(reset (fun () -> square6 .<a.(j)>.)) else 0>.))>.;;
(*
val test8b : ('_a, 'b -> int) code =
  .<fun x_3 ->
   let a_4 = [|1|]
   and j_5 = 5 in
   if (j_5 < 1) then let r_6 = (a_4.(j_5) * a_4.(j_5)) in r_6 else 0>.
*)

