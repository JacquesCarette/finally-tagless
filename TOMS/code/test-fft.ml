open Fft;;

(* List of size n with (1,0) at position l *)
let rec impulse n l =
   if n = 0 then []
   else let t = if l = 0 then (1.0,0.0) else (0.0,0.0)
   in t::(impulse (n-1) (l-1))

let rec unpair = function ((x,y)::tail) -> x::y::(unpair tail)
  | [] -> []

(* Determine the max abs difference (the inf-norm) of two lists of
   pairs.
*)
let inf_norm l1 l2 =
  let rec scan pos loc diff =
    function ((x::tailx),(y::taily)) ->
      let curr_diff = abs_float (x -. y) in
      if curr_diff >= diff then scan (succ pos) pos curr_diff (tailx,taily)
      else scan (succ pos) loc diff (tailx,taily)
      | ([],[]) -> (loc,diff)
  in scan 0 (-1) 0.0 (l1,l2)

let ttt3 = .<fun x -> .~(gen_ad Forward (4 * 2) .<x>. ) >.

let test_ad dir lst =
  let n = List.length lst in
  let arr = Array.make (n * 2) 0.0 in
  let _ = List.fold_left (fun n (a,b) -> (arr.(n) <- a; arr.(n+1) <-b; n+2))
      0 lst in
  let rec tolist arr i = if i = Array.length arr then []
      else (arr.(i),arr.(i+1))::(tolist arr (i+2)) in
  let tranc = .<fun x -> .~(gen_ad dir (n * 2) .<x>.)>. in
  let tran = (.!tranc) (arr) in
  tolist tran 0

  
(* Tests *)
let () = assert ((7,0.0) = inf_norm
                 (List.map (fun x -> 4.0 *. x) (unpair (impulse 4 1 )))
(unpair (test_ad Inverse (test_ad Forward (impulse 4 1)))
))

let () = assert ((10, 5.11946041115152184e-12) = inf_norm
                 (List.map (fun x -> 8.0 *. x) (unpair (impulse 8 1 )))
(unpair (test_ad Inverse (test_ad Forward (impulse 8 1)))
))

let () = assert ((2, 2.12345696581905941e-11) = inf_norm
                 (List.map (fun x -> 16.0 *. x) (unpair (impulse 16 1 )))
(unpair (test_ad Inverse (test_ad Forward (impulse 16 1)))
))

let ttt4 = .<fun x -> .~(gen_ad Forward (16 * 2) .<x>. ) >.
;;
