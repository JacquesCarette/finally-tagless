open Infra;;
open Rkf45;;

let pf = function
      Left x -> begin print_string x; print_endline "" end;
    | Right x -> begin print_float x; print_endline "" end;;

let ans_g f = 
    let (a,b) = f in
    begin 
        a ();
        List.iter (fun x -> pf (b 0 x)) 
        [-2.1; -2.0; -1.97; -0.97; 0.1; 1.82; 1.84; 1.93; 2.0; 2.01]
    end;;

(* constant! *)
let test_f0 x y = 
    Array.init (Array.length y) (fun i -> 0. );;

let test_f1 x y = 
    Array.init (Array.length y) (fun i -> x *. (y.(i) *. y.(i)));;

let sol0 = (odesolve (-2.0) 2.0 20 0.001 (Array.make 1 (1.0)) test_f0) ;;

let sf0 = .! sol0;;
let ans0 = ans_g sf0;;

(*
let sol1 = (odesolve (-2.0) 2.0 20 0.001 (Array.make 1 (1.0)) test_f1) ;;
let sf1 = .! sol1;;
let ans1 = ans_g sf1;;
*)
