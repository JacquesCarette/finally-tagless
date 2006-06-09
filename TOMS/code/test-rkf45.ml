open Infra;;
open Rkf45;;

let test_f x y = 
    Array.init (Array.length y) (fun i -> x *. (y.(i) *. y.(i)));;

let sol1 = 
    (odesolve (-2.0) 2.0 20 0.001 (Array.make 1 (1.0)) test_f) ;;
(*
let solutionf = .! sol;;

let ans = List.iter (fun x -> pf (solutionf x)) 
    [-2.1; -2.0; -1.97; -0.97; 0.1; 1.82; 1.84; 1.93; 2.0; 2.01] ;;
*)
