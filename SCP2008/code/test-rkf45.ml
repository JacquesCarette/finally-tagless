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

(* some poor unit testing *)
let sp1 = spline_interp (Array.of_list [1.;2.;3.;4.;5.;6.])
                     (Array.of_list (List.map (fun x -> Array.make 1 x) [1.;3.;4.;3.;4.;2.])) ;;

let knots = 
     let a = -2.0 and b = 2.0 and num_knots = 5 in
	 Array.init num_knots 
        (fun i -> (a +. ((b -. a) *. (float_of_int
        i /. float_of_int (num_knots - 1))))) ;;
let (ss,e) = rk45 knots (Array.make 1 (1.0)) test_f0 ;;
let tt = spline_interp knots ss;;
let (sol0,err0) = (odesolve (-2.0) 2.0 4 (Array.make 1 (1.0)) test_f0) ;;

let sf0 = .! sol0;;
let ans0 = ans_g sf0;;

let (sol1,err1) = (odesolve (-2.0) 2.0 20 (Array.make 1 (1.0)) test_f1) ;;
let sf1 = .! sol1;;
let ans1 = ans_g sf1;;

let (sol2,err2) = (odesolve (-2.0) 2.0 2 (Array.make 1 (1.0)) test_f0) ;;
let sf2 = .! sol2;;
let ans2 = ans_g sf2;;
