open Infra;;

type ('a, 'b) either = Left of 'a | Right of 'b;;

(* runge kutta 4th order integration *)
let evalrk45 x h yin f =
    let l = Array.length yin in
    let hh = h *. 0.5 in
    let xh = x +. hh in
    let dydx = f x yin in
    let yt = Array.init l (fun i -> yin.(i) +. hh *. dydx.(i)) in
    let dyt = f xh yt in
    let yt = Array.init l (fun i -> yin.(i) +. hh *. dyt.(i)) in
    let dym = f xh yt in
    let yt = Array.init l (fun i -> yin.(i) +. h *. dym.(i)) in
    let dym = Array.init l (fun i -> dym.(i) +. dyt.(i)) in
    let dytt = f (x+.h) yt in
    let h6 = h /. 6.0 in
    Array.init l (fun i -> 
            yin.(i) +. h6 *. (dydx.(i)+.dytt.(i)+.2.0*.dym.(i)) )
;;
let rk45 xknot yin tolerance f =
    let xknot_l = Array.length xknot 
    and yin_l = Array.length yin in
    (* Make the output array *)
    let yout = Array.make_matrix xknot_l yin_l 0.0 in
    let yh1out = Array.make_matrix xknot_l yin_l 0.0 in
    let yh2out = Array.make_matrix xknot_l yin_l 0.0 in
    let good = ref true 
    in
    (* yout.(0) = yin *)
    yout.(0) <- yin;
    yh1out.(0) <- yin;
    yh2out.(0) <- yin;
    (* For each knot, we perform r.k. 4th order integration *)
    for i = 0 to xknot_l - 2 do
        let dx = xknot.(i+1) -. xknot.(i) in
        let hdx = dx /. 2.0
        in
            yout.(i+1) <- evalrk45 (xknot.(i)) dx yout.(i) f;
            yh1out.(i+1) <- evalrk45 (xknot.(i)) hdx yout.(i) f;
            yh2out.(i+1) <- evalrk45 (xknot.(i) +. hdx) hdx yh1out.(i+1) f;
            (* compute relative truncation error *)
            let trunc_err = ref 0.0 in
            for j = 0 to Array.length yout.(i) - 1 do
                let err = abs_float ( (yh2out.(i+1).(j) -. yout.(i+1).(j)) /. yh2out.(i+1).(j)) in
                    trunc_err := if err >= !trunc_err then err else !trunc_err;
            done;
            (* check if truncation error is acceptable *)
            if !trunc_err > tolerance then
            begin
                good := false;
                print_string "Relative error tolerance not satisfied. Between: ";
                print_float xknot.(i);
                print_string " and ";
                print_float xknot.(i+1);
                print_endline "";
                print_string "truncation error = ";
                print_float !trunc_err;
                print_endline "";
            end
    done;
    (yout, !good)
;;

(* natural cubic spline interpolation *)
let spline x y =
    let y2out = Array.make_matrix (Array.length x) (Array.length y.(0)) 0.0 in
    let u = Array.make_matrix (Array.length x) (Array.length y.(0)) 0.0
    in
    (* zero the second derivatives *)
    for i = 0 to Array.length y2out.(0) - 1 do
        y2out.(0).(i) <- 0.0;
        y2out.(Array.length x - 1).(i) <- 0.0;
        u.(0).(i) <- 0.0;
        u.(Array.length x - 1).(i) <- 0.0;
    done;
    (* Tridiagonal decomposition *)
    for i = 1 to Array.length x - 2 do
        let s = (x.(i) -. x.(i-1)) /. (x.(i+1)-.x.(i-1)) in
        for j = 0 to Array.length y2out.(0) - 1 do
            let p = s *. y2out.(i-1).(j) +. 2.0 in
            y2out.(i).(j) <- (s -. 1.0) /. p;
            u.(i).(j) <- (y.(i+1).(j) -. y.(i).(j)) /. (x.(i+1) -. x.(i)) -. (y.(i).(j) -. y.(i-1).(j)) /. (x.(i) -. x.(i-1));
            u.(i).(j) <- (6.0 *. u.(i).(j)/.(x.(i+1) -. x.(i-1)) -. s*.u.(i-1).(j))/.p;
        done;
    done;
    for i = 0 to Array.length y2out.(0) - 1 do
        y2out.(Array.length x - 1).(i) <- (u.(Array.length x - 1).(i)) /. 1.0;
    done;
    (* Back substitution *)
    for i = Array.length x - 2 to 1 do
        for j = 0 to Array.length y2out.(0) - 1 do
            y2out.(i).(j) <- y2out.(i).(j) *. y2out.(i+1).(j) +. u.(i).(j);
        done
    done;
    y2out
;;

let find_spline_index num knots =
    let n1 = num-1 and n2 = num-2 in
    let scale = knots.(n1) -. knots.(0) in
    let k0 = knots.(0) in
    let fn1 = float_of_int num in
    let mult = fn1 /. scale in
    .< fun x -> min (int_of_float ((x -. k0) *. mult)) n2 >.
;;

let prespline knots yout y2out num_knots xx =
    fun lo -> (fun ii -> 
    let lo1 = lo+1 in
    let kl1 = knots.(lo1) and kl = knots.(lo) in
    let h = kl1 -. kl in
    let y1' = yout.(lo) in
    let yl1' = yout.(lo1) in
    let y2l' = y2out.(lo) in
    let y2l1' = y2out.(lo1) in
    perform
        kl1 <-- retN .<kl1>. ;
        kl  <-- retN .<kl>. ;
        h   <-- retN .<h>. ;
        l1mx <-- FloatDomain.minus kl1 xx;
        aa   <-- FloatDomain.div l1mx h;
        a2   <-- FloatDomain.times aa aa;
        a3   <-- FloatDomain.times a2 aa;
        aa'  <-- FloatDomain.minus a3 aa;
        aaa  <-- retN aa';
        xml  <-- FloatDomain.minus xx kl ;
        bb   <-- FloatDomain.div xml h;
        b2   <-- FloatDomain.times aa bb;
        b3   <-- FloatDomain.times b2 bb;
        bbb  <-- FloatDomain.minus b3 bb;
        yl   <-- Array1D.get y1' ii;
        x1   <-- FloatDomain.times aa yl;
        yl1  <-- Array1D.get yl1' ii;
        x2   <-- FloatDomain.times bbb yl1;
        y2l  <-- Array1D.get y2l' ii;
        x3   <-- FloatDomain.times aaa y2l;
        y2l1 <-- Array1D.get y2l1' ii;
        x4   <-- FloatDomain.times aaa y2l1;
        x5   <-- FloatDomain.plus  x3 x4;
        hh   <-- FloatDomain.times h  h;
        hh6  <-- FloatDomain.div   h .<6.0>.;
        x6   <-- FloatDomain.plus  x1 x2;
        x7   <-- FloatDomain.times x5 hh6;
        res  <-- FloatDomain.plus x5 x7;
        ret res)

(* this code should not use an array but instead generate an
explicit binary search tree of code *)
let splinet2 a b knots yout y2out num_knots =
    let init arr = 
        let body lo = 
        .< (.~arr).(lo) <- fun y -> fun i ->
          .~(runM (prespline knots yout y2out num_knots .<y>. lo .<i>. )) >.
        in .< .~(CodeTrans.full_unroll 0 (num_knots-2) body); () >. in
    let bod arr i = .< fun x -> 
        if ((x<a) || (x>b)) then
            Left "Error: x not in range"
        else
            Right ((.~arr).(
                .~(find_spline_index num_knots knots) x) x .~i) >. in
    (init, bod)
;;

let foo a b k y y2 n =
    let (init, bod) = splinet2 a b k y y2 n in
    .< let arr = Array.init (n-1) (fun l -> 
        (fun y -> fun i -> 0.)) in
       ( (fun () -> .~(init .<arr>. )), 
       (fun i -> .~(bod .<arr>. .<i>. ))) >. ;;

exception Tolerance of string;;

let odesolve a b num_knots tolerance yin f =
    (* Create num_knots equally spaced knots across a..b *)
    if num_knots < 3 then
    begin
        print_string "Error: There must be at least 3 knots";
        print_endline ""
    end;
    let knots = Array.init num_knots 
        (fun i -> (a +. ((b -. a) *. (float_of_int
        i /. float_of_int (num_knots - 1))))) in
    (* Compute the integrated values at each knot starting with yin *)
    let (yout,good) = rk45 knots yin tolerance f in
    (* Compute natural cubic spline second derivatives *)
    let y2out = spline knots yout in
    (* Construct the ode solution as a function that takes input x in a..b*)
    let odef = 
        if good == false then
            raise (Tolerance "Error: tolerance not satisfied")
        else 
            .< .~(foo a b knots yout y2out num_knots ) >.
    in odef ;;
