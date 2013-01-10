open StateCPSMonad

open Domains_code
open Code

type ('a, 'b) either = Left of 'a | Right of 'b

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

let rk45 xknot yin f =
    let xknot_l = Array.length xknot 
    and yin_l = Array.length yin in
    (* Make the output array *)
    let yout = Array.make_matrix xknot_l yin_l 0.0 in
    let yh1out = Array.make_matrix xknot_l yin_l 0.0 in
    let yh2out = Array.make_matrix xknot_l yin_l 0.0 in
    (* yout.(0) = yin *)
    yout.(0) <- yin;
    yh1out.(0) <- yin;
    yh2out.(0) <- yin;
	let trunc_err = ref 0.0 in
    begin
    (* For each knot, we perform r.k. 4th order integration *)
    for i = 0 to xknot_l - 2 do
        let dx = xknot.(i+1) -. xknot.(i) in
        let hdx = dx /. 2.0
        in
            yout.(i+1) <- evalrk45 (xknot.(i)) dx yout.(i) f;
            yh1out.(i+1) <- evalrk45 (xknot.(i)) hdx yout.(i) f;
            yh2out.(i+1) <- evalrk45 (xknot.(i) +. hdx) hdx yh1out.(i+1) f;
            (* compute relative truncation error *)
            for j = 0 to Array.length yout.(i) - 1 do
                let err = abs_float ( (yh2out.(i+1).(j) -. yout.(i+1).(j)) /. yh2out.(i+1).(j)) in
                    if err >= !trunc_err then trunc_err := err 
            done;
    done;
    (yout, !trunc_err)
	end

(* natural cubic spline interpolation *)
let spline_interp x y =
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
            let p = (s *. y2out.(i-1).(j)) +. 2.0 in
            y2out.(i).(j) <- (s -. 1.0) /. p;
            u.(i).(j) <- (y.(i+1).(j) -. y.(i).(j)) /. (x.(i+1) -. x.(i)) -. (y.(i).(j) -. y.(i-1).(j)) /. (x.(i) -. x.(i-1));
            u.(i).(j) <- (6.0 *. u.(i).(j)/.(x.(i+1) -. x.(i-1)) -. s*.u.(i-1).(j))/.p;
        done;
    done;
    for i = 0 to Array.length y2out.(0) - 1 do
        y2out.(Array.length x - 1).(i) <- 0.0;
    done;
    (* Back substitution *)
    for i = Array.length x - 2 downto 0 do
        for j = 0 to Array.length y2out.(0) - 1 do
            y2out.(i).(j) <- (y2out.(i).(j) *. y2out.(i+1).(j)) +. u.(i).(j);
        done
    done;
    y2out
;;

let find_spline_index num knots x =
    let n1 = num-1 and n2 = num-2 in
    let scale = knots.(n1) -. knots.(0) in
    let k0 = knots.(0) in
    let fn1 = float_of_int num in
    let mult = fn1 /. scale in
    .< min (int_of_float ((.~x -. k0) *. mult)) n2 >.
;;

open FloatDomainL
let prespline knots (yout:float array array) 
    (y2out:float array array) xx =
    fun lo -> (fun ii -> 
    let lo1 = lo+1 in
    let kl1 = knots.(lo1) and kl = knots.(lo) in
    let h = kl1 -. kl in
    let yl = yout.(lo).(ii) in
    let yl1 = yout.(lo1).(ii) in
    let y2l = y2out.(lo).(ii) in
    let y2l1 = y2out.(lo1).(ii) in
    let hh6   = (h *. h) /. 6.0 in
    let! h   = ret (lift h) in
    let! aa  = retN (divL ((lift kl1) -^ xx) h) in
    let! aaa = retN ((aa *^ aa *^ aa) -^ aa) in
    let! bb  = retN (divL (xx -^ (lift kl)) h) in
    let! bbb = retN ((bb *^ bb *^ bb) -^ bb) in
    let! x1  = ret (aa *^ (lift yl)) in
    let! x2  = ret (bb *^ (lift yl1)) in
    let! x3  = ret (aaa *^ (lift y2l)) in
    let! x4  = ret (bbb *^ (lift y2l1)) in
    let! x5  = ret ((x3 +^ x4) *^ (lift hh6)) in
    ret ((x1 +^ x2) +^ x5))

(* this code should not use an array but instead generate an
explicit binary search tree of code *)
let spline_gen a b knots (yout:float array array) 
    (y2out:float array array) num_knots =
    let init arr = 
        let body lo i = 
        .< (.~arr).(lo).(i) <- fun y ->
          .~(runM (prespline knots yout y2out .<y>. lo i ) []) >. in
        let ll = Array.length (y2out.(0)) - 1 in
        let code = (Transformers.full_unroll 0 (num_knots-2) 
            (fun j -> Transformers.full_unroll 0 ll (fun lo -> body j lo))) in
            seq code cunit in
    let bod arr i = .< fun x -> 
        if ((x<a) || (x>b)) then
            Left "Error: x not in range"
        else
            Right ((.~arr).(
                .~(find_spline_index num_knots knots .<x>.)).(.~i) x ) >. in
    (init, bod)
;;

let construct a b k y y2 n =
    let m = Array.length y in
    let (init, bod) = spline_gen a b k y y2 n in
    .< let arr = Array.init (n-1) (fun _ -> 
        Array.init (m-1) (fun _ -> (fun _ -> 0.))) in
       ( (fun () -> .~(init .<arr>. )), 
       (fun i -> .~(bod .<arr>. .<i>. ))) >. ;;

let odesolve a b num_knots yin f =
    (* Create num_knots equally spaced knots across a..b *)
    if num_knots < 2 then
    begin
        print_string "Error: There must be at least 2 knots";
        print_endline ""
    end;
    let knots = Array.init num_knots 
        (fun i -> (a +. ((b -. a) *. (float_of_int
        i /. float_of_int (num_knots - 1))))) in
    (* Compute the integrated values at each knot starting with yin *)
    let (yout,max_err) = rk45 knots yin f in
    (* Compute natural cubic spline second derivatives *)
    let y2out = spline_interp knots yout in
    (* Construct the ode solution as a function that takes input x in a..b*)
	( construct a b knots yout y2out num_knots , max_err ) ;;
