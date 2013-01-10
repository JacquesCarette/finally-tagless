(*
An attempt at a better FFT
        Use parsing in frequency rather than in time

 Formulas:
    The forward transform from an N-sample sequence
    x[j], j=0..N-1 to an N-sample sequence xf[k], k=0..N-1
    is

    (1) xf[k] = SUM{ x[j] * w^(j*k), j=0..N-1 }, k=0..N-1
    where w_N = exp(-2*PI*I/N)
    is the N-th primitive root of unity.
    For the inverse transform, w_N=exp(+2*PI*I/N)
    (like FFTW, we omit the scaling factor for the inverse transform).

 We assume that N is an even number, N=2*N2
 We re-write (1) as two equations:
    (2a) xf[2k] = SUM{ x[j] * w^(2*j*k), j=0..N-1 }, k=0..N2-1
                = SUM{ x[j] * w^(2*j*k), j=0..N2-1}
                + SUM{ x[j+N2] * w^(2*(j+N2)*k), j=0..N2-1}
                = SUM{ x[j] * w2^(j*k), j=0..N2-1}
                + SUM{ x[j+N2] * w2^(j*k), j=0..N2-1}
                      w^(2*N2*k) = w^(N*k) = 1^k = 1
                      Here w2 = w^2 = w_N2
                = FFT{ x[j] + x[j+N2], j=0..N2-1 }

    (2b) xf[2k+1] = SUM{ x[j] * w^(2*j*k+j), j=0..N-1 }, k=0..N2-1
                  = SUM{ x[j] * w^(2*j*k+j), j=0..N2-1 }
                  + SUM{ x[j+N2] * w^(2*(j+N2)*k+(j+N2)), j=0..N2-1 }
                  = SUM{ x[j]*w^j * w2^(j*k), j=0..N2-1 }
                  + SUM{-x[j+N2]*w^j * w2^(j*k), j=0..N2-1 }
                    w^N2 = exp(-PI*I) = -1 (the same for the inverse FFT)
                  = FFT{ (x[j] - x[j+N2])*w^j, j=0..N2-1 }

 So, an N-point FFT reduces to two N/2-point FFTs.

 ^Id: fft-neo.ml,v 1.13 2004/08/31 07:52:36 oleg Exp ^
*)

open StateCPSMonad

open Domains_code

(* ------------ unstaged nonmonadic list-based representation ------------ *)
let pi = 4.0 *. atan(1.0)

type dir = Forward | Inverse

(* multiplex evens and odds. They are of the same size *)
let rec merge_u = function
    ([], []) -> []
  | (y0::y0rest,y1::y1rest) -> y0::y1::(merge_u (y0rest,y1rest))
  | (_::_, []) -> failwith "should never be reached"
  | ([], _::_) -> failwith "should never be reached"

let cleave_list l = (* split a non-empty list l into two equal halves *)
    let rec take_drop n l acc =
      if n = 0 then (List.rev acc, l)
               else take_drop (n-1) (List.tl l) ((List.hd l)::acc)
    in take_drop ((List.length l) / 2) l []

(* Rational arithmetics *)
type rat = int * int (* j/n *)
let rat_one = (0,1)

(* The abstraction domain *)
type 'a newfloats_aa = Code of rat * ('a,float) code * ('a,float) code

(* *)
let rec gcd a b =
   if a > b then gcd b a
   else if a = 1 then 1
   else if a = 0 then b
   else gcd (b-a) a

let rec rationalize (j,n) =
   if j = 0 then rat_one
   else if j < 0 then rationalize (n+j,n)
   else if j >= n then rationalize (j-n,n)
   else let k = gcd j n in
   if k = 1 then (j,n) else (j/k, n/k)

let add_rat (j1,n1) (j2,n2) = rationalize (j1*n2+j2*n1, n1*n2)
let sub_rat (j1,n1) (j2,n2) = rationalize (j1*n2-j2*n1, n1*n2)
let min_rat ((j1,n1) as r1) ((j2,n2) as r2) =
   if j1*n2 > j2*n1 then r2 else r1

let w_aa dir n j = match dir with
     Forward -> rationalize (-j,n)
   | Inverse -> rationalize (j,n)

(* Multiply an abstract value by a known factor: a root of unity *)
let mult_aa (Code (f,re,im)) ru = Code (add_rat f ru, re,im)

(* rotate rat by 90 or 180 degrees to bring it to the 1 quadrant
   Return the rotation factor and the new rat.
   Their sum is the original rat.
*)
let rec rotate_to_QI ((j,n) as f) =
   if 2*j >= n then
      let (rf,newf) = rotate_to_QI (sub_rat f (1,2))
      in (add_rat rf (1,2),newf)
   else if 4*j >= n then ((1,4),sub_rat f (1,4)) else (rat_one,f)

(* Use the infrastructure module to get at the monadic infrastructure,
   as well as at the various code manipulation functions.  The aim is
   to avoid all annotations in the current code.  *)
open Code

(* Use the FloatDomain module as well.  No need to parametrize by
   Domain, as this is thoroughly float-specific *)
open FloatDomainL

let (y_sm,run) =
  let rec y_sm f = f (fun x s k -> y_sm f x s k) in
  let run f (abstract_simple, concretize) size nums =
    (* Write the results into the array *)
    let array_write l arr =
      let rec lfta l m =
        match l with
          [] -> arr
        | (c::tail) ->
            let sm = m+1 and ssm = m+2 in
			let (x,y) = concretize c in
            seq  (CArray1D.setL arr m x)
            (seq (CArray1D.setL arr sm y) (lfta tail (ssm)))
      in lfta l 0 in
    (* Read from an array *)
    let array_read arr n =
      let rec gl m l =
        if m = n then ret l
        else
          let sm = m+1 in
              let! c = abstract_simple 
                          (CArray1D.getL arr m, CArray1D.getL arr sm) in
               gl (m+2) (l @ [c])
      in gl 0 [] in
    let run_basic m = m [] (fun _ x -> array_write x nums)
    in
    run_basic ((let!) (array_read nums size) f)
  in
  (y_sm,run)

let liftcM op (x,y) =
   let! nx = op x in
   let! ny = op y in
   ret (nx, ny)

(* Decide, if multiplication by f is trivial.
   See the table above for the execution of trivial multiplication
*)
let rec trivial_mult ((j,n) as f) =
   if 2*j >= n then (* f in III and IV quadrants: reflect *)
        trivial_mult (rationalize (j-n/2,n))
   else match f with
    (0,1) -> true (* multiplication by 1 *)
   |(1,4) -> true (* multiplication by I *)
   | _ -> false
   
(* ----------------------------------------------------------------------- *)
(* Using complex multiplication with three multiplies *)
(* A more optimal way! *)
(* if x = a + ib and y = c + id *)
(* we compute t1 = a*(c+d) *)
(* t2 = d*(b+a) *)
(* t3 = c*(b-a) *)
(* so that *)
(* Re(x*y) = t1 - t2 *)
(* Im(x*y) = t1 + t3 *)
(* 3 multipllications and 5 additions *)
(* Note that if 'x' is a known factor, we can pre-compute
   a+b and a-b at compile time, so at run time we'll have only
   three multiplies and three additions *)

(* This is a semi-concretization function that forces
   the multiplication up to trivial factors, +/- 1, +/- I *)
let force_mult_ad (Code (f,rx,ix)) =
  let (newf,(j,n)) = rotate_to_QI f in
  let theta = float_of_int (2*j) *. pi /. float_of_int n in
  let c = cos theta in
  let s = sin theta in
  let spc = s +. c in
  let smc = s -. c in
  let () = assert (c > 0.0 && s > 0.0 ) in
  let! t1 = retN ((lift c) *^ (rx +^ ix)) in
  let! t2 = retN ((lift spc) *^ ix) in
  if smc > 0.0 then
      let! t3  = retN ((lift smc) *^ rx) in
      let! rcs = retN (t1 -^ t2) in
      let! ics = retN (t1 +^ t3) in
      ret (Code (newf,rcs,ics))
  else
      let cms = c -. s in (* keep all the constants positive *)
      let! nt3 = retN ((lift cms) *^ rx) in
      let! rcs = retN (t1 -^ t2) in
      let! ics = retN (t1 -^ nt3) in
      ret (Code (newf,rcs,ics))
 
(* Perform (rx,ix) +/- _complex_fy*(ry,iy) and set the resulting
   factor to fx
   Return a pair: fx* (x + complex_fy*y), fx*(x - complex_fy*y)
   This is where many simplifications happen
*)
let rec do_linear_ad fx rx ix ((j,n) as fy) ry iy =
   if 2*j >= n then (* fy in III and IV quadrants: reflect *)
        let! (p,m) = do_linear_ad fx rx ix (sub_rat fy (1,2)) ry iy in
        ret (m,p) 
   else
   match fy with 
   (0,1) ->  (* x + y *)
       let! re = retN (rx +^ ry) in
       let! im = retN (ix +^ iy) in
       let! re1 = retN (rx -^ ry) in
       let! im1 = retN (ix -^ iy) in
       ret (Code (fx,re,im),Code (fx,re1,im1))
  |(1,4) -> (* x + I*y *)
        let! re  = retN (rx -^ iy) in
        let! im  = retN (ix +^ ry) in
        let! re1 = retN (rx +^ iy) in
        let! im1 = retN (ix -^ ry) in
        ret (Code (fx,re,im),Code (fx,re1,im1) )
  |(j,8) -> (* x + (cos pi/4 + I*sin pi/4)*y *)
        let cs = sin (pi /. 4.0) in
        let! rcs = retN ((lift cs) *^ (ry -^ iy)) in
        let! ics = retN ((lift cs) *^ (ry +^ iy)) in
        do_linear_ad fx rx ix (rationalize (j-1,8)) rcs ics
  | _ -> 
        let! (Code (fy,ry,iy)) = force_mult_ad (Code (fy,ry,iy)) in
        do_linear_ad fx rx ix fy ry iy

(* This one is monadic: it may need to let-bind something
   Note: the add_sub_ad function is not symmetric!
   We need to compute fx*<code> +/- fy*<code>
   There are two ways to proceed:
    (1) let e1 = fx*<code> and e2 = fy*<code> in e1 +/- e2
    (2) fx*( <code> +/- (fy/fx)*<code> )
   The second way is better when either the multiplication by
   fx is trivial or by (fy/fx) is trivial
   Otherwise, the first way is better
   The second method is performed by do_linear
*)
let rec add_sub_ad ((Code (fx,rx,ix)) as x) ((Code (fy,ry,iy)) as y) =
   let fyx = (sub_rat fy fx) in
   if (trivial_mult fx) || (trivial_mult fyx) then
      do_linear_ad fx rx ix fyx ry iy
   else 
       let! x = force_mult_ad x in
       let! y = force_mult_ad y in
       add_sub_ad x y

(* Staged Monadic split *)
let split_ad dir l =
  let n = List.length l in
  let rec comb j = function
      (x::xs, y::ys) -> 
        let! (xpy,xmy) = add_sub_ad x y in
	let!     (a,b) = comb (j+1) (xs,ys) in
        ret (xpy::a, (mult_aa xmy (w_aa dir n j))::b)
    | _ -> ret ([],[]) in
  comb 0 (cleave_list l)

let fft_ad dir f l =
  if List.length l = 1 then ret l
  else 
    let! (p0,p1) = split_ad dir l in
    let! y0      = f p0 in
    let! y1      = f p1 in
    ret (merge_u (y0,y1))

let gen_ad dir size nums =
  run (y_sm (fft_ad dir))
    ((fun p -> let! (x,y) = (liftcM retN p) in ret (Code (rat_one, x, y))),
     (* here we assert c is a simple piece of code!*)
     (* And it is because the last stage of *)
     (* FFT is only a+b and a-b *)
     fun (Code ((0,1),x,y)) -> (x,y))
     size nums
