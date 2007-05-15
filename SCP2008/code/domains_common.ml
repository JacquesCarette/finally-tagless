(* Common code for various domains *)

open Domains_sig

module FloatDomain = struct
    type v = float
    let kind = Domain_is_Field
    let zero = 0.
    let one = 1.
    let plus x y = x +. y
    let times x y = x *. y
    let minus x y = x -. y
    let uminus x = -.x
    let div x y = x /. y
    let normalizer = None
    let better_than = Some (fun x y -> abs_float x < abs_float y)
end


module IntegerDomain = struct
    type v = int
    let kind = Domain_is_Ring
    let zero = 0
    let one = 1
    let plus x y = x + y
    let times x y = x * y
    let minus x y = x - y
    let uminus x = -x
    let div x y = x / y
    let normalizer = None
    let better_than = Some (fun x y -> abs x > abs y)
end


module RationalDomain = struct
    type v = Num.num
    let kind = Domain_is_Field
    let zero = Num.num_of_int 0
    let one = Num.num_of_int 1
    let plus x y = Num.add_num x y
    let times x y = Num.mult_num x y
    let minus x y = Num.sub_num x y
    let uminus x = Num.minus_num x
    let div x y = Num.div_num x y
    let normalizer = None
    let better_than = None
end

(* A naive primality tester. Should suffice for now, it is used
   only to make sure the generator ZpMake is instantiated correctly
*)
let is_prime n = 
  let rec loop j jsq =
    jsq > n || (n mod j <> 0 && loop (j+2) (jsq + 4*(j+1))) in
  n = 2 || ( n > 2 && (n mod 2 <> 0) && loop 3 9)
;;

module ZpMake = functor(P:sig val p:int end) -> struct
    type v = int
    let kind = Domain_is_Field
    let zero = 0
    let one = 1
    let plus x y = (x + y) mod P.p
    let times x y = (x * y) mod P.p
    let minus x y = (x - y) mod P.p
    let uminus x = -x mod P.p
    let rec extended_gcd a b =
        if a mod b == 0 then
            (0,1)
        else
            let (x,y) = extended_gcd b (a mod b) in
            (y, x-y*(a / b))
    let div x y = fst (extended_gcd x y)
    let normalizer = None
    let better_than = None
	(* Make sure this functor is instantiated correctly:
	   a run-time test, but at the generation time
	 *)
    let () = assert (is_prime P.p)
end
