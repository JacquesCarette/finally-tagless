(* Dot-conv-product and its partial unrolling 
 * we use the convolution just for the heck of it...
*)

module ENVID = struct
 type 'v m   = 'v

 let retS x = x
 let retL x = x
 let ret x  = x
 let bind m f = f m
 let run x = x
 let app f x = .<.~f .~x>.
 let ( + ) x y = .<Pervasives.( + ) .~x .~y>.
 let ( - ) x y = .<Pervasives.( - ) .~x .~y>.
 let ( * ) x y = .<Pervasives.( * ) .~x .~y>.
 let ( = ) x y = .<Pervasives.( = ) .~x .~y>.
 let ( +.) x y = .<Pervasives.( +.) .~x .~y>.
 let ( -.) x y = .<Pervasives.( -.) .~x .~y>.
 let ( *.) x y = .<Pervasives.( *.) .~x .~y>.
 let fif c t e = .<if .~c then .~t else .~e>.
 let fseq e1 e2 = .<begin .~e1; .~e2 end>.
 let ffor f e1 e2 body = 
   if f then .<for i = .~e1 to .~e2 do .~(body .<i>.) done>.
        else .<for i = .~e1 downto .~e2 do .~(body .<i>.) done>.
 let ref e = .<ref .~e>.
 let fass e1 e2 = .<(.~e1) := (.~e2)>.
 let ( ! ) e = .<! (.~e) >.
 let aref e1 e2 = .<(.~e1).(.~e2)>.
 let ym fc = .<let rec ym f x = f (ym f) x in ym .~fc>.
end;;

let dot1 = funM ENVID n a1 a2 ->
  let sum = ref 0.0 in
  for i = 0 to (n-1) do
    sum := !sum +. a1.(i) *. a2.(n-i-1)
  done;
  !sum
;;

let arr1 = [|1.0; 2.0; 3.0; 4.0|];;
let arr2 = [|2.0; 2.0; 3.0; 4.0|];;
let dot1_test = (.! dot1) 4 arr1 arr2;;

let rec full_unroll lb ub body =
  if lb = ub then body lb
  else if lb < ub then .<begin .~(body lb); 
                               .~(full_unroll (lb+1) ub body) end>.
  else .< () >.
;;

(* This function is like that in Albert Cohen's MW04:
  but note a different arrangement of brackets and escapes! *)
let partial_unroll factor e1 e2 body = 
   .<let lb = .~e1 and ub = .~e2 in
     let ntimes = (ub-lb+1)/factor in
     begin
       for ii = 0 to ntimes-1 do
	 .~(full_unroll 0 (factor-1) 
	      (fun i -> body .< ii*factor+lb +  i >.))
       done;
       for i = ntimes*factor+lb to ub do .~(body .<i>.) done
     end>.
;;
module ENVPU(N : sig val unroll_factor : int end) = struct
 include ENVID
 let ffor true e1 e2 body = partial_unroll N.unroll_factor e1 e2 body
end
;;

module ENVPU2 = ENVPU(struct let unroll_factor = 2 end);;

let dotu = funM ENVPU2 n a1 a2 ->
  let sum = ref 0.0 in
  for i = 0 to (n-1) do
    sum := !sum +. a1.(i) *. a2.(n-i-1)
  done;
  !sum
;;
let dotu_test = (.! dotu) 4 arr1 arr2;;
