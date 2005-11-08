(* Tests of the pa-lift syntactic extension.
   Be sure to do make test-lift1.txt to see the generated code
   and make test-lift1 to see the thing run...
   We're just interested in pretty-printing.
 *)
module type ENVT = sig
  type 'v m
  val retS : ('a,'v) code -> ('a,'v m) code
  val retL : ('a,'v) code -> ('a,'v m) code
  val bind : ('a,'v m) code -> (('a,'v) code -> ('a,'v1 m) code)
             -> ('a,'v1 m) code
  (* Actually, run should have 
		 ('a,'v m) code -> ('a,'v out) code
     for some 'out' type -- which is not abstract
     But creating functors parameterized by out, and instantiating this
     stuff seems more complex than it is worth.
   *)
  val run  : ('a,'v m) code -> ('a,'v) code
  val app  : ('a,('v1->'v2 m) m) code -> ('a,'v1 m) code -> ('a,'v2 m) code
  val ( + ): ('a, int m) code -> ('a, int m) code -> ('a, int m) code
  val ( - ): ('a, int m) code -> ('a, int m) code -> ('a, int m) code
  val ( * ): ('a, int m) code -> ('a, int m) code -> ('a, int m) code
  val ( +.): ('a, float m) code -> ('a, float m) code -> ('a, float m) code
  val ( -.): ('a, float m) code -> ('a, float m) code -> ('a, float m) code
  val ( *.): ('a, float m) code -> ('a, float m) code -> ('a, float m) code
  val ( = ): ('a, 'v m) code -> ('a, 'v m) code -> ('a, bool m) code
  val fif  : ('a, bool m) code -> ('a, 'v m) code -> ('a, 'v m) code ->
             ('a, 'v m) code
  val fseq : ('a, 'v1 m) code -> ('a, 'v m) code -> ('a, 'v m) code
  val ffor : bool -> ('a, int m) code -> ('a, int m) code -> 
   (('a, int) code -> ('a, 'any m) code) -> ('a, unit m) code
  val fass : ('a, ('v ref) m) code -> ('a,'v m) code -> ('a, unit m) code
  val ref  : ('a,'v m) code -> ('a, ('v ref) m) code
  val ( ! ): ('a, ('v ref) m) code -> ('a, 'v m) code
  val aref : ('a, ('v array) m) code -> ('a, int m) code -> ('a,'v m) code
  val ym   : ('a, ('v->'v1 m) -> ('v ->'v1 m)) code -> ('a, 'v ->'v1 m) code
end;;

module ENV : ENVT = struct
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

let gib = funM ENV n x y -> if true then 2 else 3
;;

let gib = funM ENV n x y -> if n then x else ""
;;

let gib = funM ENV n x y -> if false then x+1 else n-1
;;

let gib = funM ENV n x y -> if n=1 then x+1+2 else n-1
;;

let gib = funM ENV n x y -> let x = (n+1) in if true then y else n-1
;;

let fact = funM ENV n x -> 
  let rec loop n = if n = 0 then x else 
                     let n' = loop (n-1) in n * n' in loop n
;;
let test = (.!fact) 5 1
;;

let ta = funM ENV x ->
  let x = ref x in (x := !x +. 1.0; !x)
;;
let ta' = (.! ta) 3.0;;

let tar1 = funM ENV a n -> a.(n+1);;

let tfor = funM ENV e1 e2 -> 
  let x = ref 0 in for i = e1 to (e2+1) do x := !x+i done; !x
;;
let tfor' = (.!tfor) 1 99;;
