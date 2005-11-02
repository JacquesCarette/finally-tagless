(* Tests of the pa-lift syntactic extension.
   Be sure to do make test-lift1.txt to see the generated code
   and make test-lift1 to see the thing run...
   We're just interested in prettu-printing.
 *)
module ENV = struct
 let retS x = x
 let ret x  = x
 let bind m f = f m
 let (+) x y = .<Pervasives.(+) .~x .~y>.
 let (-) x y = .<Pervasives.(-) .~x .~y>.
 let ( * ) x y = .<Pervasives.( * ) .~x .~y>.
 let (=) x y = .<Pervasives.(=) .~x .~y>.
 let fif c t e = .<if .~c then .~t else .~e>.
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

