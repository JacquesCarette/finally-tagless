(* The Gibonacci example in native syntax *)

(* The Identity environment *)

module ENVID = struct
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
 let fif c t e = .<if .~c then .~t else .~e>.
 let ym fc     = .<let rec ym f x = f (ym f) x in ym .~fc>.
end;;


(* Fully dynamic, no memoization *)

let gib1 = funM ENVID n x y ->
      let rec loop n =
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in loop n
;;

let test_gib1 = (.!gib1) 8 1 1;;

(* Pathetic memoization table *)
type ('a,'k,'v) memo = 
    {lempty  : ('a, unit -> ('k * 'v) list) code;
     llookup : ('a, 'k -> ('k * 'v) list -> 'v option) code;
     lext    : ('a, ('k * 'v) list -> ('k * 'v) -> ('k * 'v) list) code}
;;
let memo'table =
  {lempty = .<fun () -> []>.;
   llookup = .<fun k ->
                 let rec loop = function 
		     [] -> None
		   | ((k',v')::t) -> if k = k' then Some v' else loop t
		 in loop>.;
   lext = .<fun s n -> n :: s>.
 }
;;

(* State-passing: State monad *)

module ENVST = struct
 let retS x = .<fun s -> (.~x, s)>.
 let retL x = .<fun s -> (.~x, s)>.
 let ret  x = .<fun s -> (.~x, s)>.
 let bind m f = .<fun s -> let (v,s) = .~m s in .~(f .<v>.) s>.
 let run x = .<fst (.~x [])>.
 let app f x = .<fun s -> let (vx,s) = .~x s in 
                          let (vf,s) = .~f s in vf vx s>.

 let lift2 op x y = .<fun s -> (.~(op .<fst (.~x s)>. .<fst (.~y s)>.),s)>.
 let ( + ) x y = lift2 ENVID.( + ) x y
 let ( - ) x y = lift2 ENVID.( - ) x y
 let ( * ) x y = lift2 ENVID.( * ) x y
 let ( = ) x y = lift2 ENVID.( = ) x y
 let fif c t e = .<fun s -> 
   let (vc,s) = .~c s in if vc then .~t s else .~e s>.
 let ym fc     = .<let rec ym f x = f (ym f) x in ym .~fc>.
end;;

let fs3 = funM ENVST n x -> 
  let rec loop m = if m = 0 then x else loop (m-1) in loop n
;;
let fs3' = (.!fs3) 4 3;;

(* Note that we specifically avoid cross-stage persistent values.
   So, the code is self-contained, albeit with a lot of silly inlining.
   BTW, a lot of trivial betas can be eliminated if retS tagged
   its result as a `simple expression', and bind, lift2, etc. operations
   check the tag.
*)
let gib2 = funM ENVST n x y ->
      let rec loop n =
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in loop n
;;
let test_gib2 = (.!gib2) 8 1 1;;

module ENVSTMEMO = struct
 include ENVST
 let run x = .<fst (.~x (.~(memo'table.lempty) ()))>.
 let ym fc =
   .<let lookup = .~(memo'table.llookup) and
         ext =    .~(memo'table.lext) in
     let rec ym f =
     f (fun x s ->
       match (lookup x s) with
       | Some r -> (r,s)
       | None   -> let (z,s) = ym f x s in
	           (z,(ext s (x,z))))
     in
   ym .~fc>.
end;;

let gib3 = funM ENVSTMEMO n x y ->
      let rec loop n =
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in loop n
;;
let test_gib3 = (.!gib3) 8 1 1;;
let test_gib3' = (.!gib3) 30 1 1;;

(*
  Note that the latter is significantly faster than the former
# gibm_idn 30 1 1;;
- : '_a IntN.d = 1346269
*)


(* CPS Monad with state *)
(* This time, we let cross-stage persistent values to appear, e.g., k0 *)

module ENVSTCPS = struct
 let retS x = .<fun s k -> k s .~x>.
 let retL x = .<fun s k -> k s .~x>.
 let ret  x = .<fun s k -> k s .~x>.
(* let retS x = .<fun s k -> let z = .~x in .<.~k .~s z>.>. *)
(* let ret  x = .<fun s k -> (fun y -> .<.~k .~s .~y>.) .<.~.~x>.>. *)
 let bind m f = .<fun s k -> .~m s (fun s x -> .~(f .<x>.) s k)>.
 let k0 s v = v
 let run x = .<.~x [] k0>.
 let app f x = .<fun s k -> 
   .~f s (fun s vf -> .~x s (fun s vx -> (vf vx) s k))>.
 let lift2 op x y = .<fun s k -> .~x s (fun s vx -> 
                                          .~y s (fun s vy -> 
					    k s .~(op .<vx>. .<vy>.)))>.
 let ( + ) x y = lift2 ENVID.( + ) x y
 let ( - ) x y = lift2 ENVID.( - ) x y
 let ( * ) x y = lift2 ENVID.( * ) x y
 let ( = ) x y = lift2 ENVID.( = ) x y
 let fif c t e = .<fun s k -> if .~c s k0 then .~t s k else .~e s k>.

 let ym fc =
   .<let lookup = .~(memo'table.llookup) and
         ext =    .~(memo'table.lext) in
     let rec ym f =
     f (fun x s k -> 
       match (lookup x s) with
       | Some r -> k s r
       | None   -> ym f x s (fun s' v -> 
	   let z = v in (k (ext s' (x,z)) z))) in
   ym .~fc>.

end;;

(* some tests to get the levels right *)
let t1 = 
  .< .<fun s k -> if true then 
   .~((fun s k -> .<.~k .~s 1>.) .<s>. .<k>.) else k s 1>. >.;;
let t1' = .!t1
;;

let t2 = let r = .<fun k -> .<.~k 1>.>. in .<fun k1 -> .<.~(.~r .<k1>.)>.>.;;
let t2' = .! t2;;


(* desired output: fun n x -> fun s k -> if n = 0 then k s x else k s 1 *)
let f1 = funM ENVSTCPS n x ->
  if n = 0  then x else 1
;;
let f1' = (.!f1) 0 3;;
let f1' = (.!f1) 4 3;;


let f2 = funM ENVSTCPS n x ->
  let y = x + 1 in y
;;
let f2' = (.!f2) 1 3;;

(* These tests can be illiminating if one likes CPS code,
   which is automatically generated in this case
 *)

let f3 = funM ENVSTCPS n x ->
  let rec loop m = if m = 0 then x else loop (m-1) in loop n
;;
let f3' = (.!f3) 4 3;;

(*------------------------------------------------------------------------*)
(* The same but full inlining: now we do code generation rather than
   compute Gib
 *)

type ('a,'v) phase = Lit of 'v | Simple of ('a,'v) code | Any of ('a,'v) code
;;

let lift2c op opl x y = 
  match (x,y) with
  | (Lit x, Lit y) -> Lit (op x y)
  | (Lit x, (Simple y | Any y)) -> Any .<.~opl x .~y>.
  | ((Simple x | Any x), Lit y) -> Any .<.~opl .~x y>.
  | ((Simple x | Any x), (Simple y | Any y)) -> Any .<.~opl .~x .~y>.
;;
let concr = function
  | Lit x -> .<x>.
  | Simple x -> x
  | Any x -> x
;;
module ENVC = struct
 let retS x = .<fun s k -> k s .~x>.
 let retL x = .<fun s k -> k s (Lit .~x)>.
 let bind m f = .<fun s k -> .~m s (fun s x -> .~(f .<x>.) s k)>.
 let k0 s v = v
 let run x = .<.~x [] k0>.
 let app f x = .<fun s k -> 
   .~f s (fun s vf -> .~x s (fun s vx -> (vf vx) s k))>.

 let lift2 op opl x y =
   .<fun s k -> .~x s (fun s vx -> .~y s (fun s vy -> 
     k s (lift2c op opl vx vy)))>.
     
 let ( + ) x y = lift2 Pervasives.( + ) .<Pervasives.( + )>. x y
 let ( - ) x y = lift2 Pervasives.( - ) .<Pervasives.( - )>. x y
 let ( * ) x y = lift2 Pervasives.( * ) .<Pervasives.( * )>. x y
 let ( = ) x y = lift2 Pervasives.( = ) .<Pervasives.( = )>. x y
 let fif c t e = .<fun s k -> 
   match (.~c s k0) with
   | Lit c -> if c then .~t s k else .~e s k
   | (Simple c | Any c) -> k s (Any .<if .~c then .~(concr (.~t s k))
	                                     else .~(concr (.~e s k))>.)>.
 let ym fc =
   .<let lookup = .~(memo'table.llookup) and
         ext =    .~(memo'table.lext) in
     let rec ym f =
     f (fun x s k -> 
       match (lookup x s) with
       | Some r -> k s r
       | None   -> ym f x s (fun s' v -> 
	   let z = v in (k (ext s' (x,z)) z))) in
   ym .~fc>.

end;;

let c1 = funM ENVC n x ->
  if n = 0  then x else 1
;;
let c1' = (.!c1) (Lit 0) (Any .<3>.)
;;
let c2 = funM ENVC n x ->
  let rec loop m = if m = 0 then x else loop (m-1) in loop n
;;
let c2' = (.!c2) (Lit 4) (Lit 3);;
(* let c2' = (.!c2) (Any .<4>.) (Lit 3);; *)


let gib4 = funM ENVC n x y ->
      let rec loop n =
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in loop n
;;
(* Alas, memoization happens on the wrong stage *)
let test_gib4 = (.!gib4) (Lit 8) (Any .<1>.) (Lit 1);;

(* But we can fix it: *)

module ENVC1 = struct
 include ENVC
 let ym fc =
   .<let lookup = .~(memo'table.llookup) and
         ext =    .~(memo'table.lext) in
     let rec ym f =
     f (fun x s k -> 
       match (lookup x s) with
       | Some r -> k s r
       | None   -> ym f x s (fun s' v -> 
	   match v with
	   | Lit _ -> k (ext s' (x,v)) v (* Don't bother for the bindings*)
	   | _ -> Any .<let z = .~(concr v) in 
	     .~(concr (k (ext s' (x,Any .<z>.)) (Any .<z>.)))>.)
       )
   in
   ym .~fc>.
end;;


let gib5 = funM ENVC1 n x y ->
      let rec loop n =
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in loop n
;;
let test_gib5 = (.!gib5) (Lit 8);;
let test_gib5' = .<fun x y -> .~(concr (test_gib5 (Any .<x>.) (Any .<y>.)))>.
;;
let test_gib5'' = (.!gib5) (Lit 8) (Lit 1) (Lit 1);;

let test_gib5''' = .<fun x y -> .~(concr (test_gib5 (Lit 5) (Any .<y>.)))>.
;;
