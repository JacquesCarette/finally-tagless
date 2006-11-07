(* Tagless staged partial evaluation *)
(* The language is simply typed lambda-calculus with booleans,
   and deBruin encoding of variables.
   The language is isomorphic to the one generated by the following
   datatype definition:
   type var = VZ | VS var
   type exp = V of var | A of exp * exp | L of exp |
              B of bool | I of int
*)

(* pure value representation *)
type ('e,'d,'s) vrep
    = ('e,'d) code * 's option;;
(* possibly effectful representation *)
type ('e,'d,'s) crep
    = { eval: 'w. (('e,'d,'s) vrep -> ('e,'w) code) -> ('e,'w) code };;

(* implementation *)
let id x = x;;

(* base *)
(* literals and pure functions *)
let boolean (x:bool) env = { eval = fun k -> k (.<x>., Some x) };;
let integer (x:int ) env = { eval = fun k -> k (.<x>., Some x) };;
let pure d s =
    { eval = fun k -> k ((match s with Some s -> .<s>. | None -> d), s) };;
let impure d s =
    { eval = fun k -> .<let r = .~d in .~(k (.<r>., None))>. };;
let arg (fd, fs) =
    let fd' od = .<fun xd -> .~(fd (od .<xd>.))>. in
    let fs' od os = { eval = fun k -> k (fd' od, Some (fun (xd,xs) ->
            fs (od xd)
               (match os, xs with
                | Some os, Some xs -> Some (os xs)
                | _ -> None))) } in
    (fd', fs');;
let unary   purity d s env = snd           (arg (id, purity))   d s;;
let binary  purity d s env = snd      (arg (arg (id, purity)))  d s;;
let ternary purity d s env = snd (arg (arg (arg (id, purity)))) d s;;

(* variables *)
let varZ ((vc: ('e,'d,'s) vrep), _)
    : ('e,'d,'s) crep
    = { eval = fun k -> k vc };;
let varS (vp: 'r -> ('e,'d,'s) crep) ((_: (_,_,_) vrep), (envr: 'r))
    : ('e,'d,'s) crep
    = vp envr;;

(* combinations *)
let app (e1: 'r -> ('e, 'd1->'d2, ('e,'d1,'s1) vrep -> ('e,'d2,'s2) crep) crep)
        (e2: 'r -> ('e, 'd1, 's1) crep)
        (env: 'r)
    : ('e,'d2,'s2) crep
    = { eval = fun k -> (e1 env).eval (fun f ->
                        (e2 env).eval (fun x -> match f with
                        | (_, Some f) -> (f x).eval k
                        | (f, None) -> .<let r = .~f .~(fst x) in
                                         .~(k (.<r>., None))>.)) };;
let lam (e: ('e,'d1,'s1) vrep * 'r -> ('e,'d2,'s2) crep) (env: 'r)
    : ('e, 'd1 -> 'd2, ('e,'d1,'s1) vrep -> ('e,'d2,'s2) crep) crep
    = { eval = fun k -> k ( .<fun x -> .~((e ((.<x>., None), env)).eval fst)>.,
                            Some (fun x -> e (x, env)) ) };;
let iif (e0: 'r -> ('e,bool,bool) crep)
        (e1: 'r -> ('e,'d,'s) crep)
        (e2: 'r -> ('e,'d,'s) crep)
        (env: 'r)
    : ('e,'d,'s) crep
    = { eval = fun k -> (e0 env).eval (function
        | (_, Some b) -> ((if b then e1 else e2) env).eval k
        | (b, None) -> .<if .~b then .~((e1 env).eval k)
                                else .~((e2 env).eval k)>.) };;

(* extensions *)
(* booleans *)
let bfalse env = boolean false env;;
let btrue  env = boolean true  env;;
let band b1 b2 env = iif b1 b2 bfalse env;;
let bor  b1 b2 env = iif b1 btrue b2  env;;
let bnot env = unary pure (fun x -> .<not .~x>.) (Some not) env;;

(* integers *)
let add env = binary pure (fun x y -> .<.~x + .~y>.) (Some ( + )) env;;
let mul env = binary pure (fun x y -> .<.~x * .~y>.) (Some ( * )) env;;
let sub env = binary pure (fun x y -> .<.~x - .~y>.) (Some ( - )) env;;
let div env = binary pure (fun x y -> .<.~x / .~y>.) (Some ( / )) env;;

(* pairs *)
let p env = binary pure (fun x y -> .<.~x,.~y>.) (Some (fun x y -> x,y)) env;;
let pfst env = unary pure (fun x -> .<fst .~x>.) (Some fst) env;;
let psnd env = unary pure (fun x -> .<snd .~x>.) (Some snd) env;;

(* references *)
let rref env = unary impure (fun x -> .<ref .~x>.) None env;;
let (!!) env = unary impure (fun x -> .<!(.~x)>.) None env;;
let (=:) env = binary impure (fun x y -> .<.~x := .~y>.) None env;;

(* sequencing *)
let seq env = binary pure (fun x y -> y) (Some (fun x y -> y)) env;;

(* even let! *)
let nlet v f = app (lam f) v;;

(* tests *)
(* --to evaluate, say something like: .! ((t1 ()).eval fst) *)
let t1 env = lam varZ env;;

let t2 env = app (lam varZ) btrue env;;

let t3 env = (lam (lam (varS varZ))) env;;

let t4 env = app (lam (lam (varS varZ))) (lam varZ) env;;

let t5 env = (app (app (lam (lam (app (app (varS varZ) btrue) varZ)))
                       (lam (lam varZ)))
	          bfalse) env;;

(* the following are expected errors *)
(* and are commented out for testing 
let t6 env = (app (app (lam (lam (app (app (varS varZ) btrue) varZ)))
                       (lam varZ))
	          bfalse) env;;

let t6' env = (lam (app varZ varZ)) env;; *)

(* a few new tests *)
let t7 env = app (app add (integer 5)) (integer 6) env;;
let t8 env = nlet (app (app add (integer 5)) (integer 6))
                  (app (app mul varZ) (integer 3)) env;;

(* test that side effects are not repeated *)
let t9 env = lam (lam (nlet (app (varS varZ) varZ)
                            (app (app add varZ) varZ))) env;;
(* test that side effects are repeated *)
let t10 env = lam (lam (app (app add (app (varS varZ) varZ))
                            (app (varS varZ) varZ))) env;;

let t9' = (t9 ()).eval fst;;
