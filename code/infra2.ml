(* Base monad type, to be used throughout *)
type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let ret a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k)
let k0 s v = v  (* Initial continuation -- for `reset' and `run' *)
let runM m = m [] k0 (* running our monad *)
let liftRef x = .< ref .~x >. 
let liftGet x = .< ! .~x >. 
let liftPair x = (.< fst .~x >., .< snd .~x >.)
let liftPPair x = (.< fst (fst .~x) >., .< snd (fst .~x) >., .< snd .~x >.)

(* Monad lifting functions *)
let l1 f = fun x -> mdo { t <-- x; f t}
let l2 f = fun x y -> mdo { tx <-- x; ty <-- y; f tx ty}
let l3 f = fun x y z -> mdo { tx <-- x; ty <-- y; tz <-- z; f tx ty tz}

(* 2 state morphisms *)
let fetch s k = k s s
let store v s k = k (v::s) ()

(* Functions that end in M take monads as arguments and partially
   run them -- in a reset fashion *)

(* sequencing *)
(* Note: the difference between `seq' and `seqM' is quite akin
   to the difference between call-by-value and call-by-name.
   Also note that `seq' can be expressed in terms of `seqM',
   but not the other way around 
*)

let seq a b = ret .< begin .~a ; .~b end >.

let seqM a b = fun s k -> k s .< begin .~(a s k0) ; .~(b s k0) end >.

(* conditional *)
let ifL test th el = ret .< if .~test then .~th else .~el >.

(* Note the implicit `reset' *)
let ifM test th el = fun s k ->
  k s .< if .~(test s k0) then .~(th s k0) else .~(el s k0) >.

let rshiftM cf = fun s k -> k s (cf s)

let whenM test th  = rshiftM (fun s -> 
  .< if .~(test s k0) then .~(th s k0) else () >.)

(* loops actually bind a value *)
let retLoopM low high body = fun s k -> 
    k s .< for j = .~low to .~high do .~(body .<j>. s k0) done >.

(* while ``loops'' do not naturally bind a value *)
let retWhileM cond body = fun s k -> 
    k s .< while .~(cond s k0) do .~(body s k0) done >.

(* match for Some/None *)
let retMatchM x som non = fun s k ->
    k s .< match .~x with
           | Some i -> .~(som .<i>. s k0)
           | None   -> .~(non s k0) >.

(* generic loop *)
let genrecloop gen rtarg = fun s k ->
    k s .<let rec loop j = .~(gen .<loop>. .<j>. s k0) in loop .~rtarg>.

(* nothing *)
(* Need to use `fun' explictly to avoid monomorphising *)
let retUnit = fun s k -> k s .< () >.

(* another non-trivial morphism: generate a bit of code *)
(* let codegen v cf = fun s k -> cf (k s v) *)

(* monadic logic code combinators *)
module LogicCode = struct
  let not a = ret .< not .~a >.
  let equal a b = ret .< .~a = .~ b >.
  let notequal a b = ret .< .~a <> .~ b >.
  let and_ a b = ret .< .~a && .~b >. 
end

(* operations on code indices *)
module Idx = struct
  let zero = .< 0 >.
  let one = .< 1 >.
  let minusone = .< -1 >.
  let succ a = .< .~a + 1 >.
  let pred a = .< .~a - 1 >.
  let less a b = .< .~a < .~b >.
  let uminus a = .< - .~a >.
end

(* Maybe code generator *)
module MaybeCode = struct
  let just x = .< Some .~x >.
  let none   = .< None >.
end

(* monadic tuples *)
module TupleCode = struct
  let tup2 a b = .< ( .~a, .~b ) >.
  let tup3 a b c = .< ( .~a, .~b, .~c ) >.
  let tup4 a b c d = .< ( .~a, .~b, .~c, .~d ) >.
end

(* List ops *)
module ListCode = struct
  let nil = .< [] >.
  let cons a b = .< .~a :: .~b >.
end

(* code generators *)
module Code = struct
  let cunit = .< () >.
  let update a f = let b = f (liftGet a) in ret .< .~a := .~b >.
  let assign a b = ret .< .~a := .~b >.
  let apply  f x = ret .< .~f .~x >.
end
