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

(* nothing *)
(* Need to use `fun' explictly to avoid monomorphising *)
let retUnit = fun s k -> k s .< () >.

(* another non-trivial morphism: generate a bit of code *)
(* let codegen v cf = fun s k -> cf (k s v) *)

(* Define the actual module types and instances *)
module type DOMAIN = sig
  type v
  type kind (* Field or Ring ? *)
  type 'a vc = ('a,v) code
  val zero : 'a vc
  val one : 'a vc
  val plus : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val times : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val minus : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val uminus : 'a vc -> ('a vc, 's, 'w) monad
  val div : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val better_than : ('a vc -> 'a vc -> (('a,bool) code, 's, 'w) monad) option
  val normalizerf : (('a,v -> v) code ) option
  val normalizerg : 'a vc -> 'a vc
end 

(* The kind of the domain: a ring or a field *)
type domain_is_field (* abstract *)
type domain_is_ring  (* abstract *)

module FloatDomain = 
  struct
    type v = float
    type kind = domain_is_field
    type 'a vc = ('a,v) code
    let zero = .< 0. >.  
    let one = .< 1. >. 
    let plus x y = ret .<.~x +. .~y>. 
    let times x y = ret .<.~x *. .~y>.
    let minus x y = ret .<.~x -. .~y>.
    let uminus x = ret .<-. .~x>.
    let div x y = ret .<.~x /. .~y>. 
    let better_than = Some (fun x y -> ret .<abs_float .~x < abs_float .~y >. )
    let normalizerf = None 
    let normalizerg = fun x -> x
end

module IntegerDomain = 
  struct
    type v = int
    type kind = domain_is_ring
    type 'a vc = ('a,v) code
    let zero = .< 0 >.  
    let one = .< 1 >. 
    let plus x y = ret .<.~x + .~y>. 
    let times x y = ret .<.~x * .~y>.
    let minus x y = ret .<.~x - .~y>.
    let uminus x = ret .< - .~x>.
    let div x y = ret .<.~x / .~y>. 
    let better_than = Some (fun x y -> ret .<abs .~x > abs .~y >. )
    let normalizerf = None 
    let normalizerg = fun x -> x
end

module RationalDomain = 
  struct
    type v = Num.num
    type kind = domain_is_field
    type 'a vc = ('a,v) code
    let zero = let zero = Num.num_of_int 0 in .< zero >.  
    let one = let one = Num.num_of_int 1 in .< one >. 
    let plus x y = ret .<Num.add_num .~x .~y >.
    let times x y = ret .<Num.mult_num .~x .~y>.
    let minus x y = ret .<Num.sub_num .~x .~y>.
    let uminus x = ret .<Num.minus_num .~x>.
    let div x y = ret .<Num.div_num .~x .~y>. 
    let better_than = None (* no such thing here *)
    let normalizerf = None 
    let normalizerg = fun x -> x
end

module type CONTAINER2D = functor(Dom:DOMAIN) -> sig
  type contr
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  val get : 'a vc -> ('a,int) code -> ('a,int) code -> ('a vo,'s,'w) monad
  val get' : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo
  val set : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo -> 
            (('a,unit) code, 's, 'w) monad
  val set' : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo -> 
            ('a,unit) code
  val dim1 : 'a vc -> ('a,int) code
  val dim2 : 'a vc -> ('a,int) code
  val mapper : ('a, Dom.v->Dom.v) code option -> 'a vc -> 'a vc
  val copy : 'a vc -> 'a vc
  val swap_rows_stmt : 'a vc -> ('a, int) code -> ('a, int) code -> 
                       ('a,unit) code
  val swap_cols_stmt : 'a vc -> ('a, int) code -> ('a, int) code -> 
                       ('a,unit) code
end

module GenericArrayContainer(Dom:DOMAIN) =
  struct
  type contr = Dom.v array array (* Array of rows *)
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let get' x n m = .< (.~x).(.~n).(.~m) >.
  let get x n m = ret (get' x n m)
  let set' x n m y = .< (.~x).(.~n).(.~m) <- .~y >.
  let set x n m y = ret (set' x n m y)
  let dim2 x = .< Array.length .~x >.       (* number of rows *)
  let dim1 x = .< Array.length (.~x).(0) >. (* number of cols *)
  let mapper g a = match g with
      | Some f -> .< Array.map (fun x -> Array.map .~f x) .~a >.
      | None   -> a
  let copy = (fun a -> .<Array.map (fun x -> Array.copy x) 
                       (Array.copy .~a) >. )
  (* this can be optimized with a swap_rows_from if it is known that
     everything before that is already = Dom.zero *)
  let swap_rows_stmt a r1 r2 =
      .< let t = (.~a).(.~r1) in
         begin 
             (.~a).(.~r1) <- (.~a).(.~r2);
             (.~a).(.~r2) <- t
         end >.
  let swap_cols_stmt a c1 c2 = .< 
      for r = 0 to .~(dim2 a)-1 do
          let t = (.~a).(r).(.~c1) in
          begin 
              (.~a).(r).(.~c1) <- (.~a).(r).(.~c2);
              (.~a).(r).(.~c2) <- t
          end
      done  >.
end

(* Matrix layed out row after row, in a C fashion *)
type 'a container2dfromvector = {arr:('a array); n:int; m:int}

module GenericVectorContainer(Dom:DOMAIN) =
  struct
  type contr = Dom.v container2dfromvector
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let get' x n m = .< ((.~x).arr).(.~n* (.~x).m + .~m) >.
  let get x n m = ret (get' x n m)
  let set' x n m y = .< ((.~x).arr).(.~n* (.~x).m + .~m) <- .~y >.
  let set x n m y = ret (set' x n m y)
  let dim2 x = .< (.~x).n >.
  let dim1 x = .< (.~x).m >.
  let mapper g a = match g with
      | Some f -> .< { (.~a) with arr = Array.map .~f (.~a).arr} >.
      | None   -> a
  let copy a = .< { (.~a) with arr = Array.copy (.~a).arr} >.
  let swap_rows_stmt b r1 r2 = .<
      let a = (.~b).arr and n = (.~b).n and m = (.~b).m in
      let i1 = .~r1*m and i2 = .~r2*m in
      for i = 0 to m-1 do
          let t = a.(i1 + i) in
          begin 
              a.(i1 + i) <- a.(i2 + i);
              a.(i2 + i) <- t
          end
      done  >.
  let swap_cols_stmt b c1 c2 = .<
      let a = (.~b).arr and nm = (.~b).n * (.~b).m and m = (.~b).m in
      let rec loop i1 i2 =
	if i2 < nm then
	  let t = a.(i1) in
	  begin
	    a.(i1) <- a.(i2);
	    a.(i2) <- t;
	    loop (i1 + m) (i2 + m)
	  end
      in loop .~c1 .~c2
     >.
end

(* set up some very general algebra that can be reused though not
   so much in the current code (yet?) 
type ('a,'b) abstract = Ground of 'b | Code of ('a, 'b) code
let concretize = function
    | Ground x -> .<x>.
    | Code x   -> x
*)

(* monadic logic code combinators *)
module LogicCode = struct
  let not a = ret .< not .~a >.
  let equal a b = ret .< .~a = .~ b >.
  let and_ a b = ret .< .~a && .~b >. 
end

(* operations on code indices *)
module Idx = struct
  let zero = .< 0 >.
  let succ a = .< .~a + 1 >.
  let pred a = .< .~a - 1 >.
  let less a b = .< .~a < .~b >.
end

(* code generators *)
module Code = struct
  let update a f = let b = f (liftGet a) in ret .< .~a := .~b >.
end
