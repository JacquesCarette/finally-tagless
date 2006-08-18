(* Base monad type, to be used throughout *)
type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let ret a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k)
let k0 s v = v  (* Initial continuation -- for `reset' and `run' *)
let runM m = m [] k0 (* running our monad *)

(* Monad lifting functions *)
let l1 f = fun x -> perform t <-- x; f t
let l2 f = fun x y -> perform tx <-- x; ty <-- y; f tx ty
let l3 f = fun x y z -> perform tx <-- x; ty <-- y; tz <-- z; f tx ty tz

(* The monad has 2 parts: the continuation and the state.  For the
   state part, we only use 2 morphisms *)
let fetch s k = k s s
let store v s k = k (v::s) ()

(* Naming conventions: 
   Functions that end in M take monads as arguments and partially
   run them -- in a reset fashion 
   Functions that end in L always return abstract values from plain
   value -- the L is stands for ``lifted'' *)

(* sequencing *)
(* Note: the difference between `seq' and `seqM' is quite akin
   to the difference between call-by-value and call-by-name.
   Also note that `seq' can be expressed in terms of `seqM',
   but not the other way around 
*)

let seq a b = .< begin .~a ; .~b end >.
let seqL a b = ret (seq a b)

let seqM a b = fun s k -> k s .< begin .~(a s k0) ; .~(b s k0) end >.

(* conditional *)
(* Note the implicit `reset'.  Also note how the condition does not
   involve a `reset' *)
let ifM test th el = fun s k ->
  k s .< if .~(test) then .~(th s k0) else .~(el s k0) >.

let rshiftM cf = fun s k -> k s (cf s)

let whenM test th  = rshiftM (fun s -> 
  .< if .~(test) then .~(th s k0) else () >.)

(* loops actually bind a value *)
let loopM low high body = fun s k -> 
    k s .< for j = .~low to .~high do .~(body .<j>. s k0) done >.

(* while ``loops'' do not naturally bind a value *)
let whileM cond body = fun s k -> 
    k s .< while .~(cond) do .~(body s k0) done >.

(* match for Some/None *)
let matchM x som non = fun s k ->
    k s .< match .~x with
           | Some i -> .~(som .<i>. s k0)
           | None   -> .~(non s k0) >.

(* generic loop *)
let genrecloop gen rtarg = fun s k ->
    k s .<let rec loop j = .~(gen .<loop>. .<j>. s k0) in loop .~rtarg>.

(* another non-trivial morphism: generate a bit of code *)
(* let codegen v cf = fun s k -> cf (k s v) *)

(* We now define a lot of infrastructure.  We will be quite
   thorough, and define base abstraction and lifted abstractions,
   This will involve a lot of boilerplate code, which unfortunately 
   cannot be so easily automated in MetaOCaml -- that would require 
   introspection.  It could be done in camlp4, but that seems too 
   much as well.  This 'base' could be elided, but when we decide
   to make more use of Abstract Interpretation, we'll regret it,
   so do it now. *)

(* Define the actual module types and instances. *)
module type DOMAIN = sig
  type v
  type kind (* Field or Ring ? *)
  val zero : v
  val one : v
  val plus : v -> v -> v
  val times : v -> v -> v
  val minus : v -> v -> v
  val uminus : v -> v
  val div : v -> v -> v
  val better_than : (v -> v -> bool) option
  val normalizer : (v -> v) option
end 

(* Lift *)
module type DOMAINL = sig
  include DOMAIN
  type 'a vc = ('a,v) code
  val zeroL : 'a vc
  val oneL : 'a vc
  val ( +^ ) : 'a vc -> 'a vc -> 'a vc
  val ( *^ ) : 'a vc -> 'a vc -> 'a vc
  val ( -^ ) : 'a vc -> 'a vc -> 'a vc
  val uminusL : 'a vc -> 'a vc
  val divL : 'a vc -> 'a vc -> 'a vc
  val better_thanL : ('a vc -> 'a vc -> ('a,bool) code) option
  val normalizerL : ('a vc -> 'a vc) option
end 

(* The kind of the domain: a ring or a field *)
type domain_is_field (* abstract *)
type domain_is_ring  (* abstract *)

module FloatDomain = struct
    type v = float
    type kind = domain_is_field
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

(* because the operations are "syntactic" to a certain extent,
   we have to repeat ourselves a lot *)
module FloatDomainL = struct
    include FloatDomain
    type 'a vc = ('a,v) code
    let zeroL = .< 0. >.  
    let oneL = .< 1. >. 
    let (+^) x y = .<.~x +. .~y>. 
    let ( *^ ) x y = .<.~x *. .~y>.
    let ( -^ ) x y = .<.~x -. .~y>.
    let uminusL x = .<-. .~x>.
    let divL x y = .<.~x /. .~y>. 
    let normalizerL = None
    let better_thanL = Some (fun x y -> .<abs_float .~x < abs_float .~y >. )
end
module IntegerDomain = struct
    type v = int
    type kind = domain_is_ring
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

(* because the operations are "syntactic" to a certain extent,
   we have to repeat ourselves a lot *)
module IntegerDomainL = struct
    include IntegerDomain
    type 'a vc = ('a,v) code
    let zeroL = .< 0 >.  
    let oneL = .< 1 >. 
    let (+^) x y = .<.~x + .~y>. 
    let ( *^ ) x y = .<.~x * .~y>.
    let ( -^ ) x y = .<.~x - .~y>.
    let uminusL x = .<- .~x>.
    let divL x y = .<.~x / .~y>. 
    let normalizerL = None
    let better_thanL = Some (fun x y -> .<abs .~x > abs .~y >. )
end

module RationalDomain = struct
    type v = Num.num
    type kind = domain_is_field
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

(* because the operations are "syntactic" to a certain extent,
   we have to repeat ourselves a lot *)
module RationalDomainL = struct
    include RationalDomain
    type 'a vc = ('a,v) code
    let zeroL = .< zero >.  
    let oneL = .< one >. 
    let (+^) x y = .< Num.add_num .~x .~y >.
    let ( *^ ) x y = .< Num.mult_num .~x .~y >.
    let ( -^ ) x y = .< Num.sub_num .~x .~y >.
    let uminusL x = .<Num.minus_num .~x>.
    let divL x y = .< Num.div_num .~x .~y >.
    let normalizerL = None
    let better_thanL = None
end

module type CONTAINER2D = sig
  module Dom:DOMAINL
  type contr
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  val getL : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo
  val dim1 : 'a vc -> ('a,int) code
  val dim2 : 'a vc -> ('a,int) code
  val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
  val copy : 'a vc -> 'a vc
  val swap_rows_stmt : 'a vc -> ('a, int) code -> ('a, int) code -> 
                       ('a,unit) code
  val swap_cols_stmt : 'a vc -> ('a, int) code -> ('a, int) code -> 
                       ('a,unit) code
  val row_head : 'a vc -> ('a, int) code -> ('a, int) code -> 'a vo
  val row_iter : 'a vc -> ('a, int) code -> 
      ('a, int) code -> ('a, int) code ->
      (('a,int) code -> 'a vo -> 's -> ('s -> 'w -> 'w) -> ('a, 'e) code) ->
          (('a,unit) code, 's, 'k) monad
  val col_head_set : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo -> 
            ('a,unit) code
  val col_iter : 'a vc -> ('a, int) code -> 
      ('a, int) code -> ('a, int) code ->
      (('a,int) code -> 'a vo -> 's -> ('s -> 'w -> 'w) -> ('a, 'e) code) ->
          (('a,unit) code, 's, 'k) monad
end


module GenericArrayContainer(Dom:DOMAINL) =
  struct
  module Dom = Dom
  type contr = Dom.v array array (* Array of rows *)
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let getL x n m = .< (.~x).(.~n).(.~m) >.
  let dim2 x = .< Array.length .~x >.       (* number of rows *)
  let dim1 x = .< Array.length (.~x).(0) >. (* number of cols *)
  let mapper (g:('a vo -> 'a vo) option) a = match g with
      | Some f -> .< Array.map (fun x -> Array.map (fun z -> .~(f .<z>.)) x) .~a >.
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
  (* this is list an iterator's start *)
  let row_head = getL
  let row_iter b c low high body = 
    let newbody j = perform
        bjc <-- retN (getL b j c);
        body j bjc
    in  loopM low high newbody

  (* only set the head of the current column *)
  let col_head_set x n m y = .< (.~x).(.~n).(.~m) <- .~y >.
  let col_iter b j low high body = 
    let newbody k = perform
        bjk <-- ret (getL b j k);
        body k bjk
    in  loopM low high newbody
end

(* Matrix layed out row after row, in a C fashion *)
type 'a container2dfromvector = {arr:('a array); n:int; m:int}

module GenericVectorContainer(Dom:DOMAINL) =
  struct
  module Dom = Dom
  type contr = Dom.v container2dfromvector
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let getL x n m = .< ((.~x).arr).(.~n* (.~x).m + .~m) >.
  let dim2 x = .< (.~x).n >.
  let dim1 x = .< (.~x).m >.
  let mapper g a = match g with
      | Some f -> .< { (.~a) with arr = Array.map (fun z -> .~(f .<z>.)) (.~a).arr} >.
      | None   -> a
  let copy a = .< { (.~a) with arr = Array.copy (.~a).arr} >.
  let swap_rows_stmt b r1 r2 = .<
      let a = (.~b).arr and m = (.~b).m in
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
  let row_head b c r = getL b r c
  let row_iter b c low high body = 
    let newbody j = perform
        bjc <-- retN (getL b j c);
        body j bjc
    in  loopM low high newbody

  (* only set the head of the current column *)
  let col_head_set x n m y = .< ((.~x).arr).(.~n* (.~x).m + .~m) <- .~y >.
  let col_iter b j low high body = 
    let newbody k = perform
        bjk <-- ret (getL b j k);
        body k bjk
    in  loopM low high newbody
end

(* we use an association list as the representation of a sparse vector.
   It is assumed to be _sorted_ in increasing order of the index *)
type 'a svect = (int*'a) list


(* Our 'Sparse' container.  Since generically our matrices will not be
rank-deficient, almost all rows will contain something, so we will
represent matrices as a full array of sparse vectors *)
type 'a container2dsparse = {sarr:('a svect array); mm:int}

(*
module GenericSparseContainer(Dom:DOMAINL) =
  struct
  module Dom = Dom
  type contr = Dom.v svect array (* Array of rows *)
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let getL x n m = .< (.~x).(.~n).(.~m) >.
  let setL x n m y = .< (.~x).(.~n).(.~m) <- .~y >.
  let dim2 x = .< Array.length .~x >.       (* number of rows *)
  let dim1 x = .< Array.length (.~x).(0) >. (* number of cols *)
  let mapper (g:('a vo -> 'a vo) option) a = match g with
      | Some f -> .< Array.map (fun x -> Array.map (fun z -> .~(f .<z>.)) x) .~a >.
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
*)

(* Matrix layed out row after row, in a C fashion *)
module Array1D =
  struct
  let getL x n = .< x.(.~n) >.
  let setL x n y = .< x.(.~n) <- .~y >.
  let dim1 x = .< Array.length x >.
  let mapper g a = match g with
      | Some f -> .< (fun x -> Array.map .~f x) .~a >.
      | None   -> a
end

module CArray1D = 
  struct
  let getL x n = .< (.~x).(n) >.
  let setL x n y = .< (.~x).(n) <- .~y >.
  let dim1 x = .< Array.length .~x >.
end

module Array2D =
  struct
  let getL x n m = .< x.(.~n).(.~m) >.
  let setL x n m y = .< x.(.~n).(.~m) <- .~y >.
  let dim2 x = .< Array.length x >.       (* number of rows *)
  let dim1 x = .< Array.length (x).(0) >. (* number of cols *)
end

(* set up some very general algebra that can be reused though not
   so much in the current code (yet?) 
type ('a,'b) abstract = Ground of 'b | Code of ('a, 'b) code
let concretize = function
    | Ground x -> .<x>.
    | Code x   -> x
*)

let lift x = .< x >.

let liftRef x = .< ref .~x >. 
let liftGet x = .< ! .~x >. 

(* Need to use `fun' explictly to avoid monomorphising *)
let retUnitL = fun s k -> k s .< () >.

(* To be able to deconstruct pairs in monadic code:
   perform (a,b) <-- liftPair pv *)
let liftPair x = ret (.< fst .~x >., .< snd .~x >.)
let liftPPair x = ret (.< fst (fst .~x) >., .< snd (fst .~x) >., .< snd .~x >.)

(* logic code combinators - plain and monadic *)
module LogicCode = struct
  let notL a        = .< not .~a >.
  let equalL a b    = .< .~a = .~ b >.
  let notequalL a b = .< .~a <> .~ b >.
  let andL a b     = .< .~a && .~b >. 
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

  (* need explicit fun to avoid monomorphising *)
  let minusoneL = fun s k -> k s minusone
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
  let update a f = let b = f (liftGet a) in .< .~a := .~b >.
  let assign a b = .< .~a := .~b >.
  let apply  f x = .< .~f .~x >.
  let updateL a f = ret (update a f)
  let assignL a b = ret (assign a b)
  let applyL  f x = ret (apply f x)
end

(* code transformers *)
module CodeTrans = struct
    let rec full_unroll lb ub body = 
        if lb>ub then Code.cunit
        else if lb = ub then body lb
        else .< begin .~(body lb); .~(full_unroll (lb+1) ub body) end >.
end

let applyMaybe g x = match g with
    | Some f -> f x
    | None   -> x
