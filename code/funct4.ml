(* Base monad type, to be used throughout *)
type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k)
let ret = retS
let k0 s v = v  (* Initial continuation -- for `reset' and `run' *)
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
   to the difference between call-by-value and call-by-value.
   Also note that `seq' can be expressed in terms of `seqM',
   but not the other way around 
*)

let seq a b = ret .< begin .~a ; .~b end >.

let seqM a b = fun s k -> k s .< begin .~(a s k0) ; .~(b s k0) end >.

(* conditional *)
let lif test th el = ret .< if .~test then .~th else .~el >.

(* Note the implicit `reset' *)
let ifM test th el = fun s k ->
  k s .< if .~(test s k0) then .~(th s k0) else .~(el s k0) >.

let whenM test th  = fun s k ->
  k s .< if .~(test s k0) then .~(th s k0) else () >.

(* loops actually bind a value *)
let retLoopM low high body = fun s k -> 
    k s .< for j = .~low to .~high do .~(body .<j>. s k0) done >.

(* while ``loops'' do not naturally bind a value *)
let retWhileM cond body = fun s k -> 
    k s .< while .~cond do .~(body s k0) done >.

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
  val better_than : 'a vc -> 'a vc -> (('a,bool) code, 's, 'w) monad 
  val normalizerf : (('a,v -> v) code ) option
  val normalizerg : 'a vc -> 'a vc
end 

(* use this type if a DOMAIN is a field, anything else otherwise *)
type domainkind = Field 

module FloatDomain = 
  struct
    type v = float
    type kind = domainkind
    type 'a vc = ('a,v) code
    let zero = .< 0. >.  
    let one = .< 1. >. 
    let plus x y = ret .<.~x +. .~y>. 
    let times x y = ret .<.~x *. .~y>.
    let minus x y = ret .<.~x -. .~y>.
    let uminus x = ret .<-. .~x>.
    let div x y = ret .<.~x /. .~y>. 
    let better_than x y = ret .<abs_float .~x < abs_float .~y >.
    let normalizerf = None 
    let normalizerg = fun x -> x
end

module IntegerDomain = 
  struct
    type v = int
    type kind = unit (* could use Ring instead *)
    type 'a vc = ('a,v) code
    let zero = .< 0 >.  
    let one = .< 1 >. 
    let plus x y = ret .<.~x + .~y>. 
    let times x y = ret .<.~x * .~y>.
    let minus x y = ret .<.~x - .~y>.
    let uminus x = ret .< - .~x>.
    let div x y = ret .<.~x / .~y>. 
    let better_than x y = ret .<abs .~x > abs .~y >.
    let normalizerf = None 
    let normalizerg = fun x -> x
end

module RationalDomain = 
  struct
    type v = Num.num
    type kind = domainkind
    type 'a vc = ('a,v) code
    let zero = let zer = Num.num_of_int 0 in .< zer >.  
    let one = let one = Num.num_of_int 1 in .< one >. 
    let plus x y = ret .<.~x Num.add_num .~y >.
    let times x y = ret .<Num.mult_num .~x .~y>.
    let minus x y = ret .<Num.sub_num .~x .~y>.
    let uminus x = ret .<Num.minus_num .~x>.
    let div x y = ret .<Num.div_num.~x .~y>. 
    let better_than x y = ret .< false >. (* no such thing here *)
    let normalizerf = None 
    let normalizerg = fun x -> x
end

module type CONTAINER2D = functor(Dom:DOMAIN) -> sig
  type contr
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  val get : 'a vc -> ('a,int) code -> ('a,int) code -> ('a vo,'s,'w) monad
  val set : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo -> 
            (('a,unit) code, 's, 'w) monad
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
  type contr = Dom.v array array
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let get x n m = ret .< (.~x).(.~n).(.~m) >.
  let set x n m y = ret .< (.~x).(.~n).(.~m) <- .~y >.
  let dim2 x = .< Array.length .~x >.
  let dim1 x = .< Array.length (.~x).(0) >.
  let mapper g a = match g with
      | Some f -> .< Array.map (fun x -> Array.map .~f x) .~a >.
      | None   -> a
  let copy = (fun a -> .<Array.map (fun x -> Array.copy x) 
                       (Array.copy .~a) >. )
  (* this can be optimized with a swap_rows_from if it is known that
     everything before that is already = *)
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

module GenericVectorContainer(Dom:DOMAIN) =
  struct
  type contr = {arr:(Dom.v array); n:int; m:int}
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let get x n m = ret .< ((.~x).arr).(.~n* (.~x).n + .~m) >.
  let set x n m y = ret .< ((.~x).arr).(.~n* (.~x).n + .~m) <- .~y >.
  let dim2 x = .< (.~x).n >.
  let dim1 x = .< (.~x).m >.
  let mapper g a = match g with
      | Some f -> .< { (.~a) with arr = Array.map .~f (.~a).arr} >.
      | None   -> a
  let copy a = .< { (.~a) with arr = Array.copy (.~a).arr} >.
  let swap_rows_stmt b r1 r2 = .<
      let a = (.~b).arr and n = (.~b).n and m = (.~b).m in
      let i1 = .~r1*n and i2 = .~r2*n in
      for i = 0 to m-1 do
          let t = a.(i1 + i) in
          begin 
              a.(i2 + i) <- a.(i1 + i);
              a.(i1 + i) <- t
          end
      done  >.
  let swap_cols_stmt b c1 c2 = .<
      let a = (.~b).arr and n = (.~b).n and m = (.~b).m in
      let i1 = .~c1 and i2 = .~c2 in
      let j = ref 0 in
      for i = 0 to n-1 do
          let t = a.(i1 + !j) in
          begin 
              a.(i2 + !j) <- a.(i1 + !j);
              a.(i1 + !j) <- t;
              j := !j + m
          end
      done  >.
end

(*
Just started this
module SparseRowContainer(Dom:DOMAIN) =
  struct
  type obj = Dom.v
  type contr = ((int*obj) list) array * int
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,obj) code
  let gdefault x l = .< try 
                            List.assoc .~x .~l 
                        with Not_found -> .~Dom.zero >.
  let sdefault loc x l = .< if not (l = .~Dom.zero) then
                                loc <- (x,l) :: loc >.
  let get x n m = ret .< let row = (.~x).(.~n) in .~(gdefault .<row>. m) >.
  let set x n m y = 
  let dim2 x = 
  let dim1 x = 
  let mapper g a = 
  let swap_cols_stmt a c1 c2 = .<failwith "swap_cols_stmt not yet implemeted">.
end
*)

(* set up some very general algebra that can be reused though not
   so much in the current code (yet?) *)
type ('a,'b) abstract = Ground of 'b | Code of ('a, 'b) code
let concretize = function
    | Ground x -> .<x>.
    | Code x   -> x

module type MONOID = sig
    type values
    val uunit : values
    val binop : values -> values -> values
    val binopC : ('a,values) code -> ('a,values) code -> ('a,values) code
end

module type LIFTEDMONOID = sig
    type t
    type ('a,'b) liftedvalues
    val lbinop : ('a,t) liftedvalues -> ('a,t) liftedvalues -> ('a,t) liftedvalues
    val toconcrete : ('a,t) liftedvalues -> ('a,t) code
end

(* The lifted version is not commutative ! *)
module LiftCommutativeMonoid(M:MONOID) = struct
    type t = M.values 
    type ('a,'b) liftedvalues = ('a, t) abstract
    let generalize x = Ground x
    let mixedop x y =
        if x=M.uunit then y else Code (M.binopC .<x>. (concretize y))
    let toconcrete (x: ('a,M.values) liftedvalues) = concretize x
    let lbinop x y =
        match (x,y) with
        | (Ground a, Ground b) -> Ground (M.binop a b)
        | (Ground a, b)        -> mixedop a b
        | (a, Ground b)        -> mixedop b a
        | (a, b) -> Code (M.binopC (concretize a) (concretize b))
end

module OrMonoid = struct
    type values = bool
    let uunit = false
    let binop x y = ( x ||  y)
    let binopC x y = .< .~x || .~y >.
end
module OrLMonoid = LiftCommutativeMonoid(OrMonoid)

module AndMonoid = struct
    type values = bool
    let uunit = true
    let binop x y = ( x &&  y)
    let binopC x y = .< .~x && .~y >.
end
module AndLMonoid = LiftCommutativeMonoid(AndMonoid)

module LogicGen(Or:LIFTEDMONOID)(And:LIFTEDMONOID) = 
  struct
    let mcode_or a b = mdo { 
        x <-- a;
        y <-- b;
        ret (Or.toconcrete (Or.lbinop x y)) }
    let mcode_and a b = mdo { 
        x <-- a;
        y <-- b;
        ret (And.toconcrete (And.lbinop x y)) }
end
module Logic = LogicGen(OrLMonoid)(AndLMonoid)

module type DETERMINANT = sig
  type indet
  type outdet
  type tdet = outdet ref
  type 'a lstate
  val decl : unit -> 
    (unit, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val upd_sign : unit -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val zero_sign : unit -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val acc : ('a,indet) code -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val get : unit ->
    (('a,tdet) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val set : ('a,indet) code -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val fin : unit -> 
    (('a,outdet) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
end

(* no need to make the type abstract here - just leads to other problems *)
module type RANK = sig
  type 'a lstate = ('a, int ref) code
  val rfetch : unit -> 
    (('a,int ref) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
  val decl : unit -> 
    (('a,int ref) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
  val succ : unit -> 
    (('a,unit) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
  val fin : unit ->  
    (('a,int) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
end;;

module type OUTPUT = sig
  type contr
  type res
  module D : DETERMINANT
  module R : RANK
  val make_result : ('a,contr) code -> 
    (('a,res) code,[> `TDet of 'a D.lstate | `TRan of 'a R.lstate] list,
     ('a,'w) code) monad
end

(* Even if no rank is output, it needs to be tracked, as the rank
   is also the outer loop index! *)

module TrackRank = 
  struct
  type 'a lstate = ('a, int ref) code
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  let coerce = function `TRan x -> `TRan x | x -> x
  let rec fetch_iter (s : [> `TRan of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TRan x -> x
    |  _ -> fetch_iter (List.tl s)
  let rfetch () = mdo { s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s) }
  let rstore v = store (`TRan v)
  let decl () = mdo {
      rdecl <-- retN (liftRef .<0>.);
      rstore rdecl;
      ret rdecl }
  let succ () = mdo {
   r <-- rfetch ();
   ret .<.~r := (! .~r) + 1>. }
end

module Rank:RANK = struct
  include TrackRank
  let fin () = mdo {
      r <-- rfetch ();
       ret (liftGet r) }
end

module NoRank:RANK = struct
  include TrackRank
  let fin () = ret .< -1 >.
end

(* In the case of a non-fraction-free algorithm with no Det
   output, it is possible to not track the determinant, but
   in all other cases it is needed.

   This module is just for deep type sharing.  The actual 
   implementations are completely different.  *)
module DetTypes(Dom: DOMAIN) = 
  struct
  type indet = Dom.v
  type outdet = indet
  type tdet = outdet ref
  (* the first part of the state is an integer: which is +1, 0, -1:
     the sign of the determinant *)
  type 'a lstate = ('a,int ref) code * ('a,tdet) code
end

(* we need the domain anyways to get things to type properly *)
module NoDet(Dom:DOMAIN) =
  struct
  module TD = DetTypes(Dom)
  type indet = Dom.v
  type outdet = unit
  type tdet = outdet ref
  type 'a lstate = 'a TD.lstate
  let decl () = ret ()
  let upd_sign () = retUnit
  let zero_sign () = retUnit
  let acc v = retUnit
  let get () = ret (liftRef .< () >. )
  let set v = retUnit
  let fin () = retUnit
end

module AbstractDet(Dom: DOMAIN) =
  struct
  module TD = DetTypes(Dom)
  type indet = TD.indet
  type outdet = TD.outdet
  type tdet = TD.tdet
  type 'a lstate = 'a TD.lstate
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  let coerce = function `TDet x -> `TDet x | x -> x
  let rec fetch_iter (s : [> `TDet of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TDet x -> x
    |  _ -> fetch_iter (List.tl s)
  let dfetch () = mdo { s <-- fetch; (* unit for monomorphism restriction *)
                     ret (fetch_iter s) }
  let dstore v = store (`TDet v)
  let decl () = mdo {
      ddecl <-- retN (liftRef Dom.one);
      dsdecl <-- retN (liftRef .<1>.);
      dstore (dsdecl,ddecl) }
  let upd_sign () = mdo {
      det <-- dfetch ();
      det1 <-- ret (fst det);
      ret .< .~det1 := - (! .~det1)>. }
  let zero_sign () = mdo {
      det <-- dfetch ();
      det1 <-- ret (fst det);
      ret .<.~det1 := 0>. }
  let acc v = mdo {
      det <-- dfetch ();
      det2 <-- ret (snd det);
      r <-- Dom.times (liftGet det2) v;
      ret .<.~det2 := .~r>. }
  let get () = mdo {
      det <-- dfetch ();
      det2 <-- ret (snd det);
      ret .<.~det2>. }
  let set v = mdo {
      det <-- dfetch ();
      det2 <-- ret (snd det);
      ret .<.~det2 := .~v>.}
  let fin () = mdo {
      (det_sign,det) <-- dfetch ();
      ifM (ret .<(! .~det_sign) = 0>.) (ret Dom.zero)
      (ifM (ret .<(! .~det_sign) = 1>.) (ret (liftGet det))
          (Dom.uminus (liftGet det))) }
end

module type UPDATE = sig
    type baseobj
    type ctr
    type 'a idx = ('a,int) code
    module D : DETERMINANT
    val update : ('a, ctr) code -> 'a idx -> 'a idx -> 'a idx -> 'a idx ->
        (('a,unit) code, [> `TDet of 'a D.lstate] list, ('a,'b) code) monad
    val update_det : ('a, baseobj) code -> 
        (('a,unit) code, [> `TDet of 'a D.lstate] list, ('a,'b) code) monad
end

(* What is the update formula? *)
module DivisionUpdate(Dom:DOMAIN with type kind = domainkind)(C:CONTAINER2D)
    (Det:DETERMINANT with type indet=Dom.v) = 
  struct
  module Ctr = C(Dom)
  type baseobj = Det.indet
  type ctr = Ctr.contr
  type 'a idx = ('a,int) code
  module D = Det
  let update b r c i k = mdo {
      t <-- l2 Dom.div (Ctr.get b i c) (Ctr.get b r c);
      l <-- l1 (Dom.times t) (Ctr.get b r k);
      y <-- l2 Dom.minus (Ctr.get b i k) (ret l);
      Ctr.set b i k (Dom.normalizerg y) }
  let update_det v = Det.acc v
end

module FractionFreeUpdate(Dom:DOMAIN)(C:CONTAINER2D)
    (Det:DETERMINANT with type indet=Dom.v and type outdet=Dom.v) =
  struct
  module Ctr = C(Dom)
  type baseobj = Dom.v
  type ctr = Ctr.contr
  type 'a idx = ('a,int) code
  module D = Det
  let update b r c i k = mdo {
      x <-- l2 Dom.times (Ctr.get b i k) (Ctr.get b r c);
      y <-- l2 Dom.times (Ctr.get b r k) (Ctr.get b i r);
      z <-- Dom.minus x y;
      t <-- ret (Dom.normalizerg z);
      d <-- Det.get ();
      ov <-- Dom.div t (liftGet d);
      Ctr.set b i k ov }
  let update_det v = Det.set v
end

(* What to return *)
module OutJustMatrix(Dom:DOMAIN)(C: CONTAINER2D)(Det : DETERMINANT) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr
  module D = Det
  module R = NoRank
  let make_result b = ret b
end

module OutDet(Dom:DOMAIN)(C: CONTAINER2D)(Det : DETERMINANT with type indet = Dom.v and type outdet = Dom.v) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * Det.outdet
  module D = Det
  module R = NoRank
  let make_result b = mdo {
    det <-- D.fin ();
    ret .< ( .~b, .~det ) >. }
end

module OutRank(Dom:DOMAIN)(C: CONTAINER2D)(Rank : RANK) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * int
  module D = NoDet(Dom)
  module R = Rank
  let make_result b = mdo {
    rank <-- R.fin ();
    ret .< ( .~b, .~rank ) >. }
end

module OutDetRank(Dom:DOMAIN)(C: CONTAINER2D)(Det : DETERMINANT with type indet = Dom.v and type outdet = Dom.v)(Rank : RANK) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * Det.outdet * int
  module D = Det
  module R = Rank
  let make_result b = mdo {
    det  <-- D.fin ();
    rank <-- R.fin ();
    ret .< ( .~b, .~det, .~rank ) >. }
end

module FDet = AbstractDet(FloatDomain)
module IDet = AbstractDet(IntegerDomain)

(* This type is needed for the output, and is tracked during
   pivoting. *)
type perm = RowSwap of (int * int) | ColSwap of (int * int)
 
module type PIVOT = 
    functor (Dom: DOMAIN) -> 
      functor (C: CONTAINER2D) ->
        functor (D: DETERMINANT with type indet = Dom.v) -> 
sig
 (* Find the pivot within [r,m-1] rows and [c,(n-1)] columns
    of containrer b.
    If pivot is found, permute the matrix rows and columns so that the pivot
    becomes the element (r,c) of the matrix,
    Return the value of the pivot option. Or zero?
    When we permute the rows of columns, we update the sign of the det.
 *)
 val findpivot : 'a C(Dom).vc -> ('a,int) code -> ('a,int) code -> 
   ('a,int) code -> ('a,int) code ->
   (('a,Dom.v option) code,[> `TDet of 'a D.lstate] list,('a,'w) code) monad
end

module RowPivot
   (Dom: DOMAIN) 
   (C: CONTAINER2D)
   (D:   DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   let findpivot b r m c n = mdo {
       pivot <-- retN (liftRef .< None >. );
       seqM (retLoopM r .<.~n-1>. (fun j -> mdo {
              bjc <-- l1 retN (Ctr.get b j c);
              whenM (ret .< not ( .~bjc = .~Dom.zero) >.)
          (retMatchM (liftGet pivot)
            (fun pv ->
              mdo {
              (i,bic) <-- ret (liftPair pv);
              whenM (Dom.better_than bic bjc)
                (ret .< .~pivot := Some (.~j,.~bjc) >.)})
             (ret .< .~pivot := Some (.~j,.~bjc) >.))}))
             (* finished the loop *)
             (retMatchM (liftGet pivot)
                (fun pv ->
                     mdo {
                         (i,bic) <-- ret (liftPair pv);
                 seqM (whenM (ret .< .~i <> .~r >. )
                                (seqM 
                                   (ret (Ctr.swap_rows_stmt b r i))
                   (D.upd_sign ())))
                              (ret .<Some .~bic>.)})
                (ret .< None >.))
   }
end

module FullPivot
   (Dom: DOMAIN) 
   (C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   let findpivot b r m c n = mdo {
       pivot <-- retN (liftRef .< None >. );
       seqM (retLoopM r .<.~n-1>. (fun j -> 
              retLoopM c .<.~m-1>. (fun k ->
           mdo {
              bjk <-- l1 retN (Ctr.get b j k);
              whenM (ret .< not ( .~bjk = .~Dom.zero) >.)
              (retMatchM (liftGet pivot)
                (fun pv ->
                  mdo {
                  (pr,pc,brc) <-- ret (liftPPair pv);
                  whenM (Dom.better_than brc bjk)
                    (ret .< .~pivot := Some ((.~j,.~k),.~bjk) >.)})
                 (ret .< .~pivot := Some ((.~j,.~k),.~bjk) >.))})))
             (* finished the loop *)
             (retMatchM (liftGet pivot)
                (fun pv ->
                     mdo {
                         (pr,pc,brc) <-- ret (liftPPair pv);
                         seqM
                             (whenM (ret .< .~pc <> .~c >. )
                                 (seqM
                                   (ret (Ctr.swap_cols_stmt b c pc))
                                   (D.upd_sign ())))
                           (seqM
                             (whenM (ret .< .~pr <> .~r >. )
                                 (seqM
                                   (ret (Ctr.swap_rows_stmt b r pr))
                                   (D.upd_sign ())))
                              (ret .<Some .~brc>.))})
                (ret .< None >.))
   }
end

module NoPivot
   (Dom: DOMAIN) 
   (C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   (* In this case, we assume diagonal dominance, and so
      just take the diagonal as ``pivot'' *)
   let findpivot b r m c n = mdo { 
       brc <-- Ctr.get b r c;
       ret .< Some (.~brc) >. }
end

module Gen(Dom: DOMAIN)(C: CONTAINER2D)(PivotF: PIVOT)
          (Update: UPDATE with type baseobj = Dom.v and type ctr = C(Dom).contr)
          (Out: OUTPUT with type contr = C(Dom).contr and type D.indet = Dom.v 
                        and type 'a D.lstate = 'a Update.D.lstate) =
   struct
    module Ctr = C(Dom)
    module Pivot = PivotF(Dom)(C)(Out.D)
    type v = Dom.v
    let gen =
      let zerobelow b r c m n brc =
        let innerbody i = mdo {
            bic <-- Ctr.get b i c;
            whenM (ret .< not (.~bic = .~Dom.zero) >. )
                (seqM (retLoopM .<.~c+1>. .<.~m-1>. 
                          (fun k -> Update.update b r c i k) )
                      (Ctr.set b i c Dom.zero)) } in 
        mdo {
              seqM (retLoopM .<.~r+1>. .<.~n-1>. innerbody) 
                   (Update.update_det brc) } in
      let dogen a = mdo {
          r <-- Out.R.decl ();
          c <-- retN (liftRef .< 0 >.);
          b <-- retN (Ctr.mapper Dom.normalizerf (Ctr.copy a));
          m <-- retN (Ctr.dim1 a);
          n <-- retN (Ctr.dim2 a);
          () <-- Update.D.decl ();
          seqM 
            (retWhileM .< !(.~c) < .~m && !(.~r) < .~n >.  ( mdo {
               rr <-- retN (liftGet r);
               cc <-- retN (liftGet c);
               pivot <-- l1 retN (Pivot.findpivot b rr m cc n);
               seqM (retMatchM pivot (fun pv -> 
                        seqM (zerobelow b rr cc m n pv)
                             (Out.R.succ ()) )
                        (Update.D.zero_sign () ))
                    (ret .< .~c := (! .~c) + 1 >. ) } ))
            (Out.make_result b) } 
    in
    .<fun a -> .~(dogen .<a>. [] k0) >.
end

module GenFA1 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (OutJustMatrix(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))

module GenFA2 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (OutDet(FloatDomain)(GenericArrayContainer)(FDet))
module GenFA3 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (OutRank(FloatDomain)(GenericArrayContainer)(Rank))
module GenFA4 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (OutDetRank(FloatDomain)(GenericArrayContainer)(FDet)(Rank));;
module GenFV1 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (OutJustMatrix(FloatDomain)(GenericVectorContainer)(FDet))
module GenFV2 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (OutDet(FloatDomain)(GenericVectorContainer)(FDet))
module GenFV3 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(NoDet(FloatDomain)))
                   (OutRank(FloatDomain)(GenericVectorContainer)(Rank))
module GenFV4 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (OutDetRank(FloatDomain)(GenericVectorContainer)(FDet)(Rank));;

(* But this is an error!
module GenIA1 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutJustMatrix(IntegerDomain)(GenericArrayContainer)(IDet));;
*)
module GenIA1 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutJustMatrix(IntegerDomain)(GenericArrayContainer)(IDet));;
module GenIA2 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutDet(IntegerDomain)(GenericArrayContainer)(IDet));;
module GenIA3 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutRank(IntegerDomain)(GenericArrayContainer)(Rank))
module GenIA4 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutDetRank(IntegerDomain)(GenericArrayContainer)(IDet)(Rank));;
module GenIV1 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutJustMatrix(IntegerDomain)(GenericVectorContainer)(NoDet(IntegerDomain)));;
module GenIV2 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutDet(IntegerDomain)(GenericVectorContainer)(IDet));;
module GenIV3 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutRank(IntegerDomain)(GenericVectorContainer)(Rank))
module GenIV4 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutDetRank(IntegerDomain)(GenericVectorContainer)(IDet)(Rank));;
module GenFA11 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (OutJustMatrix(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
module GenFA12 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (OutDet(FloatDomain)(GenericArrayContainer)(FDet))
module GenFA13 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (OutRank(FloatDomain)(GenericArrayContainer)(Rank))
module GenFA14 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (OutDetRank(FloatDomain)(GenericArrayContainer)(FDet)(Rank))
