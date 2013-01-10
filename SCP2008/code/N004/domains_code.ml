
module T = Domains_sig.S(struct type ('a, 'b) rep = ('a, 'b) code end)
open T

open Domains_common

(* Naming conventions: 
   Functions that end in M take monads as arguments and partially
   run them -- in a reset fashion 
   Functions that end in L always return abstract values from plain
   value -- the L is stands for ``lifted'' *)

(* We now define a lot of infrastructure.  We will be quite
   thorough, and define base abstraction and lifted abstractions,
   This will involve a lot of boilerplate code, which unfortunately 
   cannot be so easily automated in MetaOCaml -- that would require 
   introspection.  It could be done in camlp4, but that seems too 
   much as well.  This 'base' could be elided, but when we decide
   to make more use of Abstract Interpretation, we'll regret it,
   so do it now. *)

(* Define instances. of the module types *)

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

module ZpMakeL = functor(P:sig val p:int end) -> struct
    include ZpMake(P)
    type 'a vc = ('a,v) code
    let zeroL = .< zero >.  
    let oneL = .< one >. 
    let (+^) x y = .< plus .~x .~y >.
    let ( *^ ) x y = .< times .~x .~y >.
    let ( -^ ) x y = .< minus .~x .~y >.
    let uminusL x = .< uminus .~x>.
    let divL x y = .< div .~x .~y >.
    let normalizerL = None
    let better_thanL = None
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
  let init n m = .< Array.init .~n (fun _ -> Array.make .~m .~(Dom.zeroL)) >.
  let augment a ma na b nb = .< 
      Array.init .~ma (fun i -> Array.init (.~na + .~nb)
          (fun j -> if j< .~na then .~(getL a .<i>. .<j>.) 
                               else .~(getL b .<i>. .<j - .~na>. ))) >.
  let identity n m = .< Array.init .~n (fun i -> Array.init .~m 
      (fun j -> if (i=j) then .~(Dom.oneL) else .~(Dom.zeroL))) >.
  (* this can be optimized with a swap_rows_from if it is known that
     everything before that is already = Dom.zero *)
  let swap_rows_stmt a _ r1 r2 =
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
  (* only set the head of the current column *)
  let col_head_set x n m y = .< (.~x).(.~n).(.~m) <- .~y >.
end

(* Matrix layed out row after row, in a C fashion, using a record
   as intermediary *)
type 'a container2dfromvector = {arr:('a array); n:int; m:int}

module GenericVectorContainer(Dom:DOMAINL) =
  struct
  module Dom = Dom
  type contr = Dom.v container2dfromvector
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let getL x i j = .< ((.~x).arr).(.~i* (.~x).m + .~j) >.
  let dim2 x = .< (.~x).n >.
  let dim1 x = .< (.~x).m >.
  let mapper g a = match g with
      | Some f -> .< { (.~a) with arr = Array.map (fun z -> .~(f .<z>.)) (.~a).arr} >.
      | None   -> a
  let copy a = .< { (.~a) with arr = Array.copy (.~a).arr} >.
  let init n m = .< {arr=Array.make (.~n* .~m) .~(Dom.zeroL); n = .~n; m = .~m} >.
  let augment a na ma b nb = 
      .< let n = .~na + .~nb in 
      let aa = .~(init .<n>. ma) in
      let st = .~na * .~ma in begin
        for i = 0 to .~na - 1 do
          for j = 0 to .~ma do
            let k = i* .~ma + j in
            aa.arr.(k) <- (.~a).arr.(k) 
          done;
          for j = 0 to .~ma do
            let k = i* .~ma + j in
            aa.arr.(st + k) <- (.~b).arr.(k)
          done
        done;
        aa end 
	>.

  let identity n m = .<
    let nelems = .~n * .~ m in
    let ar = Array.make nelems .~(Dom.zeroL) in
    let rec loop i = if i >= nelems then {arr = ar; n = .~n; m = .~m}
	             else (ar.(i) <- .~(Dom.oneL); loop (i+ .~m +1)) in
    loop 0>.
  let index_default = function
      | Some x -> x
      | None   -> .< 0 >.
  let swap_rows_stmt b start r1 r2 = .<
      let a = (.~b).arr and m = (.~b).m in
      let i1 = .~r1*m and i2 = .~r2*m in
      for i = .~(index_default start) to m-1 do
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
  (* only set the head of the current column *)
  let col_head_set x n m y = .< ((.~x).arr).(.~n* (.~x).m + .~m) <- .~y >.
end

(* Matrix layed out column after column, in a Fortran fashion, using an
   algebraic type as intermediary *)
type 'a container2dfromFvector = FortranVector of ('a array * int * int)

module FortranVectorContainer(Dom:DOMAINL):CONTAINER2D =
  struct
  module Dom = Dom
  type contr = Dom.v container2dfromFvector
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,Dom.v) code
  let unpack z f = .< match .~z with 
      FortranVector(x,n,m) -> .~(f .<x>. .<n>. .<m>. ) >.
  let getL z i j = unpack z (fun x n _ -> .< (.~x).(.~i * .~n + .~j) >. )
  let dim2 z = unpack z (fun _ n _ -> .< .~n >. )
  let dim1 z = unpack z (fun _ _ m -> .< .~m >. )
  let mapper g z = match g with
      | Some f -> unpack z (fun x n m -> .< FortranVector(Array.map (fun q ->
              .~(f .<q>.)) .~x, .~n, .~m) >. )
      | None   -> z
  let copy a = unpack a (fun x n m -> .<FortranVector(Array.copy .~x, .~n, .~m) >. )
  let init n m = .< FortranVector(Array.make (.~n* .~m) .~(Dom.zeroL), .~n, .~m) >.
  let augment a na ma b nb = unpack a (fun mata _ _ -> unpack b (fun matb _ _ ->
      .< let n = .~na + .~nb in 
      let FortranVector(aa,_,_) = .~(init .<n>. ma) in
      let st = .~na * .~ma in begin
        for i = 0 to .~na - 1 do
          for j = 0 to .~ma do
            let k = i* .~ma + j in
            aa.(k) <- (.~mata).(k)
          done;
          for j = 0 to .~ma do
            let k = i* .~ma + j in
            aa.(st + k) <- (.~matb).(k)
          done
        done;
        FortranVector(aa, n, .~ma) end >. ) )
  let identity n m = .< FortranVector(Array.init (.~n* .~m) 
      (fun k -> if ((k mod .~n)* .~m + .~m = k) then .~(Dom.oneL) else
          .~(Dom.zeroL)), .~n, .~m ) >.
  let index_default = function
      | Some x -> x
      | None   -> .< 0 >.
  let swap_rows_stmt b start r1 r2 = unpack b (fun a _ m -> .<
      let i1 = .~r1* .~m and i2 = .~r2* .~m in
      for i = .~(index_default start) to .~m-1 do
          let t = (.~a).(i1 + i) in
          begin 
              (.~a).(i1 + i) <- (.~a).(i2 + i);
              (.~a).(i2 + i) <- t
          end
      done  >. )
  let swap_cols_stmt b c1 c2 = unpack b ( fun a n m -> .<
      let nm = .~n * .~m in
      let rec loop i1 i2 =
        if i2 < nm then
          let t = (.~a).(i1) in
          begin
            (.~a).(i1) <- (.~a).(i2);
            (.~a).(i2) <- t;
            loop (i1 + .~m) (i2 + .~m)
          end
          in loop .~c1 .~c2 >. )
  let row_head b c r = getL b r c
  let col_head_set z n m y = unpack z 
      (fun x _ j -> .< (.~x).(.~n* .~j + .~m) <- .~y >. )
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
  let initL f n = .< Array.init .~n .~f >.
  let getL x n = .< (.~x).(.~n) >.
  let setL x n y = .< (.~x).(.~n) <- .~y >.
  let dim1 x = .< Array.length .~x >.
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

