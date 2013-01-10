(* Emulation `code' with thunks *)
module DirectRep = struct type ('a, 'b) rep = unit -> 'b end

module T = Domains_sig.S(DirectRep)
open T
open DirectRep

open Domains_common

(* Naming conventions: 
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


(* Define instances of the module types *)

(* because the operations are "syntactic" to a certain extent,
   we have to repeat ourselves a lot *)
module FloatDomainL = struct
    include FloatDomain
    type 'a vc = ('a,v) rep
    let zeroL = fun () -> 0.
    let oneL = fun () -> 1.
    let ( +^ ) x y = fun () -> (x ()) +. (y ())
    let ( *^ ) x y = (fun () -> x () *. y ())
    let ( -^ ) x y = (fun () -> x () -. y ())
    let uminusL x  = (fun () -> -. x ())
    let divL x y = (fun () -> x () /. y ()) 
    let better_thanL = 
      Some (fun x y -> (fun () -> abs_float (x ()) < abs_float (y ()) ) )
    let normalizerL = None 
end


(* because the operations are "syntactic" to a certain extent,
   we have to repeat ourselves a lot *)
module IntegerDomainL = struct
    include IntegerDomain
    type 'a vc = ('a,v) rep
    let zeroL = fun () -> 0
    let oneL = fun () -> 1
    let (+^) x y = fun () -> (x ()) + (y ())
    let ( *^ ) x y = (fun () -> x () * y ())
    let ( -^ ) x y = (fun () -> x () - y ())
    let uminusL x = (fun () -> - x ())
    let divL x y = (fun () -> x () / y ()) 
    let better_thanL = 
      Some (fun x y -> (fun () -> abs (x ()) > abs (y ()) ) )
    let normalizerL = None 
end


(* because the operations are "syntactic" to a certain extent,
   we have to repeat ourselves a lot *)
module RationalDomainL = struct
    include RationalDomain
    type 'a vc = ('a,v) rep
    let zeroL = let zero = Num.num_of_int 0 in (fun () ->  zero)
    let oneL = let one = Num.num_of_int 1 in (fun () ->  one)
    let ( +^ ) x y = (fun () -> Num.add_num (x ()) (y ()) )
    let ( *^ ) x y = (fun () -> Num.mult_num (x ()) (y ()))
    let ( -^ ) x y = (fun () -> Num.sub_num (x ()) (y ()))
    let uminusL x = (fun () -> Num.minus_num (x ()))
    let divL x y = (fun () -> Num.div_num (x ()) (y ())) 
    let better_thanL = None (* no such thing here *)
    let normalizerL = None 
end


module ZpMakeL = functor(P:sig val p:int end) -> struct
    include ZpMake(P)
    type 'a vc = ('a,v) rep
    let zeroL = let zero = 0 in (fun () ->  zero)
    let oneL = let one = 1 in (fun () ->  one)
    let ( +^ ) x y = (fun () -> plus (x ()) (y ()) )
    let ( *^ ) x y = (fun () -> times (x ()) (y ()))
    let ( -^ ) x y = (fun () -> minus (x ()) (y ()))
    let uminusL x = (fun () -> uminus (x ()))
    let divL x y = (fun () -> div (x ()) (y ())) 
    let better_thanL = None (* no such thing here *)
    let normalizerL = None 
end

module GenericArrayContainer(Dom:DOMAINL) =
  struct
  module Dom = Dom
  type contr = Dom.v array array (* Array of rows *)
  type 'a vc = ('a,contr) rep
  type 'a vo = ('a,Dom.v) rep
  let getL x n m = fun () -> (x ()).(n ()).(m ())
  let dim2 x = fun () -> Array.length (x ())       (* number of rows *)
  let dim1 x = fun () -> Array.length (x ()).(0)   (* number of cols *)
  let mapper (g:('a vo -> 'a vo) option) a = match g with
      | Some f -> fun () -> Array.map (fun x -> Array.map (fun z -> f (fun ()
      -> z) () ) x) (a ())
      | None   -> a
  let copy a = fun () -> Array.map (fun x -> Array.copy x) 
                       (Array.copy (a ()))
  let init n m = fun () -> Array.make (n ()) (Array.make (m ()) Dom.zero)
  let augment a na ma b nb = fun () ->
      Array.init ((na ()) + (nb ())) (fun i -> Array.init (ma ())
          (fun j -> if i< (na ()) then ((getL a (fun () -> i) (fun () -> j)) ())
                               else ((getL b (fun () -> (i - (na ()))) (fun ()
                               -> j)) ())))
  let identity n m = fun () -> Array.init (n ()) (fun i -> Array.init (m ())
      (fun j -> if (i=j) then (Dom.oneL ()) else (Dom.zeroL ())))
  (* this can be optimized with a swap_rows_from if it is known that
     everything before that is already = Dom.zero *)
  let swap_rows_stmt a _ r1 r2 = fun () ->
      let t = (a ()).(r1 ()) in
         begin 
             (a ()).(r1 ()) <- (a ()).(r2 ());
             (a ()).(r2 ()) <- t
         end
  let swap_cols_stmt a c1 c2 = fun () ->
      for r = 0 to (dim2 a ())-1 do
          let t = (a ()).(r).(c1 ()) in
          begin 
              (a ()).(r).(c1 ()) <- (a ()).(r).(c2 ());
              (a ()).(r).(c2 ()) <- t
          end
      done
  (* this is list an iterator's start *)
  let row_head = getL
  (* only set the head of the current column *)
  let col_head_set x n m y = fun () -> (x ()).(n ()).(m ()) <- y ()
end

(* Matrix layed out row after row, in a C fashion *)
type 'a container2dfromvector = {arr:('a array); n:int; m:int}

module GenericVectorContainer(Dom:DOMAINL) =
  struct
  module Dom = Dom
  type contr = Dom.v container2dfromvector
  type 'a vc = ('a,contr) rep
  type 'a vo = ('a,Dom.v) rep
  let getL x n m = fun () -> ((x ()).arr).((n ())* (x ()).m + (m ()))
  let dim2 x = fun () -> (x ()).n
  let dim1 x = fun () -> (x ()).m
  let mapper g a = match g with
      | Some f -> fun () -> { (a ()) with arr = Array.map (fun z -> (f (fun ()
      -> z) ())) (a ()).arr}
      | None   -> a
  let copy a = fun () -> { (a ()) with arr = Array.copy (a ()).arr}
  let init n m = fun () -> {arr=Array.make (n () * m ()) Dom.zero; n = n (); m =
      m ()}
  let augment a na ma b nb = fun () ->
      let n = (na ()) + (nb ()) in 
      let aa = (init (fun () -> n) ma) () in
      let st = (na ()) * (ma ()) in begin
        for i = 0 to (na ()) - 1 do
          for j = 0 to (ma ()) do
            let k = i* (ma ()) + j in
            aa.arr.(k) <- (a ()).arr.(k) 
          done;
          for j = 0 to (ma ()) do
            let k = i* (ma ()) + j in
            aa.arr.(st + k) <- (b ()).arr.(k)
          done
        done;
        aa end 
  let identity n m = fun () -> {arr=Array.init (n ()* m ()) 
      (fun k -> if ((k mod (n ()))* (m ()) + (m ()) = k) then (Dom.oneL ()) else
          (Dom.zeroL ())); n = n (); m = m ()}
  let index_default = function
      | Some x -> x ()
      | None   -> 0
  let swap_rows_stmt b start r1 r2 = fun () ->
      let a = (b ()).arr and m = (b ()).m in
      let i1 = (r1 ())*m and i2 = (r2 ())*m in
      for i = (index_default start) to m-1 do
          let t = a.(i1 + i) in
          begin 
              a.(i1 + i) <- a.(i2 + i);
              a.(i2 + i) <- t
          end
      done
  let swap_cols_stmt b c1 c2 = fun () ->
      let a = (b ()).arr and nm = (b ()).n * (b ()).m and m = (b ()).m in
      let rec loop i1 i2 =
    if i2 < nm then
      let t = a.(i1) in
      begin
        a.(i1) <- a.(i2);
        a.(i2) <- t;
        loop (i1 + m) (i2 + m)
      end
      in loop (c1 ()) (c2 ())
  let row_head b c r = getL b r c
  (* only set the head of the current column *)
  let col_head_set x n m y = 
    fun () -> ((x ()).arr).((n ())* (x ()).m + m ()) <- y ()
end

(* Matrix layed out column after column, in a Fortran fashion, using an
   algebraic type as intermediary *)
type 'a container2dfromFvector = FortranVector of ('a array * int * int)

module FortranVectorContainer(Dom:DOMAINL):CONTAINER2D =
  struct
  module Dom = Dom
  type contr = Dom.v container2dfromFvector
  type 'a vc = ('a,contr) rep
  type 'a vo = ('a,Dom.v) rep
  let unpack z f = fun () -> match z () with 
      FortranVector(x,n,m) -> (f (fun () -> x) (fun () -> n) (fun () -> m)) ()
  let getL z i j = unpack z (fun x n _ -> fun () -> (x ()).((i ()) * (n ()) + (j ())) )
  let dim2 z = unpack z (fun _ n _ -> fun () -> n ())
  let dim1 z = unpack z (fun _ _ m -> fun () -> m ())
  let mapper g z = match g with
      | Some f -> unpack z (fun x n m -> fun () -> 
              FortranVector(Array.map (fun q ->
              (f (fun () -> q) () ) ) (x ()), (n ()), (m ())) )
      | None   -> z
  let copy a = unpack a (fun x n m -> fun () -> FortranVector(Array.copy (x ()),
  (n ()), (m ())) )
  let init n m = fun () -> FortranVector(Array.make ((n ())* (m ())) (Dom.zeroL
  ()), (n ()), (m ()))
  let augment a na ma b nb = unpack a (fun mata _ _ -> unpack b (fun matb _ _ ->
      fun () -> let n = (na ()) + (nb ()) in 
      let FortranVector(aa,_,_) = (init (fun () -> n) ma) () in
      let st = (na ()) * (ma ()) in begin
        for i = 0 to (na ()) - 1 do
          for j = 0 to (ma ()) do
            let k = i* (ma ()) + j in
            aa.(k) <- (mata ()).(k)
          done;
          for j = 0 to (ma ()) do
            let k = i* (ma ()) + j in
            aa.(st + k) <- (matb ()).(k)
          done
        done;
        FortranVector(aa, n, (ma ())) end ) )
  let identity n m = fun () -> FortranVector(Array.init ((n ())* (m ())) 
      (fun k -> if ((k mod (n ()))* (m ()) + (m ()) = k) then Dom.oneL () else
          Dom.zeroL ()), (n ()), (m ()) )
  let index_default = function
      | Some x -> x ()
      | None   -> 0
  let swap_rows_stmt b start r1 r2 = unpack b (fun a _ m -> fun () ->
      let i1 = (r1 ())* (m ()) and i2 = (r2 ())* (m ()) in
      for i = (index_default start) to (m ())-1 do
          let t = (a ()).(i1 + i) in
          begin 
              (a ()).(i1 + i) <- (a ()).(i2 + i);
              (a ()).(i2 + i) <- t
          end
      done  )
  let swap_cols_stmt b c1 c2 = unpack b ( fun a n m -> fun () ->
      let nm = (n ()) * (m ()) in
      let rec loop i1 i2 =
        if i2 < nm then
          let t = (a ()).(i1) in
          begin
            (a ()).(i1) <- (a ()).(i2);
            (a ()).(i2) <- t;
            loop (i1 + (m ())) (i2 + (m ()))
          end
          in loop (c1 ()) (c2 ()) )
  let row_head b c r = getL b r c
  let col_head_set z n m y = unpack z 
      (fun x _ j -> fun () -> (x ()).((n ())* (j ()) + (m ())) <- (y ()) )
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
  type 'a vc = ('a,contr) rep
  type 'a vo = ('a,Dom.v) rep
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
  let getL x n = fun () -> x.(n ())
  let setL x n y = fun () -> x.(n ()) <- y ()
  let dim1 x = fun () -> Array.length x
  let mapper g a = match g with
      | Some f -> fun () -> (fun x -> Array.map (f ()) x) (a ())
      | None   -> a
end

module CArray1D = 
  struct
  let getL x n = fun () -> (x ()).(n)
  let setL x n y = fun () -> (x ()).(n) <- y ()
  let dim1 x = fun () -> Array.length (x ())
end

module Array2D =
  struct
  let getL x n m = fun () -> x.(n ()).(m ())
  let setL x n m y = fun () -> x.(n ()).(m ()) <- y ()
  let dim2 x = fun () -> Array.length x       (* number of rows *)
  let dim1 x = fun () -> Array.length (x).(0) (* number of cols *)
end
