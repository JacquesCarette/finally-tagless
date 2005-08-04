open Infra

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
      rdecl <-- retN (liftRef Idx.zero);
      rstore rdecl;
      ret rdecl }
  let succ () = mdo {
   r <-- rfetch ();
   Code.assign r (Idx.succ (liftGet r)) }
end

module Rank:RANK = struct
  include TrackRank
  let fin () = mdo {
      r <-- rfetch ();
       ret (liftGet r) }
end

module NoRank:RANK = struct
  include TrackRank
  let fin () = ret Idx.minusone
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
  let get () = ret (liftRef Code.cunit)
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
      dsdecl <-- retN (liftRef Idx.one);
      dstore (dsdecl,ddecl) }
  let upd_sign () = mdo {
      det <-- dfetch ();
      det1 <-- ret (fst det);
      Code.assign det1 (Idx.uminus (liftGet det1)) }
  let zero_sign () = mdo {
      det <-- dfetch ();
      det1 <-- ret (fst det);
      Code.assign det1 Idx.zero }
  let acc v = mdo {
      det <-- dfetch ();
      det2 <-- ret (snd det);
      r <-- Dom.times (liftGet det2) v;
      Code.assign det2 r }
  let get () = mdo {
      det <-- dfetch ();
      ret (snd det) }
  let set v = mdo {
      det <-- dfetch ();
      det2 <-- ret (snd det);
      Code.assign det2 v }
  let fin () = mdo {
      (det_sign,det) <-- dfetch ();
      ifM (LogicCode.equal (liftGet det_sign) Idx.zero) (ret Dom.zero)
      (ifM (LogicCode.equal (liftGet det_sign) Idx.one) (ret (liftGet det))
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
module DivisionUpdate
    (Dom:DOMAIN with type kind = domain_is_field)
    (C:CONTAINER2D)
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

(* This type is needed for the output, and is tracked during
   pivoting. *)
type perm = RowSwap of (int * int) | ColSwap of (int * int)
 
module type TRACKPIVOT = sig
  type 'a lstate
  val decl : unit -> (unit,[> `TPivot of 'a lstate] list,('a,'w) code) monad
  val add : ('a,perm) code -> 
    (('a,unit) code,[> `TPivot of 'a lstate] list,('a,'w) code) monad
  val fin : unit -> 
    (('a,perm list) code ,[> `TPivot of 'a lstate] list,'w) monad
end

module TrackPivot = 
  struct
  type 'a lstate = ('a, perm list ref) code
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  let coerce = function `TPivot x -> `TPivot x | x -> x
  let rec fetch_iter (s : [> `TPivot of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TPivot x -> x
    |  _ -> fetch_iter (List.tl s)
  let pfetch () = mdo { s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s) }
  let pstore v = store (`TPivot v)
  let decl () = mdo {
      pdecl <-- retN (liftRef ListCode.nil);
      pstore pdecl }
  let add v = mdo {
   p <-- pfetch ();
   Code.assign p (ListCode.append v (liftGet p)) }
end

module KeepPivot:TRACKPIVOT = struct
  include TrackPivot
  let fin () = mdo {
      p <-- pfetch ();
      ret (liftGet p) }
end

module DiscardPivot:TRACKPIVOT = struct
  type 'a lstate = ('a, perm list ref) code
  let decl () = ret ()
  let add v = retUnit
  let fin () = ret ListCode.nil
end

module type OUTPUT = sig
  type contr
  type res
  module D : DETERMINANT
  module R : RANK
  module P : TRACKPIVOT
  val make_result : ('a,contr) code -> 
    (('a,res) code,
     [> `TDet of 'a D.lstate | `TRan of 'a R.lstate | `TPivot of 'a P.lstate]
       list,
     ('a,'w) code) monad
end

(* What to return *)
module OutJustMatrix(Dom:DOMAIN)(C: CONTAINER2D)(Det : DETERMINANT) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr
  module D = Det
  module R = NoRank
  module P = DiscardPivot
  let make_result b = ret b
end

module OutDet(Dom:DOMAIN)(C: CONTAINER2D)
    (Det : DETERMINANT with type indet = Dom.v and type outdet = Dom.v) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * Det.outdet
  module D = Det
  module R = NoRank
  module P = DiscardPivot
  let make_result b = mdo {
    det <-- D.fin ();
    ret (TupleCode.tup2 b det) }
end

module OutRank(Dom:DOMAIN)(C: CONTAINER2D)(Rank : RANK) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * int
  module D = NoDet(Dom)
  module R = Rank
  module P = DiscardPivot
  let make_result b = mdo {
    rank <-- R.fin ();
    ret (TupleCode.tup2 b rank) }
end

module OutDetRank(Dom:DOMAIN)(C: CONTAINER2D)
    (Det : DETERMINANT with type indet = Dom.v and type outdet = Dom.v)
    (Rank : RANK) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * Det.outdet * int
  module D = Det
  module R = Rank
  module P = DiscardPivot
  let make_result b = mdo {
    det  <-- D.fin ();
    rank <-- R.fin ();
    ret (TupleCode.tup3 b det rank) }
end

module OutDetRankPivot(Dom:DOMAIN)(C: CONTAINER2D)
    (Det : DETERMINANT with type indet = Dom.v and type outdet = Dom.v)
    (Rank : RANK) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * Det.outdet * int * perm list
  module D = Det
  module R = Rank
  module P = KeepPivot
  let make_result b = mdo {
    det  <-- D.fin ();
    rank <-- R.fin ();
    pivmat <-- P.fin ();
    ret (TupleCode.tup4 b det rank pivmat) }
end

module FDet = AbstractDet(FloatDomain)
module IDet = AbstractDet(IntegerDomain)
module RDet = AbstractDet(RationalDomain)

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

module RowPivot(Dom: DOMAIN)(C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   let findpivot b r n c m = mdo {
       pivot <-- retN (liftRef MaybeCode.none );
       (* If no better_than procedure defined, we just search for
	  non-zero element. Any non-zero element is a good pivot.
	  If better_than is defined, we search then for the best element *)
       seqM
        (match (Dom.better_than) with
         Some sel -> 
              retLoopM r (Idx.pred n) (fun j -> mdo {
              bjc <-- l1 retN (Ctr.get b j c);
              whenM (l1 LogicCode.not (LogicCode.equal bjc Dom.zero ))
                  (retMatchM (liftGet pivot)
                    (fun pv ->
                      mdo {
                      (i,bic) <-- ret (liftPair pv);
                      whenM (sel bic bjc)
                        (Code.assign pivot (MaybeCode.just 
                                     (TupleCode.tup2 j bjc))) })
                     (Code.assign pivot (MaybeCode.just 
                                  (TupleCode.tup2 j bjc))))
              })
         | None ->
           mdo {
            brc <-- l1 retN (Ctr.get b r c);
            ifM (l1 LogicCode.not (LogicCode.equal brc Dom.zero))
              (* the current element is good enough *)
              (Code.assign pivot (MaybeCode.just (TupleCode.tup2 r brc)))
              (mdo {
                  s <-- fetch;
                  ret .< let rec loop j =
                       if j < .~n then 
                       let bjc = .~(Ctr.get' b .<j>. c) in
                       if bjc = .~Dom.zero then loop (j+1)
                       else .~pivot := Some (j,bjc)
                      in loop (.~r+1) >.})}
         )
         (retMatchM (liftGet pivot)
                (fun pv ->
                     mdo {
                         (i,bic) <-- ret (liftPair pv);
                         seqM (whenM (LogicCode.notequal i r)
                                (seqM 
                                   (ret (Ctr.swap_rows_stmt b r i))
                                   (D.upd_sign ())))
                              (ret (MaybeCode.just bic))})
                (ret MaybeCode.none))
   }
end

module FullPivot(Dom: DOMAIN)(C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   let findpivot b r n c m = mdo {
       pivot <-- retN (liftRef MaybeCode.none );
       seqM (retLoopM r (Idx.pred n) (fun j -> 
              retLoopM c (Idx.pred m) (fun k ->
           mdo {
              bjk <-- l1 retN (Ctr.get b j k);
              whenM (l1 LogicCode.not ( LogicCode.equal bjk Dom.zero) )
              (match (Dom.better_than) with
              | Some sel ->
                  (retMatchM (liftGet pivot)
                    (fun pv ->
                      mdo {
                      (pr,pc,brc) <-- ret (liftPPair pv);
                      whenM (sel brc bjk)
                        (Code.assign pivot (MaybeCode.just
                            (TupleCode.tup2 (TupleCode.tup2 j k) bjk))) })
                     (ret .< .~pivot := Some ((.~j,.~k),.~bjk) >.))
              | None ->
                  (ret .< .~pivot := Some ((.~j,.~k),.~bjk) >.)
              )})))
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
                  (ret MaybeCode.none))
   }
end

module NoPivot(Dom: DOMAIN)(C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   (* In this case, we assume diagonal dominance, and so
      just take the diagonal as ``pivot'' *)
   let findpivot b r n c m = mdo { 
       brc <-- Ctr.get b r c;
       ret (MaybeCode.just brc)}
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
            whenM (l1 LogicCode.not (LogicCode.equal bic Dom.zero ))
                (seqM (retLoopM (Idx.succ c) (Idx.pred m)
                          (fun k -> Update.update b r c i k) )
                      (Ctr.set b i c Dom.zero)) } in 
        mdo {
              seqM (retLoopM (Idx.succ r) (Idx.pred n) innerbody) 
                   (Update.update_det brc) } in
      let dogen a = mdo {
          r <-- Out.R.decl ();
          c <-- retN (liftRef Idx.zero);
          b <-- retN (Ctr.mapper Dom.normalizerf (Ctr.copy a));
          m <-- retN (Ctr.dim1 a);
          n <-- retN (Ctr.dim2 a);
          () <-- Update.D.decl ();
          () <-- Out.P.decl ();
          seqM 
            (retWhileM (LogicCode.and_ (Idx.less (liftGet c) m)
                                       (Idx.less (liftGet r) n) )
               ( mdo {
               rr <-- retN (liftGet r);
               cc <-- retN (liftGet c);
               pivot <-- l1 retN (Pivot.findpivot b rr n cc m);
               seqM (retMatchM pivot (fun pv -> 
                        seqM (zerobelow b rr cc m n pv)
                             (Out.R.succ ()) )
                        (Update.D.zero_sign () ))
                    (Code.update c Idx.succ) } ))
            (Out.make_result b) } 
    in
    .<fun a -> .~(runM (dogen .<a>.)) >.
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
                   (OutDetRank(FloatDomain)(GenericArrayContainer)(FDet)(Rank))
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
                   (OutDetRank(FloatDomain)(GenericVectorContainer)(FDet)(Rank))
module GenFV5 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (OutDetRank(FloatDomain)(GenericVectorContainer)(FDet)(Rank))

(* But this is an error!
module GenIA1 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutJustMatrix(IntegerDomain)(GenericArrayContainer)(IDet))
*)
module GenIA1 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutJustMatrix(IntegerDomain)(GenericArrayContainer)(IDet))
module GenIA2 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutDet(IntegerDomain)(GenericArrayContainer)(IDet))
module GenIA3 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutRank(IntegerDomain)(GenericArrayContainer)(Rank))
module GenIA4 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (OutDetRank(IntegerDomain)(GenericArrayContainer)(IDet)(Rank))
module GenIV1 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutJustMatrix(IntegerDomain)(GenericVectorContainer)(NoDet(IntegerDomain)))
module GenIV2 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutDet(IntegerDomain)(GenericVectorContainer)(IDet))
module GenIV3 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutRank(IntegerDomain)(GenericVectorContainer)(Rank))
module GenIV4 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutDetRank(IntegerDomain)(GenericVectorContainer)(IDet)(Rank))
module GenIV5 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (FullPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (OutDetRank(IntegerDomain)(GenericVectorContainer)(IDet)(Rank))
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

module GenFA24 = Gen(FloatDomain)
                    (GenericArrayContainer)
                    (RowPivot)
                    (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                    (OutDetRankPivot(FloatDomain)(GenericArrayContainer)(FDet)(Rank))
module GenRA1 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (OutJustMatrix(RationalDomain)(GenericArrayContainer)(RDet))
module GenRA2 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (OutDet(RationalDomain)(GenericArrayContainer)(RDet))
module GenRA3 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (OutRank(RationalDomain)(GenericArrayContainer)(Rank))
module GenRA4 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (OutDetRank(RationalDomain)(GenericArrayContainer)(RDet)(Rank))
