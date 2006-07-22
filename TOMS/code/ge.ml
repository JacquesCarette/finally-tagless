open Infra

(* Monad used in this module: code generation monad with open union state *)
type ('a,'v,'s,'w) cmonad = (('a,'v) code, 's list, ('a,'w) code) monad


module type DETERMINANT = sig
  type indet
  type outdet
  type tdet = outdet ref
  type 'a lstate
  type 'a tag_lstate = [`TDet of 'a lstate ]
	(* Here, parameter 'b account for all the extra polymorphims *)
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
  val decl : unit -> ('b,unit) lm (* could be unit rather than unit code...*)
  val upd_sign  : unit -> ('b,unit) lm
  val zero_sign : unit -> ('b,unit) lm
  val acc       : ('a,indet) code -> ('a * 's * 'w,unit) lm
  val get       : unit -> ('b,tdet) lm
  val set       : ('a,indet) code -> ('a * 's * 'w,unit) lm
  val fin       : unit -> ('b,outdet) lm
end

(* no need to make the type abstract here - just leads to other problems *)
module type RANK = sig
  type 'a lstate = ('a, int ref) code
  type 'a tag_lstate = [`TRan of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
  val rfetch : unit -> ('b,int ref) lm
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
  type 'a tag_lstate = [`TRan of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  let coerce = function `TRan x -> `TRan x | x -> x
  let rec fetch_iter (s : [> `TRan of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TRan x -> x
    |  _ -> fetch_iter (List.tl s)
  let rfetch () = perform s <-- fetch; (* unit for monomorphism restriction *)
                          ret (fetch_iter s)
  let rstore v = store (`TRan v)
  let decl () = perform
      rdecl <-- retN (liftRef Idx.zero);
      rstore rdecl;
      ret rdecl
  let succ () = perform
   r <-- rfetch ();
   Code.assignL r (Idx.succ (liftGet r))
end

module Rank:RANK = struct
  include TrackRank
  let fin () = perform
      r <-- rfetch ();
      ret (liftGet r)
end

module NoRank:RANK = struct
  include TrackRank
  let fin () = Idx.minusoneL
end

(* In the case of a non-fraction-free algorithm with no Det
   output, it is possible to not track the determinant, but
   in all other cases it is needed.
*)

(* we need the domain anyways to get things to type properly *)
module NoDet(Dom:DOMAIN) =
  struct
  type indet = Dom.v
  type outdet = unit
  type tdet = outdet ref
  type 'a lstate = unit
  let decl () = retUnitL
  let upd_sign () = retUnitL
  let zero_sign () = retUnitL
  let acc v = retUnitL
  let get () = ret (liftRef Code.cunit)
  let set v = retUnitL
  let fin () = retUnitL
  type 'a tag_lstate = [`TDet of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
end

module AbstractDet(Dom: DOMAIN) : 
    (DETERMINANT with type indet  = Dom.v and
                      type outdet = Dom.v) =
  struct
  type indet = Dom.v
  type outdet = indet
  type tdet = outdet ref
  (* the first part of the state is an integer: which is +1, 0, -1:
     the sign of the determinant *)
  type 'a lstate = ('a,int ref) code * ('a,tdet) code
  type 'a tag_lstate = [`TDet of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  let coerce = function `TDet x -> `TDet x | x -> x
  let rec fetch_iter (s : [> 'a tag_lstate] list) =
    match (coerce (List.hd s)) with
      `TDet x -> x
    |  _ -> fetch_iter (List.tl s)
  let dfetch () = perform s <-- fetch; (* unit for monomorphism restriction *)
                     ret (fetch_iter s)
  let dstore v = store (`TDet v)
  let decl () = perform
      ddecl <-- retN (liftRef Dom.one);
      dsdecl <-- retN (liftRef Idx.one);
      dstore (dsdecl,ddecl);
      retUnitL
  let upd_sign () = perform
      det <-- dfetch ();
      det1 <-- ret (fst det);
      Code.assignL det1 (Idx.uminus (liftGet det1))
  let zero_sign () = perform
      det <-- dfetch ();
      det1 <-- ret (fst det);
      Code.assignL det1 Idx.zero
  let acc v = perform
      det <-- dfetch ();
      det2 <-- ret (snd det);
      r <-- Dom.timesL (liftGet det2) v;
      Code.assignL det2 r
  let get () = perform
      det <-- dfetch ();
      ret (snd det)
  let set v = perform
      det <-- dfetch ();
      det2 <-- ret (snd det);
      Code.assignL det2 v
  let fin () = perform
      (det_sign,det) <-- dfetch ();
      ifM (LogicCode.equalL (liftGet det_sign) Idx.zero) (ret Dom.zero)
      (ifM (LogicCode.equalL (liftGet det_sign) Idx.one) (ret (liftGet det))
          (Dom.uminusL (liftGet det)))
end

module type UPDATE = sig
    type baseobj
    type ctr
    type 'a idx = ('a,int) code
    module D : DETERMINANT
    val update : ('a, ctr) code -> 'a idx -> 'a idx -> 'a idx -> 'a idx ->
      ('a * 's * 'w,unit) D.lm
    val update_det : ('a, baseobj) code -> 
      ('a * 's * 'w,unit) D.lm
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
  let update b r c i k = perform
      t <-- Dom.divL (Ctr.get b i c) (Ctr.get b r c);
      l <-- Dom.timesL t (Ctr.get b r k);
      y <-- Dom.minusL (Ctr.get b i k) l;
      Ctr.setL b i k (Dom.normalizerg y)
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
  let update b r c i k = perform
      x <-- Dom.timesL (Ctr.get b i k) (Ctr.get b r c);
      y <-- Dom.timesL (Ctr.get b r k) (Ctr.get b i r);
      z <-- Dom.minusL x y;
      t <-- ret (Dom.normalizerg z);
      d <-- Det.get ();
      ov <-- Dom.divL t (liftGet d);
      Ctr.setL b i k ov
  let update_det v = Det.set v
end

(* This type is needed for the output, and is tracked during
   pivoting. *)
type perm = RowSwap of (int * int) | ColSwap of (int * int)
 
module type TRACKPIVOT = sig
  type 'a lstate
  type 'a tag_lstate = [`TPivot of 'a lstate ]
  val decl : unit -> (unit,[> 'a tag_lstate] list,('a,'w) code) monad
  val add : ('a,perm) code -> 
    (('a,unit) code,[> 'a tag_lstate] list,('a,'w) code) monad
  val fin : unit -> 
    (('a,perm list) code ,[> 'a tag_lstate] list,'w) monad
end

module TrackPivot = 
  struct
  type 'a lstate = ('a, perm list ref) code
  type 'a tag_lstate = [`TPivot of 'a lstate ]
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  let coerce = function `TPivot x -> `TPivot x | x -> x
  let rec fetch_iter (s : [> `TPivot of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TPivot x -> x
    |  _ -> fetch_iter (List.tl s)
  let pfetch () = perform s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s)
  let pstore v = store (`TPivot v)
  let decl () = perform
      pdecl <-- retN (liftRef ListCode.nil);
      pstore pdecl
  let add v = perform
   p <-- pfetch ();
   Code.assignL p (ListCode.cons v (liftGet p))
end

module KeepPivot:TRACKPIVOT = struct
  include TrackPivot
  let fin () = perform
      p <-- pfetch ();
      ret (liftGet p)
end

module DiscardPivot:TRACKPIVOT = struct
  type 'a lstate = ('a, perm list ref) code
  type 'a tag_lstate = [`TPivot of 'a lstate ]
  let decl () = ret ()
  let add v = retUnitL
  let fin () = ret ListCode.nil
end

module type INPUT =
    functor (Dom: DOMAIN) -> 
      functor (C: CONTAINER2D) -> sig
    type inp
    val get_input : ('a, inp) code ->
        (('a, C(Dom).contr) code * ('a, int) code * bool, 's, ('a, 'w) code) monad
end 

(* What is the input *)
module InpJustMatrix(Dom:DOMAIN)(C: CONTAINER2D) = struct
    module Ctr = C(Dom)
    type inp   = Ctr.contr
    let get_input a = ret (a, Ctr.dim2 a, false)
end

module InpMatrixMargin(Dom:DOMAIN)(C: CONTAINER2D) = struct
    module Ctr = C(Dom)
    type inp   = Ctr.contr * int
    let get_input a = perform
        (b,c) <-- liftPair a;
        ret (b, c, true)
end

module type OUTPUT = sig
  type contr
  type res
  module D : DETERMINANT
  module R : RANK
  module P : TRACKPIVOT
  val make_result : ('a,contr) code -> 
    ('a,res,[> 'a D.tag_lstate | 'a R.tag_lstate | 'a P.tag_lstate],'w) cmonad
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
  let make_result b = perform
    det <-- D.fin ();
    ret (TupleCode.tup2 b det)
end

module OutRank(Dom:DOMAIN)(C: CONTAINER2D)
    (Det: DETERMINANT with type indet = Dom.v)
    (Rank : RANK) =
  struct
  module Ctr = C(Dom)
  type contr = Ctr.contr
  type res = contr * int
  module D = Det
  module R = Rank
  module P = DiscardPivot
  let make_result b = perform
    rank <-- R.fin ();
    ret (TupleCode.tup2 b rank)
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
  let make_result b = perform
    det  <-- D.fin ();
    rank <-- R.fin ();
    ret (TupleCode.tup3 b det rank)
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
  let make_result b = perform
    det  <-- D.fin ();
    rank <-- R.fin ();
    pivmat <-- P.fin ();
    ret (TupleCode.tup4 b det rank pivmat)
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
   ('a,int) code -> ('a,int) code -> ('a * 's * 'w,Dom.v option) D.lm
end

module RowPivot(Dom: DOMAIN)(C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   let findpivot b r n c m = perform
       pivot <-- retN (liftRef MaybeCode.none );
       (* If no better_than procedure defined, we just search for
      non-zero element. Any non-zero element is a good pivot.
      If better_than is defined, we search then for the best element *)
       seqM
        (match (Dom.better_than) with
         Some sel -> 
              retLoopM r (Idx.pred n) (fun j -> perform
              bjc <-- retN (Ctr.get b j c);
              whenM (LogicCode.notL (LogicCode.equal bjc Dom.zero ))
                  (retMatchM (liftGet pivot)
                    (fun pv ->
                      perform
                      (i,bic) <-- liftPair pv;
                      whenM (sel bic bjc)
                        (Code.assignL pivot (MaybeCode.just 
                                     (TupleCode.tup2 j bjc))))
                     (Code.assignL pivot (MaybeCode.just 
                                  (TupleCode.tup2 j bjc))))
              )
         | None ->
           perform
            brc <-- retN (Ctr.get b r c);
            ifM (LogicCode.notL (LogicCode.equal brc Dom.zero))
              (* the current element is good enough *)
              (Code.assignL pivot (MaybeCode.just (TupleCode.tup2 r brc)))
              (let traverse = fun o j ->
                  whenM (ret (Idx.less j n))
                    (perform
                        bjc <-- retN (Ctr.get b j c);
                        ifM (LogicCode.equalL bjc Dom.zero)
                            (Code.applyL o (Idx.succ j))
                            (Code.assignL pivot (MaybeCode.just 
                                (TupleCode.tup2 j bjc)))) in
              (genrecloop traverse (Idx.succ r)))
         )
         (retMatchM (liftGet pivot)
                (fun pv ->
                     perform
                         (i,bic) <-- liftPair pv;
                         seqM (whenM (LogicCode.notequalL i r)
                                (seqM 
                                   (ret (Ctr.swap_rows_stmt b r i))
                                   (D.upd_sign ())))
                              (ret (MaybeCode.just bic)))
                (ret MaybeCode.none))
end

module FullPivot(Dom: DOMAIN)(C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   let findpivot b r n c m = perform
       pivot <-- retN (liftRef MaybeCode.none );
       seqM (retLoopM r (Idx.pred n) (fun j -> 
              retLoopM c (Idx.pred m) (fun k ->
           perform
              bjk <-- retN (Ctr.get b j k);
              whenM (LogicCode.notL ( LogicCode.equal bjk Dom.zero) )
              (match (Dom.better_than) with
              | Some sel ->
                  (retMatchM (liftGet pivot)
                    (fun pv ->
                      perform
                      (pr,pc,brc) <-- liftPPair pv;
                      whenM (sel brc bjk)
                        (Code.assignL pivot (MaybeCode.just
                            (TupleCode.tup2 (TupleCode.tup2 j k) bjk))))
                     (Code.assignL pivot (MaybeCode.just
                            (TupleCode.tup2 (TupleCode.tup2 j k) bjk))))
              | None ->
                  (Code.assignL pivot (MaybeCode.just (
                      TupleCode.tup2 (TupleCode.tup2 j k) bjk)))
              ))))
              (* finished the loop *)
              (retMatchM (liftGet pivot)
                  (fun pv ->
                     perform
                         (pr,pc,brc) <-- liftPPair pv;
                         seqM
                             (whenM (LogicCode.notequalL pc c)
                                 (seqM
                                   (ret (Ctr.swap_cols_stmt b c pc))
                                   (D.upd_sign ())))
                           (seqM
                             (whenM (LogicCode.notequalL pr c)
                                 (seqM
                                   (ret (Ctr.swap_rows_stmt b r pr))
                                   (D.upd_sign ())))
                              (ret (MaybeCode.just brc))))
                  (ret MaybeCode.none))
end

module NoPivot(Dom: DOMAIN)(C: CONTAINER2D)
   (D: DETERMINANT with type indet = Dom.v) =
struct
   module Ctr = C(Dom)
   (* In this case, we assume diagonal dominance, and so
      just take the diagonal as ``pivot'' *)
   let findpivot b r n c m = perform 
       brc <-- Ctr.getL b r c;
       ret (MaybeCode.just brc)
end

module Gen(Dom: DOMAIN)(C: CONTAINER2D)(PivotF: PIVOT)
          (Update: UPDATE with type baseobj = Dom.v and type ctr = C(Dom).contr)
          (In: INPUT)
          (Out: OUTPUT with type contr = C(Dom).contr and type D.indet = Dom.v 
                        and type 'a D.lstate = 'a Update.D.lstate) =
   struct
    module Ctr = C(Dom)
    module Pivot = PivotF(Dom)(C)(Out.D)
    module Input = In(Dom)(C)
    type v = Dom.v
    let gen =
      let zerobelow b r c m n brc =
        let innerbody i = perform
            bic <-- Ctr.getL b i c;
            whenM (LogicCode.notL (LogicCode.equal bic Dom.zero ))
                (seqM (retLoopM (Idx.succ c) (Idx.pred m)
                          (fun k -> Update.update b r c i k) )
                      (Ctr.setL b i c Dom.zero)) in 
        perform
              seqM (retLoopM (Idx.succ r) (Idx.pred n) innerbody) 
                   (Update.update_det brc) in
      let dogen input = perform
          (a,rmar,augmented) <-- Input.get_input input;
          r <-- Out.R.decl ();
          c <-- retN (liftRef Idx.zero);
          b <-- retN (Ctr.mapper Dom.normalizerf (Ctr.copy a));
          m <-- retN (Ctr.dim1 a);
          rmar <-- retN rmar;
          n <-- if augmented then retN (Ctr.dim2 a) else ret rmar;
          Update.D.decl ();
          () <-- Out.P.decl ();
          seqM 
            (retWhileM (LogicCode.and_L (Idx.less (liftGet c) m)
                                       (Idx.less (liftGet r) rmar) )
               ( perform
               rr <-- retN (liftGet r);
               cc <-- retN (liftGet c);
               pivot <-- l1 retN (Pivot.findpivot b rr n cc m);
               seqM (retMatchM pivot (fun pv -> 
                        seqM (zerobelow b rr cc m n pv)
                             (Out.R.succ ()) )
                        (Update.D.zero_sign () ))
                    (Code.updateL c Idx.succ) ))
            (Out.make_result b)
    in dogen
end

module GenFA1 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (InpJustMatrix)
                   (OutJustMatrix(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))

module GenFA2 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (InpJustMatrix)
                   (OutDet(FloatDomain)(GenericArrayContainer)(FDet))
module GenFA3 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (InpJustMatrix)
                   (OutRank(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain))(Rank))
module GenFA4 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (InpJustMatrix)
                   (OutDetRank(FloatDomain)(GenericArrayContainer)(FDet)(Rank))
module GenFV1 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (InpJustMatrix)
                   (OutJustMatrix(FloatDomain)(GenericVectorContainer)(FDet))
module GenFV2 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (InpJustMatrix)
                   (OutDet(FloatDomain)(GenericVectorContainer)(FDet))
module GenFV3 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(NoDet(FloatDomain)))
                   (InpJustMatrix)
                   (OutRank(FloatDomain)(GenericVectorContainer)(NoDet(FloatDomain))(Rank))
module GenFV4 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (InpJustMatrix)
                   (OutDetRank(FloatDomain)(GenericVectorContainer)(FDet)(Rank))
module GenFV5 = Gen(FloatDomain)
                   (GenericVectorContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericVectorContainer)(FDet))
                   (InpJustMatrix)
                   (OutDetRank(FloatDomain)(GenericVectorContainer)(FDet)(Rank))

(* But this is an error!
module GenIA1 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (InpJustMatrix)
                   (OutJustMatrix(IntegerDomain)(GenericArrayContainer)(IDet))
*)
module GenIA1 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (InpJustMatrix)
                   (OutJustMatrix(IntegerDomain)(GenericArrayContainer)(IDet))
module GenIA2 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (InpJustMatrix)
                   (OutDet(IntegerDomain)(GenericArrayContainer)(IDet))
module GenIA3 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (InpJustMatrix)
                   (OutRank(IntegerDomain)(GenericArrayContainer)(IDet)(Rank))
module GenIA4 = Gen(IntegerDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericArrayContainer)(IDet))
                   (InpJustMatrix)
                   (OutDetRank(IntegerDomain)(GenericArrayContainer)(IDet)(Rank))
module GenIV1 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (InpJustMatrix)
                   (OutJustMatrix(IntegerDomain)(GenericVectorContainer)(IDet))
module GenIV2 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (InpJustMatrix)
                   (OutDet(IntegerDomain)(GenericVectorContainer)(IDet))
module GenIV3 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (InpJustMatrix)
                   (OutRank(IntegerDomain)(GenericVectorContainer)(IDet)(Rank))
module GenIV4 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (RowPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (InpJustMatrix)
                   (OutDetRank(IntegerDomain)(GenericVectorContainer)(IDet)(Rank))
module GenIV5 = Gen(IntegerDomain)
                   (GenericVectorContainer)
                   (FullPivot)
                   (FractionFreeUpdate(IntegerDomain)(GenericVectorContainer)(IDet))
                   (InpJustMatrix)
                   (OutDetRank(IntegerDomain)(GenericVectorContainer)(IDet)(Rank))
module GenFA11 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (InpJustMatrix)
                   (OutJustMatrix(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
module GenFA12 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (InpJustMatrix)
                   (OutDet(FloatDomain)(GenericArrayContainer)(FDet))
module GenFA13 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (InpJustMatrix)
                   (OutRank(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain))(Rank))
module GenFA14 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (FullPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (InpJustMatrix)
                   (OutDetRank(FloatDomain)(GenericArrayContainer)(FDet)(Rank))

module GenFA24 = Gen(FloatDomain)
                    (GenericArrayContainer)
                    (RowPivot)
                    (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                    (InpJustMatrix)
                    (OutDetRankPivot(FloatDomain)(GenericArrayContainer)(FDet)(Rank))
module GenRA1 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (InpJustMatrix)
                   (OutJustMatrix(RationalDomain)(GenericArrayContainer)(RDet))
module GenRA2 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (InpJustMatrix)
                   (OutDet(RationalDomain)(GenericArrayContainer)(RDet))
module GenRA3 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (InpJustMatrix)
                   (OutRank(RationalDomain)(GenericArrayContainer)(RDet)(Rank))
module GenRA4 = Gen(RationalDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(RationalDomain)(GenericArrayContainer)(RDet))
                   (InpJustMatrix)
                   (OutDetRank(RationalDomain)(GenericArrayContainer)(RDet)(Rank))
module GenFA5 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (InpMatrixMargin)
                   (OutJustMatrix(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))

module GenFA6 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (InpMatrixMargin)
                   (OutDet(FloatDomain)(GenericArrayContainer)(FDet))
module GenFA7 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain)))
                   (InpMatrixMargin)
                   (OutRank(FloatDomain)(GenericArrayContainer)(NoDet(FloatDomain))(Rank))
module GenFA8 = Gen(FloatDomain)
                   (GenericArrayContainer)
                   (RowPivot)
                   (DivisionUpdate(FloatDomain)(GenericArrayContainer)(FDet))
                   (InpMatrixMargin)
                   (OutDetRank(FloatDomain)(GenericArrayContainer)(FDet)(Rank))
