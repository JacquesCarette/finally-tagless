open Infra

(* Monad used in this module: code generation monad with open union state *)
type ('a,'v,'s,'w) cmonad = (('a,'v) code, 's list, ('a,'w) code) monad

module type DETERMINANT = sig
  type indet
  type outdet
  type tdet = outdet ref
  type 'a lstate
  type 'a tag_lstate = [`TDet of 'a lstate ]
    (* Here, parameter 'b accounts for all the extra polymorphims *)
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  val decl : unit -> ('a,unit) lm (* could be unit rather than unit code...*)
  val upd_sign  : unit -> ('a,unit) lm
  val zero_sign : unit -> ('a,unit) lm
  val acc       : ('a,indet) code -> ('a * 's * 'w,unit) lm
  val get       : unit -> ('b,tdet) lm
  val set       : ('a,indet) code -> ('a * 's * 'w,unit) lm
  val fin       : unit -> ('b,outdet) lm
end

module type DETF = functor(D:DOMAIN) -> DETERMINANT with type indet = D.v

module type DETD = functor(D:DOMAIN) -> 
    DETERMINANT with type indet = D.v and type outdet = D.v
module type DETU = functor(D:DOMAIN) -> 
    DETERMINANT with type indet = D.v and type outdet = unit

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

module AbstractDet(Dom: DOMAIN) =
  struct
  type indet = Dom.v
  type outdet = Dom.v
  type tdet = outdet ref
  (* the first part of the state is an integer: which is +1, 0, -1:
     the sign of the determinant *)
  type 'a lstate = ('a,int ref) code * ('a,tdet) code
  type 'a tag_lstate = [`TDet of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  let rec fetch_iter s =
    match (List.hd s) with
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

module type IF = functor(T: DETF) -> functor(E:DETF) -> DETF
module IFT(X:IF)(A:DETF)(B:DETF) = struct
    module type B =
        functor(D:DOMAIN) ->
            DETERMINANT with type indet = D.v and 
            type outdet = X(A)(B)(D).outdet
end

module TRUE (T:DETD)(E:DETU) : DETD = T 
module FALSE(T:DETD)(E:DETU) : DETU = E
module AND(A:IF)(B:IF) = struct
    module C : IFT(B)(AbstractDet)(NoDet).B = B(AbstractDet)(NoDet)
    module D : IFT(A)(C)(NoDet).B = A(C)(NoDet)
end

(* Not only do we need to "pass down" the kind (in type S), we also need
   to ensure that the outdet type is properly visible when we need it.
   Type T ensure this.  It is essentially DETF but with an explicit
   leak of the outdet type.
*)
(*
module UpdateProxy(C0:CONTAINER2D)(D0:DETF) = struct
    module type T = functor(D1:DOMAIN) -> 
        DETERMINANT with type indet = D1.v and type outdet = D0(D1).outdet
    module type S =
        functor(C:CONTAINER2D with type Dom.kind = C0.Dom.kind) -> 
        functor(D:T) -> sig
        type ctr = C.contr
        type in_val = C.Dom.v
        type out_val = D(C.Dom).outdet
        type 'a idx = ('a,int) code
        val update : ('a, ctr) code -> 'a idx -> 'a idx -> 'a idx -> 'a idx ->
          ('a, out_val ref) code -> ('a, unit, 's, 'w) cmonad
        val update_det : ('a, in_val) code -> 
          (('a,in_val) code -> ('a, unit, 's, 'w) cmonad) ->
          (('a,in_val) code -> ('a, unit, 's, 'w) cmonad) ->
          ('a, unit, 's, 'w) cmonad
        end
end
*)
module UpdateProxy(C0:CONTAINER2D) = struct
    module type S =
        functor(C:CONTAINER2D with type Dom.kind = C0.Dom.kind) -> sig
        type ctr = C.contr
        type in_val = C.Dom.v
        module CHOICE : IF
        type out_val = CHOICE(AbstractDet)(NoDet)(C.Dom).outdet
        type 'a idx = ('a,int) code
        val update : ('a, ctr) code -> 'a idx -> 'a idx -> 'a idx -> 'a idx ->
          ('a, out_val ref) code -> ('a, unit, 's, 'w) cmonad
        val update_det : ('a, in_val) code -> 
          (('a,in_val) code -> ('a, unit, 's, 'w) cmonad) ->
          (('a,in_val) code -> ('a, unit, 's, 'w) cmonad) ->
          ('a, unit, 's, 'w) cmonad
        end
end

(* What is the update formula? *)
module DivisionUpdate
    (C:CONTAINER2D with type Dom.kind = domain_is_field) =
  struct
  module Dom = C.Dom
  type ctr = C.contr
  type in_val = C.Dom.v
  type out_val = C.Dom.v
  type 'a idx = ('a,int) code
  module CHOICE = TRUE
  let update b r c i k d = perform
      t <-- Dom.divL (C.get b i c) (C.get b r c);
      l <-- Dom.timesL t (C.get b r k);
      y <-- Dom.minusL (C.get b i k) l;
      C.setL b i k (Dom.normalizerg y)
  let update_det v set acc = acc v
end

module FractionFreeUpdate(Ctr:CONTAINER2D) = struct
  module Dom = Ctr.Dom
  type ctr = Ctr.contr
  type in_val = Ctr.Dom.v
  type out_val
  type 'a idx = ('a,int) code
  module CHOICE = FALSE
  let update b r c i k d = perform
      x <-- Dom.timesL (Ctr.get b i k) (Ctr.get b r c);
      y <-- Dom.timesL (Ctr.get b r k) (Ctr.get b i r);
      z <-- Dom.minusL x y;
      t <-- ret (Dom.normalizerg z);
      ov <-- Dom.divL t (liftGet d);
      Ctr.setL b i k ov
  let update_det v set acc = set v
end

(* no need to make the type abstract here - just leads to other problems *)
module type RANK = sig
  type 'a lstate = ('a, int ref) code
  type 'a tag_lstate = [`TRan of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  val rfetch : unit -> ('b, int ref) lm
  val decl   : unit -> ('b, int ref) lm
  val succ   : unit -> ('b, unit) lm
  val fin    : unit -> ('b, int) lm
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
  (* In 3.09, no need for any coercion at all, type inference works! *)
  let rec fetch_iter s =
    match (List.hd s) with
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


(* This type is needed for the output, and is tracked during
   pivoting. *)
type perm = RowSwap of (int * int) | ColSwap of (int * int)
 
module type TRACKPIVOT = sig
  type 'a lstate
  type 'a tag_lstate = [`TPivot of 'a lstate ]
	(* Here, parameter 'b accounts for all the extra polymorphims *)
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
  val decl : unit -> ('b, unit) lm
  val add : ('a,perm) code -> 
    (('a,unit) code,[> 'a tag_lstate] list,('a,'w) code) monad
  val fin : unit -> 
    (('a,perm list) code ,[> 'a tag_lstate] list,'w) monad
end

module TrackPivot = 
  struct
  type 'a lstate = ('a, perm list ref) code
  type 'a tag_lstate = [`TPivot of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  (*let coerce = function `TPivot x -> `TPivot x | x -> x *)
  let rec fetch_iter s =
    match (List.hd s) with
      `TPivot x -> x
    |  _ -> fetch_iter (List.tl s)
  let pfetch () = perform s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s)
  let pstore v = store (`TPivot v)
  let decl () = perform
      pdecl <-- retN (liftRef (ListCode.nil :> ('a, perm list) code));
      pstore pdecl;
      retUnitL
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
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
	constraint 's = [> 'a tag_lstate]
	constraint 'b = 'a * 's * 'w
  let decl () = retUnitL
  let add v = retUnitL
  let fin () = ret ListCode.nil
end

module type INPUT =
      functor (C: CONTAINER2D) -> sig
    type inp
    val get_input : ('a, inp) code ->
        (('a, C.contr) code * ('a, int) code * bool, 's, ('a, 'w) code) monad
end 

(* What is the input *)
module InpJustMatrix(C: CONTAINER2D) = struct
    type inp   = C.contr
    let get_input a = ret (a, C.dim2 a, false)
end

module InpMatrixMargin(C: CONTAINER2D) = struct
    type inp   = C.contr * int
    let get_input a = perform
        (b,c) <-- liftPair a;
        ret (b, c, true)
end

(*
module OutProxy(C0: CONTAINER2D)(Det0:DETF) = struct
    module D = Det0(C0.Dom)
    module type S = 
        functor(C: CONTAINER2D) -> 
        functor(Det: DETERMINANT with type outdet = D.outdet) -> sig
      type res
      module D : DETERMINANT
      module R : RANK
      module P : TRACKPIVOT
      val make_result : ('a,C.contr) code -> 
        ('a,res,[> 'a Det.tag_lstate | 'a R.tag_lstate | 'a P.tag_lstate],'w) cmonad
    end
end
*)
module type OUTPUT = functor(C:CONTAINER2D) -> sig
      module CHOICE : IF
      type res
      module D : DETERMINANT
      module R : RANK
      module P : TRACKPIVOT
      val make_result : ('a,C.contr) code -> 
        ('a,res,[> 'a D.tag_lstate | 'a R.tag_lstate | 'a P.tag_lstate],'w) cmonad
end

(* What to return *)
module OutJustMatrix(C: CONTAINER2D)(Det : DETERMINANT) =
  struct
  type res = C.contr
  module D = Det
  module R = NoRank
  module P = DiscardPivot
  module CHOICE = FALSE
  let make_result b = ret b
end

module OutDet(C: CONTAINER2D)(Det : DETERMINANT) = 
  struct
  type res = C.contr * Det.outdet
  module D = Det
  module R = NoRank
  module P = DiscardPivot
  let make_result b = perform
    det <-- D.fin ();
    ret (TupleCode.tup2 b det)
end

module OutRank(C: CONTAINER2D)(Det: DETERMINANT) = 
  struct
  type res = C.contr * int
  module D = Det
  module R = Rank
  module P = DiscardPivot
  let make_result b = perform
    rank <-- R.fin ();
    ret (TupleCode.tup2 b rank)
end

module OutDetRank(C: CONTAINER2D)(Det : DETERMINANT) =
  struct
  type res = C.contr * Det.outdet * int
  module D = Det
  module R = Rank
  module P = DiscardPivot
  let make_result b = perform
    det  <-- D.fin ();
    rank <-- R.fin ();
    ret (TupleCode.tup3 b det rank)
end

module OutDetRankPivot(C: CONTAINER2D)
    (Det : DETERMINANT) =
  struct
  type res = C.contr * Det.outdet * int * perm list
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
    functor (C: CONTAINER2D) ->
      functor (D: DETF) ->
sig
 (* Find the pivot within [r,m-1] rows and [c,(n-1)] columns
    of containrer b.
    If pivot is found, permute the matrix rows and columns so that the pivot
    becomes the element (r,c) of the matrix,
    Return the value of the pivot option. Or zero?
    When we permute the rows of columns, we update the sign of the det.
 *)
 val findpivot : 'a C.vc -> ('a,int) code -> ('a,int) code -> 
   ('a,int) code -> ('a,int) code -> ('a * 's * 'w, C.Dom.v option) D(C.Dom).lm
end

module RowPivot(C: CONTAINER2D)(Det: DETF) =
struct
   module D = Det(C.Dom) 
   let findpivot b r n c m = perform
       pivot <-- retN (liftRef MaybeCode.none );
       (* If no better_than procedure defined, we just search for
      non-zero element. Any non-zero element is a good pivot.
      If better_than is defined, we search then for the best element *)
       seqM
        (match (C.Dom.better_than) with
         Some sel -> 
              retLoopM r (Idx.pred n) (fun j -> perform
              bjc <-- retN (C.get b j c);
              whenM (LogicCode.notL (LogicCode.equal bjc C.Dom.zero ))
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
            brc <-- retN (C.get b r c);
            ifM (LogicCode.notL (LogicCode.equal brc C.Dom.zero))
              (* the current element is good enough *)
              (Code.assignL pivot (MaybeCode.just (TupleCode.tup2 r brc)))
              (let traverse = fun o j ->
                  whenM (ret (Idx.less j n))
                    (perform
                        bjc <-- retN (C.get b j c);
                        ifM (LogicCode.equalL bjc C.Dom.zero)
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
                                   (ret (C.swap_rows_stmt b r i))
                                   (D.upd_sign ())))
                              (ret (MaybeCode.just bic)))
                (ret MaybeCode.none))
end

module FullPivot(C: CONTAINER2D)(Det: DETF) = 
struct
   module D = Det(C.Dom)
   let findpivot b r n c m = perform
       pivot <-- retN (liftRef MaybeCode.none );
       seqM (retLoopM r (Idx.pred n) (fun j -> 
              retLoopM c (Idx.pred m) (fun k ->
           perform
              bjk <-- retN (C.get b j k);
              whenM (LogicCode.notL ( LogicCode.equal bjk C.Dom.zero) )
              (match (C.Dom.better_than) with
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
                                   (ret (C.swap_cols_stmt b c pc))
                                   (D.upd_sign ())))
                           (seqM
                             (whenM (LogicCode.notequalL pr c)
                                 (seqM
                                   (ret (C.swap_rows_stmt b r pr))
                                   (D.upd_sign ())))
                              (ret (MaybeCode.just brc))))
                  (ret MaybeCode.none))
end

module NoPivot(C: CONTAINER2D)(Det: DETF) = 
struct
   module D = Det(C.Dom)
   (* In this case, we assume diagonal dominance, and so
      just take the diagonal as ``pivot'' *)
   let findpivot b r n c m = perform 
       brc <-- C.getL b r c;
       ret (MaybeCode.just brc)
end

module Gen(C: CONTAINER2D)
          (PivotF: PIVOT)
          (Detf:DETF)
          (Update: UpdateProxy(C).S)
          (In: INPUT)
          (Out: OUTPUT) = 
   struct
    (* module Det = Detf(C.Dom) *)
    module Output = Out(C)
    module U = Update(C)
    module XX = AND(Output.CHOICE)(U.CHOICE)
    module Det = XX.D(C.Dom)
    module Pivot = PivotF(C)(Detf)
    module Input = In(C)
    let gen =
      let zerobelow b r c m n brc =
        let innerbody i = perform
            bic <-- C.getL b i c;
            whenM (LogicCode.notL (LogicCode.equal bic C.Dom.zero ))
                (seqM (retLoopM (Idx.succ c) (Idx.pred m)
                          (fun k -> perform
                              d <-- Det.get ();
                              U.update b r c i k d) )
                      (C.setL b i c C.Dom.zero)) in 
        perform
              seqM (retLoopM (Idx.succ r) (Idx.pred n) innerbody) 
                   (U.update_det brc Det.set Det.acc)in
      let dogen input = perform
          (a,rmar,augmented) <-- Input.get_input input;
          r <-- Output.R.decl ();
          c <-- retN (liftRef Idx.zero);
          b <-- retN (C.mapper C.Dom.normalizerf (C.copy a));
          m <-- retN (C.dim1 a);
          rmar <-- retN rmar;
          n <-- if augmented then retN (C.dim2 a) else ret rmar;
          Det.decl ();
          Output.P.decl ();
          seqM 
            (retWhileM (LogicCode.and_L (Idx.less (liftGet c) m)
                                       (Idx.less (liftGet r) rmar) )
               ( perform
               rr <-- retN (liftGet r);
               cc <-- retN (liftGet c);
               pivot <-- l1 retN (Pivot.findpivot b rr n cc m);
               seqM (retMatchM pivot (fun pv -> 
                        seqM (zerobelow b rr cc m n pv)
                             (Output.R.succ ()) )
                        (Det.zero_sign () ))
                    (Code.updateL c Idx.succ) ))
            (Output.make_result b)
    in dogen
end

module GAC_F = GenericArrayContainer(FloatDomain)
module GVC_F = GenericVectorContainer(FloatDomain)
module GAC_I = GenericArrayContainer(IntegerDomain)
module GVC_I = GenericVectorContainer(IntegerDomain)
module GAC_R = GenericArrayContainer(RationalDomain)

module GenFA1 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)

module GenFA2 = Gen(GAC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenFA3 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenFA4 = Gen(GAC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFV1 = Gen(GVC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenFV2 = Gen(GVC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenFV3 = Gen(GVC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenFV4 = Gen(GVC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFV5 = Gen(GVC_F)
                   (FullPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)

(* But this is an error!
module GenIA1 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix(GAC_I)(IDet))
*)
module GenIA1 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenIA2 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenIA3 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenIA4 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenIV1 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenIV2 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenIV3 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenIV4 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenIV5 = Gen(GVC_I)
                   (FullPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFA11 = Gen(GAC_F)
                   (FullPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenFA12 = Gen(GAC_F)
                   (FullPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenFA13 = Gen(GAC_F)
                   (FullPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenFA14 = Gen(GAC_F)
                   (FullPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)

module GenFA24 = Gen(GAC_F)
                    (RowPivot)
                    (AbstractDet)
                    (DivisionUpdate)
                    (InpJustMatrix)
                    (OutDetRankPivot)
module GenRA1 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenRA2 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenRA3 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenRA4 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFA5 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutJustMatrix)

module GenFA6 = Gen(GAC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutDet)
module GenFA7 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutRank)
module GenFA8 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutDetRank)