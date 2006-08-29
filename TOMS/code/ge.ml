open StateCPSMonad

module GEMake(CODE: Coderep.T) = struct

module D = Domains_sig.S(
  struct type ('a, 'v) rep = ('a,'v) CODE.abstract end)
open D
open CODE

open Kinds

(* Was needed for debugging of the kind problem. Please keep it, just in case.
module Foo(D:DOMAINL with type kind = domain_is_field) = (* debugging *)
struct
  module D = D
end
*)

(* moved from Infra (now Domains) so that the former uses no monads.
  The following code is generic over the containers anyway.
  If container-specific iterators are needed, they can still be
  coded as `exceptions'
*)
module Iters(C:CONTAINER2D) = struct
  let row_iter b c low high body = 
    let newbody j = perform
        bjc <-- retN (C.getL b j c);
        body j bjc
    in  loopM low high newbody
  let col_iter b j low high body = 
    let newbody k = perform
        bjk <-- ret (C.getL b j k);
        body k bjk
    in  loopM low high newbody
end


(* Monad used in this module: 
   (abstract) code generation monad with open union state *)
type ('a,'v,'s,'w) cmonad = (('a,'v) abstract, 's list, ('a,'w) abstract) monad
(* We also use this variant, where we _might_ generate code *)
type ('a,'v,'s,'w) omonad = (('a,'v) abstract option, 
			     's list, ('a,'w) abstract) monad

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
  type ('b,'v) om = ('a,'v,'s,'w) omonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  val decl : unit -> ('b,unit) lm (* could be unit rather than unit code...*)
  val upd_sign  : unit -> ('b,unit) om
  val zero_sign : unit -> ('b,unit) lm
  val acc       : ('a,indet) abstract -> ('a * 's * 'w,unit) lm
  val get       : unit -> ('b,tdet) lm
  val set       : ('a,indet) abstract -> ('a * 's * 'w,unit) lm
  val fin       : unit -> ('b,outdet) lm
end

module type DETF = functor(D:DOMAINL) -> DETERMINANT with type indet = D.v

(* no need to make the type abstract here - just leads to other problems *)
module type RANK = sig
  type 'a lstate = ('a, int ref) abstract
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
  type 'a lstate = ('a, int ref) abstract
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
   assignM r (Idx.succ (liftGet r))
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
module NoDet(Dom:DOMAINL) =
  struct
  type indet = Dom.v
  type outdet = unit
  type tdet = outdet ref
  type 'a lstate = unit
  let decl () = unitL
  let upd_sign () = ret None
  let zero_sign () = unitL
  let acc _ = unitL
  let get () = ret (liftRef cunit)
  let set _ = unitL
  let fin () = unitL
  type 'a tag_lstate = [`TDet of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  type ('b,'v) om = ('a,'v,'s,'w) omonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
end

module AbstractDet(Dom: DOMAINL) =
  struct
  open Dom
  type indet = v
  type outdet = v
  type tdet = outdet ref
  (* the first part of the state is an integer: which is +1, 0, -1:
     the sign of the determinant *)
  type 'a lstate = ('a,int ref) abstract * ('a,tdet) abstract
  type 'a tag_lstate = [`TDet of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  type ('b,'v) om = ('a,'v,'s,'w) omonad
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
      ddecl <-- retN (liftRef oneL);
      dsdecl <-- retN (liftRef Idx.one);
      dstore (dsdecl,ddecl);
      unitL
  let upd_sign () = perform
      det <-- dfetch ();
      det1 <-- ret (fst det);
      ret (Some (assign det1 (Idx.uminus (liftGet det1))))
  let zero_sign () = perform
      det <-- dfetch ();
      det1 <-- ret (fst det);
      assignM det1 Idx.zero
  let acc v = perform
      det <-- dfetch ();
      det2 <-- ret (snd det);
      r <-- ret ((liftGet det2) *^ v);
      assignM det2 r
  let get () = perform
      det <-- dfetch ();
      ret (snd det)
  let set v = perform
      det <-- dfetch ();
      det2 <-- ret (snd det);
      assignM det2 v
  let fin () = perform
      (det_sign,det) <-- dfetch ();
      ifM (Logic.equalL (liftGet det_sign) Idx.zero) (ret zeroL)
      (ifM (Logic.equalL (liftGet det_sign) Idx.one) (ret (liftGet det))
          (ret (uminusL (liftGet det))))
end


(* Not only do we need to "pass down" the kind (in type S), we also need
   to ensure that the outdet type is properly visible when we need it.
   Type T ensure this.  It is essentially DETF but with an explicit
   leak of the outdet type.
*)
module UpdateProxy(C0:CONTAINER2D)(D0:DETF) = struct
    module type T = functor(D1:DOMAINL) -> 
        DETERMINANT with type indet = D1.v and type outdet = D0(D1).outdet
    module type S =
        functor(C:CONTAINER2D with type Dom.kind = C0.Dom.kind)  ->
(*        functor(CD: sig type 'a vc end) -> *)
        functor(D:T) -> sig
        type 'a in_val = 'a C.Dom.vc
        type out_val = D(C.Dom).outdet
        val update : 
            'a in_val -> 'a in_val -> 'a in_val -> 'a in_val -> 
          ('a in_val -> ('a, unit) abstract) ->
          ('a, out_val ref) abstract -> ('a, unit, 's, 'w) cmonad
        val update_det : 'a in_val -> 
          ('a in_val -> ('a, unit, 's, 'w) cmonad) ->
          ('a in_val -> ('a, unit, 's, 'w) cmonad) ->
          ('a, unit, 's, 'w) cmonad
        end
end

(* What is the update formula? *)
module DivisionUpdate
    (C:CONTAINER2D with type Dom.kind = domain_is_field)
(*    (C:sig module Dom : DOMAINL with type kind = float end) *)
    (Det:DETF) =
  struct
  module Dom = C.Dom
  open Dom
  type 'a in_val = 'a vc
  type out_val = Det(C.Dom).outdet
  let update bic brc brk bik setter _ = perform
      y <-- ret (bik -^ ((divL bic brc) *^ brk));
      ret (setter (applyMaybe normalizerL y))
  let update_det v _ acc = acc v
end

module FractionFreeUpdate(Ctr:CONTAINER2D)(Det:DETF) = struct
  module Dom = Ctr.Dom
  open Dom
  type 'a in_val = ('a, v) abstract
  type out_val = Det(Ctr.Dom).outdet
  let update bic brc brk bik setter d = perform
      z <-- ret ((bik *^ brc) -^ (brk *^ bic));
      t <-- ret (applyMaybe normalizerL z);
      ov <-- ret (divL t (liftGet d));
      ret (setter ov)
  let update_det v set _ = set v
end

module type TRACKPIVOT = sig
  type 'a lstate
  type 'a tag_lstate = [`TPivot of 'a lstate ]
    (* Here, parameter 'b accounts for all the extra polymorphims *)
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  val decl : unit -> ('b, unit) lm
  val add : ('a, perm) abstract -> 
    (('a,unit) abstract option,[> 'a tag_lstate] list,('a,'w) abstract) monad
  val fin : unit -> 
    (('a, perm list) abstract ,[> 'a tag_lstate] list,'w) monad
end

module TrackPivot = 
  struct
  type 'a lstate = ('a,  perm list ref) abstract
  type 'a tag_lstate = [`TPivot of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  let rec fetch_iter s =
    match (List.hd s) with
      `TPivot x -> x
    |  _ -> fetch_iter (List.tl s)
  let pfetch () = perform s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s)
  let pstore v = store (`TPivot v)
  let decl () = perform
      pdecl <-- retN (liftRef (CList.nil : ('a,  perm list) abstract));
      pstore pdecl;
      unitL
  let add v = perform
   p <-- pfetch ();
   ret (Some (assign p (CList.cons v (liftGet p))))
end

module KeepPivot = struct
  include TrackPivot
  let fin () = perform
      p <-- pfetch ();
      ret (liftGet p)
end

module DiscardPivot = struct
  type 'a lstate = ('a,  perm list ref) abstract
  type 'a tag_lstate = [`TPivot of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  let decl () = unitL
  let add _ = ret None
  let fin () = ret CList.nil
end

module type INPUT =
      functor (C: CONTAINER2D) -> sig
    type inp
    val get_input : ('a, inp) abstract ->
        (('a, C.contr) abstract * ('a, int) abstract * bool, 's, ('a, 'w)
        abstract) monad
end 

(* What is the input *)
module InpJustMatrix(C: CONTAINER2D) = struct
    type inp   = C.contr
    let get_input a = ret (a, C.dim2 a, false)
end

module InpMatrixMargin(C: CONTAINER2D) = struct
    type inp   = C.contr * int
    let get_input a = perform
        (b,c) <-- ret (liftPair a);
        ret (b, c, true)
end

module OutProxy(C0: CONTAINER2D)(Det0:DETF) = struct
    module D = Det0(C0.Dom)
    module type S = 
        functor(C: CONTAINER2D) -> 
        functor(Det: DETERMINANT with type outdet = D.outdet) -> sig
      type res
      module D : DETERMINANT
      module R : RANK
      module P : TRACKPIVOT
      val make_result : ('a,C.contr) abstract -> 
        ('a,res,[> 'a Det.tag_lstate | 'a R.tag_lstate | 'a P.tag_lstate],'w) cmonad
    end
end

(* What to return *)
module OutJustMatrix(C: CONTAINER2D)(Det : DETERMINANT) =
  struct
  type res = C.contr
  module D = Det
  module R = NoRank
  module P = DiscardPivot
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
    ret (Tuple.tup2 b det)
end

module OutRank(C: CONTAINER2D)(Det: DETERMINANT) = 
  struct
  type res = C.contr * int
  module D = Det
  module R = Rank
  module P = DiscardPivot
  let make_result b = perform
    rank <-- R.fin ();
    ret (Tuple.tup2 b rank)
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
    ret (Tuple.tup3 b det rank)
end

module OutDetRankPivot(C: CONTAINER2D)
    (Det : DETERMINANT) =
  struct
  type res = C.contr * Det.outdet * int *  perm list
  module D = Det
  module R = Rank
  module P = KeepPivot
  let make_result b = perform
    det  <-- D.fin ();
    rank <-- R.fin ();
    pivmat <-- P.fin ();
    ret (Tuple.tup4 b det rank pivmat)
end


module type PIVOT = 
    functor (C: CONTAINER2D) ->
      functor (D: DETF) -> 
        functor (P: TRACKPIVOT) -> sig
 (* Find the pivot within [r,m-1] rows and [c,(n-1)] columns
    of containrer b.
    If pivot is found, permute the matrix rows and columns so that the pivot
    becomes the element (r,c) of the matrix,
    Return the value of the pivot option. Or zero?
    When we permute the rows of columns, we update the sign of the det.
 *)
 val findpivot : 'a C.vc -> ('a,int) abstract -> ('a,int) abstract -> 
   ('a,int) abstract -> ('a,int) abstract -> 
   ('a,C.Dom.v option,[> 'a D(C.Dom).tag_lstate | 'a P.tag_lstate],'w) cmonad
end

module RowPivot(Ctr: CONTAINER2D)(Det: DETF)(P: TRACKPIVOT) =
struct
   module D = Det(Ctr.Dom)
   module I = Iters(Ctr)
   let findpivot b r n c _ = perform
       pivot <-- retN (liftRef Maybe.none );
       (* If no better_than procedure defined, we just search for
      non-zero element. Any non-zero element is a good pivot.
      If better_than is defined, we search then for the best element *)
       seqM
        (match (Ctr.Dom.better_thanL) with
         Some sel -> 
              I.row_iter b c r (Idx.pred n) (fun j bjc ->
              whenM (Logic.notequalL bjc Ctr.Dom.zeroL )
                  (matchM (liftGet pivot)
                    (fun pv ->
                      perform
                      (_,bic) <-- ret (liftPair pv);
                      whenM (sel bic bjc)
                        (assignM pivot (Maybe.just 
                                     (Tuple.tup2 j bjc))))
                     (assignM pivot (Maybe.just 
                                  (Tuple.tup2 j bjc))))
              )
         | None ->
           perform
            brc <-- retN (Ctr.row_head b r c);
            ifM (Logic.notequalL brc Ctr.Dom.zeroL)
              (* the current element is good enough *)
              (assignM pivot (Maybe.just (Tuple.tup2 r brc)))
              (let traverse = fun o j ->
                  whenM (Idx.less j n)
                    (perform
                        bjc <-- retN (Ctr.getL b j c);
                        ifM (Logic.equalL bjc Ctr.Dom.zeroL)
                            (applyM o (Idx.succ j))
                            (assignM pivot (Maybe.just 
                                (Tuple.tup2 j bjc)))) in
              (genrecloop traverse (Idx.succ r)))
         )
         (matchM (liftGet pivot)
                (fun pv -> perform
                     (i,bic) <-- ret (liftPair pv);
                     seqM (whenM (Logic.notequalL i r) (perform
                            s1 <-- ret (Ctr.swap_rows_stmt b r i);
                            s2 <-- D.upd_sign ();
                            s3 <-- ret (optSeq s1 s2);
                            s4 <-- P.add (liftRowSwap i r );
                            ret (optSeq s3 s4)
                           ))
                          (ret (Maybe.just bic)))
                (ret Maybe.none))
end

module FullPivot(C: CONTAINER2D)(Det: DETF)(P: TRACKPIVOT) = 
struct
   module D = Det(C.Dom)
   let findpivot b r n c m = perform
       pivot <-- retN (liftRef Maybe.none );
       (* this is not really a row/column iteration, this is a
          a full-matrix iteration, and should be coded as such *)
       seqM (loopM r (Idx.pred n) (fun j -> 
              loopM c (Idx.pred m) (fun k ->
           perform
              bjk <-- retN (C.getL b j k);
              whenM (Logic.notequalL bjk C.Dom.zeroL)
              (match (C.Dom.better_thanL) with
              | Some sel ->
                  (matchM (liftGet pivot)
                    (fun pv ->
                      perform
                      (pr,_,brc) <-- ret (liftPPair pv);
                      whenM (sel brc bjk)
                        (assignM pivot (Maybe.just
                            (Tuple.tup2 (Tuple.tup2 j k) bjk))))
                     (assignM pivot (Maybe.just
                            (Tuple.tup2 (Tuple.tup2 j k) bjk))))
              | None ->
                  (assignM pivot (Maybe.just (
                      Tuple.tup2 (Tuple.tup2 j k) bjk)))
              ))))
              (* finished the loop *)
              (matchM (liftGet pivot)
                  (fun pv -> perform
                     (pr,pc,brc) <-- ret (liftPPair pv);
                     seqM
                         (whenM (Logic.notequalL pc c) (perform
                           s1 <-- ret (C.swap_cols_stmt b c pc);
                           s2 <-- D.upd_sign ();
                           ret (optSeq s1 s2)))
                       (seqM
                         (whenM (Logic.notequalL pr c) (perform
                           s1 <-- ret (C.swap_rows_stmt b r pc);
                           s2 <-- D.upd_sign ();
                           ret (optSeq s1 s2)))
                         (ret (Maybe.just brc))))
                  (ret Maybe.none))
end

module NoPivot(C: CONTAINER2D)(Det: DETF)(P: TRACKPIVOT) = 
struct
   module D = Det(C.Dom)
   (* In this case, we assume diagonal dominance, and so
      just take the diagonal as ``pivot'' *)
   let findpivot b r _ c _ = perform 
       ret (Maybe.just (C.row_head b r c));
end

module Gen(C: CONTAINER2D)
          (PivotF: PIVOT)
          (Detf:DETF)
          (Update: UpdateProxy(C)(Detf).S)
          (In: INPUT)
          (Out: OutProxy(C)(Detf).S) = 
   struct
    module Det = Detf(C.Dom)
    module U = Update(C)(Detf)
    module Input = In(C)
    module Output = Out(C)(Det)
    module Pivot = PivotF(C)(Detf)(Output.P)
    module I = Iters(C)
    let gen =
      let zerobelow b r c m n brc =
        let innerbody j bjc = perform
            whenM (Logic.notequalL bjc C.Dom.zeroL )
                (seqM (I.col_iter b j (Idx.succ c) (Idx.pred m)
                          (fun k bjk -> perform
                              d <-- Det.get ();
                              brk <-- ret (C.getL b r k);
                              U.update bjc brc brk bjk 
                                  (fun ov -> C.col_head_set b j k ov) d) )
                      (ret (C.col_head_set b j c C.Dom.zeroL))) in 
        perform
              seqM (I.row_iter b c (Idx.succ r) (Idx.pred n) innerbody) 
                   (U.update_det brc Det.set Det.acc)in
      let dogen input = perform
          (a,rmar,augmented) <-- Input.get_input input;
          r <-- Output.R.decl ();
          c <-- retN (liftRef Idx.zero);
          b <-- retN (C.mapper C.Dom.normalizerL (C.copy a));
          m <-- retN (C.dim1 a);
          rmar <-- retN rmar;
          n <-- if augmented then retN (C.dim2 a) else ret rmar;
          Det.decl ();
          Output.P.decl ();
          seqM 
            (whileM (Logic.andL (Idx.less (liftGet c) m)
                                       (Idx.less (liftGet r) rmar) )
               ( perform
               rr <-- retN (liftGet r);
               cc <-- retN (liftGet c);
               pivot <-- l1 retN (Pivot.findpivot b rr n cc m);
               seqM (matchM pivot (fun pv -> 
                        seqM (zerobelow b rr cc m n pv)
                             (Output.R.succ ()) )
                        (Det.zero_sign () ))
                    (updateM c Idx.succ) ))
            (Output.make_result b)
    in dogen
end

end
