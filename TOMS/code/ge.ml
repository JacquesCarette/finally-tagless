open StateCPSMonad

exception CannotHappen

type algo_requirements = {
    wants_pack  : bool;
    back_elim   : bool;
    can_pack    : bool
}

type update_kind = FractionFree | DivisionBased

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

(* Monad used in this module: 
   (abstract) code generation monad with open union state *)
type ('a,'v,'s,'w) cmonad = (('a,'v) abstract, 's list, ('a,'w) abstract) monad
(* We also use this variant, where we _might_ generate code *)
type ('a,'v,'s,'w) omonad = (('a,'v) abstract option, 
                 's list, ('a,'w) abstract) monad

(* Here are the various design aspects of GE-type algorithms *)
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
(* Even if no rank is output, it needs to be tracked, as the rank
   is also the outer loop index! *)
(* Note how this module contains its own signature *)
module TrackRank = 
  struct
  type 'a lstate = ('a, int ref) abstract
	(* some magic for the proper visibility of identifiers *)
  type 'a tag_lstate_ = [`TRan of 'a lstate ]
  type 'a tag_lstate = 'a tag_lstate_
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

      (* The signature of the above *)
  module type RANK = sig
    type 'a tag_lstate = 'a tag_lstate_
    val rfetch : unit -> ('b, int ref) lm
    val decl   : unit -> ('b, int ref) lm
    val succ   : unit -> ('b, unit) lm
    val fin    : unit -> ('b, int) lm
  end
end

module Rank = struct
  include TrackRank
  let fin () = perform
      r <-- rfetch ();
      ret (liftGet r)
end

module NoRank = struct
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

module type PIVOTKIND = sig
  type idx_rep = int
  type flip_rep
  type perm_rep
  type 'a ira = ('a, idx_rep) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  val add : 'a fra -> 'a pra -> 'a pra
  val empty : 'a ira -> 'a pra
  val rowrep : 'a ira -> 'a ira -> 'a fra
  val colrep : 'a ira -> 'a ira -> 'a fra
end

module PermList = struct
  type idx_rep = int
  type flip_rep = perm
  type perm_rep = perm list
  type 'a ira = ('a, idx_rep) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  let add x l = CList.cons x l
  let empty _ = (CList.nil : 'a pra)
  let rowrep x y = liftRowSwap x y
  let colrep x y = liftColSwap x y
end

module RowVectorPerm = struct
  type idx_rep = int
  type flip_rep = int*int
  type perm_rep = int array
  type 'a ira = ('a, idx_rep) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  let add x l = Array1Dim.setL l x
  let empty n = Array1Dim.init n
  let rowrep x y = Tuple.tup2 x y
  let colrep x y = Tuple.tup2 x y
end

module type TRACKPIVOT = sig
  type idx_rep = int
  type flip_rep
  type perm_rep
  type 'a ira = ('a, idx_rep) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  type 'a lstate = ('a, perm_rep ref) abstract
  type 'a tag_lstate = [`TPivot of 'a lstate ]
    (* Here, parameter 'b accounts for all the extra polymorphims *)
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  val rowrep : 'a ira -> 'a ira -> 'a fra
  val colrep : 'a ira -> 'a ira -> 'a fra
  val decl : ('a, int) abstract -> ('a*'s*'w, unit) lm
  val add : 'a fra ->
    (('a,unit) abstract option,[> 'a tag_lstate] list,('a,'w) abstract) monad
  val fin : unit -> 
    ('a pra option,[> 'a tag_lstate] list,('a,'w) abstract) monad
end

module PivotCommon(PK:PIVOTKIND) = 
  struct
  type idx_rep = PK.idx_rep
  type flip_rep = PK.flip_rep
  type perm_rep = PK.perm_rep
  type 'a ira = ('a, idx_rep) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  type 'a lstate = ('a, PK.perm_rep ref) abstract
  type 'a tag_lstate = [`TPivot of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  let rowrep = PK.rowrep
  let colrep = PK.colrep
end

module KeepPivot(PK:PIVOTKIND) = struct
  include PivotCommon(PK)
  let rec fetch_iter s =
    match (List.hd s) with
      `TPivot x -> x
    |  _ -> fetch_iter (List.tl s)
  let pfetch () = perform s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s)
  let pstore v = store (`TPivot v)
  let decl n = perform
      pdecl <-- retN (liftRef (PK.empty n));
      pstore pdecl;
      unitL
  let add v = perform
   p <-- pfetch ();
   ret (Some (assign p (PK.add v (liftGet p))))
  let fin () = perform
      p <-- pfetch ();
      ret (Some (liftGet p))
end

module DiscardPivot(PK:PIVOTKIND) = struct
  include PivotCommon(PK)
  let decl _ = unitL
  let add _ = ret None
  let fin () = ret None
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
(* this is only needed if we try to deal with FractionFree LU,
   which is really tough, especially since the L Matrix still has
   to be over the fraction field, which we don't have available.
        val update_lower : 
            'a in_val -> 'a in_val -> 'a in_val -> 'a in_val -> 
          ('a, out_val ref) abstract -> ('a, C.Dom.v, 's, 'w) cmonad *)
        val upd_kind : update_kind
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
(*let update_lower bic brc _ _ _ = perform
      y <-- ret (divL bic brc);
      ret (applyMaybe normalizerL y) *)
  let upd_kind = DivisionBased
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
(*let update_lower bic brc lrk lik d = perform
      rat <-- ret (liftGet d);
      z <-- ret (divL ((rat *^ lik) +^ (bic *^ lrk)) brc);
      t <-- ret (applyMaybe normalizerL z);
      ret t *)
  let upd_kind = FractionFree
end

(* moved from Infra (now Domains) so that the former uses no monads.
  The following code is generic over the containers anyway.
  If container-specific iterators are needed, they can still be
  coded as `exceptions'
*)
module Iters = struct
  let row_iter b c low high get body = 
    let newbody j = perform
        bjc <-- retN (get b j c);
        body j bjc
    in  loopM low high newbody
  let col_iter b r low high get body = 
    let newbody k = perform
        brk <-- ret (get b r k);
        body k brk
    in  loopM low high newbody
end

(* Given a container, we can generate a whole "Linear Algebra"
   set of modules and generators all based on that container.
   This layout makes sure the same container is shared (and thus
   the same domain at the same time).

   The main reason that some of the above modules are not contained
   in this one are twofold:
   1) some of the are container-independent, so why put them in here?
   2) some have explicit kind restrictions, which have to be kept!
*)
module GenLA(C:CONTAINER2D) = struct

(* Bundle up some information, helps abstract some argument lists *)
type 'a wmatrix = {matrix: 'a C.vc; numrow: ('a,int) abstract; 
                   numcol: ('a,int) abstract}
type 'a curpos  = {rowpos: ('a, int) abstract; colpos: ('a, int) abstract}
type 'a curposval = {p: 'a curpos; curval: ('a, C.Dom.v) abstract}

(* This is for tracking L, as in LU decomposition.
   Naturally, when doing only GE, this is not needed at all, and should
   not generate any code *)
module type LOWER = sig
  type 'a lstate = ('a, C.contr) abstract
  type 'a tag_lstate = [`TLower of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  type ('b,'v) om = ('a,'v,'s,'w) omonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  val mfetch : unit -> ('b, C.contr) lm
  val decl   : ('a, C.contr) abstract -> ('a*'s*'w, C.contr) lm
  val updt   : 'a C.vc -> ('a,int) abstract -> ('a,int) abstract -> 'a C.vo -> 
            'a C.Dom.vc -> ('a*'s*'w, unit) lm option
  val fin    : unit -> ('a,  C.contr) om
  val wants_pack : bool
end

(* Do we keep the lower part? *)
module TrackLower = 
  struct
  type 'a lstate = ('a, C.contr) abstract
  type 'a tag_lstate = [`TLower of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  type ('b,'v) om = ('a,'v,'s,'w) omonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  (* In 3.09, no need for any coercion at all, type inference works! *)
  let rec fetch_iter s =
    match (List.hd s) with
      `TLower x -> x
    |  _ -> fetch_iter (List.tl s)
  let mfetch () = perform s <-- fetch; (* unit for monomorphism restriction *)
                          ret (fetch_iter s)
  let mstore v = store (`TLower v)
end

(* Even this form cannot be done with FractionFree, because in
   general the L matrix will be over the fraction field, even
   if the U is not.  So we won't even try to deal with it
   for now *)
module SeparateLower = struct
  include TrackLower
  let decl c = perform
      udecl <-- retN c;
      mstore udecl;
      ret udecl
  (* also need to 'set' lower! *)
  let updt mat row col defval nv = Some( perform
      lower <-- mfetch ();
      l1 <-- ret (C.col_head_set lower row col nv);
      l2 <-- ret (C.col_head_set mat row col defval);
      ret (seq l1 l2) )
  let fin () = perform
      m <-- mfetch ();
      ret (Some m)
  let wants_pack = false
end

(* Packed form cannot be done with FractionFree, so things
   are considerably simpler *)
module PackedLower = struct
  include TrackLower
  let decl c = perform
      udecl <-- ret c;
      mstore udecl;
      ret udecl
  let updt  _ _ _ _ _ = None
  let fin () = perform
      m <-- mfetch ();
      ret (Some m)
  let wants_pack = true
end

module NoLower = struct
  include TrackLower
  let decl c = ret c
  let updt mat row col defval _ = 
      Some (ret (C.col_head_set mat row col defval))
  let fin () = ret None
  let wants_pack = false
end

module type INPUT = sig
    type inp
    val get_input : ('a, inp) abstract ->
        (('a, C.contr) abstract * ('a, int) abstract * bool, 's, ('a, 'w)
        abstract) monad
end 

(* What is the input *)
module InpJustMatrix = struct
    type inp   = C.contr
    let get_input a = ret (a, C.dim2 a, false)
end

module InpMatrixMargin = struct
    type inp   = C.contr * int
    let get_input a = perform
        (b,c) <-- ret (liftPair a);
        ret (b, c, true)
end

(* The 'with type' below are not strictly needed, they just make the
   printing of the types much nicer *)
module OutProxy(C0:CONTAINER2D)(Det0:DETF) = struct
    module DD = Det0(C0.Dom)
    module type S = 
      functor(C1:CONTAINER2D with type Dom.kind = C0.Dom.kind) ->
        functor(Det: DETERMINANT with type outdet = DD.outdet and
           type 'a lstate = 'a DD.lstate) -> 
          functor(PK: PIVOTKIND) -> sig
      type res
      (* module D : DETERMINANT *)
      module R : TrackRank.RANK
      module P : TRACKPIVOT with type flip_rep = PK.flip_rep and 
                                 type perm_rep = PK.perm_rep
      module L : LOWER
      val make_result : 'a wmatrix ->
        ('a,res,[> 'a Det.tag_lstate | 'a R.tag_lstate 
                 | 'a P.tag_lstate   | 'a L.tag_lstate],'w) cmonad
    end
end

(* What to return *)
module OutJustMatrix(C0:CONTAINER2D)(Det : DETERMINANT)(PK : PIVOTKIND) =
  struct
  type res = C.contr
  (* module D = Det *)
  module R = NoRank
  module P = DiscardPivot(PK)
  module L = NoLower
  let make_result m = ret m.matrix
end

module OutDet(C0:CONTAINER2D)(Det : DETERMINANT)(PK : PIVOTKIND) =
  struct
  type res = C.contr * Det.outdet
  (* module D = Det *)
  module R = NoRank
  module P = DiscardPivot(PK)
  module L = NoLower
  let make_result m = perform
    det <-- Det.fin ();
    ret (Tuple.tup2 m.matrix det)
end

module OutRank(C0:CONTAINER2D)(Det: DETERMINANT)(PK : PIVOTKIND) =
  struct
  type res = C.contr * int
  (* module D = Det *)
  module R = Rank
  module P = DiscardPivot(PK)
  module L = NoLower
  let make_result m = perform
    rank <-- R.fin ();
    ret (Tuple.tup2 m.matrix rank)
end

module OutDetRank(C0:CONTAINER2D)(Det : DETERMINANT)(PK : PIVOTKIND) =
  struct
  type res = C.contr * Det.outdet * int
  (* module D = Det *)
  module R = Rank
  module P = DiscardPivot(PK)
  module L = NoLower
  let make_result m = perform
    det  <-- Det.fin ();
    rank <-- R.fin ();
    ret (Tuple.tup3 m.matrix det rank)
end

module OutDetRankPivot(C0:CONTAINER2D)(Det : DETERMINANT)(PK : PIVOTKIND) =
  struct
  type res = C.contr * Det.outdet * int *  PK.perm_rep
  (* module D = Det *)
  module R = Rank
  module P = KeepPivot(PK)
  module L = NoLower
  let make_result m = perform
    det  <-- Det.fin ();
    rank <-- R.fin ();
    pivmat <-- P.fin ();
    match pivmat with
    | Some p -> ret (Tuple.tup4 m.matrix det rank p)
    | None   -> raise CannotHappen
end

(* The restriction is because we can't extract the L in a non-field *)
module Out_L_U(C0:CONTAINER2D with type Dom.kind = domain_is_field)(Det : DETERMINANT)(PK: PIVOTKIND) =
  struct
  type res = C.contr * C.contr * PK.perm_rep
  (* module D = Det *)
  module R = Rank
  module P = KeepPivot(PK)
  module L = SeparateLower
  let make_result m = perform
    pivmat <-- P.fin ();
    lower <-- L.fin ();
    match (pivmat,lower) with
    | (Some p, Some l) -> ret (Tuple.tup3 m.matrix l p)
    | _                -> raise CannotHappen
end

(* The restriction is because we can't extract the L in a non-field *)
module Out_LU_Packed(C0:CONTAINER2D with type Dom.kind = domain_is_field)(Det : DETERMINANT)(PK: PIVOTKIND) =
  struct
  type res = C.contr * PK.perm_rep
  (* module D = Det *)
  module R = Rank
  module P = KeepPivot(PK)
  module L = PackedLower
  let make_result _ = perform
    pivmat <-- P.fin ();
    lower <-- L.fin ();
    (* we really should be able to assert that lower == m.matrix
    here, but can't because the representation of lower/m.matrix
    could be 'functional' !*)
    match (pivmat,lower) with
    | (Some p, Some l) -> ret (Tuple.tup2 l p)
    | _                -> raise CannotHappen
end


module type PIVOT = 
      functor (D: DETF) -> 
        functor (P: TRACKPIVOT) -> sig
 (* Find the pivot within [r,m-1] rows and [c,(n-1)] columns
    of containrer b.
    If pivot is found, permute the matrix rows and columns so that the pivot
    becomes the element (r,c) of the matrix,
    Return the value of the pivot option. Or zero?
    When we permute the rows of columns, we update the sign of the det.
 *)
 val findpivot : 'a wmatrix -> 'a curpos ->
   ('a,C.Dom.v option,[> 'a D(C.Dom).tag_lstate | 'a P.tag_lstate],'w) cmonad
end

module RowPivot(Det: DETF)(P: TRACKPIVOT) =
struct
   module D = Det(C.Dom)
   module I = Iters
   let findpivot mat pos = perform
       pivot <-- retN (liftRef Maybe.none );
       (* If no better_than procedure defined, we just search for
      non-zero element. Any non-zero element is a good pivot.
      If better_than is defined, we search then for the best element *)
       seqM
        (match (C.Dom.better_thanL) with
         Some sel -> 
              I.row_iter mat.matrix pos.colpos pos.rowpos (Idx.pred mat.numrow)
              C.getL (fun j bjc ->
              whenM (Logic.notequalL bjc C.Dom.zeroL )
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
            brc <-- retN (C.row_head mat.matrix pos.rowpos pos.colpos);
            ifM (Logic.notequalL brc C.Dom.zeroL)
              (* the current element is good enough *)
              (assignM pivot (Maybe.just (Tuple.tup2 pos.rowpos brc)))
              (let traverse = fun o j ->
                  whenM (Idx.less j mat.numrow)
                    (perform
                        bjc <-- retN (C.getL mat.matrix j pos.colpos);
                        ifM (Logic.equalL bjc C.Dom.zeroL)
                            (applyM o (Idx.succ j))
                            (assignM pivot (Maybe.just 
                                (Tuple.tup2 j bjc)))) in
              (genrecloop traverse (Idx.succ pos.rowpos)))
         )
         (matchM (liftGet pivot)
                (fun pv -> perform
                     (i,bic) <-- ret (liftPair pv);
                     seqM (whenM (Logic.notequalL i pos.rowpos) (perform
                            s1 <-- ret (C.swap_rows_stmt mat.matrix pos.rowpos i);
                            s2 <-- D.upd_sign ();
                            s3 <-- ret (optSeq s1 s2);
                            s4 <-- P.add (P.rowrep i pos.rowpos );
                            ret (optSeq s3 s4)
                           ))
                          (ret (Maybe.just bic)))
                (ret Maybe.none))
end

module FullPivot(Det: DETF)(P: TRACKPIVOT) = 
struct
   module D = Det(C.Dom)
   let findpivot mat pos = perform
       pivot <-- retN (liftRef Maybe.none );
       (* this is not really a row/column iteration, this is a
          a full-matrix iteration, and should be coded as such *)
       seqM (loopM pos.rowpos (Idx.pred mat.numrow) (fun j -> 
              loopM pos.colpos (Idx.pred mat.numcol) (fun k ->
           perform
              bjk <-- retN (C.getL mat.matrix j k);
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
                         (whenM (Logic.notequalL pc pos.colpos) (perform
                           s1 <-- ret (C.swap_cols_stmt mat.matrix pos.colpos pc);
                           s2 <-- D.upd_sign ();
                           s3 <-- ret (optSeq s1 s2);
                           s4 <-- P.add (P.colrep pc pos.rowpos );
                           ret (optSeq s3 s4)))
                       (seqM
                         (whenM (Logic.notequalL pr pos.rowpos) (perform
                           s1 <-- ret (C.swap_rows_stmt mat.matrix pos.rowpos pc);
                           s2 <-- D.upd_sign ();
                           s3 <-- ret (optSeq s1 s2);
                           s4 <-- P.add (P.rowrep pr pos.rowpos );
                           ret (optSeq s3 s4)))
                         (ret (Maybe.just brc))))
                  (ret Maybe.none))
end

module NoPivot(Det: DETF)(P: TRACKPIVOT) = 
struct
   module D = Det(C.Dom)
   (* In this case, we assume diagonal dominance, and so
      just take the diagonal as ``pivot'' *)
   let findpivot mat pos = perform 
       ret (Maybe.just (C.row_head mat.matrix pos.rowpos pos.colpos));
end

module GenGE(PivotF: PIVOT)
          (PK:PIVOTKIND)
          (Detf:DETF)
          (Update: UpdateProxy(C)(Detf).S)
          (In: INPUT)
          (Out: OutProxy(C)(Detf).S) = struct
    module Det = Detf(C.Dom)
    module U = Update(C)(Detf)
    module Input = In
    module Output = Out(C)(Det)(PK)
    module Pivot = PivotF(Detf)(Output.P)
    module I = Iters

    let gen =
      let opt () = {wants_pack = Output.L.wants_pack;
                    back_elim  = false;
                    can_pack   = (U.upd_kind = DivisionBased) } in
      let zerobelow mat pos = 
        let innerbody j bjc = perform
            whenM (Logic.notequalL bjc C.Dom.zeroL ) (perform
                det <-- Det.get ();
                optSeqM (I.col_iter mat.matrix j (Idx.succ pos.p.colpos) (Idx.pred mat.numcol) C.getL
                      (fun k bjk -> perform
                      brk <-- ret (C.getL mat.matrix pos.p.rowpos k);
                      U.update bjc pos.curval brk bjk 
                          (fun ov -> C.col_head_set mat.matrix j k ov) det) )
                      (Output.L.updt mat.matrix j pos.p.colpos C.Dom.zeroL 
                          (* this makes no sense outside a field! *)
                          (C.Dom.divL bjc pos.curval))) in
        perform
              seqM (I.row_iter mat.matrix pos.p.colpos (Idx.succ pos.p.rowpos)
              (Idx.pred mat.numrow) C.getL innerbody) 
                   (U.update_det pos.curval Det.set Det.acc) in
      let init input = 
          let opt = opt () in 
          let _ = assert ((not opt.wants_pack) || opt.can_pack) in
          perform
          (a,rmar,augmented) <-- Input.get_input input;
          r <-- Output.R.decl ();
          c <-- retN (liftRef Idx.zero);
          b <-- retN (C.mapper C.Dom.normalizerL (C.copy a));
          m <-- retN (C.dim1 a);
          rmar <-- retN rmar;
          n <-- if augmented then retN (C.dim2 a) else ret rmar;
          Det.decl ();
          Output.P.decl rmar;
          _ <-- Output.L.decl (if opt.wants_pack then b else C.identity rmar m);
          let mat = {matrix=b; numrow=n; numcol=m} in
          ret (mat, r, c, rmar)  in
      let forward_elim (mat, r, c, rmar) = perform
          whileM (Logic.andL (Idx.less (liftGet c) mat.numcol)
                              (Idx.less (liftGet r) rmar) )
             ( perform
             rr <-- retN (liftGet r);
             cc <-- retN (liftGet c);
             let cp  = {rowpos=rr; colpos=cc} in
             pivot <-- l1 retN (Pivot.findpivot mat cp);
             seqM (matchM pivot (fun pv -> 
                      seqM (zerobelow mat {p=cp; curval=pv} )
                           (Output.R.succ ()) )
                      (Det.zero_sign () ))
                  (updateM c Idx.succ) ) in
      let backward_elim () =
          let opt = opt () in
          if opt.back_elim then
              Some unitL
          else
              None in
      let ge_gen input = perform
          (mat, r, c, rmar) <-- init input;
          seqM 
            (optSeqM
                (forward_elim (mat, r, c, rmar))
                (backward_elim ()))
            (Output.make_result mat)
    in ge_gen
end

end

end
