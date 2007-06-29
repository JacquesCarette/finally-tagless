open StateCPSMonad
open Prelude

type update_kind = FractionFree | DivisionBased

(* It used to be that we used an option type to encode some domain
   information, like the need to compute a Determinant, Rank or Pivot.
   Now this is done directly with an exception instead, so these routines
   are no longer needed.
   For more detail, see the message
http://caml.inria.fr/pub/ml-archives/caml-list/2006/11/df527bcc780e6f3106889e2d5e8b5e2a.en.html
  Posted on the caml-list on Nov 17, 2006.
let fromJust = function Some x -> x | None -> failwith "Can't happen"
let notNone v str = match v with None -> failwith str | Some _ -> () *)
let ensure cond str = if not cond then failwith str else ()


(* A few utility functions to handle `open records', lists of variants *)
(* Each `field' is characterized by a triple: injection, projection functions
   and the name. The latter is used for printing error messages.
*)
let rec lookup ((_,prj,_) as ip) = 
   function [] -> raise Not_found
   | (h::t) -> (match prj h with Some x -> x | _ -> lookup ip t)

let orec_store ((inj,_,name) as ip) v s =
  let () = 
    try let _ = lookup ip s in 
        failwith ("The field of an open record is already present: " ^ name)
    with Not_found -> () in
  (inj v)::s

let orec_find ((_,_,name) as ip) s =
  try lookup ip s 
  with Not_found -> failwith ("Failed to locate orec field: " ^ name)

let mo_extend ip v = 
  perform s <-- fetch; store (orec_store ip v s)

let mo_lookup ip =
  perform s <-- fetch; ret (orec_find ip s)


(* A lot of "Linear Algebra" is generic, so structure things 
   to leverage that.  Some modules are container-independent,
   so these are pulled out.
*)

module LAMake(CODE: Coderep.T) = struct

module D = Domains_sig.S(
  struct type ('a, 'v) rep = ('a,'v) CODE.abstract end)
open D
open CODE

(* Was needed for debugging of the kind problem. Please keep it, just in case.
module Foo(D:DOMAINL with type kind = domain_is_field) = (* debugging *)
struct
  module D = D
end
*)

(* Monad used in this module: 
   (abstract) code generation monad with open union state *)
type ('pc,'v) cmonad = 
    ('p,('a,'v) abstract) monad
      constraint
	  'p = <state : 's list;
                answer : ('a,'w) abstract>
      constraint
          'pc = <classif : 'a; answer : 'w; state : 's; ..>
       
(* We also use this variant, where we _might_ generate code *)
type ('pc,'v) omonad = 
    ('p,('a,'v) abstract option) monad
      constraint
	  'p = <state : 's list;
                answer : ('a,'w) abstract>
      constraint
          'pc = <classif : 'a; answer : 'w; state : 's; ..>


(* moved from Infra (now Domains) so that the former uses no monads.
  The following code is generic over the containers anyway.
  If container-specific iterators are needed, they can still be
  coded as `exceptions'
*)
module Iters = struct
  let row_iter b c low high get body d = 
    let newbody j = perform
        bjc <-- retN (get b j c);
        body j bjc
    in  loopM low high newbody d
  let col_iter b r low high get body d = 
    let newbody k = perform
        brk <-- ret (get b r k);
        body k brk
    in  loopM low high newbody d
end


(* Here are the various design aspects of GE-type algorithms *)

(* Rank is container-independent *)

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
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> 'a tag_lstate]; classif : 'a; ..>

  let ip = (fun x -> `TRan x), (function `TRan x -> Some x | _ -> None),
           "TrackRank"
  let decl () = perform
      rdecl <-- retN (liftRef Idx.zero);
      mo_extend ip rdecl;
      ret rdecl
  let succ () = perform
   r <-- mo_lookup ip;
   assignM r (Idx.succ (liftGet r))

      (* The signature of the above *)
  module type RANK = sig
    type 'a tag_lstate = 'a tag_lstate_
    val decl   : unit -> ('b, int ref) lm
    val succ   : unit -> ('b, unit) lm
    val fin    : unit -> ('b, int) lm
  end
end

module Rank = struct
  include TrackRank
  let fin () = perform
      r <-- mo_lookup ip;
      ret (liftGet r)
end

module NoRank = struct
  include TrackRank
  let fin () = failwith "Rank is needed but is not computed"
end

module type PIVOTKIND = sig
  type perm_rep
  type 'a ira = ('a, int) abstract
  type 'a fra
  type 'a pra = ('a, perm_rep) abstract
  val add : 'a fra -> 'a pra -> 'a pra
  val empty : 'a ira -> 'a pra
  val rowrep : 'a ira -> 'a ira -> 'a fra
  val colrep : 'a ira -> 'a ira -> 'a fra
end

module PermList = struct
  type flip_rep = perm
  type perm_rep = perm list
  type 'a ira = ('a, int) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  let add x l = CList.cons x l
  let empty _ = (CList.nil : 'a pra)
  let rowrep x y = liftRowSwap x y
  let colrep x y = liftColSwap x y
end

module RowVectorPerm = struct
  type flip_rep = int*int
  type perm_rep = int array
  type 'a ira = ('a, int) abstract
  type 'a fra = ('a, flip_rep) abstract
  type 'a pra = ('a, perm_rep) abstract
  let add x l = Array1Dim.setL l x
  let empty n = Array1Dim.init n
  let rowrep x y = Tuple.tup2 x y
  let colrep x y = Tuple.tup2 x y
end

module type TRACKPIVOT = sig
  type perm_rep
  type 'a ira = ('a, int) abstract
  type 'a fra
  type 'a pra
  type 'a lstate
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TPivot of 'a lstate ]; classif : 'a; ..>
  val rowrep : 'a ira -> 'a ira -> 'a fra
  val colrep : 'a ira -> 'a ira -> 'a fra
  val decl : ('a, int) abstract -> (<classif : 'a; ..>, unit) lm
  val add : 'a fra -> 
    (<classif : 'a; state : [> `TPivot of 'a lstate ]; ..>, unit) omonad
    (*
    (('a,unit) abstract option,[> 'a tag_lstate] list,('a,'w) abstract) monad
    *)
  val fin : unit -> ('b,perm_rep) lm
end

   
module PivotCommon(PK:PIVOTKIND) = 
  struct
  type perm_rep = PK.perm_rep
  type 'a ira = 'a PK.ira
  type 'a fra = 'a PK.fra
  type 'a pra = 'a PK.pra
  type 'a lstate = ('a, PK.perm_rep ref) abstract
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TPivot of 'a lstate ]; classif : 'a; ..>
  let rowrep = PK.rowrep
  let colrep = PK.colrep
  let ip = (fun x -> `TPivot x), (function `TPivot x -> Some x | _ -> None),
           "Pivot"
end

module KeepPivot(PK:PIVOTKIND) = struct
  include PivotCommon(PK)
  let decl n = perform
      pdecl <-- retN (liftRef (PK.empty n));
      mo_extend ip pdecl;
      unitL
  let add v = perform
   p <-- mo_lookup ip;
   ret (Some (assign p (PK.add v (liftGet p))))
  let fin  () = perform
      p <-- mo_lookup ip;
      ret (liftGet p)
end

module DiscardPivot = struct
  include PivotCommon(PermList)
  let decl _ = unitL
  let add _ = ret None
  let fin () = failwith "Pivot is needed but is not computed"
end

(* Given a container, we can generate a whole "Linear Algebra"
   set of modules and generators all based on that container.
   This layout makes sure the same container is shared (and thus
   the same domain at the same time).

   The main reason that some of the above modules are not contained
   in this:
   1) some of the are container-independent, so why put them in here?
*)

module GenLA(C:CONTAINER2D) = struct

(* Bundle up some information, helps abstract some argument lists *)
type 'a wmatrix = {matrix: 'a C.vc; numrow: ('a,int) abstract; 
                   numcol: ('a,int) abstract}
type 'a curpos  = {rowpos: ('a, int) abstract; colpos: ('a, int) abstract}
type 'a curposval = {p: 'a curpos; curval: ('a, C.Dom.v) abstract}

module type DETERMINANT = sig
  type tdet = C.Dom.v ref
  type 'a lstate
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TDet of 'a lstate ]; classif : 'a; ..>
  type ('pc,'v) om = ('pc,'v) omonad
    constraint 'pc = <state : [> `TDet of 'a lstate ]; classif : 'a; ..>
  val decl : unit -> ('b,unit) lm (* could be unit rather than unit code...*)
  val upd_sign  : unit -> ('b,unit) om
  val zero_sign : unit -> ('b,unit) lm
  val acc       : ('a,C.Dom.v) abstract -> (<classif : 'a; ..>,unit) lm
  val get       : unit -> ('b,tdet) lm
  val set       : ('a,C.Dom.v) abstract -> (<classif : 'a; ..>,unit) lm
  val fin       : unit -> ('b,C.Dom.v) lm
end

module type PIVOT = 
      functor (D: DETERMINANT) -> 
        functor (P: TRACKPIVOT) -> sig
 (* Find the pivot within [r,m-1] rows and [c,(n-1)] columns
    of containrer b.
    If pivot is found, permute the matrix rows and columns so that the pivot
    becomes the element (r,c) of the matrix,
    Return the value of the pivot option. Or zero?
    When we permute the rows of columns, we update the sign of the det.
 *)
 val findpivot : 'a wmatrix -> 'a curpos ->
   (<classif : 'a; 
     state : [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ]; ..>, 
    C.Dom.v option) cmonad
(* [> 'a D.tag_lstate | 'a P.tag_lstate] *)
end

(* In the case of a non-fraction-free algorithm with no Det
   output, it is possible to not track the determinant, but
   in all other cases it is needed.
*)
 
module NoDet =
  struct
  type tdet = C.Dom.v ref
  type 'a lstate = unit
  let decl () = unitL
  let upd_sign () = ret None
  let zero_sign () = unitL
  let acc _ = unitL
  let get () = ret (liftRef C.Dom.zeroL) (* hack alert! *)
  let set _ = unitL
  let fin () = failwith "Determinant is needed but not computed"
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TDet of 'a lstate ]; classif : 'a; ..>
  type ('pc,'v) om = ('pc,'v) omonad
    constraint 'pc = <state : [> `TDet of 'a lstate ]; classif : 'a; ..>
end

module AbstractDet =
  struct
  open C.Dom
  type tdet = v ref
  (* the first part of the state is an integer: which is +1, 0, -1:
     the sign of the determinant *)
  type 'a lstate = ('a,int ref) abstract * ('a,tdet) abstract
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TDet of 'a lstate ]; classif : 'a; ..>
  type ('pc,'v) om = ('pc,'v) omonad
    constraint 'pc = <state : [> `TDet of 'a lstate ]; classif : 'a; ..>
  let ip = (fun x -> `TDet x), (function `TDet x -> Some x | _ -> None), "Det"
(* check later: XXX
  include Foo(struct type 'a tags = private [> `TDet of 'a lstate ] end)
*)
  let decl () = perform
      ddecl <-- retN (liftRef oneL);
      dsdecl <-- retN (liftRef Idx.one);
      mo_extend ip (dsdecl,ddecl);
      unitL
  let upd_sign () = perform
      det <-- mo_lookup ip;
      det1 <-- ret (fst det);
      ret (Some (assign det1 (Idx.uminus (liftGet det1))))
  let zero_sign () = perform
      det <-- mo_lookup ip;
      det1 <-- ret (fst det);
      assignM det1 Idx.zero
  let get () = perform
      det <-- mo_lookup ip;
      ret (snd det)
  let set v = perform
      det2 <-- get ();
      assignM det2 v
  let acc v = perform
      det2 <-- get ();
      r <-- ret ((liftGet det2) *^ v);
      assignM det2 r
  let fin = fun () -> perform
      (det_sign,det) <-- mo_lookup ip;
      ifM (Logic.equalL (liftGet det_sign) Idx.zero) (ret zeroL)
      (ifM (Logic.equalL (liftGet det_sign) Idx.one) (ret (liftGet det))
          (ret (uminusL (liftGet det))))
end

module type UPDATE =
        functor(D:DETERMINANT) -> sig
        type 'a in_val = 'a C.Dom.vc
        val update : 
            'a in_val -> 'a in_val -> 'a in_val -> 'a in_val -> 
          ('a in_val -> ('a, unit) abstract) ->
          ('a, C.Dom.v ref) abstract -> 
          (<classif : 'a; ..>, unit) cmonad
        val update_det : 'a in_val -> (<classif : 'a; ..>,unit) D.lm
(* this is only needed if we try to deal with FractionFree LU,
   which is really tough, especially since the L Matrix still has
   to be over the fraction field, which we don't have available.
        val update_lower : 
            'a in_val -> 'a in_val -> 'a in_val -> 'a in_val -> 
          ('a, C.Dom.v ref) abstract -> ('a, C.Dom.v, 's, 'w) cmonad *)
        val upd_kind : update_kind
end


    (* all this stuff is GE specific, wrap it up *)
module GE = struct

(* What is the update formula? *)
module DivisionUpdate(Det:DETERMINANT) =
  struct
  open C.Dom
  type 'a in_val = 'a vc
  let update bic brc brk bik setter _ = perform
      y <-- ret (bik -^ ((divL bic brc) *^ brk));
      ret (setter (applyMaybe normalizerL y))
  let update_det v = Det.acc v
(*let update_lower bic brc _ _ _ = perform
      y <-- ret (divL bic brc);
      ret (applyMaybe normalizerL y) *)
  let upd_kind = DivisionBased
  (* Initialization: check the preconditions of instantiation of this struct*)
  let _ = ensure (C.Dom.kind = Domains_sig.Domain_is_Field)
      "Cannot do Division in a non-field"
end

module FractionFreeUpdate(Det:DETERMINANT) = struct
  open C.Dom
  type 'a in_val = 'a vc
  let update bic brc brk bik setter d = perform
      z <-- ret ((bik *^ brc) -^ (brk *^ bic));
      t <-- ret (applyMaybe normalizerL z);
      ov <-- ret (divL t (liftGet d));
      ret (setter ov)
  let update_det v = Det.set v
(*let update_lower bic brc lrk lik d = perform
      rat <-- ret (liftGet d);
      z <-- ret (divL ((rat *^ lik) +^ (bic *^ lrk)) brc);
      t <-- ret (applyMaybe normalizerL z);
      ret t *)
  let upd_kind = FractionFree
end


(* This is for tracking L, as in LU decomposition.
   Naturally, when doing only GE, this is not needed at all, and should
   not generate any code *)
module type LOWER = sig
  type 'a lstate = ('a, C.contr) abstract
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TLower of 'a lstate ]; classif : 'a; ..>
  val decl   : ('a, C.contr) abstract -> (<classif : 'a; ..>, C.contr) lm
  val updt   : 'a C.vc -> ('a,int) abstract -> ('a,int) abstract -> 'a C.vo -> 
            'a C.Dom.vc -> (<classif : 'a;..>, unit) lm option
  val fin    : unit -> ('a,  C.contr) lm
  val wants_pack : bool
end


(* Do we keep the lower part? *)
module TrackLower = 
  struct
  type 'a lstate = ('a, C.contr) abstract
  type ('pc,'v) lm = ('pc,'v) cmonad
    constraint 'pc = <state : [> `TLower of 'a lstate ]; classif : 'a; ..>
  let ip = (fun x -> `TLower x), (function `TLower x -> Some x | _ -> None), 
           "TrackLower"
end

(* Even this form cannot be done with FractionFree, because in
   general the L matrix will be over the fraction field, even
   if the U is not.  So we won't even try to deal with it
   for now *)
module SeparateLower = struct
  include TrackLower
  let decl c = perform
      udecl <-- retN c;
      mo_extend ip udecl;
      ret udecl
  (* also need to 'set' lower! *)
  let updt mat row col defval nv = Some( perform
      lower <-- mo_lookup ip;
      l1 <-- ret (C.col_head_set lower row col nv);
      l2 <-- ret (C.col_head_set mat row col defval);
      ret (seq l1 l2) )
  let fin () = mo_lookup ip
  let wants_pack = false
end

(* Packed form cannot be done with FractionFree, so things
   are considerably simpler *)
module PackedLower = struct
  include TrackLower
  let decl c = perform
      udecl <-- ret c;
      mo_extend ip udecl;
      ret udecl
  let updt  _ _ _ _ _ = None
  let fin () = mo_lookup ip
  let wants_pack = true
end

module NoLower = struct
  include TrackLower
  let decl c = ret c
  let updt mat row col defval _ = 
      Some (ret (C.col_head_set mat row col defval))
  let fin () = failwith "Lower matrix L is needed but not computed"
  let wants_pack = false
end

module type INPUT = sig
    type inp
    val get_input : ('a, inp) abstract ->
    (<classif : 'a; ..>, ('a, C.contr) abstract * ('a, int) abstract * bool)
   monad
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


module RowPivot(Det: DETERMINANT)(P: TRACKPIVOT) =
struct
   let findpivot mat pos = perform
       pivot <-- retN (liftRef Maybe.none );
       (* If no better_than procedure defined, we just search for
      non-zero element. Any non-zero element is a good pivot.
      If better_than is defined, we search then for the best element *)
       seqM
        (match (C.Dom.better_thanL) with
         Some sel -> 
              Iters.row_iter mat.matrix pos.colpos pos.rowpos
          (Idx.pred mat.numrow) C.getL (fun j bjc ->
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
              ) UP
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
                            s2 <--  Det.upd_sign ();
                            s3 <-- ret (optSeq s1 s2);
                            s4 <-- P.add (P.rowrep i pos.rowpos );
                            ret (optSeq s3 s4)
                           ))
                          (ret (Maybe.just bic)))
                (ret Maybe.none))
end

module FullPivot(Det: DETERMINANT)(P: TRACKPIVOT) = 
struct
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
              )) UP ) UP )
              (* finished the loop *)
              (matchM (liftGet pivot)
                  (fun pv -> perform
                     (pr,pc,brc) <-- ret (liftPPair pv);
                     seqM
                         (whenM (Logic.notequalL pc pos.colpos) (perform
                           s1 <-- ret (C.swap_cols_stmt mat.matrix pos.colpos pc);
                           s2 <-- Det.upd_sign ();
                           s3 <-- ret (optSeq s1 s2);
                           s4 <-- P.add (P.colrep pc pos.rowpos );
                           ret (optSeq s3 s4)))
                       (seqM
                         (whenM (Logic.notequalL pr pos.rowpos) (perform
                           s1 <-- ret (C.swap_rows_stmt mat.matrix pos.rowpos pc);
                           s2 <-- Det.upd_sign ();
                           s3 <-- ret (optSeq s1 s2);
                           s4 <-- P.add (P.rowrep pr pos.rowpos );
                           ret (optSeq s3 s4)))
                         (ret (Maybe.just brc))))
                  (ret Maybe.none))
end

module NoPivot(Det: DETERMINANT)(P: TRACKPIVOT) = 
struct
   (* In this case, we assume diagonal dominance, and so
      just take the diagonal as ``pivot'' *)
   let findpivot mat pos = perform 
       ret (Maybe.just (C.row_head mat.matrix pos.rowpos pos.colpos));
end

module type OUTPUTDEP = sig 
    module PivotRep : PIVOTKIND 
    module Det      : DETERMINANT
end

(* What to return *)
module OutJustMatrix(OD : OUTPUTDEP) = struct
  module IF = struct
      module R   = NoRank
      module P   = DiscardPivot
      module L   = NoLower end
  type res = C.contr
  let make_result m = ret m.matrix
end

module OutDet(OD : OUTPUTDEP) = struct
  module IF = struct
      module R   = NoRank
      module P   = DiscardPivot
      module L   = NoLower end
  type res = C.contr * C.Dom.v
  let make_result m = perform
    det <-- OD.Det.fin ();
    ret (Tuple.tup2 m.matrix det)
  (* Initialization: check the preconditions of instantiation of this struct*)
  let _ = OD.Det.fin ()
end

module OutRank(OD : OUTPUTDEP) = struct
  module IF = struct
      module R   = Rank
      module P   = DiscardPivot
      module L   = NoLower end
  type res = C.contr * int
  let make_result m = perform
    rank <-- IF.R.fin ();
    ret (Tuple.tup2 m.matrix rank)
  (* Initialization: check the preconditions of instantiation of this struct*)
  let _ = IF.R.fin ()
end

module OutDetRank(OD : OUTPUTDEP) = struct
  module IF = struct
      module R   = Rank
      module P   = DiscardPivot
      module L   = NoLower end
  type res = C.contr * C.Dom.v * int
  let make_result m = perform
    det  <-- OD.Det.fin ();
    rank <-- IF.R.fin ();
    ret (Tuple.tup3 m.matrix det rank)
  (* Initialization: check the preconditions of instantiation of this struct*)
  let _ = OD.Det.fin ()
  let _ = IF.R.fin ()
end


module OutDetRankPivot(OD : OUTPUTDEP) = struct
  module IF = struct
      module R   = Rank
      module P   = KeepPivot(OD.PivotRep)
      module L   = NoLower end
  type res = C.contr * C.Dom.v * int *  IF.P.perm_rep
(* I wish to write that, but it can't be generalized!
  let pivfin = match P.fin with Some fin -> fin
  | None -> failwith "No Pivot computed provided"
*)
  let make_result m = perform
    det  <-- OD.Det.fin ();
    rank <-- IF.R.fin ();
    pivmat <-- IF.P.fin ();
    ret (Tuple.tup4 m.matrix det rank pivmat)
  (* Initialization: check the preconditions of instantiation of this struct*)
  let _ = OD.Det.fin ()
  let _ = IF.R.fin ()
  let _ = IF.P.fin ()
end


(* Only for Fields: because we can't extract the L in a non-field *)
module Out_L_U(OD : OUTPUTDEP) = struct
  module IF = struct
      module R   = NoRank
      module P   = KeepPivot(OD.PivotRep)
      module L   = SeparateLower 
  end
  type res = C.contr * C.contr * IF.P.perm_rep
  let make_result m = perform
    pivmat <-- IF.P.fin ();
    lower <-- IF.L.fin ();
    ret (Tuple.tup3 m.matrix lower pivmat)
  (* Initialization: check the preconditions of instantiation of this struct*)
  let _ = C.Dom.kind = Domains_sig.Domain_is_Field ||
          failwith "Out_L_U: Can't extract the L in a non-field"
  let _ = IF.P.fin ()
end

(* Only for Fields: because we can't extract the L in a non-field *)
module Out_LU_Packed(OD : OUTPUTDEP) = struct
  module IF = struct
      module R   = NoRank
      module P   = KeepPivot(OD.PivotRep)
      module L   = PackedLower end
  type res = C.contr * IF.P.perm_rep
  let make_result _ = perform
    pivmat <-- IF.P.fin ();
    lower <-- IF.L.fin ();
    ret (Tuple.tup2 lower pivmat)
    (* we really should be able to assert that lower == m.matrix
    here, but can't because the representation of lower/m.matrix
    could be 'functional' !*)
  (* Initialization: check the preconditions of instantiation of this struct*)
  let _ = C.Dom.kind = Domains_sig.Domain_is_Field ||
          failwith "Out_LU_Packed: Can't extract the L in a non-field"
  let _ = IF.P.fin ()
end

(* The `keyword' list of all the present internal features *)
module type INTERNAL_FEATURES = sig
  module R      : TrackRank.RANK
  module P      : TRACKPIVOT
  module L      : LOWER
end

module type OUTPUT = functor(OD : OUTPUTDEP) -> sig
  module IF : INTERNAL_FEATURES
  type res
  val make_result : 'a wmatrix ->
   (<classif : 'a; 
     state : [> `TDet of 'a OD.Det.lstate |
                'a IF.R.tag_lstate |
                `TPivot of 'a IF.P.lstate |
                `TLower of 'a IF.L.lstate]; ..>,res) cmonad
   (*
    ('a,res,[> 'a OD.Det.tag_lstate | 'a IF.R.tag_lstate 
             | 'a IF.P.tag_lstate   | 'a IF.L.tag_lstate],'w) cmonad
      *)
end

(* The `keyword' list of all the present external features *)
module type FEATURES = sig
  module Det       : DETERMINANT
  module PivotF    : PIVOT
  module PivotRep  : PIVOTKIND
  module Update    : UPDATE
  module Input     : INPUT
  module Output    : OUTPUT
end

module GenGE(F : FEATURES) = struct
    module O = F.Output(F)
    (* module U = F.Update(F.Det) *)
    (* module Verify = Test(F)  that does the pre-flight test *)

    let wants_pack = O.IF.L.wants_pack
    let can_pack   = 
        let module U = F.Update(F.Det) in
        (U.upd_kind = DivisionBased)
    (* some more pre-flight tests *)
    let _ = ensure ((not wants_pack) || can_pack) 
           "Cannot return a packed L in this case"

    let zerobelow mat pos = 
        let module IF = O.IF in
        let module U = F.Update(F.Det) in
        let innerbody j bjc = perform
            whenM (Logic.notequalL bjc C.Dom.zeroL ) (perform
                det <-- F.Det.get ();
                optSeqM (Iters.col_iter mat.matrix j (Idx.succ pos.p.colpos) 
               (Idx.pred mat.numcol) C.getL
                      (fun k bjk -> perform
                      brk <-- ret (C.getL mat.matrix pos.p.rowpos k);
                      U.update bjc pos.curval brk bjk 
                          (fun ov -> C.col_head_set mat.matrix j k ov) det) UP )
                      (IF.L.updt mat.matrix j pos.p.colpos C.Dom.zeroL 
                          (* this makes no sense outside a field! *)
                          (C.Dom.divL bjc pos.curval))) in
        perform
              seqM (Iters.row_iter mat.matrix pos.p.colpos
              (Idx.succ pos.p.rowpos)
              (Idx.pred mat.numrow) C.getL innerbody UP) 
                   (U.update_det pos.curval)

   let init input = perform
        let module IF = O.IF in
          (a,rmar,augmented) <-- F.Input.get_input input;
          r <-- IF.R.decl ();
          c <-- retN (liftRef Idx.zero);
          b <-- retN (C.mapper C.Dom.normalizerL (C.copy a));
          m <-- retN (C.dim1 a);
          rmar <-- retN rmar;
          n <-- if augmented then retN (C.dim2 a) else ret rmar;
          F.Det.decl ();
          IF.P.decl rmar;
          _ <-- IF.L.decl (if wants_pack then b else C.identity rmar m);
          let mat = {matrix=b; numrow=n; numcol=m} in
          ret (mat, r, c, rmar)

   let forward_elim (mat, r, c, rmar) = perform
        let module IF = O.IF in
          whileM (Logic.andL (Idx.less (liftGet c) mat.numcol)
                              (Idx.less (liftGet r) rmar) )
             ( perform
             rr <-- retN (liftGet r);
             cc <-- retN (liftGet c);
             let cp  = {rowpos=rr; colpos=cc} in
             let module Pivot = F.PivotF(F.Det)(IF.P) in
             pivot <-- l1 retN (Pivot.findpivot mat cp);
             seqM (matchM pivot (fun pv -> 
                      seqM (zerobelow mat {p=cp; curval=pv} )
                           (IF.R.succ ()) )
                      (F.Det.zero_sign () ))
                  (updateM c Idx.succ) )

   let gen input = perform
          (mat, r, c, rmar) <-- init input;
          seqM 
            (forward_elim (mat, r, c, rmar))
            (O.make_result mat)
end

end
(* end of GE-specific stuff *)


(* ======================================================================
   Now comes the solver module 
*)

module Solve = struct
module type INPUT = sig
    type inp
    type rhs = C.contr
    val get_input : ('a, inp) abstract ->
      (<classif : 'a;..>, ('a, C.contr) abstract * ('a, rhs) abstract) monad
end 

(* What is the input *)
module InpMatrixVector = struct
    type inp   = C.contr * C.contr
    type rhs   = C.contr
    let get_input a = perform
        (b,c) <-- ret (liftPair a);
        bb <-- retN b;
        cc <-- retN c;
        ret (bb, cc)
end

module type OUTPUT = sig
  type res
  val make_result : ('a, C.contr) abstract -> ('a, C.contr) abstract ->
      ('a, int) abstract -> ('a, int) abstract -> ('a, int) abstract ->
      (<classif : 'a;..>, res) cmonad
end

module OutJustAnswer = struct
  type res = C.contr
  let make_result src dest rmar cols rows = perform
      seqM
      (loopM Idx.zero (Idx.pred cols) (fun j -> 
          loopM Idx.zero (Idx.pred rows) (fun i -> perform
              aij <-- ret (C.getL src i (Idx.add j rmar));
              ret (C.col_head_set dest i j aij)
          ) UP) UP)
      (ret dest)
end

(* The `keyword' list of all the present external features *)
(* Really, these features are exposed by GE, but they can be seen
   through this implementation of Solve.
   
   As we do not support solving over non-fields (GE does not seem
   to be the right way to do that!), there is no point to expose
   the Update choice, we might as well always use DivisionUpdate. 
   Also, the pivor representation is irrelevant.
*)
module type FEATURES = sig
  module Det       : DETERMINANT
  module PivotF    : PIVOT
  module Input     : INPUT
  module Output    : OUTPUT
end

module GenSolve(F : FEATURES) = struct
    (* module Verify = Test(F)  that does the pre-flight test *)

    (* some more pre-flight tests *)
    let _ = ensure (C.Dom.kind = Domains_sig.Domain_is_Field)
        "Cannot Solve in a non-field"
    (* more to be filled in *)

    (* We will solve via GE. We inline the structure to improve
       compilation time... *)
    module GE' = GE.GenGE(struct
        module Det = F.Det
        module PivotF = F.PivotF
        module PivotRep = RowVectorPerm
        module Update = GE.DivisionUpdate
        module Input = GE.InpMatrixMargin
        module Output = GE.OutJustMatrix
    end)

    open C.Dom
    let init input = perform
        (a,b) <-- F.Input.get_input input;
        ret (a, b)

    let back_elim a m n = 
        let innerloop k = perform
            t <-- retN (divL oneL (C.getL a k k));
            seqM 
              (loopM m (Idx.pred n) (fun j -> perform
                  ret (C.col_head_set a k j (t *^ (C.getL a k j)))) UP)
              (seqM
              (loopM (Idx.zero) (Idx.pred k) (fun i -> perform
                seqM
                  (loopM m (Idx.pred n) (fun j -> perform
                      aij <-- ret (C.getL a i j);
                      aik <-- ret (C.getL a i k);
                      akj <-- ret (C.getL a k j);
                      ret (C.col_head_set a i j (aij -^ (aik *^ akj)))) UP) 
                  (ret (C.col_head_set a i k zeroL))) UP)
              (ret (C.col_head_set a k k oneL)))
        in 
        seqM
          (loopM (Idx.pred m) Idx.zero (innerloop) DOWN)
          (ret a)

    let gen input = perform
        (a,b)   <-- F.Input.get_input input;
        ma      <-- retN (C.dim1 a);
        na      <-- retN (C.dim2 a);
        nb      <-- retN (C.dim1 b);
        aug_a   <-- retN (C.augment a ma na b nb);
        u       <-- GE'.gen (Tuple.tup2 aug_a (C.dim2 a));
        uu      <-- retN u;
        eli     <-- back_elim uu ma (Idx.add na nb);
        res     <-- F.Output.make_result eli b na nb ma;
        ret res
end

end
(* end of Solve-specific stuff *)

end

end
