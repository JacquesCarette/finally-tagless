        Objective Caml version 3.09.1

#                 module GEF :
  sig
    module D :
      sig
        module type DOMAIN =
          sig
            type v
            val kind : Domains_sig.domain_kind
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
        module type DOMAINL =
          sig
            type v
            val kind : Domains_sig.domain_kind
            val zero : v
            val one : v
            val plus : v -> v -> v
            val times : v -> v -> v
            val minus : v -> v -> v
            val uminus : v -> v
            val div : v -> v -> v
            val better_than : (v -> v -> bool) option
            val normalizer : (v -> v) option
            type 'a vc = ('a, v) Direct.abstract
            val zeroL : 'a vc
            val oneL : 'a vc
            val ( +^ ) : 'a vc -> 'a vc -> 'a vc
            val ( *^ ) : 'a vc -> 'a vc -> 'a vc
            val ( -^ ) : 'a vc -> 'a vc -> 'a vc
            val uminusL : 'a vc -> 'a vc
            val divL : 'a vc -> 'a vc -> 'a vc
            val better_thanL :
              ('a vc -> 'a vc -> ('a, bool) Direct.abstract) option
            val normalizerL : ('a vc -> 'a vc) option
          end
        module type CONTAINER2D =
          sig
            module Dom : DOMAINL
            type contr
            type 'a vc = ('a, contr) Direct.abstract
            type 'a vo = ('a, Dom.v) Direct.abstract
            val getL :
              'a vc ->
              ('a, int) Direct.abstract -> ('a, int) Direct.abstract -> 'a vo
            val dim1 : 'a vc -> ('a, int) Direct.abstract
            val dim2 : 'a vc -> ('a, int) Direct.abstract
            val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
            val copy : 'a vc -> 'a vc
            val init :
              ('a, int) Direct.abstract -> ('a, int) Direct.abstract -> 'a vc
            val identity :
              ('a, int) Direct.abstract -> ('a, int) Direct.abstract -> 'a vc
            val swap_rows_stmt :
              'a vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract -> ('a, unit) Direct.abstract
            val swap_cols_stmt :
              'a vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract -> ('a, unit) Direct.abstract
            val row_head :
              'a vc ->
              ('a, int) Direct.abstract -> ('a, int) Direct.abstract -> 'a vo
            val col_head_set :
              'a vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a vo -> ('a, unit) Direct.abstract
          end
      end
    type ('a, 'b, 'c, 'd) cmonad =
        (('a, 'b) Direct.abstract, 'c list, ('a, 'd) Direct.abstract)
        StateCPSMonad.monad
    type ('a, 'b, 'c, 'd) omonad =
        (('a, 'b) Direct.abstract option, 'c list, ('a, 'd) Direct.abstract)
        StateCPSMonad.monad
    module Iters :
      sig
        val row_iter :
          'a ->
          'b ->
          ('c, int) Direct.abstract ->
          ('c, int) Direct.abstract ->
          ('a -> ('c, int) Direct.abstract -> 'b -> ('c, 'd) Direct.abstract) ->
          (('c, int) Direct.abstract ->
           ('c, 'd) Direct.abstract ->
           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
          'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
        val col_iter :
          'a ->
          'b ->
          ('c, int) Direct.abstract ->
          ('c, int) Direct.abstract ->
          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
          (('c, int) Direct.abstract ->
           'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
          'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
      end
    module TrackRank :
      sig
        type 'a lstate = ('a, int ref) Direct.abstract
        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
        type 'a tag_lstate = 'a tag_lstate_
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TRan of 'a ] list -> 'a
        val rfetch :
          unit ->
          ([> `TRan of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val rstore :
          'a ->
          ([> `TRan of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
          ('a list ->
           ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
          ('b, 'c) Direct.abstract
        val succ :
          unit ->
          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
        module type RANK =
          sig
            type 'a tag_lstate = 'a tag_lstate_
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
      end
    module Rank :
      sig
        type 'a lstate = ('a, int ref) Direct.abstract
        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
        type 'a tag_lstate = 'a tag_lstate_
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TRan of 'a ] list -> 'a
        val rfetch :
          unit ->
          ([> `TRan of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val rstore :
          'a ->
          ([> `TRan of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
          ('a list ->
           ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
          ('b, 'c) Direct.abstract
        val succ :
          unit ->
          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
        module type RANK =
          sig
            type 'a tag_lstate = 'a tag_lstate_
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        val fin :
          unit ->
          ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
          ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
      end
    module NoRank :
      sig
        type 'a lstate = ('a, int ref) Direct.abstract
        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
        type 'a tag_lstate = 'a tag_lstate_
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TRan of 'a ] list -> 'a
        val rfetch :
          unit ->
          ([> `TRan of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val rstore :
          'a ->
          ([> `TRan of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
          ('a list ->
           ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
          ('b, 'c) Direct.abstract
        val succ :
          unit ->
          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
        module type RANK =
          sig
            type 'a tag_lstate = 'a tag_lstate_
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        val fin : unit -> 'a
      end
    module type PIVOTKIND =
      sig
        type perm_rep
        type 'a ira = ('a, int) Direct.abstract
        type 'a fra
        type 'a pra = ('a, perm_rep) Direct.abstract
        val add : 'a fra -> 'a pra -> 'a pra
        val empty : 'a ira -> 'a pra
        val rowrep : 'a ira -> 'a ira -> 'a fra
        val colrep : 'a ira -> 'a ira -> 'a fra
      end
    module PermList :
      sig
        type flip_rep = Direct.perm
        type perm_rep = Direct.perm list
        type 'a ira = ('a, int) Direct.abstract
        type 'a fra = ('a, flip_rep) Direct.abstract
        type 'a pra = ('a, perm_rep) Direct.abstract
        val add :
          ('a, 'b) Direct.abstract ->
          ('a, 'b list) Direct.abstract -> ('a, 'b list) Direct.abstract
        val empty : 'a -> 'b pra
        val rowrep :
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract -> ('a, Direct.perm) Direct.abstract
        val colrep :
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract -> ('a, Direct.perm) Direct.abstract
      end
    module RowVectorPerm :
      sig
        type flip_rep = int * int
        type perm_rep = int array
        type 'a ira = ('a, int) Direct.abstract
        type 'a fra = ('a, flip_rep) Direct.abstract
        type 'a pra = ('a, perm_rep) Direct.abstract
        val add :
          ('a, int * int) Direct.abstract ->
          ('a, int array) Direct.abstract -> ('a, int array) Direct.abstract
        val empty :
          ('a, int) Direct.abstract -> ('a, int array) Direct.abstract
        val rowrep :
          ('a, 'b) Direct.abstract ->
          ('a, 'c) Direct.abstract -> ('a, 'b * 'c) Direct.abstract
        val colrep :
          ('a, 'b) Direct.abstract ->
          ('a, 'c) Direct.abstract -> ('a, 'b * 'c) Direct.abstract
      end
    module type TRACKPIVOT =
      sig
        type perm_rep
        type 'a ira = ('a, int) Direct.abstract
        type 'a fra
        type 'a pra
        type 'a lstate
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rowrep : 'a ira -> 'a ira -> 'a fra
        val colrep : 'a ira -> 'a ira -> 'a fra
        val decl :
          ('a, int) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val add :
          'a fra ->
          (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
           ('a, 'b) Direct.abstract)
          StateCPSMonad.monad
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
      end
    module PivotCommon :
      functor (PK : PIVOTKIND) ->
        sig
          type perm_rep = PK.perm_rep
          type 'a ira = 'a PK.ira
          type 'a fra = 'a PK.fra
          type 'a pra = 'a PK.pra
          type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
          type 'a tag_lstate = [ `TPivot of 'a lstate ]
          type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
            constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
          val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
          val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
        end
    module KeepPivot :
      functor (PK : PIVOTKIND) ->
        sig
          type perm_rep = PK.perm_rep
          type 'a ira = 'a PK.ira
          type 'a fra = 'a PK.fra
          type 'a pra = 'a PK.pra
          type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
          type 'a tag_lstate = [ `TPivot of 'a lstate ]
          type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
            constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
          val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
          val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
          val fetch_iter : [> `TPivot of 'a ] list -> 'a
          val pfetch :
            unit ->
            ([> `TPivot of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
          val pstore :
            'a ->
            ([> `TPivot of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
          val decl :
            'a PK.ira ->
            ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ] as 'b)
            list ->
            ('b list ->
             ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
          val add :
            'a PK.fra ->
            ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ] as 'b)
            list ->
            ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
          val fin :
            unit ->
            ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
            ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
        end
    module DiscardPivot :
      sig
        type perm_rep = PermList.perm_rep
        type 'a ira = 'a PermList.ira
        type 'a fra = 'a PermList.fra
        type 'a pra = 'a PermList.pra
        type 'a lstate = ('a, PermList.perm_rep ref) Direct.abstract
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rowrep : 'a PermList.ira -> 'a PermList.ira -> 'a PermList.fra
        val colrep : 'a PermList.ira -> 'a PermList.ira -> 'a PermList.fra
        val decl : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
        val fin : unit -> 'a
      end
    module GenLA :
      functor (C : D.CONTAINER2D) ->
        sig
          type 'a wmatrix =
            'a Ge.GEMake(Direct).GenLA(C).wmatrix = {
            matrix : 'a C.vc;
            numrow : ('a, int) Direct.abstract;
            numcol : ('a, int) Direct.abstract;
          }
          type 'a curpos =
            'a Ge.GEMake(Direct).GenLA(C).curpos = {
            rowpos : ('a, int) Direct.abstract;
            colpos : ('a, int) Direct.abstract;
          }
          type 'a curposval =
            'a Ge.GEMake(Direct).GenLA(C).curposval = {
            p : 'a curpos;
            curval : ('a, C.Dom.v) Direct.abstract;
          }
          module type DETERMINANT =
            sig
              type tdet = C.Dom.v ref
              type 'a lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
              val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
              val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
              val acc :
                ('a, C.Dom.v) Direct.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, unit) lm
              val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
              val set :
                ('a, C.Dom.v) Direct.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, unit) lm
              val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, C.Dom.v) lm
            end
          module NoDet :
            sig
              type tdet = C.Dom.v ref
              type 'a lstate = unit
              val decl :
                unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
              val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
              val zero_sign :
                unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
              val acc :
                'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
              val get :
                unit ->
                'a -> ('a -> ('b, C.Dom.v ref) Direct.abstract -> 'c) -> 'c
              val set :
                'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
              val fin : unit -> 'a
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            end
          module AbstractDet :
            sig
              type tdet = C.Dom.v ref
              type 'a lstate =
                  ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val fetch_iter : [> `TDet of 'a ] list -> 'a
              val dfetch :
                unit ->
                ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
              val dstore :
                'a ->
                ([> `TDet of 'a ] as 'b) list ->
                ('b list -> unit -> 'c) -> 'c
              val decl :
                unit ->
                ([> `TDet of
                      ('b, int ref) Direct.abstract *
                      ('b, C.Dom.v ref) Direct.abstract ]
                 as 'a)
                list ->
                ('a list ->
                 ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
                ('b, 'd) Direct.abstract
              val upd_sign :
                unit ->
                ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
                ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
              val zero_sign :
                unit ->
                ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
                ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
              val acc :
                'a C.Dom.vc ->
                ([> `TDet of 'c * ('a, C.Dom.v ref) Direct.abstract ] as 'b)
                list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
              val get :
                unit ->
                ([> `TDet of 'b * 'c ] as 'a) list ->
                ('a list -> 'c -> 'd) -> 'd
              val set :
                ('a, 'b) Direct.abstract ->
                ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
                ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
              val fin :
                unit ->
                ([> `TDet of
                      ('b, int ref) Direct.abstract *
                      ('b, C.Dom.v ref) Direct.abstract ]
                 as 'a)
                list ->
                ('a list -> ('b, C.Dom.v) Direct.abstract -> 'c) -> 'c
            end
          module type UPDATE =
            functor (D : DETERMINANT) ->
              sig
                type 'a in_val = 'a C.Dom.vc
                val update :
                  'a in_val ->
                  'a in_val ->
                  'a in_val ->
                  'a in_val ->
                  ('a in_val -> ('a, unit) Direct.abstract) ->
                  ('a, C.Dom.v ref) Direct.abstract ->
                  ('a, unit, 'b, 'c) cmonad
                val update_det :
                  'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
                val upd_kind : Ge.update_kind
              end
          module DivisionUpdate :
            functor (Det : DETERMINANT) ->
              sig
                type 'a in_val = 'a C.Dom.vc
                val update :
                  'a C.Dom.vc ->
                  'a C.Dom.vc ->
                  'a C.Dom.vc ->
                  'a C.Dom.vc ->
                  ('a C.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
                val update_det :
                  ('a, C.Dom.v) Direct.abstract ->
                  ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
                val upd_kind : Ge.update_kind
              end
          module FractionFreeUpdate :
            functor (Det : DETERMINANT) ->
              sig
                type 'a in_val = 'a C.Dom.vc
                val update :
                  'a C.Dom.vc ->
                  'a C.Dom.vc ->
                  'a C.Dom.vc ->
                  'a C.Dom.vc ->
                  ('a C.Dom.vc -> 'b) ->
                  ('a, C.Dom.v ref) Direct.abstract ->
                  'c -> ('c -> 'b -> 'd) -> 'd
                val update_det :
                  ('a, C.Dom.v) Direct.abstract ->
                  ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
                val upd_kind : Ge.update_kind
              end
          module type LOWER =
            sig
              type 'a lstate = ('a, C.contr) Direct.abstract
              type 'a tag_lstate = [ `TLower of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val decl :
                ('a, C.contr) Direct.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val updt :
                'a C.vc ->
                ('a, int) Direct.abstract ->
                ('a, int) Direct.abstract ->
                'a C.vo ->
                'a C.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
              val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val wants_pack : bool
            end
          module TrackLower :
            sig
              type 'a lstate = ('a, C.contr) Direct.abstract
              type 'a tag_lstate = [ `TLower of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val fetch_iter : [> `TLower of 'a ] list -> 'a
              val mfetch :
                unit ->
                ([> `TLower of 'b ] as 'a) list ->
                ('a list -> 'b -> 'c) -> 'c
              val mstore :
                'a ->
                ([> `TLower of 'a ] as 'b) list ->
                ('b list -> unit -> 'c) -> 'c
            end
          module SeparateLower :
            sig
              type 'a lstate = ('a, C.contr) Direct.abstract
              type 'a tag_lstate = [ `TLower of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val fetch_iter : [> `TLower of 'a ] list -> 'a
              val mfetch :
                unit ->
                ([> `TLower of 'b ] as 'a) list ->
                ('a list -> 'b -> 'c) -> 'c
              val mstore :
                'a ->
                ([> `TLower of 'a ] as 'b) list ->
                ('b list -> unit -> 'c) -> 'c
              val decl :
                ('a, 'b) Direct.abstract ->
                ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                ('c list ->
                 ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                ('a, 'd) Direct.abstract
              val updt :
                'a C.vc ->
                ('a, int) Direct.abstract ->
                ('a, int) Direct.abstract ->
                'a C.vo ->
                'a C.vo ->
                (([> `TLower of 'a C.vc ] as 'b) list ->
                 ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                option
              val fin :
                unit ->
                ([> `TLower of 'b ] as 'a) list ->
                ('a list -> 'b -> 'c) -> 'c
              val wants_pack : bool
            end
          module PackedLower :
            sig
              type 'a lstate = ('a, C.contr) Direct.abstract
              type 'a tag_lstate = [ `TLower of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val fetch_iter : [> `TLower of 'a ] list -> 'a
              val mfetch :
                unit ->
                ([> `TLower of 'b ] as 'a) list ->
                ('a list -> 'b -> 'c) -> 'c
              val mstore :
                'a ->
                ([> `TLower of 'a ] as 'b) list ->
                ('b list -> unit -> 'c) -> 'c
              val decl :
                'a ->
                ([> `TLower of 'a ] as 'b) list ->
                ('b list -> 'a -> 'c) -> 'c
              val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
              val fin :
                unit ->
                ([> `TLower of 'b ] as 'a) list ->
                ('a list -> 'b -> 'c) -> 'c
              val wants_pack : bool
            end
          module NoLower :
            sig
              type 'a lstate = ('a, C.contr) Direct.abstract
              type 'a tag_lstate = [ `TLower of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val fetch_iter : [> `TLower of 'a ] list -> 'a
              val mfetch :
                unit ->
                ([> `TLower of 'b ] as 'a) list ->
                ('a list -> 'b -> 'c) -> 'c
              val mstore :
                'a ->
                ([> `TLower of 'a ] as 'b) list ->
                ('b list -> unit -> 'c) -> 'c
              val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
              val updt :
                'a C.vc ->
                ('a, int) Direct.abstract ->
                ('a, int) Direct.abstract ->
                'a C.vo ->
                'b ->
                ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
              val fin : unit -> 'a
              val wants_pack : bool
            end
          module type INPUT =
            sig
              type inp
              val get_input :
                ('a, inp) Direct.abstract ->
                (('a, C.contr) Direct.abstract * ('a, int) Direct.abstract *
                 bool, 'b, ('a, 'c) Direct.abstract)
                StateCPSMonad.monad
            end
          module InpJustMatrix :
            sig
              type inp = C.contr
              val get_input :
                'a C.vc ->
                'b ->
                ('b -> 'a C.vc * ('a, int) Direct.abstract * bool -> 'c) ->
                'c
            end
          module InpMatrixMargin :
            sig
              type inp = C.contr * int
              val get_input :
                ('a, 'b * 'c) Direct.abstract ->
                'd ->
                ('d ->
                 ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool ->
                 'e) ->
                'e
            end
          module type PIVOT =
            functor (D : DETERMINANT) ->
              functor (P : TRACKPIVOT) ->
                sig
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    ('a, C.Dom.v option,
                     [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
                    cmonad
                end
          module RowPivot :
            functor (Det : DETERMINANT) ->
              functor (P : TRACKPIVOT) ->
                sig
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('a, C.Dom.v option) Direct.abstract ->
                     ('a, 'c) Direct.abstract) ->
                    ('a, 'c) Direct.abstract
                end
          module FullPivot :
            functor (Det : DETERMINANT) ->
              functor (P : TRACKPIVOT) ->
                sig
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('a, C.Dom.v option) Direct.abstract ->
                     ('a, 'c) Direct.abstract) ->
                    ('a, 'c) Direct.abstract
                end
          module NoPivot :
            functor (Det : DETERMINANT) ->
              functor (P : TRACKPIVOT) ->
                sig
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    'b ->
                    ('b -> ('a, C.Dom.v option) Direct.abstract -> 'c) -> 'c
                end
          module type OUTPUTDEP =
            sig module PivotRep : PIVOTKIND module Det : DETERMINANT end
          module OutJustMatrix :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Direct.abstract
                        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                        type 'a tag_lstate = 'a tag_lstate_
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TRan of 'a ] list -> 'a
                        val rfetch :
                          unit ->
                          ([> `TRan of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val rstore :
                          'a ->
                          ([> `TRan of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Direct.abstract ->
                           ('b, 'c) Direct.abstract) ->
                          ('b, 'c) Direct.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                        module type RANK =
                          sig
                            type 'a tag_lstate = 'a tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val decl :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val succ :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                          end
                        val fin : unit -> 'a
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val colrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Direct.abstract
                        type 'a tag_lstate = [ `TLower of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TLower of 'a ] list -> 'a
                        val mfetch :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val mstore :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                        val updt :
                          'a C.vc ->
                          ('a, int) Direct.abstract ->
                          ('a, int) Direct.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c ->
                           ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                          option
                        val fin : unit -> 'a
                        val wants_pack : bool
                      end
                  end
                type res = C.contr
                val make_result :
                  'a wmatrix -> 'b -> ('b -> 'a C.vc -> 'c) -> 'c
              end
          module OutDet :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Direct.abstract
                        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                        type 'a tag_lstate = 'a tag_lstate_
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TRan of 'a ] list -> 'a
                        val rfetch :
                          unit ->
                          ([> `TRan of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val rstore :
                          'a ->
                          ([> `TRan of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Direct.abstract ->
                           ('b, 'c) Direct.abstract) ->
                          ('b, 'c) Direct.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                        module type RANK =
                          sig
                            type 'a tag_lstate = 'a tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val decl :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val succ :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                          end
                        val fin : unit -> 'a
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val colrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Direct.abstract
                        type 'a tag_lstate = [ `TLower of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TLower of 'a ] list -> 'a
                        val mfetch :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val mstore :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                        val updt :
                          'a C.vc ->
                          ('a, int) Direct.abstract ->
                          ('a, int) Direct.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c ->
                           ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                          option
                        val fin : unit -> 'a
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.Dom.v
                val make_result :
                  'a wmatrix ->
                  ([> 'a OD.Det.tag_lstate ] as 'b) list ->
                  ('b list ->
                   ('a, C.contr * C.Dom.v) Direct.abstract ->
                   ('a, 'c) Direct.abstract) ->
                  ('a, 'c) Direct.abstract
              end
          module OutRank :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Direct.abstract
                        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                        type 'a tag_lstate = 'a tag_lstate_
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TRan of 'a ] list -> 'a
                        val rfetch :
                          unit ->
                          ([> `TRan of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val rstore :
                          'a ->
                          ([> `TRan of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Direct.abstract ->
                           ('b, 'c) Direct.abstract) ->
                          ('b, 'c) Direct.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                        module type RANK =
                          sig
                            type 'a tag_lstate = 'a tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val decl :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val succ :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                          end
                        val fin :
                          unit ->
                          ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val colrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Direct.abstract
                        type 'a tag_lstate = [ `TLower of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TLower of 'a ] list -> 'a
                        val mfetch :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val mstore :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                        val updt :
                          'a C.vc ->
                          ('a, int) Direct.abstract ->
                          ('a, int) Direct.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c ->
                           ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                          option
                        val fin : unit -> 'a
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * int
                val make_result :
                  'a wmatrix ->
                  ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
                  ('b list -> ('a, C.contr * 'c) Direct.abstract -> 'd) -> 'd
              end
          module OutDetRank :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Direct.abstract
                        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                        type 'a tag_lstate = 'a tag_lstate_
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TRan of 'a ] list -> 'a
                        val rfetch :
                          unit ->
                          ([> `TRan of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val rstore :
                          'a ->
                          ([> `TRan of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Direct.abstract ->
                           ('b, 'c) Direct.abstract) ->
                          ('b, 'c) Direct.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                        module type RANK =
                          sig
                            type 'a tag_lstate = 'a tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val decl :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val succ :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                          end
                        val fin :
                          unit ->
                          ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val colrep :
                          'a PermList.ira ->
                          'a PermList.ira -> 'a PermList.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Direct.abstract
                        type 'a tag_lstate = [ `TLower of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TLower of 'a ] list -> 'a
                        val mfetch :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val mstore :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                        val updt :
                          'a C.vc ->
                          ('a, int) Direct.abstract ->
                          ('a, int) Direct.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c ->
                           ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                          option
                        val fin : unit -> 'a
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.Dom.v * int
                val make_result :
                  'a wmatrix ->
                  ([> `TDet of 'a OD.Det.lstate
                    | `TRan of ('a, 'c ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, C.contr * C.Dom.v * 'c) Direct.abstract ->
                   ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
              end
          module OutDetRankPivot :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Direct.abstract
                        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                        type 'a tag_lstate = 'a tag_lstate_
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TRan of 'a ] list -> 'a
                        val rfetch :
                          unit ->
                          ([> `TRan of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val rstore :
                          'a ->
                          ([> `TRan of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Direct.abstract ->
                           ('b, 'c) Direct.abstract) ->
                          ('b, 'c) Direct.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                        module type RANK =
                          sig
                            type 'a tag_lstate = 'a tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val decl :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val succ :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                          end
                        val fin :
                          unit ->
                          ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                      end
                    module P :
                      sig
                        type perm_rep = OD.PivotRep.perm_rep
                        type 'a ira = 'a OD.PivotRep.ira
                        type 'a fra = 'a OD.PivotRep.fra
                        type 'a pra = 'a OD.PivotRep.pra
                        type 'a lstate =
                            ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep :
                          'a OD.PivotRep.ira ->
                          'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                        val colrep :
                          'a OD.PivotRep.ira ->
                          'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                        val fetch_iter : [> `TPivot of 'a ] list -> 'a
                        val pfetch :
                          unit ->
                          ([> `TPivot of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val pstore :
                          'a ->
                          ([> `TPivot of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          'a OD.PivotRep.ira ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref)
                                Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Direct.abstract ->
                           ('a, 'd) Direct.abstract) ->
                          ('a, 'd) Direct.abstract
                        val add :
                          'a OD.PivotRep.fra ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref)
                                Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                          'c
                        val fin :
                          unit ->
                          ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Direct.abstract
                        type 'a tag_lstate = [ `TLower of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TLower of 'a ] list -> 'a
                        val mfetch :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val mstore :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                        val updt :
                          'a C.vc ->
                          ('a, int) Direct.abstract ->
                          ('a, int) Direct.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c ->
                           ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                          option
                        val fin : unit -> 'a
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.Dom.v * int * IF.P.perm_rep
                val make_result :
                  'a wmatrix ->
                  ([> `TDet of 'a OD.Det.lstate
                    | `TPivot of ('a, 'c ref) Direct.abstract
                    | `TRan of ('a, 'd ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, C.contr * C.Dom.v * 'd * 'c) Direct.abstract ->
                   ('a, 'e) Direct.abstract) ->
                  ('a, 'e) Direct.abstract
              end
          module Out_L_U :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Direct.abstract
                        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                        type 'a tag_lstate = 'a tag_lstate_
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TRan of 'a ] list -> 'a
                        val rfetch :
                          unit ->
                          ([> `TRan of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val rstore :
                          'a ->
                          ([> `TRan of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Direct.abstract ->
                           ('b, 'c) Direct.abstract) ->
                          ('b, 'c) Direct.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                        module type RANK =
                          sig
                            type 'a tag_lstate = 'a tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val decl :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val succ :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                          end
                        val fin : unit -> 'a
                      end
                    module P :
                      sig
                        type perm_rep = OD.PivotRep.perm_rep
                        type 'a ira = 'a OD.PivotRep.ira
                        type 'a fra = 'a OD.PivotRep.fra
                        type 'a pra = 'a OD.PivotRep.pra
                        type 'a lstate =
                            ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep :
                          'a OD.PivotRep.ira ->
                          'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                        val colrep :
                          'a OD.PivotRep.ira ->
                          'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                        val fetch_iter : [> `TPivot of 'a ] list -> 'a
                        val pfetch :
                          unit ->
                          ([> `TPivot of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val pstore :
                          'a ->
                          ([> `TPivot of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          'a OD.PivotRep.ira ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref)
                                Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Direct.abstract ->
                           ('a, 'd) Direct.abstract) ->
                          ('a, 'd) Direct.abstract
                        val add :
                          'a OD.PivotRep.fra ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref)
                                Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                          'c
                        val fin :
                          unit ->
                          ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Direct.abstract
                        type 'a tag_lstate = [ `TLower of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TLower of 'a ] list -> 'a
                        val mfetch :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val mstore :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          ('a, 'b) Direct.abstract ->
                          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c)
                          list ->
                          ('c list ->
                           ('a, 'b) Direct.abstract ->
                           ('a, 'd) Direct.abstract) ->
                          ('a, 'd) Direct.abstract
                        val updt :
                          'a C.vc ->
                          ('a, int) Direct.abstract ->
                          ('a, int) Direct.abstract ->
                          'a C.vo ->
                          'a C.vo ->
                          (([> `TLower of 'a C.vc ] as 'b) list ->
                           ('b list -> ('a, unit) Direct.abstract -> 'c) ->
                           'c)
                          option
                        val fin :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.contr * IF.P.perm_rep
                val make_result :
                  'a wmatrix ->
                  ([> `TLower of ('a, 'c) Direct.abstract
                    | `TPivot of ('a, 'd ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, C.contr * 'c * 'd) Direct.abstract -> 'e) ->
                  'e
              end
          module Out_LU_Packed :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Direct.abstract
                        type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                        type 'a tag_lstate = 'a tag_lstate_
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TRan of 'a ] list -> 'a
                        val rfetch :
                          unit ->
                          ([> `TRan of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val rstore :
                          'a ->
                          ([> `TRan of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Direct.abstract ->
                           ('b, 'c) Direct.abstract) ->
                          ('b, 'c) Direct.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                        module type RANK =
                          sig
                            type 'a tag_lstate = 'a tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val decl :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                            val succ :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                          end
                        val fin : unit -> 'a
                      end
                    module P :
                      sig
                        type perm_rep = OD.PivotRep.perm_rep
                        type 'a ira = 'a OD.PivotRep.ira
                        type 'a fra = 'a OD.PivotRep.fra
                        type 'a pra = 'a OD.PivotRep.pra
                        type 'a lstate =
                            ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep :
                          'a OD.PivotRep.ira ->
                          'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                        val colrep :
                          'a OD.PivotRep.ira ->
                          'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                        val fetch_iter : [> `TPivot of 'a ] list -> 'a
                        val pfetch :
                          unit ->
                          ([> `TPivot of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val pstore :
                          'a ->
                          ([> `TPivot of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          'a OD.PivotRep.ira ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref)
                                Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Direct.abstract ->
                           ('a, 'd) Direct.abstract) ->
                          ('a, 'd) Direct.abstract
                        val add :
                          'a OD.PivotRep.fra ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref)
                                Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                          'c
                        val fin :
                          unit ->
                          ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Direct.abstract
                        type 'a tag_lstate = [ `TLower of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val fetch_iter : [> `TLower of 'a ] list -> 'a
                        val mfetch :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val mstore :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> unit -> 'c) -> 'c
                        val decl :
                          'a ->
                          ([> `TLower of 'a ] as 'b) list ->
                          ('b list -> 'a -> 'c) -> 'c
                        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                        val fin :
                          unit ->
                          ([> `TLower of 'b ] as 'a) list ->
                          ('a list -> 'b -> 'c) -> 'c
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * IF.P.perm_rep
                val make_result :
                  'a ->
                  ([> `TLower of ('c, 'd) Direct.abstract
                    | `TPivot of ('c, 'e ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
              end
          module type INTERNAL_FEATURES =
            sig
              module R : TrackRank.RANK
              module P : TRACKPIVOT
              module L : LOWER
            end
          module type OUTPUT =
            functor (OD : OUTPUTDEP) ->
              sig
                module IF : INTERNAL_FEATURES
                type res
                val make_result :
                  'a wmatrix ->
                  ('a, res,
                   [> `TDet of 'a OD.Det.lstate
                    | `TLower of 'a IF.L.lstate
                    | `TPivot of 'a IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ],
                   'b)
                  cmonad
              end
          module type FEATURES =
            sig
              module Det : DETERMINANT
              module PivotF : PIVOT
              module PivotRep : PIVOTKIND
              module Update : UPDATE
              module Input : INPUT
              module Output : OUTPUT
            end
          module GenGE :
            functor (F : FEATURES) ->
              sig
                module O :
                  sig
                    module IF :
                      sig
                        module R :
                          sig
                            type 'a tag_lstate = 'a TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a TrackRank.tag_lstate ] * 'b,
                               int ref)
                              TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a TrackRank.tag_lstate ] * 'b,
                               int ref)
                              TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a TrackRank.tag_lstate ] * 'b, unit)
                              TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a TrackRank.tag_lstate ] * 'b, int)
                              TrackRank.lm
                          end
                        module P :
                          sig
                            type perm_rep = F.Output(F).IF.P.perm_rep
                            type 'a ira = ('a, int) Direct.abstract
                            type 'a fra = 'a F.Output(F).IF.P.fra
                            type 'a pra = 'a F.Output(F).IF.P.pra
                            type 'a lstate = 'a F.Output(F).IF.P.lstate
                            type 'a tag_lstate = [ `TPivot of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val rowrep : 'a ira -> 'a ira -> 'a fra
                            val colrep : 'a ira -> 'a ira -> 'a fra
                            val decl :
                              ('a, int) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val add :
                              'a fra ->
                              (('a, unit) Direct.abstract option,
                               [> 'a tag_lstate ] list,
                               ('a, 'b) Direct.abstract)
                              StateCPSMonad.monad
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                          end
                        module L :
                          sig
                            type 'a lstate = ('a, C.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                            val decl :
                              ('a, C.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                            val updt :
                              'a C.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a C.vo ->
                              'a C.Dom.vc ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                            val wants_pack : bool
                          end
                      end
                    type res = F.Output(F).res
                    val make_result :
                      'a wmatrix ->
                      ('a, res,
                       [> `TDet of 'a F.Det.lstate
                        | `TLower of 'a IF.L.lstate
                        | `TPivot of 'a IF.P.lstate
                        | `TRan of 'a TrackRank.lstate ],
                       'b)
                      cmonad
                  end
                val wants_pack : bool
                val back_elim : bool
                val can_pack : bool
                val zerobelow :
                  'a wmatrix ->
                  'a curposval ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of ('a, C.contr) Direct.abstract ]
                   as 'b)
                  list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
                val init :
                  ('a, F.Input.inp) Direct.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of ('a, C.contr) Direct.abstract
                    | `TPivot of 'a F.Output(F).IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list ->
                  ('b list ->
                   'a wmatrix * ('a, int ref) Direct.abstract *
                   ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
                   ('a, 'c) Direct.abstract) ->
                  ('a, 'c) Direct.abstract
                val forward_elim :
                  'a wmatrix * ('a, int ref) Direct.abstract *
                  ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of ('a, C.contr) Direct.abstract
                    | `TPivot of 'a F.Output(F).IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
                val backward_elim :
                  unit ->
                  ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c)
                  option
                val ge_gen :
                  ('a, F.Input.inp) Direct.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of 'a O.IF.L.lstate
                    | `TPivot of 'a O.IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
                  ('a, 'c) Direct.abstract
                val gen :
                  ('a, F.Input.inp) Direct.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of 'a O.IF.L.lstate
                    | `TPivot of 'a O.IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
                  ('a, 'c) Direct.abstract
              end
        end
  end
val instantiate :
  ((unit -> 'a) -> 'b list -> ('c -> 'd -> 'd) -> unit -> 'e) -> 'a -> 'e =
  <fun>
#   type 'a pr = { pf : 'a; }
# val runit : 'a pr -> 'a = <fun>
#   * * * * * * * * *     module Z3 :
  sig
    type v = int
    val kind : Domains_sig.domain_kind
    val zero : int
    val one : int
    val plus : int -> int -> int
    val times : int -> int -> int
    val minus : int -> int -> int
    val uminus : int -> int
    val extended_gcd : int -> int -> int * int
    val div : int -> int -> int
    val normalizer : 'a option
    val better_than : 'a option
    type 'a vc = ('a, v) Domains_direct.DirectRep.rep
    val zeroL : unit -> int
    val oneL : unit -> int
    val ( +^ ) : (unit -> int) -> (unit -> int) -> unit -> int
    val ( *^ ) : (unit -> int) -> (unit -> int) -> unit -> int
    val ( -^ ) : (unit -> int) -> (unit -> int) -> unit -> int
    val uminusL : (unit -> int) -> unit -> int
    val divL : (unit -> int) -> (unit -> int) -> unit -> int
    val better_thanL : 'a option
    val normalizerL : 'a option
  end
module Z19 :
  sig
    type v = int
    val kind : Domains_sig.domain_kind
    val zero : int
    val one : int
    val plus : int -> int -> int
    val times : int -> int -> int
    val minus : int -> int -> int
    val uminus : int -> int
    val extended_gcd : int -> int -> int * int
    val div : int -> int -> int
    val normalizer : 'a option
    val better_than : 'a option
    type 'a vc = ('a, v) Domains_direct.DirectRep.rep
    val zeroL : unit -> int
    val oneL : unit -> int
    val ( +^ ) : (unit -> int) -> (unit -> int) -> unit -> int
    val ( *^ ) : (unit -> int) -> (unit -> int) -> unit -> int
    val ( -^ ) : (unit -> int) -> (unit -> int) -> unit -> int
    val uminusL : (unit -> int) -> unit -> int
    val divL : (unit -> int) -> (unit -> int) -> unit -> int
    val better_thanL : 'a option
    val normalizerL : 'a option
  end
module GAC_F :
  sig
    module Dom :
      sig
        type v = Domains_direct.FloatDomainL.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val dim2 : (unit -> 'a array) -> unit -> int
    val dim1 : (unit -> 'a array array) -> unit -> int
    val mapper :
      ('a vo -> 'a vo) option ->
      (unit -> Dom.v array array) -> unit -> Dom.v array array
    val copy : (unit -> 'a array array) -> unit -> 'a array array
    val init : (unit -> int) -> (unit -> int) -> unit -> Dom.v array array
    val identity :
      (unit -> int) -> (unit -> int) -> unit -> Dom.v array array
    val swap_rows_stmt :
      (unit -> 'a array) -> (unit -> int) -> (unit -> int) -> unit -> unit
    val swap_cols_stmt :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val row_head :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val col_head_set :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> (unit -> 'a) -> unit -> unit
  end
module GVC_F :
  sig
    module Dom :
      sig
        type v = Domains_direct.FloatDomainL.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_direct.container2dfromvector
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val dim2 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val dim1 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val mapper :
      ((unit -> 'a) -> unit -> 'a) option ->
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val copy :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val init :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val identity :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val swap_rows_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val swap_cols_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val row_head :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val col_head_set :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> (unit -> 'a) -> unit -> unit
  end
module GAC_I :
  sig
    module Dom :
      sig
        type v = Domains_direct.IntegerDomainL.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val dim2 : (unit -> 'a array) -> unit -> int
    val dim1 : (unit -> 'a array array) -> unit -> int
    val mapper :
      ('a vo -> 'a vo) option ->
      (unit -> Dom.v array array) -> unit -> Dom.v array array
    val copy : (unit -> 'a array array) -> unit -> 'a array array
    val init : (unit -> int) -> (unit -> int) -> unit -> Dom.v array array
    val identity :
      (unit -> int) -> (unit -> int) -> unit -> Dom.v array array
    val swap_rows_stmt :
      (unit -> 'a array) -> (unit -> int) -> (unit -> int) -> unit -> unit
    val swap_cols_stmt :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val row_head :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val col_head_set :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> (unit -> 'a) -> unit -> unit
  end
module GVC_I :
  sig
    module Dom :
      sig
        type v = Domains_direct.IntegerDomainL.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_direct.container2dfromvector
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val dim2 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val dim1 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val mapper :
      ((unit -> 'a) -> unit -> 'a) option ->
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val copy :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val init :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val identity :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val swap_rows_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val swap_cols_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val row_head :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val col_head_set :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> (unit -> 'a) -> unit -> unit
  end
module GAC_R :
  sig
    module Dom :
      sig
        type v = Domains_direct.RationalDomainL.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val dim2 : (unit -> 'a array) -> unit -> int
    val dim1 : (unit -> 'a array array) -> unit -> int
    val mapper :
      ('a vo -> 'a vo) option ->
      (unit -> Dom.v array array) -> unit -> Dom.v array array
    val copy : (unit -> 'a array array) -> unit -> 'a array array
    val init : (unit -> int) -> (unit -> int) -> unit -> Dom.v array array
    val identity :
      (unit -> int) -> (unit -> int) -> unit -> Dom.v array array
    val swap_rows_stmt :
      (unit -> 'a array) -> (unit -> int) -> (unit -> int) -> unit -> unit
    val swap_cols_stmt :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val row_head :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val col_head_set :
      (unit -> 'a array array) ->
      (unit -> int) -> (unit -> int) -> (unit -> 'a) -> unit -> unit
  end
module GVC_Z3 :
  sig
    module Dom :
      sig
        type v = Z3.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_direct.container2dfromvector
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val dim2 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val dim1 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val mapper :
      ((unit -> 'a) -> unit -> 'a) option ->
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val copy :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val init :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val identity :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val swap_rows_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val swap_cols_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val row_head :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val col_head_set :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> (unit -> 'a) -> unit -> unit
  end
module GVC_Z19 :
  sig
    module Dom :
      sig
        type v = Z19.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_direct.container2dfromvector
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val dim2 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val dim1 :
      (unit -> 'a Domains_direct.container2dfromvector) -> unit -> int
    val mapper :
      ((unit -> 'a) -> unit -> 'a) option ->
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val copy :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      unit -> 'a Domains_direct.container2dfromvector
    val init :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val identity :
      (unit -> int) ->
      (unit -> int) -> unit -> Dom.v Domains_direct.container2dfromvector
    val swap_rows_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val swap_cols_stmt :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> unit
    val row_head :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> unit -> 'a
    val col_head_set :
      (unit -> 'a Domains_direct.container2dfromvector) ->
      (unit -> int) -> (unit -> int) -> (unit -> 'a) -> unit -> unit
  end
module GFC_F :
  sig
    module Dom :
      sig
        type v =
            Domains_direct.FortranVectorContainer(Domains_direct.FloatDomainL).Dom.v
        val kind : Domains_sig.domain_kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) Domains_direct.DirectRep.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) Domains_direct.DirectRep.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr =
        Domains_direct.FortranVectorContainer(Domains_direct.FloatDomainL).contr
    type 'a vc = ('a, contr) Domains_direct.DirectRep.rep
    type 'a vo = ('a, Dom.v) Domains_direct.DirectRep.rep
    val getL :
      'a vc ->
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, int) Domains_direct.DirectRep.rep -> 'a vo
    val dim1 : 'a vc -> ('a, int) Domains_direct.DirectRep.rep
    val dim2 : 'a vc -> ('a, int) Domains_direct.DirectRep.rep
    val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
    val copy : 'a vc -> 'a vc
    val init :
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, int) Domains_direct.DirectRep.rep -> 'a vc
    val identity :
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, int) Domains_direct.DirectRep.rep -> 'a vc
    val swap_rows_stmt :
      'a vc ->
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, unit) Domains_direct.DirectRep.rep
    val swap_cols_stmt :
      'a vc ->
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, unit) Domains_direct.DirectRep.rep
    val row_head :
      'a vc ->
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, int) Domains_direct.DirectRep.rep -> 'a vo
    val col_head_set :
      'a vc ->
      ('a, int) Domains_direct.DirectRep.rep ->
      ('a, int) Domains_direct.DirectRep.rep ->
      'a vo -> ('a, unit) Domains_direct.DirectRep.rep
  end
module G_GAC_F :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GAC_F).wmatrix = {
      matrix : 'a GAC_F.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GAC_F).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GAC_F).curposval = {
      p : 'a curpos;
      curval : ('a, GAC_F.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GAC_F.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GAC_F.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GAC_F.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GAC_F.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GAC_F.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GAC_F.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GAC_F.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GAC_F.Dom.vc ->
          ([> `TDet of 'c * ('a, GAC_F.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GAC_F.Dom.v ref) Direct.abstract ]
           as 'a)
          list -> ('a list -> ('b, GAC_F.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_F.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GAC_F.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_F.Dom.vc
          val update :
            'a GAC_F.Dom.vc ->
            'a GAC_F.Dom.vc ->
            'a GAC_F.Dom.vc ->
            'a GAC_F.Dom.vc ->
            ('a GAC_F.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GAC_F.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_F.Dom.vc
          val update :
            'a GAC_F.Dom.vc ->
            'a GAC_F.Dom.vc ->
            'a GAC_F.Dom.vc ->
            'a GAC_F.Dom.vc ->
            ('a GAC_F.Dom.vc -> 'b) ->
            ('a, GAC_F.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GAC_F.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GAC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val decl :
          ('a, GAC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val updt :
          'a GAC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_F.vo ->
          'a GAC_F.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GAC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GAC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GAC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_F.vo ->
          'a GAC_F.vo ->
          (([> `TLower of 'a GAC_F.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GAC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GAC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GAC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_F.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GAC_F.contr
        val get_input :
          'a GAC_F.vc ->
          'b ->
          ('b -> 'a GAC_F.vc * ('a, int) Direct.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GAC_F.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GAC_F.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GAC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GAC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_F.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GAC_F.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GAC_F.contr * GAC_F.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GAC_F.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_F.contr * GAC_F.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_F.contr * GAC_F.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GAC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_F.vo ->
                    'a GAC_F.vo ->
                    (([> `TLower of 'a GAC_F.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GAC_F.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                      val decl :
                        ('a, GAC_F.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                      val updt :
                        'a GAC_F.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GAC_F.vo ->
                        'a GAC_F.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_F.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_F.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_F.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module G_GVC_F :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GVC_F).wmatrix = {
      matrix : 'a GVC_F.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GVC_F).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GVC_F).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_F.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GVC_F.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GVC_F.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_F.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GVC_F.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_F.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GVC_F.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_F.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GVC_F.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_F.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_F.Dom.v ref) Direct.abstract ]
           as 'a)
          list -> ('a list -> ('b, GVC_F.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_F.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GVC_F.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_F.Dom.vc
          val update :
            'a GVC_F.Dom.vc ->
            'a GVC_F.Dom.vc ->
            'a GVC_F.Dom.vc ->
            'a GVC_F.Dom.vc ->
            ('a GVC_F.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GVC_F.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_F.Dom.vc
          val update :
            'a GVC_F.Dom.vc ->
            'a GVC_F.Dom.vc ->
            'a GVC_F.Dom.vc ->
            'a GVC_F.Dom.vc ->
            ('a GVC_F.Dom.vc -> 'b) ->
            ('a, GVC_F.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_F.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val decl :
          ('a, GVC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val updt :
          'a GVC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_F.vo ->
          'a GVC_F.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GVC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GVC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_F.vo ->
          'a GVC_F.vo ->
          (([> `TLower of 'a GVC_F.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GVC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_F.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_F.contr
        val get_input :
          'a GVC_F.vc ->
          'b ->
          ('b -> 'a GVC_F.vc * ('a, int) Direct.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_F.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_F.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_F.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GVC_F.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_F.contr * GVC_F.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_F.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_F.contr * GVC_F.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_F.contr * GVC_F.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GVC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_F.vo ->
                    'a GVC_F.vo ->
                    (([> `TLower of 'a GVC_F.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_F.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                      val decl :
                        ('a, GVC_F.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                      val updt :
                        'a GVC_F.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GVC_F.vo ->
                        'a GVC_F.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_F.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_F.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_F.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module G_GAC_I :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GAC_I).wmatrix = {
      matrix : 'a GAC_I.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GAC_I).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GAC_I).curposval = {
      p : 'a curpos;
      curval : ('a, GAC_I.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GAC_I.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GAC_I.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GAC_I.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GAC_I.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GAC_I.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GAC_I.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GAC_I.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GAC_I.Dom.vc ->
          ([> `TDet of 'c * ('a, GAC_I.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GAC_I.Dom.v ref) Direct.abstract ]
           as 'a)
          list -> ('a list -> ('b, GAC_I.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_I.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GAC_I.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_I.Dom.vc
          val update :
            'a GAC_I.Dom.vc ->
            'a GAC_I.Dom.vc ->
            'a GAC_I.Dom.vc ->
            'a GAC_I.Dom.vc ->
            ('a GAC_I.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GAC_I.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_I.Dom.vc
          val update :
            'a GAC_I.Dom.vc ->
            'a GAC_I.Dom.vc ->
            'a GAC_I.Dom.vc ->
            'a GAC_I.Dom.vc ->
            ('a GAC_I.Dom.vc -> 'b) ->
            ('a, GAC_I.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GAC_I.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GAC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val decl :
          ('a, GAC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val updt :
          'a GAC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_I.vo ->
          'a GAC_I.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GAC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GAC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GAC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_I.vo ->
          'a GAC_I.vo ->
          (([> `TLower of 'a GAC_I.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GAC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GAC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GAC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_I.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GAC_I.contr
        val get_input :
          'a GAC_I.vc ->
          'b ->
          ('b -> 'a GAC_I.vc * ('a, int) Direct.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GAC_I.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GAC_I.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GAC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GAC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_I.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GAC_I.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GAC_I.contr * GAC_I.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GAC_I.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_I.contr * GAC_I.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_I.contr * GAC_I.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GAC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_I.vo ->
                    'a GAC_I.vo ->
                    (([> `TLower of 'a GAC_I.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GAC_I.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                      val decl :
                        ('a, GAC_I.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                      val updt :
                        'a GAC_I.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GAC_I.vo ->
                        'a GAC_I.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_I.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_I.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_I.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module G_GVC_I :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GVC_I).wmatrix = {
      matrix : 'a GVC_I.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GVC_I).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GVC_I).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_I.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GVC_I.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GVC_I.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_I.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GVC_I.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_I.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GVC_I.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_I.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GVC_I.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_I.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_I.Dom.v ref) Direct.abstract ]
           as 'a)
          list -> ('a list -> ('b, GVC_I.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_I.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GVC_I.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_I.Dom.vc
          val update :
            'a GVC_I.Dom.vc ->
            'a GVC_I.Dom.vc ->
            'a GVC_I.Dom.vc ->
            'a GVC_I.Dom.vc ->
            ('a GVC_I.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GVC_I.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_I.Dom.vc
          val update :
            'a GVC_I.Dom.vc ->
            'a GVC_I.Dom.vc ->
            'a GVC_I.Dom.vc ->
            'a GVC_I.Dom.vc ->
            ('a GVC_I.Dom.vc -> 'b) ->
            ('a, GVC_I.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_I.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val decl :
          ('a, GVC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val updt :
          'a GVC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_I.vo ->
          'a GVC_I.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GVC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GVC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_I.vo ->
          'a GVC_I.vo ->
          (([> `TLower of 'a GVC_I.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_I.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GVC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_I.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_I.contr
        val get_input :
          'a GVC_I.vc ->
          'b ->
          ('b -> 'a GVC_I.vc * ('a, int) Direct.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_I.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_I.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_I.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GVC_I.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_I.contr * GVC_I.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_I.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_I.contr * GVC_I.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_I.contr * GVC_I.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GVC_I.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_I.vo ->
                    'a GVC_I.vo ->
                    (([> `TLower of 'a GVC_I.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_I.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                      val decl :
                        ('a, GVC_I.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                      val updt :
                        'a GVC_I.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GVC_I.vo ->
                        'a GVC_I.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_I.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_I.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_I.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module G_GAC_R :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GAC_R).wmatrix = {
      matrix : 'a GAC_R.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GAC_R).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GAC_R).curposval = {
      p : 'a curpos;
      curval : ('a, GAC_R.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GAC_R.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GAC_R.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GAC_R.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GAC_R.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GAC_R.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GAC_R.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GAC_R.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GAC_R.Dom.vc ->
          ([> `TDet of 'c * ('a, GAC_R.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GAC_R.Dom.v ref) Direct.abstract ]
           as 'a)
          list -> ('a list -> ('b, GAC_R.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_R.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GAC_R.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_R.Dom.vc
          val update :
            'a GAC_R.Dom.vc ->
            'a GAC_R.Dom.vc ->
            'a GAC_R.Dom.vc ->
            'a GAC_R.Dom.vc ->
            ('a GAC_R.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GAC_R.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GAC_R.Dom.vc
          val update :
            'a GAC_R.Dom.vc ->
            'a GAC_R.Dom.vc ->
            'a GAC_R.Dom.vc ->
            'a GAC_R.Dom.vc ->
            ('a GAC_R.Dom.vc -> 'b) ->
            ('a, GAC_R.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GAC_R.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GAC_R.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val decl :
          ('a, GAC_R.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val updt :
          'a GAC_R.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_R.vo ->
          'a GAC_R.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GAC_R.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GAC_R.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GAC_R.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_R.vo ->
          'a GAC_R.vo ->
          (([> `TLower of 'a GAC_R.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GAC_R.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GAC_R.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GAC_R.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_R.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_R.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GAC_R.contr
        val get_input :
          'a GAC_R.vc ->
          'b ->
          ('b -> 'a GAC_R.vc * ('a, int) Direct.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GAC_R.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GAC_R.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GAC_R.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GAC_R.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_R.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_R.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GAC_R.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_R.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GAC_R.contr * GAC_R.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_R.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GAC_R.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_R.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_R.contr * GAC_R.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GAC_R.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_R.contr * GAC_R.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GAC_R.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GAC_R.vo ->
                    'a GAC_R.vo ->
                    (([> `TLower of 'a GAC_R.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GAC_R.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                      val decl :
                        ('a, GAC_R.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                      val updt :
                        'a GAC_R.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GAC_R.vo ->
                        'a GAC_R.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_R.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_R.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_R.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module G_GVC_Z3 :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GVC_Z3).wmatrix = {
      matrix : 'a GVC_Z3.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GVC_Z3).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GVC_Z3).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_Z3.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GVC_Z3.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GVC_Z3.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_Z3.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GVC_Z3.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_Z3.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GVC_Z3.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_Z3.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GVC_Z3.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_Z3.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_Z3.Dom.v ref) Direct.abstract ]
           as 'a)
          list -> ('a list -> ('b, GVC_Z3.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_Z3.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GVC_Z3.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_Z3.Dom.vc
          val update :
            'a GVC_Z3.Dom.vc ->
            'a GVC_Z3.Dom.vc ->
            'a GVC_Z3.Dom.vc ->
            'a GVC_Z3.Dom.vc ->
            ('a GVC_Z3.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GVC_Z3.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_Z3.Dom.vc
          val update :
            'a GVC_Z3.Dom.vc ->
            'a GVC_Z3.Dom.vc ->
            'a GVC_Z3.Dom.vc ->
            'a GVC_Z3.Dom.vc ->
            ('a GVC_Z3.Dom.vc -> 'b) ->
            ('a, GVC_Z3.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_Z3.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val decl :
          ('a, GVC_Z3.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val updt :
          'a GVC_Z3.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z3.vo ->
          'a GVC_Z3.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GVC_Z3.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z3.vo ->
          'a GVC_Z3.vo ->
          (([> `TLower of 'a GVC_Z3.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GVC_Z3.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z3.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_Z3.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_Z3.contr
        val get_input :
          'a GVC_Z3.vc ->
          'b ->
          ('b -> 'a GVC_Z3.vc * ('a, int) Direct.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_Z3.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_Z3.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z3.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z3.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_Z3.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z3.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GVC_Z3.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z3.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_Z3.contr * GVC_Z3.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z3.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_Z3.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z3.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z3.contr * GVC_Z3.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z3.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z3.contr * GVC_Z3.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GVC_Z3.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z3.vo ->
                    'a GVC_Z3.vo ->
                    (([> `TLower of 'a GVC_Z3.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_Z3.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                      val decl :
                        ('a, GVC_Z3.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                      val updt :
                        'a GVC_Z3.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GVC_Z3.vo ->
                        'a GVC_Z3.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z3.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z3.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z3.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module G_GVC_Z19 :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GVC_Z19).wmatrix = {
      matrix : 'a GVC_Z19.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GVC_Z19).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GVC_Z19).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_Z19.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GVC_Z19.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GVC_Z19.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_Z19.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GVC_Z19.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_Z19.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GVC_Z19.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_Z19.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GVC_Z19.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_Z19.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GVC_Z19.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('b, GVC_Z19.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_Z19.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GVC_Z19.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_Z19.Dom.vc
          val update :
            'a GVC_Z19.Dom.vc ->
            'a GVC_Z19.Dom.vc ->
            'a GVC_Z19.Dom.vc ->
            'a GVC_Z19.Dom.vc ->
            ('a GVC_Z19.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GVC_Z19.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GVC_Z19.Dom.vc
          val update :
            'a GVC_Z19.Dom.vc ->
            'a GVC_Z19.Dom.vc ->
            'a GVC_Z19.Dom.vc ->
            'a GVC_Z19.Dom.vc ->
            ('a GVC_Z19.Dom.vc -> 'b) ->
            ('a, GVC_Z19.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_Z19.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val decl :
          ('a, GVC_Z19.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val updt :
          'a GVC_Z19.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z19.vo ->
          'a GVC_Z19.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GVC_Z19.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z19.vo ->
          'a GVC_Z19.vo ->
          (([> `TLower of 'a GVC_Z19.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GVC_Z19.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z19.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_Z19.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_Z19.contr
        val get_input :
          'a GVC_Z19.vc ->
          'b ->
          ('b -> 'a GVC_Z19.vc * ('a, int) Direct.abstract * bool -> 'c) ->
          'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_Z19.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_Z19.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z19.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z19.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_Z19.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z19.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GVC_Z19.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z19.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_Z19.contr * GVC_Z19.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z19.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_Z19.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z19.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z19.contr * GVC_Z19.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GVC_Z19.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z19.contr * GVC_Z19.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GVC_Z19.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GVC_Z19.vo ->
                    'a GVC_Z19.vo ->
                    (([> `TLower of 'a GVC_Z19.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_Z19.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                      val decl :
                        ('a, GVC_Z19.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                      val updt :
                        'a GVC_Z19.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GVC_Z19.vo ->
                        'a GVC_Z19.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z19.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z19.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z19.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module G_GFC_F :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Direct).GenLA(GFC_F).wmatrix = {
      matrix : 'a GFC_F.vc;
      numrow : ('a, int) Direct.abstract;
      numcol : ('a, int) Direct.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Direct).GenLA(GFC_F).curpos = {
      rowpos : ('a, int) Direct.abstract;
      colpos : ('a, int) Direct.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Direct).GenLA(GFC_F).curposval = {
      p : 'a curpos;
      curval : ('a, GFC_F.Dom.v) Direct.abstract;
    }
    module type DETERMINANT =
      sig
        type tdet = GFC_F.Dom.v ref
        type 'a lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
        val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val acc :
          ('a, GFC_F.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GFC_F.Dom.v) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GFC_F.Dom.v) lm
      end
    module NoDet :
      sig
        type tdet = GFC_F.Dom.v ref
        type 'a lstate = unit
        val decl :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GFC_F.Dom.v ref) Direct.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
        val fin : unit -> 'a
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
      end
    module AbstractDet :
      sig
        type tdet = GFC_F.Dom.v ref
        type 'a lstate =
            ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TDet of 'a ] list -> 'a
        val dfetch :
          unit ->
          ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val dstore :
          'a ->
          ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GFC_F.Dom.v ref) Direct.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Direct.abstract -> ('b, 'd) Direct.abstract) ->
          ('b, 'd) Direct.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Direct.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Direct.abstract -> 'd) -> 'd
        val acc :
          'a GFC_F.Dom.vc ->
          ([> `TDet of 'c * ('a, GFC_F.Dom.v ref) Direct.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Direct.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Direct.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) Direct.abstract *
                ('b, GFC_F.Dom.v ref) Direct.abstract ]
           as 'a)
          list -> ('a list -> ('b, GFC_F.Dom.v) Direct.abstract -> 'c) -> 'c
      end
    module type UPDATE =
      functor (D : DETERMINANT) ->
        sig
          type 'a in_val = 'a GFC_F.Dom.vc
          val update :
            'a in_val ->
            'a in_val ->
            'a in_val ->
            'a in_val ->
            ('a in_val -> ('a, unit) Direct.abstract) ->
            ('a, GFC_F.Dom.v ref) Direct.abstract ->
            ('a, unit, 'b, 'c) GEF.cmonad
          val update_det :
            'a in_val -> ('a * [> 'a D.tag_lstate ] * 'b, unit) D.lm
          val upd_kind : Ge.update_kind
        end
    module DivisionUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GFC_F.Dom.vc
          val update :
            'a GFC_F.Dom.vc ->
            'a GFC_F.Dom.vc ->
            'a GFC_F.Dom.vc ->
            'a GFC_F.Dom.vc ->
            ('a GFC_F.Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
          val update_det :
            ('a, GFC_F.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module FractionFreeUpdate :
      functor (Det : DETERMINANT) ->
        sig
          type 'a in_val = 'a GFC_F.Dom.vc
          val update :
            'a GFC_F.Dom.vc ->
            'a GFC_F.Dom.vc ->
            'a GFC_F.Dom.vc ->
            'a GFC_F.Dom.vc ->
            ('a GFC_F.Dom.vc -> 'b) ->
            ('a, GFC_F.Dom.v ref) Direct.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GFC_F.Dom.v) Direct.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GFC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
        val decl :
          ('a, GFC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
        val updt :
          'a GFC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GFC_F.vo ->
          'a GFC_F.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GFC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
      end
    module SeparateLower :
      sig
        type 'a lstate = ('a, GFC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          ('a, 'b) Direct.abstract ->
          ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
          ('a, 'd) Direct.abstract
        val updt :
          'a GFC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GFC_F.vo ->
          'a GFC_F.vo ->
          (([> `TLower of 'a GFC_F.vc ] as 'b) list ->
           ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
          option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GFC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> 'a -> 'c) -> 'c
        val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
        val fin :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GFC_F.contr) Direct.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TLower of 'a ] list -> 'a
        val mfetch :
          unit ->
          ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val mstore :
          'a ->
          ([> `TLower of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
        val updt :
          'a GFC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GFC_F.vo ->
          'b -> ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd) option
        val fin : unit -> 'a
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GFC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GFC_F.contr
        val get_input :
          'a GFC_F.vc ->
          'b ->
          ('b -> 'a GFC_F.vc * ('a, int) Direct.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GFC_F.contr * int
        val get_input :
          ('a, 'b * 'c) Direct.abstract ->
          'd ->
          ('d ->
           ('a, 'b) Direct.abstract * ('a, 'c) Direct.abstract * bool -> 'e) ->
          'e
      end
    module type PIVOT =
      functor (D : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GFC_F.Dom.v option,
               [> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ], 'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GFC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a Det.lstate | `TPivot of 'a P.lstate ] as 'b)
              list ->
              ('b list ->
               ('a, GFC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GFC_F.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module type OUTPUTDEP =
      sig module PivotRep : GEF.PIVOTKIND module Det : DETERMINANT end
    module OutJustMatrix :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GFC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr
          val make_result :
            'a wmatrix -> 'b -> ('b -> 'a GFC_F.vc -> 'c) -> 'c
        end
    module OutDet :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GFC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GFC_F.contr * GFC_F.Dom.v) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GFC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, GFC_F.contr * 'c) Direct.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val colrep :
                    'a GEF.PermList.ira ->
                    'a GEF.PermList.ira -> 'a GEF.PermList.fra
                  val decl :
                    'a ->
                    'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : unit -> 'a
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GFC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GFC_F.contr * GFC_F.Dom.v * 'c) Direct.abstract ->
             ('a, 'd) Direct.abstract) ->
            ('a, 'd) Direct.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin :
                    unit ->
                    ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
                  val updt :
                    'a GFC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Direct.abstract -> 'd) -> 'd)
                    option
                  val fin : unit -> 'a
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Direct.abstract
              | `TRan of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GFC_F.contr * GFC_F.Dom.v * 'd * 'c) Direct.abstract ->
             ('a, 'e) Direct.abstract) ->
            ('a, 'e) Direct.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    ('a, 'b) Direct.abstract ->
                    ([> `TLower of ('a, 'b) Direct.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val updt :
                    'a GFC_F.vc ->
                    ('a, int) Direct.abstract ->
                    ('a, int) Direct.abstract ->
                    'a GFC_F.vo ->
                    'a GFC_F.vo ->
                    (([> `TLower of 'a GFC_F.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Direct.abstract
              | `TPivot of ('a, 'd ref) Direct.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GFC_F.contr * 'c * 'd) Direct.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Direct.abstract
                  type 'a tag_lstate_ = [ `TRan of 'a lstate ]
                  type 'a tag_lstate = 'a tag_lstate_
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TRan of 'a ] list -> 'a
                  val rfetch :
                    unit ->
                    ([> `TRan of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val rstore :
                    'a ->
                    ([> `TRan of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Direct.abstract ->
                     ('b, 'c) Direct.abstract) ->
                    ('b, 'c) Direct.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Direct.abstract -> 'c) -> 'c
                  module type RANK =
                    sig
                      type 'a tag_lstate = 'a tag_lstate_
                      val rfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                      val succ :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                    end
                  val fin : unit -> 'a
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Direct.abstract
                  type 'a tag_lstate = [ `TPivot of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val rowrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val colrep :
                    'a OD.PivotRep.ira ->
                    'a OD.PivotRep.ira -> 'a OD.PivotRep.fra
                  val fetch_iter : [> `TPivot of 'a ] list -> 'a
                  val pfetch :
                    unit ->
                    ([> `TPivot of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val pstore :
                    'a ->
                    ([> `TPivot of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a OD.PivotRep.ira ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                    'c
                  val fin :
                    unit ->
                    ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                    ('a list -> ('b, 'c) Direct.abstract -> 'd) -> 'd
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                  type 'a tag_lstate = [ `TLower of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val fetch_iter : [> `TLower of 'a ] list -> 'a
                  val mfetch :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val mstore :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> unit -> 'c) -> 'c
                  val decl :
                    'a ->
                    ([> `TLower of 'a ] as 'b) list ->
                    ('b list -> 'a -> 'c) -> 'c
                  val updt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f option
                  val fin :
                    unit ->
                    ([> `TLower of 'b ] as 'a) list ->
                    ('a list -> 'b -> 'c) -> 'c
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Direct.abstract
              | `TPivot of ('c, 'e ref) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
        end
    module type INTERNAL_FEATURES =
      sig
        module R : GEF.TrackRank.RANK
        module P : GEF.TRACKPIVOT
        module L : LOWER
      end
    module type OUTPUT =
      functor (OD : OUTPUTDEP) ->
        sig
          module IF : INTERNAL_FEATURES
          type res
          val make_result :
            'a wmatrix ->
            ('a, res,
             [> `TDet of 'a OD.Det.lstate
              | `TLower of 'a IF.L.lstate
              | `TPivot of 'a IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ],
             'b)
            GEF.cmonad
        end
    module type FEATURES =
      sig
        module Det : DETERMINANT
        module PivotF : PIVOT
        module PivotRep : GEF.PIVOTKIND
        module Update : UPDATE
        module Input : INPUT
        module Output : OUTPUT
      end
    module GenGE :
      functor (F : FEATURES) ->
        sig
          module O :
            sig
              module IF :
                sig
                  module R :
                    sig
                      type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                      val rfetch :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val decl :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                        GEF.TrackRank.lm
                      val succ :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                        GEF.TrackRank.lm
                      val fin :
                        unit ->
                        ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                        GEF.TrackRank.lm
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Direct.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Direct.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                        StateCPSMonad.monad
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GFC_F.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
                      val decl :
                        ('a, GFC_F.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
                      val updt :
                        'a GFC_F.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a GFC_F.vo ->
                        'a GFC_F.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
                      val wants_pack : bool
                    end
                end
              type res = F.Output(F).res
              val make_result :
                'a wmatrix ->
                ('a, res,
                 [> `TDet of 'a F.Det.lstate
                  | `TLower of 'a IF.L.lstate
                  | `TPivot of 'a IF.P.lstate
                  | `TRan of 'a GEF.TrackRank.lstate ],
                 'b)
                GEF.cmonad
            end
          val wants_pack : bool
          val back_elim : bool
          val can_pack : bool
          val zerobelow :
            'a wmatrix ->
            'a curposval ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GFC_F.contr) Direct.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GFC_F.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Direct.abstract *
             ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
             ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Direct.abstract *
            ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GFC_F.contr) Direct.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
          val gen :
            ('a, F.Input.inp) Direct.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
            ('a, 'c) Direct.abstract
        end
  end
module GenFA1 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA2 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA3 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA4 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA11 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA12 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA13 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA14 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA24 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int * Direct.perm list
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA25 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = int array
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, int * int) Direct.abstract
                type 'a pra = ('a, int array) Direct.abstract
                type 'a lstate = ('a, int array ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int * int array
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, int array ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, int array ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA26 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA5 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA6 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA7 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA8 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr * int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA9 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * Direct.perm list
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA31 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.contr * Direct.perm list
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA32 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * Direct.perm list
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_F.wmatrix ->
      'a G_GAC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV1 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_F.wmatrix ->
      'a G_GVC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GVC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV2 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * GVC_F.Dom.v
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_F.wmatrix ->
      'a G_GVC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV3 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * int
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_F.wmatrix ->
      'a G_GVC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GVC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV4 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * GVC_F.Dom.v * int
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_F.wmatrix ->
      'a G_GVC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV5 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * GVC_F.Dom.v * int
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_F.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_F.wmatrix ->
      'a G_GVC_F.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_F.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV6 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * GVC_F.contr * Direct.perm list
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_F.wmatrix ->
      'a G_GVC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GVC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV7 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * Direct.perm list
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_F.wmatrix ->
      'a G_GVC_F.curposval ->
      ([> `TDet of unit | `TLower of ('a, GVC_F.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_F.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA1 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_I.wmatrix ->
      'a G_GAC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
- : unit = ()
module GenIA2 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr * GAC_I.Dom.v
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_I.wmatrix ->
      'a G_GAC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA3 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr * int
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_I.wmatrix ->
      'a G_GAC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA4 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr * GAC_I.Dom.v * int
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_I.wmatrix ->
      'a G_GAC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
- : unit = ()
- : unit = ()
module GenIV1 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_I.wmatrix ->
      'a G_GVC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV2 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_I.wmatrix ->
      'a G_GVC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV3 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * int
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_I.wmatrix ->
      'a G_GVC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV4 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v * int
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_I.wmatrix ->
      'a G_GVC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV5 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v * int
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_I.wmatrix ->
      'a G_GVC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV6 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v * int * Direct.perm list
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_I.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_I.wmatrix ->
      'a G_GVC_I.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_I.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_I.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_I.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA1 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_R.contr
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_R.wmatrix ->
      'a G_GAC_R.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_R.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA2 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_R.contr * GAC_R.Dom.v
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_R.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_R.wmatrix ->
      'a G_GAC_R.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_R.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA3 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_R.contr * int
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of unit
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_R.wmatrix ->
      'a G_GAC_R.curposval ->
      ([> `TDet of unit | `TLower of ('a, GAC_R.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA4 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val wants_pack : bool
              end
          end
        type res = GAC_R.contr * GAC_R.Dom.v * int
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GAC_R.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GAC_R.wmatrix ->
      'a G_GAC_R.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_R.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of ('a, GAC_R.contr) Direct.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GAC_R.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GAC_R.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenZp3 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_Z3.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                val decl :
                  ('a, GVC_Z3.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                val updt :
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  'a GVC_Z3.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_Z3.contr * GVC_Z3.Dom.v * int * Direct.perm list
        val make_result :
          'a G_GVC_Z3.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_Z3.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_Z3.wmatrix ->
      'a G_GVC_Z3.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z3.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_Z3.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_Z3.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z3.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_Z3.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_Z3.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_Z3.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z3.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_Z3.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_Z3.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z3.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_Z3.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z3.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenZp19 :
  sig
    module O :
      sig
        module IF :
          sig
            module R :
              sig
                type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                val rfetch :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val decl :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int ref)
                  GEF.TrackRank.lm
                val succ :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, unit)
                  GEF.TrackRank.lm
                val fin :
                  unit ->
                  ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                  GEF.TrackRank.lm
              end
            module P :
              sig
                type perm_rep = Direct.perm list
                type 'a ira = ('a, int) Direct.abstract
                type 'a fra = ('a, Direct.perm) Direct.abstract
                type 'a pra = ('a, Direct.perm list) Direct.abstract
                type 'a lstate = ('a, Direct.perm list ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Direct.abstract option,
                   [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
                  StateCPSMonad.monad
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_Z19.contr) Direct.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                val decl :
                  ('a, GVC_Z19.contr) Direct.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                val updt :
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  'a GVC_Z19.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                val wants_pack : bool
              end
          end
        type res = GVC_Z19.contr * GVC_Z19.Dom.v * int * Direct.perm list
        val make_result :
          'a G_GVC_Z19.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Direct.abstract *
                ('a, GVC_Z19.Dom.v ref) Direct.abstract
            | `TLower of 'a IF.L.lstate
            | `TPivot of 'a IF.P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    val wants_pack : bool
    val back_elim : bool
    val can_pack : bool
    val zerobelow :
      'a G_GVC_Z19.wmatrix ->
      'a G_GVC_Z19.curposval ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z19.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_Z19.contr) Direct.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_Z19.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z19.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_Z19.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_Z19.wmatrix * ('a, int ref) Direct.abstract *
       ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
       ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val forward_elim :
      'a G_GVC_Z19.wmatrix * ('a, int ref) Direct.abstract *
      ('a, int ref) Direct.abstract * ('a, int) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z19.Dom.v ref) Direct.abstract
        | `TLower of ('a, GVC_Z19.contr) Direct.abstract
        | `TPivot of ('a, Direct.perm list ref) Direct.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Direct.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_Z19.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z19.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
    val gen :
      ('a, GVC_Z19.contr) Direct.abstract ->
      ([> `TDet of
            ('a, int ref) Direct.abstract *
            ('a, GVC_Z19.Dom.v ref) Direct.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
val resFA1 : GAC_F.contr -> GenFA1.O.res = <fun>
val resFA2 : GAC_F.contr -> GenFA2.O.res = <fun>
val resFA3 : GAC_F.contr -> GenFA3.O.res = <fun>
val resFA4 : GAC_F.contr -> GenFA4.O.res = <fun>
val resFV1 : GVC_F.contr -> GenFV1.O.res = <fun>
val resFV2 : GVC_F.contr -> GenFV2.O.res = <fun>
val resFV3 : GVC_F.contr -> GenFV3.O.res = <fun>
val resFV4 : GVC_F.contr -> GenFV4.O.res = <fun>
val resFV5 : GVC_F.contr -> GenFV5.O.res = <fun>
val resIA1 : GAC_I.contr -> GenIA1.O.res = <fun>
val resIA2 : GAC_I.contr -> GenIA2.O.res = <fun>
val resIA3 : GAC_I.contr -> GenIA3.O.res = <fun>
val resIA4 : GAC_I.contr -> GenIA4.O.res = <fun>
val resIV1 : GVC_I.contr -> GenIV1.O.res = <fun>
val resIV2 : GVC_I.contr -> GenIV2.O.res = <fun>
val resIV3 : GVC_I.contr -> GenIV3.O.res = <fun>
val resIV4 : GVC_I.contr -> GenIV4.O.res = <fun>
val resIV5 : GVC_I.contr -> GenIV5.O.res = <fun>
val resIV6 : GVC_I.contr -> GenIV6.O.res = <fun>
val resFA11 : GAC_F.contr -> GenFA11.O.res = <fun>
val resFA12 : GAC_F.contr -> GenFA12.O.res = <fun>
val resFA13 : GAC_F.contr -> GenFA13.O.res = <fun>
val resFA14 : GAC_F.contr -> GenFA14.O.res = <fun>
val resFA24 : GAC_F.contr -> GenFA24.O.res = <fun>
val resFA25 : GAC_F.contr -> GenFA25.O.res = <fun>
val resFA26 : GAC_F.contr -> GenFA26.O.res = <fun>
val resRA1 : GAC_R.contr -> GenRA1.O.res = <fun>
val resRA2 : GAC_R.contr -> GenRA2.O.res = <fun>
val resRA3 : GAC_R.contr -> GenRA3.O.res = <fun>
val resRA4 : GAC_R.contr -> GenRA4.O.res = <fun>
val resFA5 : GAC_F.contr * int -> GenFA5.O.res = <fun>
val resFA6 : GAC_F.contr * int -> GenFA6.O.res = <fun>
val resFA7 : GAC_F.contr * int -> GenFA7.O.res = <fun>
val resFA8 : GAC_F.contr * int -> GenFA8.O.res = <fun>
val resFA9 : GAC_F.contr -> GenFA9.O.res = <fun>
val resFA31 : GAC_F.contr -> GenFA31.O.res = <fun>
val resFA32 : GAC_F.contr -> GenFA32.O.res = <fun>
val resZp3 : GVC_Z3.contr -> GenZp3.O.res = <fun>
val resZp19 : GVC_Z19.contr -> GenZp19.O.res = <fun>
val rFA1 : GAC_F.contr -> GenFA1.O.res = <fun>
val rFA2 : GAC_F.contr -> GenFA2.O.res = <fun>
val rFA3 : GAC_F.contr -> GenFA3.O.res = <fun>
val rFA4 : GAC_F.contr -> GenFA4.O.res = <fun>
val rFV1 : GVC_F.contr -> GenFV1.O.res = <fun>
val rFV2 : GVC_F.contr -> GenFV2.O.res = <fun>
val rFV3 : GVC_F.contr -> GenFV3.O.res = <fun>
val rFV4 : GVC_F.contr -> GenFV4.O.res = <fun>
val rFV5 : GVC_F.contr -> GenFV5.O.res = <fun>
val rIA1 : GAC_I.contr -> GenIA1.O.res = <fun>
val rIA2 : GAC_I.contr -> GenIA2.O.res = <fun>
val rIA3 : GAC_I.contr -> GenIA3.O.res = <fun>
val rIA4 : GAC_I.contr -> GenIA4.O.res = <fun>
val rIV1 : GVC_I.contr -> GenIV1.O.res = <fun>
val rIV2 : GVC_I.contr -> GenIV2.O.res = <fun>
val rIV3 : GVC_I.contr -> GenIV3.O.res = <fun>
val rIV4 : GVC_I.contr -> GenIV4.O.res = <fun>
val rIV5 : GVC_I.contr -> GenIV5.O.res = <fun>
val rIV6 : GVC_I.contr -> GenIV6.O.res = <fun>
val rFA11 : GAC_F.contr -> GenFA11.O.res = <fun>
val rFA12 : GAC_F.contr -> GenFA12.O.res = <fun>
val rFA13 : GAC_F.contr -> GenFA13.O.res = <fun>
val rFA14 : GAC_F.contr -> GenFA14.O.res = <fun>
val rFA24 : GAC_F.contr -> GenFA24.O.res = <fun>
val rFA25 : GAC_F.contr -> GenFA25.O.res = <fun>
val rFA26 : GAC_F.contr -> GenFA26.O.res = <fun>
val rRA1 : GAC_R.contr -> GenRA1.O.res = <fun>
val rRA2 : GAC_R.contr -> GenRA2.O.res = <fun>
val rRA3 : GAC_R.contr -> GenRA3.O.res = <fun>
val rRA4 : GAC_R.contr -> GenRA4.O.res = <fun>
val rFA5 : GAC_F.contr * int -> GenFA5.O.res = <fun>
val rFA6 : GAC_F.contr * int -> GenFA6.O.res = <fun>
val rFA7 : GAC_F.contr * int -> GenFA7.O.res = <fun>
val rFA8 : GAC_F.contr * int -> GenFA8.O.res = <fun>
val rFA9 : GAC_F.contr -> GenFA9.O.res = <fun>
val rFA31 : GAC_F.contr -> GenFA31.O.res = <fun>
val rFA32 : GAC_F.contr -> GenFA32.O.res = <fun>
val rZp3 : GVC_Z3.contr -> GenZp3.O.res = <fun>
val rZp19 : GVC_Z19.contr -> GenZp19.O.res = <fun>
val ia0 : int array array = [|[|1|]|]
val ia1 : int array array = [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|]
val ia2 : int array array =
  [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|]
val ia3 : int array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|]
val ia4 : int array array = [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]
val ia5 : int array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|];
   [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|];
   [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]]
val resI11 : GenIA1.O.res list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|]]
val resI12 : GenIA2.O.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0)]
val resI13 : GenIA3.O.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 2)]
val resI14 : GenIA4.O.res list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0, 2)]
val iv0 : int Domains_direct.container2dfromvector =
  {arr = [|1|]; n = 1; m = 1}
val iv1 : int Domains_direct.container2dfromvector =
  {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3}
val iv2 : int Domains_direct.container2dfromvector =
  {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4}
val iv4 : int Domains_direct.container2dfromvector =
  {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}
val iv5 : int Domains_direct.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}]
val resI21 : GenIV1.O.res list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}]
val resI22 : GenIV2.O.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0)]
val resI23 : GenIV3.O.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 2)]
val resI24 : GenIV4.O.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
val resI25 : GenIV5.O.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
val resI26 : GenIV6.O.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1, []);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3,
    [Direct.RowSwap (2, 1); Direct.ColSwap (2, 1)]);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3,
    [Direct.RowSwap (2, 1); Direct.ColSwap (2, 1)]);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2,
    [Direct.RowSwap (2, 1); Direct.ColSwap (2, 1); Direct.ColSwap (1, 0)])]
val fa0 : float array array = [|[|1.|]|]
val fa1 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|]
val fa2 : float array array =
  [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|]
val fa3 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|]
val fa4 : float array array =
  [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]
val fa5 : float array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]]
val fa6 : float array array = [|[|1.; 1.|]|]
val fa7 : float array array =
  [|[|1.; 2.; 3.; 1.; 0.; 0.|]; [|4.; 13.; 5.; 0.; 1.; 0.|];
    [|-1.; 3.; 0.; 0.; 0.; 1.|]|]
- : unit = ()
- : unit = ()
- : unit = ()
- : unit = ()
- : unit = ()
- : unit = ()
val resFA9 : GenFA9.O.res =
  ([|[|4.; 13.; 5.|]; [|-1.; 6.25; 1.25|]; [|1.; -1.25; 2.|]|],
   [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)])
- : unit = ()
- : unit = ()
- : unit = ()
val resF1 : GenFA1.O.res list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
- : unit = ()
- : unit = ()
- : unit = ()
val resFA31 : GenFA31.O.res list =
  [([|[|1.|]|], [|[|1.|]|], []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|],
    [|[|1.; 0.; 0.|]; [|0.25; 1.; 0.|]; [|-0.25; -0.2; 1.|]|],
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|],
    [|[|1.; 0.; 0.; 0.|]; [|0.25; 1.; 0.; 0.|]; [|-0.25; -0.2; 1.; 0.|]|],
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    [|[|1.; 0.; 0.|]; [|0.25; 1.; 0.|]; [|-0.25; -0.2; 1.|]; [|0.; 0.; 0.|]|],
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    [|[|1.; 0.; 0.|]; [|0.; 0.2; 0.|]; [|0.; 0.3; -0.75|]|],
    [Direct.RowSwap (1, 0)])]
val resFA32 : GenFA32.O.res list =
  [([|[|1.|]|], []);
   ([|[|4.; 13.; 5.|]; [|-1.; 6.25; 1.25|]; [|1.; -1.25; 2.|]|],
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|-1.; 6.25; 1.25; 0.|]; [|1.; -1.25; 2.; 0.|]|],
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|-1.; 6.25; 1.25|]; [|1.; -1.25; 2.|];
      [|0.; 0.; 0.|]|],
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 2.; 2.|]; [|0.; 3.; -1.5|]|],
    [Direct.RowSwap (1, 0)])]
val a2v : 'a array array -> 'a Domains_direct.container2dfromvector = <fun>
val xxx : GAC_F.Dom.v Domains_direct.container2dfromvector list =
  [{arr = [|1.|]; n = 1; m = 1};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.|]; n = 3; m = 3};
   {arr = [|1.; 2.; 3.; 0.; 4.; 13.; 5.; 0.; -1.; 3.; 0.; 0.|]; n = 3; m = 4};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.; 0.; 0.; 0.|]; n = 4; m = 3};
   {arr = [|0.; 2.; 3.; 0.; 10.; 5.; 0.; 3.; 0.|]; n = 3; m = 3}]
- : unit = ()
val resFV5 : GenFV5.O.res list =
  [({arr = [|1.|]; n = 1; m = 1}, 1., 1);
   ({arr =
      [|13.; 5.; 4.; 0.; 2.23076923076923084; 0.384615384615384581; 0.; 0.;
        -1.72413793103448287|];
     n = 3; m = 3},
    50., 3);
   ({arr =
      [|13.; 5.; 4.; 0.; 0.; 2.23076923076923084; 0.384615384615384581; 0.;
        0.; 0.; -1.72413793103448287; 0.|];
     n = 3; m = 4},
    50., 3);
   ({arr =
      [|13.; 5.; 4.; 0.; 2.23076923076923084; 0.384615384615384581; 0.; 0.;
        -1.72413793103448287; 0.; 0.; 0.|];
     n = 4; m = 3},
    50., 3);
   ({arr = [|10.; 5.; 0.; 0.; 2.; 0.; 0.; 0.; 0.|]; n = 3; m = 3}, 0., 2)]
val resF11 : GenFA11.O.res list =
  [[|[|1.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]|];
   [|[|13.; 5.; 4.; 0.|];
     [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
     [|0.; 0.; -1.72413793103448287; 0.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|];
   [|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|]]
val resF12 : GenFA12.O.res list =
  [([|[|1.|]|], 1.);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]|],
    50.);
   ([|[|13.; 5.; 4.; 0.|];
      [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
      [|0.; 0.; -1.72413793103448287; 0.|]|],
    50.);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|],
    50.);
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 0.)]
val resF13 : GenFA13.O.res list =
  [([|[|1.|]|], 1);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]|],
    3);
   ([|[|13.; 5.; 4.; 0.|];
      [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
      [|0.; 0.; -1.72413793103448287; 0.|]|],
    3);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|],
    3);
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 2)]
val resF14 : GenFA14.O.res list =
  [([|[|1.|]|], 1., 1);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]|],
    50., 3);
   ([|[|13.; 5.; 4.; 0.|];
      [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
      [|0.; 0.; -1.72413793103448287; 0.|]|],
    50., 3);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|],
    50., 3);
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 0., 2)]
val resF24 : GenFA24.O.res list =
  [([|[|1.|]|], 1., 1, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2,
    [Direct.RowSwap (1, 0)])]
val resF25 : GenFA25.O.res list =
  [([|[|1.|]|], 1., 1, [|0|]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [|1; 2; 0|]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [|1; 2; 0|]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [|1; 2; 0; 3|]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2, [|1; 0; 2|])]
val resF26 : GenFA26.O.res list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
val ra0 : Num.num array array = [|[|Num.Int 1|]|]
val ra1 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
    [|Num.Int 4; Num.Int 13; Num.Int 5|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0|]|]
val ra2 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
    [|Num.Int 4; Num.Int 13; Num.Int 5; Num.Int 0|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0; Num.Int 0|]|]
val ra3 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
    [|Num.Int 4; Num.Int 13; Num.Int 5|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0|];
    [|Num.Int 0; Num.Int 0; Num.Int 0|]|]
val ra4 : Num.num array array =
  [|[|Num.Int 0; Num.Int 2; Num.Int 3|];
    [|Num.Int 0; Num.Int 13; Num.Int 5|];
    [|Num.Int 0; Num.Int 3; Num.Int 0|]|]
val ra5 : Num.num array array list =
  [[|[|Num.Int 1|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 4; Num.Int 13; Num.Int 5|];
     [|Num.Int (-1); Num.Int 3; Num.Int 0|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
     [|Num.Int 4; Num.Int 13; Num.Int 5; Num.Int 0|];
     [|Num.Int (-1); Num.Int 3; Num.Int 0; Num.Int 0|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 4; Num.Int 13; Num.Int 5|];
     [|Num.Int (-1); Num.Int 3; Num.Int 0|];
     [|Num.Int 0; Num.Int 0; Num.Int 0|]|];
   [|[|Num.Int 0; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 13; Num.Int 5|];
     [|Num.Int 0; Num.Int 3; Num.Int 0|]|]]
val resR11 : GenRA1.O.res list =
  [[|[|Num.Int 1|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
     [|Num.Int 0; Num.Int 0; Num.Int 10|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
     [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
     [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
     [|Num.Int 0; Num.Int 0; Num.Int 10|];
     [|Num.Int 0; Num.Int 0; Num.Int 0|]|];
   [|[|Num.Int 0; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
     [|Num.Int 0; Num.Int 0; Num.Int 0|]|]]
val resR12 : GenRA2.O.res list =
  [([|[|Num.Int 1|]|], Num.Int 1);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|]|],
    Num.Int 50);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
      [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|],
    Num.Int 50);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 50);
   ([|[|Num.Int 0; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 0)]
val resR13 : GenRA3.O.res list =
  [([|[|Num.Int 1|]|], 1);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|]|],
    3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
      [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|],
    3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    3);
   ([|[|Num.Int 0; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    2)]
val resR14 : GenRA4.O.res list =
  [([|[|Num.Int 1|]|], Num.Int 1, 1);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|]|],
    Num.Int 50, 3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
      [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|],
    Num.Int 50, 3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 50, 3);
   ([|[|Num.Int 0; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 0, 2)]
# 
