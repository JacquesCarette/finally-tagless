        Objective Caml version 3.09.1

#               module GEF :
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
            type 'a vc = ('a, v) Code.abstract
            val zeroL : 'a vc
            val oneL : 'a vc
            val ( +^ ) : 'a vc -> 'a vc -> 'a vc
            val ( *^ ) : 'a vc -> 'a vc -> 'a vc
            val ( -^ ) : 'a vc -> 'a vc -> 'a vc
            val uminusL : 'a vc -> 'a vc
            val divL : 'a vc -> 'a vc -> 'a vc
            val better_thanL :
              ('a vc -> 'a vc -> ('a, bool) Code.abstract) option
            val normalizerL : ('a vc -> 'a vc) option
          end
        module type CONTAINER2D =
          sig
            module Dom : DOMAINL
            type contr
            type 'a vc = ('a, contr) Code.abstract
            type 'a vo = ('a, Dom.v) Code.abstract
            val getL :
              'a vc ->
              ('a, int) Code.abstract -> ('a, int) Code.abstract -> 'a vo
            val dim1 : 'a vc -> ('a, int) Code.abstract
            val dim2 : 'a vc -> ('a, int) Code.abstract
            val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
            val copy : 'a vc -> 'a vc
            val init :
              ('a, int) Code.abstract -> ('a, int) Code.abstract -> 'a vc
            val identity :
              ('a, int) Code.abstract -> ('a, int) Code.abstract -> 'a vc
            val swap_rows_stmt :
              'a vc ->
              ('a, int) Code.abstract ->
              ('a, int) Code.abstract -> ('a, unit) Code.abstract
            val swap_cols_stmt :
              'a vc ->
              ('a, int) Code.abstract ->
              ('a, int) Code.abstract -> ('a, unit) Code.abstract
            val row_head :
              'a vc ->
              ('a, int) Code.abstract -> ('a, int) Code.abstract -> 'a vo
            val col_head_set :
              'a vc ->
              ('a, int) Code.abstract ->
              ('a, int) Code.abstract -> 'a vo -> ('a, unit) Code.abstract
          end
      end
    type ('a, 'b, 'c, 'd) cmonad =
        (('a, 'b) Code.abstract, 'c list, ('a, 'd) Code.abstract)
        StateCPSMonad.monad
    type ('a, 'b, 'c, 'd) omonad =
        (('a, 'b) Code.abstract option, 'c list, ('a, 'd) Code.abstract)
        StateCPSMonad.monad
    module Iters :
      sig
        val row_iter :
          'a ->
          'b ->
          ('c, int) Code.abstract ->
          ('c, int) Code.abstract ->
          ('a -> ('c, int) Code.abstract -> 'b -> ('c, 'd) Code.abstract) ->
          (('c, int) Code.abstract ->
           ('c, 'd) Code.abstract ->
           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Code.abstract) ->
          'e -> ('e -> ('c, unit) Code.abstract -> 'i) -> 'i
        val col_iter :
          'a ->
          'b ->
          ('c, int) Code.abstract ->
          ('c, int) Code.abstract ->
          ('a -> 'b -> ('c, int) Code.abstract -> 'd) ->
          (('c, int) Code.abstract ->
           'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Code.abstract) ->
          'e -> ('e -> ('c, unit) Code.abstract -> 'i) -> 'i
      end
    module TrackRank :
      sig
        type 'a lstate = ('a, int ref) Code.abstract
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
          ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
          ('a list -> ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
          ('b, 'c) Code.abstract
        val succ :
          unit ->
          ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
        module type RANK =
          sig
            type 'a tag_lstate = 'a tag_lstate_
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm) option
          end
      end
    module Rank :
      sig
        type 'a lstate = ('a, int ref) Code.abstract
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
          ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
          ('a list -> ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
          ('b, 'c) Code.abstract
        val succ :
          unit ->
          ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
        module type RANK =
          sig
            type 'a tag_lstate = 'a tag_lstate_
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm) option
          end
        val fin :
          (unit ->
           ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
           ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
          option
      end
    module NoRank :
      sig
        type 'a lstate = ('a, int ref) Code.abstract
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
          ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
          ('a list -> ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
          ('b, 'c) Code.abstract
        val succ :
          unit ->
          ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
        module type RANK =
          sig
            type 'a tag_lstate = 'a tag_lstate_
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm) option
          end
        val fin : 'a option
      end
    module type PIVOTKIND =
      sig
        type perm_rep
        type 'a ira = ('a, int) Code.abstract
        type 'a fra
        type 'a pra = ('a, perm_rep) Code.abstract
        val add : 'a fra -> 'a pra -> 'a pra
        val empty : 'a ira -> 'a pra
        val rowrep : 'a ira -> 'a ira -> 'a fra
        val colrep : 'a ira -> 'a ira -> 'a fra
      end
    module PermList :
      sig
        type flip_rep = Code.perm
        type perm_rep = Code.perm list
        type 'a ira = ('a, int) Code.abstract
        type 'a fra = ('a, flip_rep) Code.abstract
        type 'a pra = ('a, perm_rep) Code.abstract
        val add :
          ('a, 'b) Code.abstract ->
          ('a, 'b list) Code.abstract -> ('a, 'b list) Code.abstract
        val empty : 'a -> 'b pra
        val rowrep :
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract -> ('a, Code.perm) Code.abstract
        val colrep :
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract -> ('a, Code.perm) Code.abstract
      end
    module RowVectorPerm :
      sig
        type flip_rep = int * int
        type perm_rep = int array
        type 'a ira = ('a, int) Code.abstract
        type 'a fra = ('a, flip_rep) Code.abstract
        type 'a pra = ('a, perm_rep) Code.abstract
        val add :
          ('a, int * int) Code.abstract ->
          ('a, int array) Code.abstract -> ('a, int array) Code.abstract
        val empty : ('a, int) Code.abstract -> ('a, int array) Code.abstract
        val rowrep :
          ('a, 'b) Code.abstract ->
          ('a, 'c) Code.abstract -> ('a, 'b * 'c) Code.abstract
        val colrep :
          ('a, 'b) Code.abstract ->
          ('a, 'c) Code.abstract -> ('a, 'b * 'c) Code.abstract
      end
    module type TRACKPIVOT =
      sig
        type perm_rep
        type 'a ira = ('a, int) Code.abstract
        type 'a fra
        type 'a pra
        type 'a lstate
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rowrep : 'a ira -> 'a ira -> 'a fra
        val colrep : 'a ira -> 'a ira -> 'a fra
        val decl :
          ('a, int) Code.abstract -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val add :
          'a fra ->
          (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
           ('a, 'b) Code.abstract)
          StateCPSMonad.monad
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm) option
      end
    module PivotCommon :
      functor (PK : PIVOTKIND) ->
        sig
          type perm_rep = PK.perm_rep
          type 'a ira = 'a PK.ira
          type 'a fra = 'a PK.fra
          type 'a pra = 'a PK.pra
          type 'a lstate = ('a, PK.perm_rep ref) Code.abstract
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
          type 'a lstate = ('a, PK.perm_rep ref) Code.abstract
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
            ([> `TPivot of ('a, PK.perm_rep ref) Code.abstract ] as 'b) list ->
            ('b list -> ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
          val add :
            'a PK.fra ->
            ([> `TPivot of ('a, PK.perm_rep ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
          val fin :
            (unit ->
             ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
             ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
            option
        end
    module DiscardPivot :
      sig
        type perm_rep = PermList.perm_rep
        type 'a ira = 'a PermList.ira
        type 'a fra = 'a PermList.fra
        type 'a pra = 'a PermList.pra
        type 'a lstate = ('a, PermList.perm_rep ref) Code.abstract
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rowrep : 'a PermList.ira -> 'a PermList.ira -> 'a PermList.fra
        val colrep : 'a PermList.ira -> 'a PermList.ira -> 'a PermList.fra
        val decl : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
        val fin : 'a option
      end
    module GenLA :
      functor (C : D.CONTAINER2D) ->
        sig
          type 'a wmatrix =
            'a Ge.GEMake(Code).GenLA(C).wmatrix = {
            matrix : 'a C.vc;
            numrow : ('a, int) Code.abstract;
            numcol : ('a, int) Code.abstract;
          }
          type 'a curpos =
            'a Ge.GEMake(Code).GenLA(C).curpos = {
            rowpos : ('a, int) Code.abstract;
            colpos : ('a, int) Code.abstract;
          }
          type 'a curposval =
            'a Ge.GEMake(Code).GenLA(C).curposval = {
            p : 'a curpos;
            curval : ('a, C.Dom.v) Code.abstract;
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
                ('a, C.Dom.v) Code.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, unit) lm
              val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
              val set :
                ('a, C.Dom.v) Code.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, unit) lm
              val fin :
                (unit -> ('a * [> 'a tag_lstate ] * 'b, C.Dom.v) lm) option
            end
          module NoDet :
            sig
              type tdet = C.Dom.v ref
              type 'a lstate = unit
              val decl :
                unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
              val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
              val zero_sign :
                unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
              val acc :
                'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
              val get :
                unit ->
                'a -> ('a -> ('b, C.Dom.v ref) Code.abstract -> 'c) -> 'c
              val set :
                'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
              val fin : 'a option
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
                  ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                      ('b, int ref) Code.abstract *
                      ('b, C.Dom.v ref) Code.abstract ]
                 as 'a)
                list ->
                ('a list ->
                 ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
                ('b, 'd) Code.abstract
              val upd_sign :
                unit ->
                ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
                ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
              val zero_sign :
                unit ->
                ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
                ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
              val acc :
                'a C.Dom.vc ->
                ([> `TDet of 'c * ('a, C.Dom.v ref) Code.abstract ] as 'b)
                list -> ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
              val get :
                unit ->
                ([> `TDet of 'b * 'c ] as 'a) list ->
                ('a list -> 'c -> 'd) -> 'd
              val set :
                ('a, 'b) Code.abstract ->
                ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
                ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
              val fin :
                (unit ->
                 ([> `TDet of
                       ('b, int ref) Code.abstract *
                       ('b, C.Dom.v ref) Code.abstract ]
                  as 'a)
                 list -> ('a list -> ('b, C.Dom.v) Code.abstract -> 'c) -> 'c)
                option
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
                  ('a in_val -> ('a, unit) Code.abstract) ->
                  ('a, C.Dom.v ref) Code.abstract ->
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
                  ('a, C.Dom.v) Code.abstract ->
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
                  ('a, C.Dom.v ref) Code.abstract ->
                  'c -> ('c -> 'b -> 'd) -> 'd
                val update_det :
                  ('a, C.Dom.v) Code.abstract ->
                  ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
                val upd_kind : Ge.update_kind
              end
          module type LOWER =
            sig
              type 'a lstate = ('a, C.contr) Code.abstract
              type 'a tag_lstate = [ `TLower of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val decl :
                ('a, C.contr) Code.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val updt :
                'a C.vc ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                'a C.vo ->
                'a C.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
              val fin :
                (unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm) option
              val wants_pack : bool
            end
          module TrackLower :
            sig
              type 'a lstate = ('a, C.contr) Code.abstract
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
              type 'a lstate = ('a, C.contr) Code.abstract
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
                ('a, 'b) Code.abstract ->
                ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                ('a, 'd) Code.abstract
              val updt :
                'a C.vc ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                'a C.vo ->
                'a C.vo ->
                (([> `TLower of 'a C.vc ] as 'b) list ->
                 ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                option
              val fin :
                (unit ->
                 ([> `TLower of 'b ] as 'a) list ->
                 ('a list -> 'b -> 'c) -> 'c)
                option
              val wants_pack : bool
            end
          module PackedLower :
            sig
              type 'a lstate = ('a, C.contr) Code.abstract
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
                (unit ->
                 ([> `TLower of 'b ] as 'a) list ->
                 ('a list -> 'b -> 'c) -> 'c)
                option
              val wants_pack : bool
            end
          module NoLower :
            sig
              type 'a lstate = ('a, C.contr) Code.abstract
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
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                'a C.vo ->
                'b ->
                ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
              val fin : 'a option
              val wants_pack : bool
            end
          module type INPUT =
            sig
              type inp
              val get_input :
                ('a, inp) Code.abstract ->
                (('a, C.contr) Code.abstract * ('a, int) Code.abstract * bool,
                 'b, ('a, 'c) Code.abstract)
                StateCPSMonad.monad
            end
          module InpJustMatrix :
            sig
              type inp = C.contr
              val get_input :
                'a C.vc ->
                'b ->
                ('b -> 'a C.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
            end
          module InpMatrixMargin :
            sig
              type inp = C.contr * int
              val get_input :
                ('a, 'b * 'c) Code.abstract ->
                'd ->
                ('d ->
                 ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
                     ('a, C.Dom.v option) Code.abstract ->
                     ('a, 'c) Code.abstract) ->
                    ('a, 'c) Code.abstract
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
                     ('a, C.Dom.v option) Code.abstract ->
                     ('a, 'c) Code.abstract) ->
                    ('a, 'c) Code.abstract
                end
          module NoPivot :
            functor (Det : DETERMINANT) ->
              functor (P : TRACKPIVOT) ->
                sig
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    'b ->
                    ('b -> ('a, C.Dom.v option) Code.abstract -> 'c) -> 'c
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
                        type 'a lstate = ('a, int ref) Code.abstract
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
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Code.abstract ->
                           ('b, 'c) Code.abstract) ->
                          ('b, 'c) Code.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                              (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                              option
                          end
                        val fin : 'a option
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Code.abstract
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
                          'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : 'a option
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Code.abstract
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
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                          option
                        val fin : 'a option
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
                        type 'a lstate = ('a, int ref) Code.abstract
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
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Code.abstract ->
                           ('b, 'c) Code.abstract) ->
                          ('b, 'c) Code.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                              (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                              option
                          end
                        val fin : 'a option
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Code.abstract
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
                          'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : 'a option
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Code.abstract
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
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                          option
                        val fin : 'a option
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.Dom.v
                val make_result :
                  'a wmatrix ->
                  ([> 'a OD.Det.tag_lstate ] as 'b) list ->
                  ('b list ->
                   ('a, C.contr * C.Dom.v) Code.abstract ->
                   ('a, 'c) Code.abstract) ->
                  ('a, 'c) Code.abstract
              end
          module OutRank :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Code.abstract
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
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Code.abstract ->
                           ('b, 'c) Code.abstract) ->
                          ('b, 'c) Code.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                              (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                              option
                          end
                        val fin :
                          (unit ->
                           ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a)
                           list ->
                           ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                          option
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Code.abstract
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
                          'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : 'a option
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Code.abstract
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
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                          option
                        val fin : 'a option
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * int
                val make_result :
                  'a wmatrix ->
                  ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
                  ('b list -> ('a, C.contr * 'c) Code.abstract -> 'd) -> 'd
              end
          module OutDetRank :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Code.abstract
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
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Code.abstract ->
                           ('b, 'c) Code.abstract) ->
                          ('b, 'c) Code.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                              (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                              option
                          end
                        val fin :
                          (unit ->
                           ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a)
                           list ->
                           ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                          option
                      end
                    module P :
                      sig
                        type perm_rep = PermList.perm_rep
                        type 'a ira = 'a PermList.ira
                        type 'a fra = 'a PermList.fra
                        type 'a pra = 'a PermList.pra
                        type 'a lstate =
                            ('a, PermList.perm_rep ref) Code.abstract
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
                          'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : 'a option
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Code.abstract
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
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                          option
                        val fin : 'a option
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.Dom.v * int
                val make_result :
                  'a wmatrix ->
                  ([> `TDet of 'a OD.Det.lstate
                    | `TRan of ('a, 'c ref) Code.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, C.contr * C.Dom.v * 'c) Code.abstract ->
                   ('a, 'd) Code.abstract) ->
                  ('a, 'd) Code.abstract
              end
          module OutDetRankPivot :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Code.abstract
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
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Code.abstract ->
                           ('b, 'c) Code.abstract) ->
                          ('b, 'c) Code.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                              (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                              option
                          end
                        val fin :
                          (unit ->
                           ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a)
                           list ->
                           ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                          option
                      end
                    module P :
                      sig
                        type perm_rep = OD.PivotRep.perm_rep
                        type 'a ira = 'a OD.PivotRep.ira
                        type 'a fra = 'a OD.PivotRep.fra
                        type 'a pra = 'a OD.PivotRep.pra
                        type 'a lstate =
                            ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                                ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                          ('a, 'd) Code.abstract
                        val add :
                          'a OD.PivotRep.fra ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Code.abstract option -> 'c) ->
                          'c
                        val fin :
                          (unit ->
                           ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a)
                           list ->
                           ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                          option
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Code.abstract
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
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          'a C.vo ->
                          'b ->
                          ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                          option
                        val fin : 'a option
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.Dom.v * int * IF.P.perm_rep
                val make_result :
                  'a wmatrix ->
                  ([> `TDet of 'a OD.Det.lstate
                    | `TPivot of ('a, 'c ref) Code.abstract
                    | `TRan of ('a, 'd ref) Code.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, C.contr * C.Dom.v * 'd * 'c) Code.abstract ->
                   ('a, 'e) Code.abstract) ->
                  ('a, 'e) Code.abstract
              end
          module Out_L_U :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Code.abstract
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
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Code.abstract ->
                           ('b, 'c) Code.abstract) ->
                          ('b, 'c) Code.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                              (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                              option
                          end
                        val fin : 'a option
                      end
                    module P :
                      sig
                        type perm_rep = OD.PivotRep.perm_rep
                        type 'a ira = 'a OD.PivotRep.ira
                        type 'a fra = 'a OD.PivotRep.fra
                        type 'a pra = 'a OD.PivotRep.pra
                        type 'a lstate =
                            ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                                ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                          ('a, 'd) Code.abstract
                        val add :
                          'a OD.PivotRep.fra ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Code.abstract option -> 'c) ->
                          'c
                        val fin :
                          (unit ->
                           ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a)
                           list ->
                           ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                          option
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Code.abstract
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
                          ('a, 'b) Code.abstract ->
                          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                          ('c list ->
                           ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                          ('a, 'd) Code.abstract
                        val updt :
                          'a C.vc ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          'a C.vo ->
                          'a C.vo ->
                          (([> `TLower of 'a C.vc ] as 'b) list ->
                           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                          option
                        val fin :
                          (unit ->
                           ([> `TLower of 'b ] as 'a) list ->
                           ('a list -> 'b -> 'c) -> 'c)
                          option
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * C.contr * IF.P.perm_rep
                val make_result :
                  'a wmatrix ->
                  ([> `TLower of ('a, 'c) Code.abstract
                    | `TPivot of ('a, 'd ref) Code.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, C.contr * 'c * 'd) Code.abstract -> 'e) ->
                  'e
              end
          module Out_LU_Packed :
            functor (OD : OUTPUTDEP) ->
              sig
                module IF :
                  sig
                    module R :
                      sig
                        type 'a lstate = ('a, int ref) Code.abstract
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
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list ->
                           ('b, int ref) Code.abstract ->
                           ('b, 'c) Code.abstract) ->
                          ('b, 'c) Code.abstract
                        val succ :
                          unit ->
                          ([> `TRan of ('b, int ref) Code.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                              (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                              option
                          end
                        val fin : 'a option
                      end
                    module P :
                      sig
                        type perm_rep = OD.PivotRep.perm_rep
                        type 'a ira = 'a OD.PivotRep.ira
                        type 'a fra = 'a OD.PivotRep.fra
                        type 'a pra = 'a OD.PivotRep.pra
                        type 'a lstate =
                            ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                                ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                          ('a, 'd) Code.abstract
                        val add :
                          'a OD.PivotRep.fra ->
                          ([> `TPivot of
                                ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Code.abstract option -> 'c) ->
                          'c
                        val fin :
                          (unit ->
                           ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a)
                           list ->
                           ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                          option
                      end
                    module L :
                      sig
                        type 'a lstate = ('a, C.contr) Code.abstract
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
                          (unit ->
                           ([> `TLower of 'b ] as 'a) list ->
                           ('a list -> 'b -> 'c) -> 'c)
                          option
                        val wants_pack : bool
                      end
                  end
                type res = C.contr * IF.P.perm_rep
                val make_result :
                  'a ->
                  ([> `TLower of ('c, 'd) Code.abstract
                    | `TPivot of ('c, 'e ref) Code.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                              (unit ->
                               ('a * [> 'a TrackRank.tag_lstate ] * 'b, int)
                               TrackRank.lm)
                              option
                          end
                        module P :
                          sig
                            type perm_rep = F.Output(F).IF.P.perm_rep
                            type 'a ira = ('a, int) Code.abstract
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
                              ('a, int) Code.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val add :
                              'a fra ->
                              (('a, unit) Code.abstract option,
                               [> 'a tag_lstate ] list,
                               ('a, 'b) Code.abstract)
                              StateCPSMonad.monad
                            val fin :
                              (unit ->
                               ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                              option
                          end
                        module L :
                          sig
                            type 'a lstate = ('a, C.contr) Code.abstract
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
                              ('a, C.contr) Code.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                            val updt :
                              'a C.vc ->
                              ('a, int) Code.abstract ->
                              ('a, int) Code.abstract ->
                              'a C.vo ->
                              'a C.Dom.vc ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              (unit ->
                               ('a * [> 'a tag_lstate ] * 'b, C.contr) lm)
                              option
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
                    | `TLower of ('a, C.contr) Code.abstract ]
                   as 'b)
                  list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
                val init :
                  ('a, F.Input.inp) Code.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of ('a, C.contr) Code.abstract
                    | `TPivot of 'a F.Output(F).IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list ->
                  ('b list ->
                   'a wmatrix * ('a, int ref) Code.abstract *
                   ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
                   ('a, 'c) Code.abstract) ->
                  ('a, 'c) Code.abstract
                val forward_elim :
                  'a wmatrix * ('a, int ref) Code.abstract *
                  ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of ('a, C.contr) Code.abstract
                    | `TPivot of 'a F.Output(F).IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
                val backward_elim :
                  unit ->
                  ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
                val ge_gen :
                  ('a, F.Input.inp) Code.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of 'a O.IF.L.lstate
                    | `TPivot of 'a O.IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
                  ('a, 'c) Code.abstract
                val gen :
                  ('a, F.Input.inp) Code.abstract ->
                  ([> `TDet of 'a F.Det.lstate
                    | `TLower of 'a O.IF.L.lstate
                    | `TPivot of 'a O.IF.P.lstate
                    | `TRan of 'a TrackRank.lstate ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
                  ('a, 'c) Code.abstract
              end
        end
  end
type 'a pr = { pf : 'b. ('b, 'a) code; }
#   val instantiate :
  (('a, 'b) code -> 'c list -> ('d -> 'e -> 'e) -> ('a, 'f) code) ->
  ('a, 'b -> 'f) code = <fun>
#   val runit : 'a pr -> 'a = <fun>
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
    type 'a vc = ('a, v) code
    val zeroL : ('a, int) code
    val oneL : ('a, int) code
    val ( +^ ) : ('a, int) code -> ('a, int) code -> ('a, int) code
    val ( *^ ) : ('a, int) code -> ('a, int) code -> ('a, int) code
    val ( -^ ) : ('a, int) code -> ('a, int) code -> ('a, int) code
    val uminusL : ('a, int) code -> ('a, int) code
    val divL : ('a, int) code -> ('a, int) code -> ('a, int) code
    val normalizerL : 'a option
    val better_thanL : 'a option
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
    type 'a vc = ('a, v) code
    val zeroL : ('a, int) code
    val oneL : ('a, int) code
    val ( +^ ) : ('a, int) code -> ('a, int) code -> ('a, int) code
    val ( *^ ) : ('a, int) code -> ('a, int) code -> ('a, int) code
    val ( -^ ) : ('a, int) code -> ('a, int) code -> ('a, int) code
    val uminusL : ('a, int) code -> ('a, int) code
    val divL : ('a, int) code -> ('a, int) code -> ('a, int) code
    val normalizerL : 'a option
    val better_thanL : 'a option
  end
module GAC_F :
  sig
    module Dom :
      sig
        type v = Domains_code.FloatDomainL.v
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val dim2 : ('a, 'b array) code -> ('a, int) code
    val dim1 : ('a, 'b array array) code -> ('a, int) code
    val mapper :
      ('a vo -> 'a vo) option ->
      ('a, Dom.v array array) code -> ('a, Dom.v array array) code
    val copy : ('a, 'b array array) code -> ('a, 'b array array) code
    val init :
      ('a, int) code -> ('a, int) code -> ('a, Dom.v array array) code
    val identity :
      ('a, int) code -> ('a, int) code -> ('a, Dom.v array array) code
    val swap_rows_stmt :
      ('a, 'b array) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val col_head_set :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code -> ('a, unit) code
  end
module GVC_F :
  sig
    module Dom :
      sig
        type v = Domains_code.FloatDomainL.v
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_code.container2dfromvector
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val dim2 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val dim1 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val mapper :
      (('a, 'b) code -> ('a, 'b) code) option ->
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val copy :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val init :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val identity :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val swap_rows_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val col_head_set :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code -> ('a, unit) code
  end
module GAC_I :
  sig
    module Dom :
      sig
        type v = Domains_code.IntegerDomainL.v
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val dim2 : ('a, 'b array) code -> ('a, int) code
    val dim1 : ('a, 'b array array) code -> ('a, int) code
    val mapper :
      ('a vo -> 'a vo) option ->
      ('a, Dom.v array array) code -> ('a, Dom.v array array) code
    val copy : ('a, 'b array array) code -> ('a, 'b array array) code
    val init :
      ('a, int) code -> ('a, int) code -> ('a, Dom.v array array) code
    val identity :
      ('a, int) code -> ('a, int) code -> ('a, Dom.v array array) code
    val swap_rows_stmt :
      ('a, 'b array) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val col_head_set :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code -> ('a, unit) code
  end
module GVC_I :
  sig
    module Dom :
      sig
        type v = Domains_code.IntegerDomainL.v
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_code.container2dfromvector
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val dim2 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val dim1 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val mapper :
      (('a, 'b) code -> ('a, 'b) code) option ->
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val copy :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val init :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val identity :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val swap_rows_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val col_head_set :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code -> ('a, unit) code
  end
module GAC_R :
  sig
    module Dom :
      sig
        type v = Domains_code.RationalDomainL.v
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val dim2 : ('a, 'b array) code -> ('a, int) code
    val dim1 : ('a, 'b array array) code -> ('a, int) code
    val mapper :
      ('a vo -> 'a vo) option ->
      ('a, Dom.v array array) code -> ('a, Dom.v array array) code
    val copy : ('a, 'b array array) code -> ('a, 'b array array) code
    val init :
      ('a, int) code -> ('a, int) code -> ('a, Dom.v array array) code
    val identity :
      ('a, int) code -> ('a, int) code -> ('a, Dom.v array array) code
    val swap_rows_stmt :
      ('a, 'b array) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val col_head_set :
      ('a, 'b array array) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code -> ('a, unit) code
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_code.container2dfromvector
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val dim2 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val dim1 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val mapper :
      (('a, 'b) code -> ('a, 'b) code) option ->
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val copy :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val init :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val identity :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val swap_rows_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val col_head_set :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code -> ('a, unit) code
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v Domains_code.container2dfromvector
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val dim2 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val dim1 :
      ('a, 'b Domains_code.container2dfromvector) code -> ('a, int) code
    val mapper :
      (('a, 'b) code -> ('a, 'b) code) option ->
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val copy :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, 'b Domains_code.container2dfromvector) code
    val init :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val identity :
      ('a, int) code ->
      ('a, int) code -> ('a, Dom.v Domains_code.container2dfromvector) code
    val swap_rows_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code
    val col_head_set :
      ('a, 'b Domains_code.container2dfromvector) code ->
      ('a, int) code -> ('a, int) code -> ('a, 'b) code -> ('a, unit) code
  end
module GFC_F :
  sig
    module Dom :
      sig
        type v =
            Domains_code.FortranVectorContainer(Domains_code.FloatDomainL).Dom.v
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
        type 'a vc = ('a, v) code
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL : ('a vc -> 'a vc -> ('a, bool) code) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr =
        Domains_code.FortranVectorContainer(Domains_code.FloatDomainL).contr
    type 'a vc = ('a, contr) code
    type 'a vo = ('a, Dom.v) code
    val getL : 'a vc -> ('a, int) code -> ('a, int) code -> 'a vo
    val dim1 : 'a vc -> ('a, int) code
    val dim2 : 'a vc -> ('a, int) code
    val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
    val copy : 'a vc -> 'a vc
    val init : ('a, int) code -> ('a, int) code -> 'a vc
    val identity : ('a, int) code -> ('a, int) code -> 'a vc
    val swap_rows_stmt :
      'a vc -> ('a, int) code -> ('a, int) code -> ('a, unit) code
    val swap_cols_stmt :
      'a vc -> ('a, int) code -> ('a, int) code -> ('a, unit) code
    val row_head : 'a vc -> ('a, int) code -> ('a, int) code -> 'a vo
    val col_head_set :
      'a vc -> ('a, int) code -> ('a, int) code -> 'a vo -> ('a, unit) code
  end
module G_GAC_F :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GAC_F).wmatrix = {
      matrix : 'a GAC_F.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GAC_F).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GAC_F).curposval = {
      p : 'a curpos;
      curval : ('a, GAC_F.Dom.v) Code.abstract;
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
          ('a, GAC_F.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GAC_F.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GAC_F.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GAC_F.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GAC_F.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GAC_F.Dom.vc ->
          ([> `TDet of 'c * ('a, GAC_F.Dom.v ref) Code.abstract ] as 'b) list ->
          ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GAC_F.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GAC_F.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GAC_F.Dom.v ref) Code.abstract ->
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
            ('a, GAC_F.Dom.v) Code.abstract ->
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
            ('a, GAC_F.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GAC_F.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GAC_F.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val decl :
          ('a, GAC_F.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val updt :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_F.vo ->
          'a GAC_F.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
        type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_F.vo ->
          'a GAC_F.vo ->
          (([> `TLower of 'a GAC_F.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_F.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GAC_F.contr
        val get_input :
          'a GAC_F.vc ->
          'b ->
          ('b -> 'a GAC_F.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GAC_F.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GAC_F.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GAC_F.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_F.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GAC_F.contr * GAC_F.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GAC_F.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_F.contr * GAC_F.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_F.contr * GAC_F.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GAC_F.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_F.vo ->
                    'a GAC_F.vo ->
                    (([> `TLower of 'a GAC_F.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * GAC_F.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GAC_F.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_F.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GAC_F.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GAC_F.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                      val decl :
                        ('a, GAC_F.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                      val updt :
                        'a GAC_F.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GAC_F.vo ->
                        'a GAC_F.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                        option
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
              | `TLower of ('a, GAC_F.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_F.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_F.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
  end
module G_GVC_F :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GVC_F).wmatrix = {
      matrix : 'a GVC_F.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GVC_F).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GVC_F).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_F.Dom.v) Code.abstract;
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
          ('a, GVC_F.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_F.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GVC_F.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_F.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GVC_F.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GVC_F.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_F.Dom.v ref) Code.abstract ] as 'b) list ->
          ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GVC_F.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GVC_F.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GVC_F.Dom.v ref) Code.abstract ->
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
            ('a, GVC_F.Dom.v) Code.abstract ->
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
            ('a, GVC_F.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_F.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_F.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val decl :
          ('a, GVC_F.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val updt :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_F.vo ->
          'a GVC_F.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
        type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_F.vo ->
          'a GVC_F.vo ->
          (([> `TLower of 'a GVC_F.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_F.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_F.contr
        val get_input :
          'a GVC_F.vc ->
          'b ->
          ('b -> 'a GVC_F.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_F.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GVC_F.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GVC_F.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_F.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_F.contr * GVC_F.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_F.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_F.contr * GVC_F.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_F.contr * GVC_F.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GVC_F.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_F.vo ->
                    'a GVC_F.vo ->
                    (([> `TLower of 'a GVC_F.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * GVC_F.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_F.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_F.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_F.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_F.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                      val decl :
                        ('a, GVC_F.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                      val updt :
                        'a GVC_F.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GVC_F.vo ->
                        'a GVC_F.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm)
                        option
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
              | `TLower of ('a, GVC_F.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_F.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_F.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
  end
module G_GAC_I :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GAC_I).wmatrix = {
      matrix : 'a GAC_I.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GAC_I).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GAC_I).curposval = {
      p : 'a curpos;
      curval : ('a, GAC_I.Dom.v) Code.abstract;
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
          ('a, GAC_I.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GAC_I.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GAC_I.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GAC_I.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GAC_I.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GAC_I.Dom.vc ->
          ([> `TDet of 'c * ('a, GAC_I.Dom.v ref) Code.abstract ] as 'b) list ->
          ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GAC_I.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GAC_I.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GAC_I.Dom.v ref) Code.abstract ->
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
            ('a, GAC_I.Dom.v) Code.abstract ->
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
            ('a, GAC_I.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GAC_I.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GAC_I.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val decl :
          ('a, GAC_I.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val updt :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_I.vo ->
          'a GAC_I.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
        type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_I.vo ->
          'a GAC_I.vo ->
          (([> `TLower of 'a GAC_I.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_I.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GAC_I.contr
        val get_input :
          'a GAC_I.vc ->
          'b ->
          ('b -> 'a GAC_I.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GAC_I.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GAC_I.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GAC_I.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_I.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GAC_I.contr * GAC_I.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GAC_I.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_I.contr * GAC_I.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_I.contr * GAC_I.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GAC_I.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_I.vo ->
                    'a GAC_I.vo ->
                    (([> `TLower of 'a GAC_I.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * GAC_I.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GAC_I.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_I.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GAC_I.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GAC_I.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                      val decl :
                        ('a, GAC_I.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                      val updt :
                        'a GAC_I.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GAC_I.vo ->
                        'a GAC_I.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm)
                        option
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
              | `TLower of ('a, GAC_I.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_I.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_I.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
  end
module G_GVC_I :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GVC_I).wmatrix = {
      matrix : 'a GVC_I.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GVC_I).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GVC_I).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_I.Dom.v) Code.abstract;
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
          ('a, GVC_I.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_I.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GVC_I.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_I.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GVC_I.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GVC_I.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_I.Dom.v ref) Code.abstract ] as 'b) list ->
          ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GVC_I.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GVC_I.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GVC_I.Dom.v ref) Code.abstract ->
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
            ('a, GVC_I.Dom.v) Code.abstract ->
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
            ('a, GVC_I.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_I.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_I.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val decl :
          ('a, GVC_I.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val updt :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_I.vo ->
          'a GVC_I.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
        type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_I.vo ->
          'a GVC_I.vo ->
          (([> `TLower of 'a GVC_I.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_I.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_I.contr
        val get_input :
          'a GVC_I.vc ->
          'b ->
          ('b -> 'a GVC_I.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_I.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GVC_I.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GVC_I.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_I.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_I.contr * GVC_I.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_I.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_I.contr * GVC_I.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_I.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_I.contr * GVC_I.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GVC_I.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_I.vo ->
                    'a GVC_I.vo ->
                    (([> `TLower of 'a GVC_I.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * GVC_I.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_I.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_I.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_I.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_I.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                      val decl :
                        ('a, GVC_I.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                      val updt :
                        'a GVC_I.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GVC_I.vo ->
                        'a GVC_I.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm)
                        option
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
              | `TLower of ('a, GVC_I.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_I.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_I.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
  end
module G_GAC_R :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GAC_R).wmatrix = {
      matrix : 'a GAC_R.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GAC_R).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GAC_R).curposval = {
      p : 'a curpos;
      curval : ('a, GAC_R.Dom.v) Code.abstract;
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
          ('a, GAC_R.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GAC_R.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GAC_R.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GAC_R.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GAC_R.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GAC_R.Dom.vc ->
          ([> `TDet of 'c * ('a, GAC_R.Dom.v ref) Code.abstract ] as 'b) list ->
          ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GAC_R.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GAC_R.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GAC_R.Dom.v ref) Code.abstract ->
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
            ('a, GAC_R.Dom.v) Code.abstract ->
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
            ('a, GAC_R.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GAC_R.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GAC_R.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val decl :
          ('a, GAC_R.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val updt :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_R.vo ->
          'a GAC_R.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
        type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_R.vo ->
          'a GAC_R.vo ->
          (([> `TLower of 'a GAC_R.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GAC_R.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_R.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GAC_R.contr
        val get_input :
          'a GAC_R.vc ->
          'b ->
          ('b -> 'a GAC_R.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GAC_R.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GAC_R.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GAC_R.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_R.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GAC_R.contr * GAC_R.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GAC_R.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_R.contr * GAC_R.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_R.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GAC_R.contr * GAC_R.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GAC_R.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GAC_R.vo ->
                    'a GAC_R.vo ->
                    (([> `TLower of 'a GAC_R.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * GAC_R.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GAC_R.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GAC_R.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GAC_R.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GAC_R.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                      val decl :
                        ('a, GAC_R.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                      val updt :
                        'a GAC_R.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GAC_R.vo ->
                        'a GAC_R.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm)
                        option
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
              | `TLower of ('a, GAC_R.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_R.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GAC_R.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
  end
module G_GVC_Z3 :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GVC_Z3).wmatrix = {
      matrix : 'a GVC_Z3.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GVC_Z3).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GVC_Z3).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_Z3.Dom.v) Code.abstract;
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
          ('a, GVC_Z3.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_Z3.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GVC_Z3.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_Z3.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GVC_Z3.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GVC_Z3.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_Z3.Dom.v ref) Code.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GVC_Z3.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GVC_Z3.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GVC_Z3.Dom.v ref) Code.abstract ->
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
            ('a, GVC_Z3.Dom.v) Code.abstract ->
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
            ('a, GVC_Z3.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_Z3.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val decl :
          ('a, GVC_Z3.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val updt :
          'a GVC_Z3.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_Z3.vo ->
          'a GVC_Z3.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
        type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GVC_Z3.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_Z3.vo ->
          'a GVC_Z3.vo ->
          (([> `TLower of 'a GVC_Z3.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_Z3.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_Z3.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_Z3.contr
        val get_input :
          'a GVC_Z3.vc ->
          'b ->
          ('b -> 'a GVC_Z3.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_Z3.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GVC_Z3.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GVC_Z3.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_Z3.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_Z3.contr * GVC_Z3.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_Z3.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z3.contr * GVC_Z3.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z3.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z3.contr * GVC_Z3.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GVC_Z3.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z3.vo ->
                    'a GVC_Z3.vo ->
                    (([> `TLower of 'a GVC_Z3.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * GVC_Z3.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_Z3.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z3.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                      val decl :
                        ('a, GVC_Z3.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                      val updt :
                        'a GVC_Z3.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GVC_Z3.vo ->
                        'a GVC_Z3.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm)
                        option
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
              | `TLower of ('a, GVC_Z3.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z3.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z3.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
  end
module G_GVC_Z19 :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GVC_Z19).wmatrix = {
      matrix : 'a GVC_Z19.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GVC_Z19).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GVC_Z19).curposval = {
      p : 'a curpos;
      curval : ('a, GVC_Z19.Dom.v) Code.abstract;
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
          ('a, GVC_Z19.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GVC_Z19.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GVC_Z19.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GVC_Z19.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GVC_Z19.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GVC_Z19.Dom.vc ->
          ([> `TDet of 'c * ('a, GVC_Z19.Dom.v ref) Code.abstract ] as 'b)
          list -> ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GVC_Z19.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GVC_Z19.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GVC_Z19.Dom.v ref) Code.abstract ->
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
            ('a, GVC_Z19.Dom.v) Code.abstract ->
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
            ('a, GVC_Z19.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GVC_Z19.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val decl :
          ('a, GVC_Z19.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val updt :
          'a GVC_Z19.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_Z19.vo ->
          'a GVC_Z19.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
        type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GVC_Z19.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_Z19.vo ->
          'a GVC_Z19.vo ->
          (([> `TLower of 'a GVC_Z19.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GVC_Z19.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_Z19.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GVC_Z19.contr
        val get_input :
          'a GVC_Z19.vc ->
          'b ->
          ('b -> 'a GVC_Z19.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GVC_Z19.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GVC_Z19.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GVC_Z19.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_Z19.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GVC_Z19.contr * GVC_Z19.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GVC_Z19.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z19.contr * GVC_Z19.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z19.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GVC_Z19.contr * GVC_Z19.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GVC_Z19.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GVC_Z19.vo ->
                    'a GVC_Z19.vo ->
                    (([> `TLower of 'a GVC_Z19.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * GVC_Z19.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GVC_Z19.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GVC_Z19.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                      val decl :
                        ('a, GVC_Z19.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                      val updt :
                        'a GVC_Z19.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GVC_Z19.vo ->
                        'a GVC_Z19.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm)
                        option
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
              | `TLower of ('a, GVC_Z19.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z19.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GVC_Z19.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
  end
module G_GFC_F :
  sig
    type 'a wmatrix =
      'a Ge.GEMake(Code).GenLA(GFC_F).wmatrix = {
      matrix : 'a GFC_F.vc;
      numrow : ('a, int) Code.abstract;
      numcol : ('a, int) Code.abstract;
    }
    type 'a curpos =
      'a Ge.GEMake(Code).GenLA(GFC_F).curpos = {
      rowpos : ('a, int) Code.abstract;
      colpos : ('a, int) Code.abstract;
    }
    type 'a curposval =
      'a Ge.GEMake(Code).GenLA(GFC_F).curposval = {
      p : 'a curpos;
      curval : ('a, GFC_F.Dom.v) Code.abstract;
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
          ('a, GFC_F.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, GFC_F.Dom.v) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GFC_F.Dom.v) lm) option
      end
    module NoDet :
      sig
        type tdet = GFC_F.Dom.v ref
        type 'a lstate = unit
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        val zero_sign :
          unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          'a -> ('a -> ('b, GFC_F.Dom.v ref) Code.abstract -> 'c) -> 'c
        val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
        val fin : 'a option
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
            ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
                ('b, int ref) Code.abstract *
                ('b, GFC_F.Dom.v ref) Code.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) Code.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) Code.abstract -> 'd) -> 'd
        val acc :
          'a GFC_F.Dom.vc ->
          ([> `TDet of 'c * ('a, GFC_F.Dom.v ref) Code.abstract ] as 'b) list ->
          ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) Code.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
        val fin :
          (unit ->
           ([> `TDet of
                 ('b, int ref) Code.abstract *
                 ('b, GFC_F.Dom.v ref) Code.abstract ]
            as 'a)
           list -> ('a list -> ('b, GFC_F.Dom.v) Code.abstract -> 'c) -> 'c)
          option
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
            ('a in_val -> ('a, unit) Code.abstract) ->
            ('a, GFC_F.Dom.v ref) Code.abstract ->
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
            ('a, GFC_F.Dom.v) Code.abstract ->
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
            ('a, GFC_F.Dom.v ref) Code.abstract ->
            'c -> ('c -> 'b -> 'd) -> 'd
          val update_det :
            ('a, GFC_F.Dom.v) Code.abstract ->
            ('a * [> 'a Det.tag_lstate ] * 'b, unit) Det.lm
          val upd_kind : Ge.update_kind
        end
    module type LOWER =
      sig
        type 'a lstate = ('a, GFC_F.contr) Code.abstract
        type 'a tag_lstate = [ `TLower of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
        val decl :
          ('a, GFC_F.contr) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
        val updt :
          'a GFC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GFC_F.vo ->
          'a GFC_F.Dom.vc -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin :
          (unit -> ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm) option
        val wants_pack : bool
      end
    module TrackLower :
      sig
        type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
        type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
          ('a, 'b) Code.abstract ->
          ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
          ('c list -> ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
          ('a, 'd) Code.abstract
        val updt :
          'a GFC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GFC_F.vo ->
          'a GFC_F.vo ->
          (([> `TLower of 'a GFC_F.vc ] as 'b) list ->
           ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
          option
        val fin :
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module PackedLower :
      sig
        type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
          (unit ->
           ([> `TLower of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c)
          option
        val wants_pack : bool
      end
    module NoLower :
      sig
        type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          'a GFC_F.vo ->
          'b -> ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd) option
        val fin : 'a option
        val wants_pack : bool
      end
    module type INPUT =
      sig
        type inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GFC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module InpJustMatrix :
      sig
        type inp = GFC_F.contr
        val get_input :
          'a GFC_F.vc ->
          'b ->
          ('b -> 'a GFC_F.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
      end
    module InpMatrixMargin :
      sig
        type inp = GFC_F.contr * int
        val get_input :
          ('a, 'b * 'c) Code.abstract ->
          'd ->
          ('d -> ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
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
               ('a, GFC_F.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
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
               ('a, GFC_F.Dom.v option) Code.abstract ->
               ('a, 'c) Code.abstract) ->
              ('a, 'c) Code.abstract
          end
    module NoPivot :
      functor (Det : DETERMINANT) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GFC_F.Dom.v option) Code.abstract -> 'c) -> 'c
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
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
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.Dom.v
          val make_result :
            'a wmatrix ->
            ([> 'a OD.Det.tag_lstate ] as 'b) list ->
            ('b list ->
             ('a, GFC_F.contr * GFC_F.Dom.v) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
        end
    module OutRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * int
          val make_result :
            'a wmatrix ->
            ([> `TRan of ('a, 'c ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, GFC_F.contr * 'c) Code.abstract -> 'd) -> 'd
        end
    module OutDetRank :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = GEF.PermList.perm_rep
                  type 'a ira = 'a GEF.PermList.ira
                  type 'a fra = 'a GEF.PermList.fra
                  type 'a pra = 'a GEF.PermList.pra
                  type 'a lstate =
                      ('a, GEF.PermList.perm_rep ref) Code.abstract
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
                    'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
                  val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                  val fin : 'a option
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.Dom.v * int
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TRan of ('a, 'c ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GFC_F.contr * GFC_F.Dom.v * 'c) Code.abstract ->
             ('a, 'd) Code.abstract) ->
            ('a, 'd) Code.abstract
        end
    module OutDetRankPivot :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin :
                    (unit ->
                     ([> `TRan of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GFC_F.vo ->
                    'b ->
                    ('c -> ('c -> ('a, unit) Code.abstract -> 'd) -> 'd)
                    option
                  val fin : 'a option
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.Dom.v * int * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TDet of 'a OD.Det.lstate
              | `TPivot of ('a, 'c ref) Code.abstract
              | `TRan of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list ->
             ('a, GFC_F.contr * GFC_F.Dom.v * 'd * 'c) Code.abstract ->
             ('a, 'e) Code.abstract) ->
            ('a, 'e) Code.abstract
        end
    module Out_L_U :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
                    ('a, 'b) Code.abstract ->
                    ([> `TLower of ('a, 'b) Code.abstract ] as 'c) list ->
                    ('c list ->
                     ('a, 'b) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val updt :
                    'a GFC_F.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    'a GFC_F.vo ->
                    'a GFC_F.vo ->
                    (([> `TLower of 'a GFC_F.vc ] as 'b) list ->
                     ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c)
                    option
                  val fin :
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * GFC_F.contr * IF.P.perm_rep
          val make_result :
            'a wmatrix ->
            ([> `TLower of ('a, 'c) Code.abstract
              | `TPivot of ('a, 'd ref) Code.abstract ]
             as 'b)
            list ->
            ('b list -> ('a, GFC_F.contr * 'c * 'd) Code.abstract -> 'e) ->
            'e
        end
    module Out_LU_Packed :
      functor (OD : OUTPUTDEP) ->
        sig
          module IF :
            sig
              module R :
                sig
                  type 'a lstate = ('a, int ref) Code.abstract
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
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list ->
                     ('b, int ref) Code.abstract -> ('b, 'c) Code.abstract) ->
                    ('b, 'c) Code.abstract
                  val succ :
                    unit ->
                    ([> `TRan of ('b, int ref) Code.abstract ] as 'a) list ->
                    ('a list -> ('b, unit) Code.abstract -> 'c) -> 'c
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
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm)
                        option
                    end
                  val fin : 'a option
                end
              module P :
                sig
                  type perm_rep = OD.PivotRep.perm_rep
                  type 'a ira = 'a OD.PivotRep.ira
                  type 'a fra = 'a OD.PivotRep.fra
                  type 'a pra = 'a OD.PivotRep.pra
                  type 'a lstate =
                      ('a, OD.PivotRep.perm_rep ref) Code.abstract
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
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, unit) Code.abstract -> ('a, 'd) Code.abstract) ->
                    ('a, 'd) Code.abstract
                  val add :
                    'a OD.PivotRep.fra ->
                    ([> `TPivot of
                          ('a, OD.PivotRep.perm_rep ref) Code.abstract ]
                     as 'b)
                    list ->
                    ('b list -> ('a, unit) Code.abstract option -> 'c) -> 'c
                  val fin :
                    (unit ->
                     ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                     ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd)
                    option
                end
              module L :
                sig
                  type 'a lstate = ('a, GFC_F.contr) Code.abstract
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
                    (unit ->
                     ([> `TLower of 'b ] as 'a) list ->
                     ('a list -> 'b -> 'c) -> 'c)
                    option
                  val wants_pack : bool
                end
            end
          type res = GFC_F.contr * IF.P.perm_rep
          val make_result :
            'a ->
            ([> `TLower of ('c, 'd) Code.abstract
              | `TPivot of ('c, 'e ref) Code.abstract ]
             as 'b)
            list -> ('b list -> ('c, 'd * 'e) Code.abstract -> 'f) -> 'f
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
                        (unit ->
                         ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                         GEF.TrackRank.lm)
                        option
                    end
                  module P :
                    sig
                      type perm_rep = F.Output(F).IF.P.perm_rep
                      type 'a ira = ('a, int) Code.abstract
                      type 'a fra = 'a F.Output(F).IF.P.fra
                      type 'a pra = 'a F.Output(F).IF.P.pra
                      type 'a lstate = 'a F.Output(F).IF.P.lstate
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a ira -> 'a ira -> 'a fra
                      val colrep : 'a ira -> 'a ira -> 'a fra
                      val decl :
                        ('a, int) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val add :
                        'a fra ->
                        (('a, unit) Code.abstract option,
                         [> 'a tag_lstate ] list, ('a, 'b) Code.abstract)
                        StateCPSMonad.monad
                      val fin :
                        (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                        option
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, GFC_F.contr) Code.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit ->
                        ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
                      val decl :
                        ('a, GFC_F.contr) Code.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm
                      val updt :
                        'a GFC_F.vc ->
                        ('a, int) Code.abstract ->
                        ('a, int) Code.abstract ->
                        'a GFC_F.vo ->
                        'a GFC_F.Dom.vc ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        (unit ->
                         ('a * [> 'a tag_lstate ] * 'b, GFC_F.contr) lm)
                        option
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
              | `TLower of ('a, GFC_F.contr) Code.abstract ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val init :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GFC_F.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list ->
             'a wmatrix * ('a, int ref) Code.abstract *
             ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
             ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val forward_elim :
            'a wmatrix * ('a, int ref) Code.abstract *
            ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of ('a, GFC_F.contr) Code.abstract
              | `TPivot of 'a F.Output(F).IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
          val backward_elim :
            unit ->
            ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
          val ge_gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
          val gen :
            ('a, F.Input.inp) Code.abstract ->
            ([> `TDet of 'a F.Det.lstate
              | `TLower of 'a O.IF.L.lstate
              | `TPivot of 'a O.IF.P.lstate
              | `TRan of 'a GEF.TrackRank.lstate ]
             as 'b)
            list ->
            ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
            ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = Code.perm list
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, Code.perm) Code.abstract
                type 'a pra = ('a, Code.perm list) Code.abstract
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int * Code.perm list
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = int array
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, int * int) Code.abstract
                type 'a pra = ('a, int array) Code.abstract
                type 'a lstate = ('a, int array ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int * int array
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, int array ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, int array ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.Dom.v * int
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr * int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = Code.perm list
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, Code.perm) Code.abstract
                type 'a pra = ('a, Code.perm list) Code.abstract
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * Code.perm list
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = Code.perm list
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, Code.perm) Code.abstract
                type 'a pra = ('a, Code.perm list) Code.abstract
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * GAC_F.contr * Code.perm list
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = Code.perm list
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, Code.perm) Code.abstract
                type 'a pra = ('a, Code.perm list) Code.abstract
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val decl :
                  ('a, GAC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                val updt :
                  'a GAC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_F.vo ->
                  'a GAC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_F.contr * Code.perm list
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
      ([> `TDet of unit | `TLower of ('a, GAC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_F.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GVC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * GVC_F.Dom.v
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GVC_F.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * GVC_F.Dom.v * int
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_F.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val decl :
                  ('a, GVC_F.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                val updt :
                  'a GVC_F.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_F.vo ->
                  'a GVC_F.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_F.contr * GVC_F.Dom.v * int
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_F.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_F.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_F.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_F.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_F.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
  end
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr * GAC_I.Dom.v
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr * int
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val decl :
                  ('a, GAC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                val updt :
                  'a GAC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_I.vo ->
                  'a GAC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_I.contr * GAC_I.Dom.v * int
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
  end
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * int
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v * int
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v * int
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = Code.perm list
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, Code.perm) Code.abstract
                type 'a pra = ('a, Code.perm list) Code.abstract
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_I.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val decl :
                  ('a, GVC_I.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                val updt :
                  'a GVC_I.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_I.vo ->
                  'a GVC_I.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_I.contr * GVC_I.Dom.v * int * Code.perm list
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_I.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_I.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_I.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_I.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GVC_I.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_R.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_R.contr * GAC_R.Dom.v
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_R.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_R.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm)
                  option
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
      ([> `TDet of unit | `TLower of ('a, GAC_R.contr) Code.abstract ] as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of unit
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of unit
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = GEF.PermList.perm_rep
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = 'a GEF.PermList.fra
                type 'a pra = 'a GEF.PermList.pra
                type 'a lstate =
                    ('a, GEF.PermList.perm_rep ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GAC_R.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val decl :
                  ('a, GAC_R.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                val updt :
                  'a GAC_R.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GAC_R.vo ->
                  'a GAC_R.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GAC_R.contr * GAC_R.Dom.v * int
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GAC_R.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_R.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GAC_R.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of ('a, GAC_R.contr) Code.abstract
        | `TPivot of ('a, GEF.PermList.perm_rep ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GAC_R.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract * ('a, GAC_R.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = Code.perm list
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, Code.perm) Code.abstract
                type 'a pra = ('a, Code.perm list) Code.abstract
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_Z3.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                val decl :
                  ('a, GVC_Z3.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                val updt :
                  'a GVC_Z3.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_Z3.vo ->
                  'a GVC_Z3.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_Z3.contr * GVC_Z3.Dom.v * int * Code.perm list
        val make_result :
          'a G_GVC_Z3.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_Z3.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract *
            ('a, GVC_Z3.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_Z3.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_Z3.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z3.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_Z3.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_Z3.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_Z3.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z3.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_Z3.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_Z3.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z3.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_Z3.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z3.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
                  (unit ->
                   ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b, int)
                   GEF.TrackRank.lm)
                  option
              end
            module P :
              sig
                type perm_rep = Code.perm list
                type 'a ira = ('a, int) Code.abstract
                type 'a fra = ('a, Code.perm) Code.abstract
                type 'a pra = ('a, Code.perm list) Code.abstract
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a ira -> 'a ira -> 'a fra
                val colrep : 'a ira -> 'a ira -> 'a fra
                val decl :
                  ('a, int) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val add :
                  'a fra ->
                  (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
                   ('a, 'b) Code.abstract)
                  StateCPSMonad.monad
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, perm_rep) lm)
                  option
              end
            module L :
              sig
                type 'a lstate = ('a, GVC_Z19.contr) Code.abstract
                type 'a tag_lstate = [ `TLower of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val mfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                val decl :
                  ('a, GVC_Z19.contr) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
                val updt :
                  'a GVC_Z19.vc ->
                  ('a, int) Code.abstract ->
                  ('a, int) Code.abstract ->
                  'a GVC_Z19.vo ->
                  'a GVC_Z19.Dom.vc ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  (unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm)
                  option
                val wants_pack : bool
              end
          end
        type res = GVC_Z19.contr * GVC_Z19.Dom.v * int * Code.perm list
        val make_result :
          'a G_GVC_Z19.wmatrix ->
          ('a, res,
           [> `TDet of
                ('a, int ref) Code.abstract *
                ('a, GVC_Z19.Dom.v ref) Code.abstract
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
            ('a, int ref) Code.abstract *
            ('a, GVC_Z19.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_Z19.contr) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val init :
      ('a, GVC_Z19.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z19.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_Z19.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       'a G_GVC_Z19.wmatrix * ('a, int ref) Code.abstract *
       ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
       ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val forward_elim :
      'a G_GVC_Z19.wmatrix * ('a, int ref) Code.abstract *
      ('a, int ref) Code.abstract * ('a, int) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z19.Dom.v ref) Code.abstract
        | `TLower of ('a, GVC_Z19.contr) Code.abstract
        | `TPivot of ('a, Code.perm list ref) Code.abstract
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'c) -> 'c
    val backward_elim :
      unit -> ('a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c) option
    val ge_gen :
      ('a, GVC_Z19.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z19.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
    val gen :
      ('a, GVC_Z19.contr) Code.abstract ->
      ([> `TDet of
            ('a, int ref) Code.abstract *
            ('a, GVC_Z19.Dom.v ref) Code.abstract
        | `TLower of 'a O.IF.L.lstate
        | `TPivot of 'a O.IF.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, O.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
  end
val resFA1 : ('a, GAC_F.contr -> GenFA1.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0.) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs_float (snd i_17)) < (abs_float t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14
          else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0.) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((t_5.(j_20)).(j_22) -. ((t_21 /. i_19) *. (t_5.(t_10)).(j_22)))
           done;
           (t_5.(j_20)).(t_11) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resFA2 : ('a, GAC_F.contr -> GenFA2.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_12 to (t_7 - 1) do
       let t_18 = (t_5.(j_17)).(t_13) in
       if (t_18 <> 0.) then
        (match (! t_14) with
         | Some (i_19) ->
            if ((abs_float (snd i_19)) < (abs_float t_18)) then
             (t_14 := (Some (j_17, t_18)))
            else ()
         | None -> (t_14 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_12 + 1) to (t_7 - 1) do
          let t_23 = (t_5.(j_22)).(t_13) in
          if (t_23 <> 0.) then begin
           for j_24 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_22)).(j_24) <-
             ((t_5.(j_22)).(j_24) -. ((t_23 /. i_21) *. (t_5.(t_12)).(j_24)))
           done;
           (t_5.(j_22)).(t_13) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_21))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)))>.
val resFA3 : ('a, GAC_F.contr -> GenFA3.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0.) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs_float (snd i_17)) < (abs_float t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14
          else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0.) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((t_5.(j_20)).(j_22) -. ((t_21 /. i_19) *. (t_5.(t_10)).(j_22)))
           done;
           (t_5.(j_20)).(t_11) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resFA4 : ('a, GAC_F.contr -> GenFA4.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_12 to (t_7 - 1) do
       let t_18 = (t_5.(j_17)).(t_13) in
       if (t_18 <> 0.) then
        (match (! t_14) with
         | Some (i_19) ->
            if ((abs_float (snd i_19)) < (abs_float t_18)) then
             (t_14 := (Some (j_17, t_18)))
            else ()
         | None -> (t_14 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_12 + 1) to (t_7 - 1) do
          let t_23 = (t_5.(j_22)).(t_13) in
          if (t_23 <> 0.) then begin
           for j_24 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_22)).(j_24) <-
             ((t_5.(j_22)).(j_24) -. ((t_23 /. i_21) *. (t_5.(t_12)).(j_24)))
           done;
           (t_5.(j_22)).(t_13) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_21))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2))>.
val resFV1 : ('a, GVC_F.contr -> GenFV1.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_21 =
     begin
      for j_18 = t_8 to (t_6 - 1) do
       let t_19 = (t_4.arr).((j_18 * t_4.m) + t_9) in
       if (t_19 <> 0.) then
        (match (! t_10) with
         | Some (i_20) ->
            if ((abs_float (snd i_20)) < (abs_float t_19)) then
             (t_10 := (Some (j_18, t_19)))
            else ()
         | None -> (t_10 := (Some (j_18, t_19))))
       else ()
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((fst i_11) <> t_8) then
           let a_12 = t_4.arr
           and m_13 = t_4.m in
           let i1_14 = (t_8 * m_13)
           and i2_15 = ((fst i_11) * m_13) in
           for i_16 = 0 to (m_13 - 1) do
            let t_17 = a_12.(i1_14 + i_16) in
            a_12.(i1_14 + i_16) <- a_12.(i2_15 + i_16);
            a_12.(i2_15 + i_16) <- t_17
           done
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_8 + 1) to (t_6 - 1) do
          let t_24 = (t_4.arr).((j_23 * t_4.m) + t_9) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_9 + 1) to (t_5 - 1) do
            (t_4.arr).((j_23 * t_4.m) + j_25) <-
             ((t_4.arr).((j_23 * t_4.m) + j_25) -.
               ((t_24 /. i_22) *. (t_4.arr).((t_8 * t_4.m) + j_25)))
           done;
           (t_4.arr).((j_23 * t_4.m) + t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
val resFV2 : ('a, GVC_F.contr -> GenFV2.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_23 =
     begin
      for j_20 = t_10 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_11) in
       if (t_21 <> 0.) then
        (match (! t_12) with
         | Some (i_22) ->
            if ((abs_float (snd i_22)) < (abs_float t_21)) then
             (t_12 := (Some (j_20, t_21)))
            else ()
         | None -> (t_12 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((fst i_13) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_10 + 1) to (t_6 - 1) do
          let t_26 = (t_4.arr).((j_25 * t_4.m) + t_11) in
          if (t_26 <> 0.) then begin
           for j_27 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_27) <-
             ((t_4.arr).((j_25 * t_4.m) + j_27) -.
               ((t_26 /. i_24) *. (t_4.arr).((t_10 * t_4.m) + j_27)))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_11) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_24))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)))>.
val resFV3 : ('a, GVC_F.contr -> GenFV3.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_21 =
     begin
      for j_18 = t_8 to (t_6 - 1) do
       let t_19 = (t_4.arr).((j_18 * t_4.m) + t_9) in
       if (t_19 <> 0.) then
        (match (! t_10) with
         | Some (i_20) ->
            if ((abs_float (snd i_20)) < (abs_float t_19)) then
             (t_10 := (Some (j_18, t_19)))
            else ()
         | None -> (t_10 := (Some (j_18, t_19))))
       else ()
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((fst i_11) <> t_8) then
           let a_12 = t_4.arr
           and m_13 = t_4.m in
           let i1_14 = (t_8 * m_13)
           and i2_15 = ((fst i_11) * m_13) in
           for i_16 = 0 to (m_13 - 1) do
            let t_17 = a_12.(i1_14 + i_16) in
            a_12.(i1_14 + i_16) <- a_12.(i2_15 + i_16);
            a_12.(i2_15 + i_16) <- t_17
           done
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_8 + 1) to (t_6 - 1) do
          let t_24 = (t_4.arr).((j_23 * t_4.m) + t_9) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_9 + 1) to (t_5 - 1) do
            (t_4.arr).((j_23 * t_4.m) + j_25) <-
             ((t_4.arr).((j_23 * t_4.m) + j_25) -.
               ((t_24 /. i_22) *. (t_4.arr).((t_8 * t_4.m) + j_25)))
           done;
           (t_4.arr).((j_23 * t_4.m) + t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
val resFV4 : ('a, GVC_F.contr -> GenFV4.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_23 =
     begin
      for j_20 = t_10 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_11) in
       if (t_21 <> 0.) then
        (match (! t_12) with
         | Some (i_22) ->
            if ((abs_float (snd i_22)) < (abs_float t_21)) then
             (t_12 := (Some (j_20, t_21)))
            else ()
         | None -> (t_12 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((fst i_13) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_10 + 1) to (t_6 - 1) do
          let t_26 = (t_4.arr).((j_25 * t_4.m) + t_11) in
          if (t_26 <> 0.) then begin
           for j_27 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_27) <-
             ((t_4.arr).((j_25 * t_4.m) + j_27) -.
               ((t_26 /. i_24) *. (t_4.arr).((t_10 * t_4.m) + j_27)))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_11) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_24))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
val resFV5 : ('a, GVC_F.contr -> GenFV5.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_31 =
     begin
      for j_27 = t_10 to (t_6 - 1) do
       for j_28 = t_11 to (t_5 - 1) do
        let t_29 = (t_4.arr).((j_27 * t_4.m) + j_28) in
        if (t_29 <> 0.) then
         (match (! t_12) with
          | Some (i_30) ->
             if ((abs_float (snd i_30)) < (abs_float t_29)) then
              (t_12 := (Some ((j_27, j_28), t_29)))
             else ()
          | None -> (t_12 := (Some ((j_27, j_28), t_29))))
        else ()
       done
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((snd (fst i_13)) <> t_11) then begin
           let a_20 = t_4.arr
           and nm_21 = (t_4.n * t_4.m)
           and m_22 = t_4.m in
           let rec loop_23 =
            fun i1_24 ->
             fun i2_25 ->
              if (i2_25 < nm_21) then
               let t_26 = a_20.(i1_24) in
               a_20.(i1_24) <- a_20.(i2_25);
               a_20.(i2_25) <- t_26;
               (loop_23 (i1_24 + m_22) (i2_25 + m_22))
              else () in
           (loop_23 t_11 (snd (fst i_13)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_13)) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((snd (fst i_13)) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_31 with
     | Some (i_32) ->
        begin
         for j_33 = (t_10 + 1) to (t_6 - 1) do
          let t_34 = (t_4.arr).((j_33 * t_4.m) + t_11) in
          if (t_34 <> 0.) then begin
           for j_35 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_33 * t_4.m) + j_35) <-
             ((t_4.arr).((j_33 * t_4.m) + j_35) -.
               ((t_34 /. i_32) *. (t_4.arr).((t_10 * t_4.m) + j_35)))
           done;
           (t_4.arr).((j_33 * t_4.m) + t_11) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_32))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
val resIA1 : ('a, GAC_I.contr -> GenIA1.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_12 to (t_7 - 1) do
       let t_18 = (t_5.(j_17)).(t_13) in
       if (t_18 <> 0) then
        (match (! t_14) with
         | Some (i_19) ->
            if ((abs (snd i_19)) > (abs t_18)) then
             (t_14 := (Some (j_17, t_18)))
            else ()
         | None -> (t_14 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_12 + 1) to (t_7 - 1) do
          let t_23 = (t_5.(j_22)).(t_13) in
          if (t_23 <> 0) then begin
           for j_24 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_22)).(j_24) <-
             ((((t_5.(j_22)).(j_24) * i_21) - ((t_5.(t_12)).(j_24) * t_23)) /
               (! t_8))
           done;
           (t_5.(j_22)).(t_13) <- 0
          end else ()
         done;
         (t_8 := i_21)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resIA2 : ('a, GAC_I.contr -> GenIA2.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_12 to (t_7 - 1) do
       let t_18 = (t_5.(j_17)).(t_13) in
       if (t_18 <> 0) then
        (match (! t_14) with
         | Some (i_19) ->
            if ((abs (snd i_19)) > (abs t_18)) then
             (t_14 := (Some (j_17, t_18)))
            else ()
         | None -> (t_14 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_12 + 1) to (t_7 - 1) do
          let t_23 = (t_5.(j_22)).(t_13) in
          if (t_23 <> 0) then begin
           for j_24 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_22)).(j_24) <-
             ((((t_5.(j_22)).(j_24) * i_21) - ((t_5.(t_12)).(j_24) * t_23)) /
               (! t_8))
           done;
           (t_5.(j_22)).(t_13) <- 0
          end else ()
         done;
         (t_8 := i_21)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0
    else if ((! t_9) = 1) then (! t_8)
    else (~- (! t_8)))>.
val resIA3 : ('a, GAC_I.contr -> GenIA3.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_12 to (t_7 - 1) do
       let t_18 = (t_5.(j_17)).(t_13) in
       if (t_18 <> 0) then
        (match (! t_14) with
         | Some (i_19) ->
            if ((abs (snd i_19)) > (abs t_18)) then
             (t_14 := (Some (j_17, t_18)))
            else ()
         | None -> (t_14 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_12 + 1) to (t_7 - 1) do
          let t_23 = (t_5.(j_22)).(t_13) in
          if (t_23 <> 0) then begin
           for j_24 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_22)).(j_24) <-
             ((((t_5.(j_22)).(j_24) * i_21) - ((t_5.(t_12)).(j_24) * t_23)) /
               (! t_8))
           done;
           (t_5.(j_22)).(t_13) <- 0
          end else ()
         done;
         (t_8 := i_21)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resIA4 : ('a, GAC_I.contr -> GenIA4.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_12 to (t_7 - 1) do
       let t_18 = (t_5.(j_17)).(t_13) in
       if (t_18 <> 0) then
        (match (! t_14) with
         | Some (i_19) ->
            if ((abs (snd i_19)) > (abs t_18)) then
             (t_14 := (Some (j_17, t_18)))
            else ()
         | None -> (t_14 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_12 + 1) to (t_7 - 1) do
          let t_23 = (t_5.(j_22)).(t_13) in
          if (t_23 <> 0) then begin
           for j_24 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_22)).(j_24) <-
             ((((t_5.(j_22)).(j_24) * i_21) - ((t_5.(t_12)).(j_24) * t_23)) /
               (! t_8))
           done;
           (t_5.(j_22)).(t_13) <- 0
          end else ()
         done;
         (t_8 := i_21)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0
    else if ((! t_9) = 1) then (! t_8)
    else (~- (! t_8)), (! t_2))>.
val resIV1 : ('a, GVC_I.contr -> GenIV1.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_23 =
     begin
      for j_20 = t_10 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_11) in
       if (t_21 <> 0) then
        (match (! t_12) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_12 := (Some (j_20, t_21)))
            else ()
         | None -> (t_12 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((fst i_13) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_10 + 1) to (t_6 - 1) do
          let t_26 = (t_4.arr).((j_25 * t_4.m) + t_11) in
          if (t_26 <> 0) then begin
           for j_27 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_27) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_27) * i_24) -
                ((t_4.arr).((t_10 * t_4.m) + j_27) * t_26)) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_11) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
val resIV2 : ('a, GVC_I.contr -> GenIV2.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_23 =
     begin
      for j_20 = t_10 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_11) in
       if (t_21 <> 0) then
        (match (! t_12) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_12 := (Some (j_20, t_21)))
            else ()
         | None -> (t_12 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((fst i_13) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_10 + 1) to (t_6 - 1) do
          let t_26 = (t_4.arr).((j_25 * t_4.m) + t_11) in
          if (t_26 <> 0) then begin
           for j_27 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_27) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_27) * i_24) -
                ((t_4.arr).((t_10 * t_4.m) + j_27) * t_26)) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_11) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)))>.
val resIV3 : ('a, GVC_I.contr -> GenIV3.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_23 =
     begin
      for j_20 = t_10 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_11) in
       if (t_21 <> 0) then
        (match (! t_12) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_12 := (Some (j_20, t_21)))
            else ()
         | None -> (t_12 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((fst i_13) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_10 + 1) to (t_6 - 1) do
          let t_26 = (t_4.arr).((j_25 * t_4.m) + t_11) in
          if (t_26 <> 0) then begin
           for j_27 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_27) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_27) * i_24) -
                ((t_4.arr).((t_10 * t_4.m) + j_27) * t_26)) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_11) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
val resIV4 : ('a, GVC_I.contr -> GenIV4.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_23 =
     begin
      for j_20 = t_10 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_11) in
       if (t_21 <> 0) then
        (match (! t_12) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_12 := (Some (j_20, t_21)))
            else ()
         | None -> (t_12 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((fst i_13) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_10 + 1) to (t_6 - 1) do
          let t_26 = (t_4.arr).((j_25 * t_4.m) + t_11) in
          if (t_26 <> 0) then begin
           for j_27 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_27) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_27) * i_24) -
                ((t_4.arr).((t_10 * t_4.m) + j_27) * t_26)) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_11) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
val resIV5 : ('a, GVC_I.contr -> GenIV5.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_31 =
     begin
      for j_27 = t_10 to (t_6 - 1) do
       for j_28 = t_11 to (t_5 - 1) do
        let t_29 = (t_4.arr).((j_27 * t_4.m) + j_28) in
        if (t_29 <> 0) then
         (match (! t_12) with
          | Some (i_30) ->
             if ((abs (snd i_30)) > (abs t_29)) then
              (t_12 := (Some ((j_27, j_28), t_29)))
             else ()
          | None -> (t_12 := (Some ((j_27, j_28), t_29))))
        else ()
       done
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((snd (fst i_13)) <> t_11) then begin
           let a_20 = t_4.arr
           and nm_21 = (t_4.n * t_4.m)
           and m_22 = t_4.m in
           let rec loop_23 =
            fun i1_24 ->
             fun i2_25 ->
              if (i2_25 < nm_21) then
               let t_26 = a_20.(i1_24) in
               a_20.(i1_24) <- a_20.(i2_25);
               a_20.(i2_25) <- t_26;
               (loop_23 (i1_24 + m_22) (i2_25 + m_22))
              else () in
           (loop_23 t_11 (snd (fst i_13)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_13)) <> t_10) then begin
           let a_14 = t_4.arr
           and m_15 = t_4.m in
           let i1_16 = (t_10 * m_15)
           and i2_17 = ((snd (fst i_13)) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_14.(i1_16 + i_18) in
            a_14.(i1_16 + i_18) <- a_14.(i2_17 + i_18);
            a_14.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_31 with
     | Some (i_32) ->
        begin
         for j_33 = (t_10 + 1) to (t_6 - 1) do
          let t_34 = (t_4.arr).((j_33 * t_4.m) + t_11) in
          if (t_34 <> 0) then begin
           for j_35 = (t_11 + 1) to (t_5 - 1) do
            (t_4.arr).((j_33 * t_4.m) + j_35) <-
             ((((t_4.arr).((j_33 * t_4.m) + j_35) * i_32) -
                ((t_4.arr).((t_10 * t_4.m) + j_35) * t_34)) / (! t_7))
           done;
           (t_4.arr).((j_33 * t_4.m) + t_11) <- 0
          end else ()
         done;
         (t_7 := i_32)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
val resIV6 : ('a, GVC_I.contr -> GenIV6.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   let t_9 = (ref ([])) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_32 =
     begin
      for j_28 = t_11 to (t_6 - 1) do
       for j_29 = t_12 to (t_5 - 1) do
        let t_30 = (t_4.arr).((j_28 * t_4.m) + j_29) in
        if (t_30 <> 0) then
         (match (! t_13) with
          | Some (i_31) ->
             if ((abs (snd i_31)) > (abs t_30)) then
              (t_13 := (Some ((j_28, j_29), t_30)))
             else ()
          | None -> (t_13 := (Some ((j_28, j_29), t_30))))
        else ()
       done
      done;
      (match (! t_13) with
       | Some (i_14) ->
          if ((snd (fst i_14)) <> t_12) then begin
           begin
            let a_21 = t_4.arr
            and nm_22 = (t_4.n * t_4.m)
            and m_23 = t_4.m in
            let rec loop_24 =
             fun i1_25 ->
              fun i2_26 ->
               if (i2_26 < nm_22) then
                let t_27 = a_21.(i1_25) in
                a_21.(i1_25) <- a_21.(i2_26);
                a_21.(i2_26) <- t_27;
                (loop_24 (i1_25 + m_23) (i2_26 + m_23))
               else () in
            (loop_24 t_12 (snd (fst i_14)));
            (t_8 := (~- (! t_8)))
           end;
           (t_9 := ((ColSwap ((snd (fst i_14)), t_11)) :: (! t_9)))
          end else ();
          if ((fst (fst i_14)) <> t_11) then begin
           begin
            let a_15 = t_4.arr
            and m_16 = t_4.m in
            let i1_17 = (t_11 * m_16)
            and i2_18 = ((snd (fst i_14)) * m_16) in
            for i_19 = 0 to (m_16 - 1) do
             let t_20 = a_15.(i1_17 + i_19) in
             a_15.(i1_17 + i_19) <- a_15.(i2_18 + i_19);
             a_15.(i2_18 + i_19) <- t_20
            done;
            (t_8 := (~- (! t_8)))
           end;
           (t_9 := ((RowSwap ((fst (fst i_14)), t_11)) :: (! t_9)))
          end else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_32 with
     | Some (i_33) ->
        begin
         for j_34 = (t_11 + 1) to (t_6 - 1) do
          let t_35 = (t_4.arr).((j_34 * t_4.m) + t_12) in
          if (t_35 <> 0) then begin
           for j_36 = (t_12 + 1) to (t_5 - 1) do
            (t_4.arr).((j_34 * t_4.m) + j_36) <-
             ((((t_4.arr).((j_34 * t_4.m) + j_36) * i_33) -
                ((t_4.arr).((t_11 * t_4.m) + j_36) * t_35)) / (! t_7))
           done;
           (t_4.arr).((j_34 * t_4.m) + t_12) <- 0
          end else ()
         done;
         (t_7 := i_33)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2), (! t_9))>.
val resFA11 : ('a, GAC_F.contr -> GenFA11.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_21 =
     begin
      for j_17 = t_10 to (t_7 - 1) do
       for j_18 = t_11 to (t_6 - 1) do
        let t_19 = (t_5.(j_17)).(j_18) in
        if (t_19 <> 0.) then
         (match (! t_12) with
          | Some (i_20) ->
             if ((abs_float (snd i_20)) < (abs_float t_19)) then
              (t_12 := (Some ((j_17, j_18), t_19)))
             else ()
          | None -> (t_12 := (Some ((j_17, j_18), t_19))))
        else ()
       done
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((snd (fst i_13)) <> t_11) then
           for r_15 = 0 to ((Array.length t_5) - 1) do
            let t_16 = (t_5.(r_15)).(t_11) in
            (t_5.(r_15)).(t_11) <- (t_5.(r_15)).(snd (fst i_13));
            (t_5.(r_15)).(snd (fst i_13)) <- t_16
           done
          else ();
          if ((fst (fst i_13)) <> t_10) then
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(snd (fst i_13));
           t_5.(snd (fst i_13)) <- t_14
          else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_10 + 1) to (t_7 - 1) do
          let t_24 = (t_5.(j_23)).(t_11) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             ((t_5.(j_23)).(j_25) -. ((t_24 /. i_22) *. (t_5.(t_10)).(j_25)))
           done;
           (t_5.(j_23)).(t_11) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resFA12 : ('a, GAC_F.contr -> GenFA12.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_23 =
     begin
      for j_19 = t_12 to (t_7 - 1) do
       for j_20 = t_13 to (t_6 - 1) do
        let t_21 = (t_5.(j_19)).(j_20) in
        if (t_21 <> 0.) then
         (match (! t_14) with
          | Some (i_22) ->
             if ((abs_float (snd i_22)) < (abs_float t_21)) then
              (t_14 := (Some ((j_19, j_20), t_21)))
             else ()
          | None -> (t_14 := (Some ((j_19, j_20), t_21))))
        else ()
       done
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((snd (fst i_15)) <> t_13) then begin
           for r_17 = 0 to ((Array.length t_5) - 1) do
            let t_18 = (t_5.(r_17)).(t_13) in
            (t_5.(r_17)).(t_13) <- (t_5.(r_17)).(snd (fst i_15));
            (t_5.(r_17)).(snd (fst i_15)) <- t_18
           done;
           (t_9 := (~- (! t_9)))
          end else ();
          if ((fst (fst i_15)) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(snd (fst i_15));
           t_5.(snd (fst i_15)) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_12 + 1) to (t_7 - 1) do
          let t_26 = (t_5.(j_25)).(t_13) in
          if (t_26 <> 0.) then begin
           for j_27 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_25)).(j_27) <-
             ((t_5.(j_25)).(j_27) -. ((t_26 /. i_24) *. (t_5.(t_12)).(j_27)))
           done;
           (t_5.(j_25)).(t_13) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_24))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)))>.
val resFA13 : ('a, GAC_F.contr -> GenFA13.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_21 =
     begin
      for j_17 = t_10 to (t_7 - 1) do
       for j_18 = t_11 to (t_6 - 1) do
        let t_19 = (t_5.(j_17)).(j_18) in
        if (t_19 <> 0.) then
         (match (! t_12) with
          | Some (i_20) ->
             if ((abs_float (snd i_20)) < (abs_float t_19)) then
              (t_12 := (Some ((j_17, j_18), t_19)))
             else ()
          | None -> (t_12 := (Some ((j_17, j_18), t_19))))
        else ()
       done
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((snd (fst i_13)) <> t_11) then
           for r_15 = 0 to ((Array.length t_5) - 1) do
            let t_16 = (t_5.(r_15)).(t_11) in
            (t_5.(r_15)).(t_11) <- (t_5.(r_15)).(snd (fst i_13));
            (t_5.(r_15)).(snd (fst i_13)) <- t_16
           done
          else ();
          if ((fst (fst i_13)) <> t_10) then
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(snd (fst i_13));
           t_5.(snd (fst i_13)) <- t_14
          else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_10 + 1) to (t_7 - 1) do
          let t_24 = (t_5.(j_23)).(t_11) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             ((t_5.(j_23)).(j_25) -. ((t_24 /. i_22) *. (t_5.(t_10)).(j_25)))
           done;
           (t_5.(j_23)).(t_11) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resFA14 : ('a, GAC_F.contr -> GenFA14.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_23 =
     begin
      for j_19 = t_12 to (t_7 - 1) do
       for j_20 = t_13 to (t_6 - 1) do
        let t_21 = (t_5.(j_19)).(j_20) in
        if (t_21 <> 0.) then
         (match (! t_14) with
          | Some (i_22) ->
             if ((abs_float (snd i_22)) < (abs_float t_21)) then
              (t_14 := (Some ((j_19, j_20), t_21)))
             else ()
          | None -> (t_14 := (Some ((j_19, j_20), t_21))))
        else ()
       done
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((snd (fst i_15)) <> t_13) then begin
           for r_17 = 0 to ((Array.length t_5) - 1) do
            let t_18 = (t_5.(r_17)).(t_13) in
            (t_5.(r_17)).(t_13) <- (t_5.(r_17)).(snd (fst i_15));
            (t_5.(r_17)).(snd (fst i_15)) <- t_18
           done;
           (t_9 := (~- (! t_9)))
          end else ();
          if ((fst (fst i_15)) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(snd (fst i_15));
           t_5.(snd (fst i_15)) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_12 + 1) to (t_7 - 1) do
          let t_26 = (t_5.(j_25)).(t_13) in
          if (t_26 <> 0.) then begin
           for j_27 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_25)).(j_27) <-
             ((t_5.(j_25)).(j_27) -. ((t_26 /. i_24) *. (t_5.(t_12)).(j_27)))
           done;
           (t_5.(j_25)).(t_13) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_24))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2))>.
val resFA24 : ('a, GAC_F.contr -> GenFA24.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   let t_10 = (ref ([])) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_13 = (! t_2) in
    let t_14 = (! t_3) in
    let t_15 = (ref (None)) in
    let t_21 =
     begin
      for j_18 = t_13 to (t_7 - 1) do
       let t_19 = (t_5.(j_18)).(t_14) in
       if (t_19 <> 0.) then
        (match (! t_15) with
         | Some (i_20) ->
            if ((abs_float (snd i_20)) < (abs_float t_19)) then
             (t_15 := (Some (j_18, t_19)))
            else ()
         | None -> (t_15 := (Some (j_18, t_19))))
       else ()
      done;
      (match (! t_15) with
       | Some (i_16) ->
          if ((fst i_16) <> t_13) then begin
           begin
            let t_17 = t_5.(t_13) in
            t_5.(t_13) <- t_5.(fst i_16);
            t_5.(fst i_16) <- t_17;
            (t_9 := (~- (! t_9)))
           end;
           (t_10 := ((RowSwap ((fst i_16), t_13)) :: (! t_10)))
          end else ();
          (Some (snd i_16))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_13 + 1) to (t_7 - 1) do
          let t_24 = (t_5.(j_23)).(t_14) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_14 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             ((t_5.(j_23)).(j_25) -. ((t_24 /. i_22) *. (t_5.(t_13)).(j_25)))
           done;
           (t_5.(j_23)).(t_14) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2), (! t_10))>.
val resFA25 : ('a, GAC_F.contr -> GenFA25.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   let t_11 = (ref (Array.init t_7 (fun i_10 -> i_10))) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_14 = (! t_2) in
    let t_15 = (! t_3) in
    let t_16 = (ref (None)) in
    let t_26 =
     begin
      for j_23 = t_14 to (t_7 - 1) do
       let t_24 = (t_5.(j_23)).(t_15) in
       if (t_24 <> 0.) then
        (match (! t_16) with
         | Some (i_25) ->
            if ((abs_float (snd i_25)) < (abs_float t_24)) then
             (t_16 := (Some (j_23, t_24)))
            else ()
         | None -> (t_16 := (Some (j_23, t_24))))
       else ()
      done;
      (match (! t_16) with
       | Some (i_17) ->
          if ((fst i_17) <> t_14) then begin
           begin
            let t_18 = t_5.(t_14) in
            t_5.(t_14) <- t_5.(fst i_17);
            t_5.(fst i_17) <- t_18;
            (t_9 := (~- (! t_9)))
           end;
           (t_11 :=
             let (x_19, y_20) = ((fst i_17), t_14) in
             let b_21 = (! t_11)
             and t_22 = (! t_11).(x_19) in
             b_21.(x_19) <- b_21.(y_20);
             b_21.(y_20) <- t_22;
             b_21)
          end else ();
          (Some (snd i_17))
       | None -> (None))
     end in
    (match t_26 with
     | Some (i_27) ->
        begin
         for j_28 = (t_14 + 1) to (t_7 - 1) do
          let t_29 = (t_5.(j_28)).(t_15) in
          if (t_29 <> 0.) then begin
           for j_30 = (t_15 + 1) to (t_6 - 1) do
            (t_5.(j_28)).(j_30) <-
             ((t_5.(j_28)).(j_30) -. ((t_29 /. i_27) *. (t_5.(t_14)).(j_30)))
           done;
           (t_5.(j_28)).(t_15) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_27))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2), (! t_11))>.
val resFA26 : ('a, GAC_F.contr -> GenFA26.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0.) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs_float (snd i_17)) < (abs_float t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14
          else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0.) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((t_5.(j_20)).(j_22) -. ((t_21 /. i_19) *. (t_5.(t_10)).(j_22)))
           done;
           (t_5.(j_20)).(t_11) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resRA1 : ('a, GAC_R.contr -> GenRA1.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_19 =
     begin
      let t_15 = (t_5.(t_10)).(t_11) in
      if (t_15 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_12 := (Some (t_10, t_15)))
      else
       let rec loop_16 =
        fun j_17 ->
         if (j_17 < t_7) then
          let t_18 = (t_5.(j_17)).(t_11) in
          if (t_18 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_16 (j_17 + 1))
          else (t_12 := (Some (j_17, t_18)))
         else () in
       (loop_16 (t_10 + 1));
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14
          else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_10 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_11) in
          if (t_22 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_23 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_23)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_22 i_20) (t_5.(t_10)).(j_23)))
           done;
           (t_5.(j_21)).(t_11) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resRA2 : ('a, GAC_R.contr -> GenRA2.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_21 =
     begin
      let t_17 = (t_5.(t_12)).(t_13) in
      if (t_17 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_14 := (Some (t_12, t_17)))
      else
       let rec loop_18 =
        fun j_19 ->
         if (j_19 < t_7) then
          let t_20 = (t_5.(j_19)).(t_13) in
          if (t_20 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_18 (j_19 + 1))
          else (t_14 := (Some (j_19, t_20)))
         else () in
       (loop_18 (t_12 + 1));
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_12 + 1) to (t_7 - 1) do
          let t_24 = (t_5.(j_23)).(t_13) in
          if (t_24 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_25 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_23)).(j_25)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_24 i_22) (t_5.(t_12)).(j_25)))
           done;
           (t_5.(j_23)).(t_13) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_9) = 1) then (! t_8)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_8)))>.
val resRA3 : ('a, GAC_R.contr -> GenRA3.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_19 =
     begin
      let t_15 = (t_5.(t_10)).(t_11) in
      if (t_15 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_12 := (Some (t_10, t_15)))
      else
       let rec loop_16 =
        fun j_17 ->
         if (j_17 < t_7) then
          let t_18 = (t_5.(j_17)).(t_11) in
          if (t_18 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_16 (j_17 + 1))
          else (t_12 := (Some (j_17, t_18)))
         else () in
       (loop_16 (t_10 + 1));
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14
          else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_10 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_11) in
          if (t_22 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_23 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_23)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_22 i_20) (t_5.(t_10)).(j_23)))
           done;
           (t_5.(j_21)).(t_11) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resRA4 : ('a, GAC_R.contr -> GenRA4.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_21 =
     begin
      let t_17 = (t_5.(t_12)).(t_13) in
      if (t_17 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_14 := (Some (t_12, t_17)))
      else
       let rec loop_18 =
        fun j_19 ->
         if (j_19 < t_7) then
          let t_20 = (t_5.(j_19)).(t_13) in
          if (t_20 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_18 (j_19 + 1))
          else (t_14 := (Some (j_19, t_20)))
         else () in
       (loop_18 (t_12 + 1));
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_12 + 1) to (t_7 - 1) do
          let t_24 = (t_5.(j_23)).(t_13) in
          if (t_24 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_25 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_23)).(j_25)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_24 i_22) (t_5.(t_12)).(j_25)))
           done;
           (t_5.(j_23)).(t_13) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_9) = 1) then (! t_8)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_8)),
    (! t_2))>.
val resFA5 : ('a, GAC_F.contr * int -> GenFA5.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_19 =
     begin
      for j_16 = t_11 to (t_8 - 1) do
       let t_17 = (t_5.(j_16)).(t_12) in
       if (t_17 <> 0.) then
        (match (! t_13) with
         | Some (i_18) ->
            if ((abs_float (snd i_18)) < (abs_float t_17)) then
             (t_13 := (Some (j_16, t_17)))
            else ()
         | None -> (t_13 := (Some (j_16, t_17))))
       else ()
      done;
      (match (! t_13) with
       | Some (i_14) ->
          if ((fst i_14) <> t_11) then
           let t_15 = t_5.(t_11) in
           t_5.(t_11) <- t_5.(fst i_14);
           t_5.(fst i_14) <- t_15
          else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_11 + 1) to (t_8 - 1) do
          let t_22 = (t_5.(j_21)).(t_12) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_12 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_11)).(j_23)))
           done;
           (t_5.(j_21)).(t_12) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resFA6 : ('a, GAC_F.contr * int -> GenFA6.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   let t_9 = (ref 1.) in
   let t_10 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_13 = (! t_2) in
    let t_14 = (! t_3) in
    let t_15 = (ref (None)) in
    let t_21 =
     begin
      for j_18 = t_13 to (t_8 - 1) do
       let t_19 = (t_5.(j_18)).(t_14) in
       if (t_19 <> 0.) then
        (match (! t_15) with
         | Some (i_20) ->
            if ((abs_float (snd i_20)) < (abs_float t_19)) then
             (t_15 := (Some (j_18, t_19)))
            else ()
         | None -> (t_15 := (Some (j_18, t_19))))
       else ()
      done;
      (match (! t_15) with
       | Some (i_16) ->
          if ((fst i_16) <> t_13) then begin
           let t_17 = t_5.(t_13) in
           t_5.(t_13) <- t_5.(fst i_16);
           t_5.(fst i_16) <- t_17;
           (t_10 := (~- (! t_10)))
          end else ();
          (Some (snd i_16))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_13 + 1) to (t_8 - 1) do
          let t_24 = (t_5.(j_23)).(t_14) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_14 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             ((t_5.(j_23)).(j_25) -. ((t_24 /. i_22) *. (t_5.(t_13)).(j_25)))
           done;
           (t_5.(j_23)).(t_14) <- 0.
          end else ()
         done;
         (t_9 := ((! t_9) *. i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_10 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_10) = 0) then 0.
    else if ((! t_10) = 1) then (! t_9)
    else (~-. (! t_9)))>.
val resFA7 : ('a, GAC_F.contr * int -> GenFA7.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_19 =
     begin
      for j_16 = t_11 to (t_8 - 1) do
       let t_17 = (t_5.(j_16)).(t_12) in
       if (t_17 <> 0.) then
        (match (! t_13) with
         | Some (i_18) ->
            if ((abs_float (snd i_18)) < (abs_float t_17)) then
             (t_13 := (Some (j_16, t_17)))
            else ()
         | None -> (t_13 := (Some (j_16, t_17))))
       else ()
      done;
      (match (! t_13) with
       | Some (i_14) ->
          if ((fst i_14) <> t_11) then
           let t_15 = t_5.(t_11) in
           t_5.(t_11) <- t_5.(fst i_14);
           t_5.(fst i_14) <- t_15
          else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_11 + 1) to (t_8 - 1) do
          let t_22 = (t_5.(j_21)).(t_12) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_12 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_11)).(j_23)))
           done;
           (t_5.(j_21)).(t_12) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resFA8 : ('a, GAC_F.contr * int -> GenFA8.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   let t_9 = (ref 1.) in
   let t_10 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_13 = (! t_2) in
    let t_14 = (! t_3) in
    let t_15 = (ref (None)) in
    let t_21 =
     begin
      for j_18 = t_13 to (t_8 - 1) do
       let t_19 = (t_5.(j_18)).(t_14) in
       if (t_19 <> 0.) then
        (match (! t_15) with
         | Some (i_20) ->
            if ((abs_float (snd i_20)) < (abs_float t_19)) then
             (t_15 := (Some (j_18, t_19)))
            else ()
         | None -> (t_15 := (Some (j_18, t_19))))
       else ()
      done;
      (match (! t_15) with
       | Some (i_16) ->
          if ((fst i_16) <> t_13) then begin
           let t_17 = t_5.(t_13) in
           t_5.(t_13) <- t_5.(fst i_16);
           t_5.(fst i_16) <- t_17;
           (t_10 := (~- (! t_10)))
          end else ();
          (Some (snd i_16))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_13 + 1) to (t_8 - 1) do
          let t_24 = (t_5.(j_23)).(t_14) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_14 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             ((t_5.(j_23)).(j_25) -. ((t_24 /. i_22) *. (t_5.(t_13)).(j_25)))
           done;
           (t_5.(j_23)).(t_14) <- 0.
          end else ()
         done;
         (t_9 := ((! t_9) *. i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_10 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_10) = 0) then 0.
    else if ((! t_10) = 1) then (! t_9)
    else (~-. (! t_9)), (! t_2))>.
val resFA9 : ('a, GAC_F.contr -> GenFA9.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref ([])) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_7 - 1) do
       let t_15 = (t_5.(j_14)).(t_10) in
       if (t_15 <> 0.) then
        (match (! t_11) with
         | Some (i_16) ->
            if ((abs_float (snd i_16)) < (abs_float t_15)) then
             (t_11 := (Some (j_14, t_15)))
            else ()
         | None -> (t_11 := (Some (j_14, t_15))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13;
           (t_8 := ((RowSwap ((fst i_12), t_9)) :: (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_7 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done
          else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_8))>.
val resFA31 : ('a, GAC_F.contr -> GenFA31.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref ([])) in
   let t_11 =
    (Array.init t_7
      (fun i_9 ->
        (Array.init t_6 (fun j_10 -> if (i_9 = j_10) then 1. else 0.)))) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_12 = (! t_2) in
    let t_13 = (! t_3) in
    let t_14 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_12 to (t_7 - 1) do
       let t_18 = (t_5.(j_17)).(t_13) in
       if (t_18 <> 0.) then
        (match (! t_14) with
         | Some (i_19) ->
            if ((abs_float (snd i_19)) < (abs_float t_18)) then
             (t_14 := (Some (j_17, t_18)))
            else ()
         | None -> (t_14 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_14) with
       | Some (i_15) ->
          if ((fst i_15) <> t_12) then begin
           let t_16 = t_5.(t_12) in
           t_5.(t_12) <- t_5.(fst i_15);
           t_5.(fst i_15) <- t_16;
           (t_8 := ((RowSwap ((fst i_15), t_12)) :: (! t_8)))
          end else ();
          (Some (snd i_15))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_12 + 1) to (t_7 - 1) do
          let t_23 = (t_5.(j_22)).(t_13) in
          if (t_23 <> 0.) then begin
           for j_24 = (t_13 + 1) to (t_6 - 1) do
            (t_5.(j_22)).(j_24) <-
             ((t_5.(j_22)).(j_24) -. ((t_23 /. i_21) *. (t_5.(t_12)).(j_24)))
           done;
           (t_11.(j_22)).(t_13) <- (t_23 /. i_21);
           (t_5.(j_22)).(t_13) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, t_11, (! t_8))>.
val resFA32 : ('a, GAC_F.contr -> GenFA32.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref ([])) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_7 - 1) do
       let t_15 = (t_5.(j_14)).(t_10) in
       if (t_15 <> 0.) then
        (match (! t_11) with
         | Some (i_16) ->
            if ((abs_float (snd i_16)) < (abs_float t_15)) then
             (t_11 := (Some (j_14, t_15)))
            else ()
         | None -> (t_11 := (Some (j_14, t_15))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13;
           (t_8 := ((RowSwap ((fst i_12), t_9)) :: (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_7 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done
          else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_8))>.
val resZp3 : ('a, GVC_Z3.contr -> GenZp3.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   let t_9 = (ref ([])) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_25 =
     begin
      let t_21 = (t_4.arr).((t_12 * t_4.m) + t_11) in
      if (t_21 <> 0) then (t_13 := (Some (t_11, t_21)))
      else
       let rec loop_22 =
        fun j_23 ->
         if (j_23 < t_6) then
          let t_24 = (t_4.arr).((j_23 * t_4.m) + t_12) in
          if (t_24 = 0) then (loop_22 (j_23 + 1))
          else (t_13 := (Some (j_23, t_24)))
         else () in
       (loop_22 (t_11 + 1));
      (match (! t_13) with
       | Some (i_14) ->
          if ((fst i_14) <> t_11) then begin
           begin
            let a_15 = t_4.arr
            and m_16 = t_4.m in
            let i1_17 = (t_11 * m_16)
            and i2_18 = ((fst i_14) * m_16) in
            for i_19 = 0 to (m_16 - 1) do
             let t_20 = a_15.(i1_17 + i_19) in
             a_15.(i1_17 + i_19) <- a_15.(i2_18 + i_19);
             a_15.(i2_18 + i_19) <- t_20
            done;
            (t_8 := (~- (! t_8)))
           end;
           (t_9 := ((RowSwap ((fst i_14), t_11)) :: (! t_9)))
          end else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_25 with
     | Some (i_26) ->
        begin
         for j_27 = (t_11 + 1) to (t_6 - 1) do
          let t_28 = (t_4.arr).((j_27 * t_4.m) + t_12) in
          if (t_28 <> 0) then begin
           for j_29 = (t_12 + 1) to (t_5 - 1) do
            (t_4.arr).((j_27 * t_4.m) + j_29) <-
             (((* cross-stage persistent value (as id: minus) *))
               (t_4.arr).((j_27 * t_4.m) + j_29)
               (((* cross-stage persistent value (as id: times) *))
                 (((* cross-stage persistent value (as id: div) *)) t_28
                   i_26) (t_4.arr).((t_11 * t_4.m) + j_29)))
           done;
           (t_4.arr).((j_27 * t_4.m) + t_12) <- 0
          end else ()
         done;
         (t_7 :=
           (((* cross-stage persistent value (as id: times) *)) (! t_7) i_26))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (((* cross-stage persistent value (as id: uminus) *)) (! t_7)),
    (! t_2), (! t_9))>.
val resZp19 : ('a, GVC_Z19.contr -> GenZp19.O.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   let t_9 = (ref ([])) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_25 =
     begin
      let t_21 = (t_4.arr).((t_12 * t_4.m) + t_11) in
      if (t_21 <> 0) then (t_13 := (Some (t_11, t_21)))
      else
       let rec loop_22 =
        fun j_23 ->
         if (j_23 < t_6) then
          let t_24 = (t_4.arr).((j_23 * t_4.m) + t_12) in
          if (t_24 = 0) then (loop_22 (j_23 + 1))
          else (t_13 := (Some (j_23, t_24)))
         else () in
       (loop_22 (t_11 + 1));
      (match (! t_13) with
       | Some (i_14) ->
          if ((fst i_14) <> t_11) then begin
           begin
            let a_15 = t_4.arr
            and m_16 = t_4.m in
            let i1_17 = (t_11 * m_16)
            and i2_18 = ((fst i_14) * m_16) in
            for i_19 = 0 to (m_16 - 1) do
             let t_20 = a_15.(i1_17 + i_19) in
             a_15.(i1_17 + i_19) <- a_15.(i2_18 + i_19);
             a_15.(i2_18 + i_19) <- t_20
            done;
            (t_8 := (~- (! t_8)))
           end;
           (t_9 := ((RowSwap ((fst i_14), t_11)) :: (! t_9)))
          end else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_25 with
     | Some (i_26) ->
        begin
         for j_27 = (t_11 + 1) to (t_6 - 1) do
          let t_28 = (t_4.arr).((j_27 * t_4.m) + t_12) in
          if (t_28 <> 0) then begin
           for j_29 = (t_12 + 1) to (t_5 - 1) do
            (t_4.arr).((j_27 * t_4.m) + j_29) <-
             (((* cross-stage persistent value (as id: div) *))
               (((* cross-stage persistent value (as id: minus) *))
                 (((* cross-stage persistent value (as id: times) *))
                   (t_4.arr).((j_27 * t_4.m) + j_29) i_26)
                 (((* cross-stage persistent value (as id: times) *))
                   (t_4.arr).((t_11 * t_4.m) + j_29) t_28)) (! t_7))
           done;
           (t_4.arr).((j_27 * t_4.m) + t_12) <- 0
          end else ()
         done;
         (t_7 := i_26)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (((* cross-stage persistent value (as id: uminus) *)) (! t_7)),
    (! t_2), (! t_9))>.
val rFA1 : GAC_F.contr -> GenFA1.O.res = <fun>
val rFA2 : float array array -> float array array * float = <fun>
val rFA3 : float array array -> float array array * int = <fun>
val rFA4 : float array array -> float array array * float * int = <fun>
val rFV1 :
  float Domains_code.container2dfromvector ->
  float Domains_code.container2dfromvector = <fun>
val rFV2 :
  float Domains_code.container2dfromvector ->
  float Domains_code.container2dfromvector * float = <fun>
val rFV3 :
  float Domains_code.container2dfromvector ->
  float Domains_code.container2dfromvector * int = <fun>
val rFV4 :
  float Domains_code.container2dfromvector ->
  float Domains_code.container2dfromvector * float * int = <fun>
val rFV5 :
  float Domains_code.container2dfromvector ->
  float Domains_code.container2dfromvector * float * int = <fun>
val rIA1 : int array array -> int array array = <fun>
val rIA2 : int array array -> int array array * int = <fun>
val rIA3 : int array array -> int array array * int = <fun>
val rIA4 : int array array -> int array array * int * int = <fun>
val rIV1 :
  int Domains_code.container2dfromvector ->
  int Domains_code.container2dfromvector = <fun>
val rIV2 :
  int Domains_code.container2dfromvector ->
  int Domains_code.container2dfromvector * int = <fun>
val rIV3 :
  int Domains_code.container2dfromvector ->
  int Domains_code.container2dfromvector * int = <fun>
val rIV4 : GVC_I.contr -> GVC_I.contr * GVC_I.Dom.v * int = <fun>
val rIV5 : GVC_I.contr -> GVC_I.contr * GVC_I.Dom.v * int = <fun>
val rIV6 : GVC_I.contr -> GVC_I.contr * GVC_I.Dom.v * int * Code.perm list =
  <fun>
val rFA11 : GAC_F.contr -> GAC_F.Dom.v array array = <fun>
val rFA12 : GAC_F.contr -> GAC_F.contr * GAC_F.Dom.v = <fun>
val rFA13 : GAC_F.contr -> GAC_F.contr * int = <fun>
val rFA14 : GAC_F.contr -> GAC_F.contr * GAC_F.Dom.v * int = <fun>
val rFA24 : GAC_F.contr -> GAC_F.contr * GAC_F.Dom.v * int * Code.perm list =
  <fun>
val rFA25 : GAC_F.contr -> GAC_F.contr * GAC_F.Dom.v * int * int array =
  <fun>
val rFA26 : GAC_F.contr -> GAC_F.Dom.v array array = <fun>
val rRA1 : GAC_R.contr -> GAC_R.Dom.v array array = <fun>
val rRA2 : GAC_R.contr -> GAC_R.contr * GAC_R.Dom.v = <fun>
val rRA3 : GAC_R.contr -> GAC_R.contr * int = <fun>
val rRA4 : GAC_R.contr -> GAC_R.contr * GAC_R.Dom.v * int = <fun>
val rFA5 : GAC_F.contr * int -> GAC_F.Dom.v array array = <fun>
val rFA6 : GAC_F.contr * int -> GAC_F.contr * GAC_F.Dom.v = <fun>
val rFA7 : GAC_F.contr * int -> GAC_F.contr * int = <fun>
val rFA8 : GAC_F.contr * int -> GAC_F.contr * GAC_F.Dom.v * int = <fun>
val rFA9 : GAC_F.contr -> GAC_F.contr * Code.perm list = <fun>
val rFA31 : GAC_F.contr -> GAC_F.contr * GAC_F.contr * Code.perm list = <fun>
val rFA32 : GAC_F.contr -> GAC_F.contr * Code.perm list = <fun>
val rZp3 : GVC_Z3.contr -> GVC_Z3.contr * GVC_Z3.Dom.v * int * Code.perm list =
  <fun>
val rZp19 :
  GVC_Z19.contr -> GVC_Z19.contr * GVC_Z19.Dom.v * int * Code.perm list =
  <fun>
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
val resI11 : int array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|]]
val resI12 : (int array array * int) list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0)]
val resI13 : (int array array * int) list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 2)]
val resI14 : (int array array * int * int) list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0, 2)]
val iv0 : int Domains_code.container2dfromvector =
  {arr = [|1|]; n = 1; m = 1}
val iv1 : int Domains_code.container2dfromvector =
  {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3}
val iv2 : int Domains_code.container2dfromvector =
  {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4}
val iv4 : int Domains_code.container2dfromvector =
  {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}
val iv5 : int Domains_code.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}]
val resI21 : int Domains_code.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}]
val resI22 : (int Domains_code.container2dfromvector * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0)]
val resI23 : (int Domains_code.container2dfromvector * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 2)]
val resI24 : (GVC_I.contr * GVC_I.Dom.v * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
val resI25 : (GVC_I.contr * GVC_I.Dom.v * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
val resI26 : (GVC_I.contr * GVC_I.Dom.v * int * Code.perm list) list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1, []);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3,
    [Code.RowSwap (2, 1); Code.ColSwap (2, 1)]);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3,
    [Code.RowSwap (2, 1); Code.ColSwap (2, 1)]);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2,
    [Code.RowSwap (2, 1); Code.ColSwap (2, 1); Code.ColSwap (1, 0)])]
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
val resFA9 : GAC_F.contr * Code.perm list =
  ([|[|4.; 13.; 5.|]; [|-1.; 6.25; 1.25|]; [|1.; -1.25; 2.|]|],
   [Code.RowSwap (2, 1); Code.RowSwap (1, 0)])
- : unit = ()
- : unit = ()
- : unit = ()
val resF1 : GAC_F.Dom.v array array list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
- : unit = ()
- : unit = ()
- : unit = ()
val resFA31 : (GAC_F.contr * GAC_F.contr * Code.perm list) list =
  [([|[|1.|]|], [|[|1.|]|], []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|],
    [|[|1.; 0.; 0.|]; [|0.25; 1.; 0.|]; [|-0.25; -0.2; 1.|]|],
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|],
    [|[|1.; 0.; 0.; 0.|]; [|0.25; 1.; 0.; 0.|]; [|-0.25; -0.2; 1.; 0.|]|],
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    [|[|1.; 0.; 0.|]; [|0.25; 1.; 0.|]; [|-0.25; -0.2; 1.|]; [|0.; 0.; 0.|]|],
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    [|[|1.; 0.; 0.|]; [|0.; 0.2; 0.|]; [|0.; 0.3; -0.75|]|],
    [Code.RowSwap (1, 0)])]
val resFA32 : (GAC_F.contr * Code.perm list) list =
  [([|[|1.|]|], []);
   ([|[|4.; 13.; 5.|]; [|-1.; 6.25; 1.25|]; [|1.; -1.25; 2.|]|],
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|-1.; 6.25; 1.25; 0.|]; [|1.; -1.25; 2.; 0.|]|],
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|-1.; 6.25; 1.25|]; [|1.; -1.25; 2.|];
      [|0.; 0.; 0.|]|],
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 2.; 2.|]; [|0.; 3.; -1.5|]|],
    [Code.RowSwap (1, 0)])]
val a2v : 'a array array -> 'a Domains_code.container2dfromvector = <fun>
val xxx : float Domains_code.container2dfromvector list =
  [{arr = [|1.|]; n = 1; m = 1};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.|]; n = 3; m = 3};
   {arr = [|1.; 2.; 3.; 0.; 4.; 13.; 5.; 0.; -1.; 3.; 0.; 0.|]; n = 3; m = 4};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.; 0.; 0.; 0.|]; n = 4; m = 3};
   {arr = [|0.; 2.; 3.; 0.; 10.; 5.; 0.; 3.; 0.|]; n = 3; m = 3}]
- : unit = ()
val resFV5 : (float Domains_code.container2dfromvector * float * int) list =
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
val resF11 : GAC_F.Dom.v array array list =
  [[|[|1.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]|];
   [|[|13.; 5.; 4.; 0.|];
     [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
     [|0.; 0.; -1.72413793103448287; 0.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|];
   [|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|]]
val resF12 : (GAC_F.contr * float) list =
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
val resF13 : (GAC_F.contr * int) list =
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
val resF14 : (GAC_F.contr * GAC_F.Dom.v * int) list =
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
val resF24 : (GAC_F.contr * GAC_F.Dom.v * int * Code.perm list) list =
  [([|[|1.|]|], 1., 1, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2,
    [Code.RowSwap (1, 0)])]
val resF25 : (GAC_F.contr * GAC_F.Dom.v * int * int array) list =
  [([|[|1.|]|], 1., 1, [|0|]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [|1; 2; 0|]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [|1; 2; 0|]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [|1; 2; 0; 3|]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2, [|1; 0; 2|])]
val resF26 : GAC_F.Dom.v array array list =
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
val resR11 : GAC_R.Dom.v array array list =
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
val resR12 : (GAC_R.contr * GAC_R.Dom.v) list =
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
val resR13 : (GAC_R.contr * int) list =
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
val resR14 : (GAC_R.contr * GAC_R.Dom.v * int) list =
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
