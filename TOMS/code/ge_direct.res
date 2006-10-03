        Objective Caml version 3.09.1

#                 module GEF :
  sig
    module D :
      sig
        module type DOMAIN =
          sig
            type v
            type kind
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
            type kind
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
    module type DETERMINANT =
      sig
        type indet
        type outdet
        type tdet = outdet ref
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
          ('a, indet) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
      end
    module type DETF =
      functor (D : D.DOMAINL) ->
        sig
          type indet = D.v
          type outdet
          type tdet = outdet ref
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
            ('a, indet) Direct.abstract ->
            ('a * [> 'a tag_lstate ] * 'b, unit) lm
          val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
          val set :
            ('a, indet) Direct.abstract ->
            ('a * [> 'a tag_lstate ] * 'b, unit) lm
          val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
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
        val fin : unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
      end
    module NoDet :
      functor (Dom : D.DOMAINL) ->
        sig
          type indet = Dom.v
          type outdet = unit
          type tdet = outdet ref
          type 'a lstate = unit
          val decl :
            unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
          val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
          val zero_sign :
            unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
          val acc :
            'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
          val get :
            unit -> 'a -> ('a -> ('b, unit ref) Direct.abstract -> 'c) -> 'c
          val set :
            'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
          val fin :
            unit -> 'a -> ('a -> ('b, unit) Direct.abstract -> 'c) -> 'c
          type 'a tag_lstate = [ `TDet of 'a lstate ]
          type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
            constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
          type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
            constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        end
    module AbstractDet :
      functor (Dom : D.DOMAINL) ->
        sig
          type indet = Dom.v
          type outdet = Dom.v
          type tdet = outdet ref
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
            ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
          val decl :
            unit ->
            ([> `TDet of
                  ('b, int ref) Direct.abstract *
                  ('b, Dom.v ref) Direct.abstract ]
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
            'a Dom.vc ->
            ([> `TDet of 'c * ('a, Dom.v ref) Direct.abstract ] as 'b) list ->
            ('b list -> ('a, unit) Direct.abstract -> 'd) -> 'd
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
                  ('b, Dom.v ref) Direct.abstract ]
             as 'a)
            list -> ('a list -> ('b, Dom.v) Direct.abstract -> 'c) -> 'c
        end
    module type PIVOTKIND =
      sig
        type idx_rep = int
        type flip_rep
        type perm_rep
        type 'a ira = ('a, idx_rep) Direct.abstract
        type 'a fra = ('a, flip_rep) Direct.abstract
        type 'a pra = ('a, perm_rep) Direct.abstract
        val add : 'a fra -> 'a pra -> 'a pra
        val empty : 'a ira -> 'a pra
        val rowrep : 'a ira -> 'a ira -> 'a fra
        val colrep : 'a ira -> 'a ira -> 'a fra
      end
    module PermList :
      sig
        type idx_rep = int
        type flip_rep = Direct.perm
        type perm_rep = Direct.perm list
        type 'a ira = ('a, idx_rep) Direct.abstract
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
        type idx_rep = int
        type flip_rep = int * int
        type perm_rep = int array
        type 'a ira = ('a, idx_rep) Direct.abstract
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
        type idx_rep = int
        type flip_rep
        type perm_rep
        type 'a ira = ('a, idx_rep) Direct.abstract
        type 'a fra = ('a, flip_rep) Direct.abstract
        type 'a pra = ('a, perm_rep) Direct.abstract
        type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
        val fin :
          unit ->
          ('a pra option, [> 'a tag_lstate ] list, ('a, 'b) Direct.abstract)
          StateCPSMonad.monad
      end
    module PivotCommon :
      functor (PK : PIVOTKIND) ->
        sig
          type idx_rep = PK.idx_rep
          type flip_rep = PK.flip_rep
          type perm_rep = PK.perm_rep
          type 'a ira = ('a, idx_rep) Direct.abstract
          type 'a fra = ('a, flip_rep) Direct.abstract
          type 'a pra = ('a, perm_rep) Direct.abstract
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
          type idx_rep = PK.idx_rep
          type flip_rep = PK.flip_rep
          type perm_rep = PK.perm_rep
          type 'a ira = ('a, idx_rep) Direct.abstract
          type 'a fra = ('a, flip_rep) Direct.abstract
          type 'a pra = ('a, perm_rep) Direct.abstract
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
            ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
        end
    module DiscardPivot :
      functor (PK : PIVOTKIND) ->
        sig
          type idx_rep = PK.idx_rep
          type flip_rep = PK.flip_rep
          type perm_rep = PK.perm_rep
          type 'a ira = ('a, idx_rep) Direct.abstract
          type 'a fra = ('a, flip_rep) Direct.abstract
          type 'a pra = ('a, perm_rep) Direct.abstract
          type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
          type 'a tag_lstate = [ `TPivot of 'a lstate ]
          type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
            constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
          val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
          val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
          val decl :
            'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
          val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
          val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
        end
    module UpdateProxy :
      functor (C0 : D.CONTAINER2D) ->
        functor (D0 : DETF) ->
          sig
            module type T =
              functor (D1 : D.DOMAINL) ->
                sig
                  type indet = D1.v
                  type outdet = D0(D1).outdet
                  type tdet = outdet ref
                  type 'a lstate
                  type 'a tag_lstate = [ `TDet of 'a lstate ]
                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                    constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                  val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val upd_sign :
                    unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                  val zero_sign :
                    unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val acc :
                    ('a, indet) Direct.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) Direct.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                end
            module type S =
              functor
                (C : sig
                       module Dom :
                         sig
                           type v
                           type kind = C0.Dom.kind
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
                             ('a vc -> 'a vc -> ('a, bool) Direct.abstract)
                             option
                           val normalizerL : ('a vc -> 'a vc) option
                         end
                       type contr
                       type 'a vc = ('a, contr) Direct.abstract
                       type 'a vo = ('a, Dom.v) Direct.abstract
                       val getL :
                         'a vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract -> 'a vo
                       val dim1 : 'a vc -> ('a, int) Direct.abstract
                       val dim2 : 'a vc -> ('a, int) Direct.abstract
                       val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
                       val copy : 'a vc -> 'a vc
                       val init :
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract -> 'a vc
                       val identity :
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract -> 'a vc
                       val swap_rows_stmt :
                         'a vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         ('a, unit) Direct.abstract
                       val swap_cols_stmt :
                         'a vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         ('a, unit) Direct.abstract
                       val row_head :
                         'a vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract -> 'a vo
                       val col_head_set :
                         'a vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a vo -> ('a, unit) Direct.abstract
                     end) ->
                functor (D : T) ->
                  sig
                    type 'a in_val = 'a C.Dom.vc
                    type out_val = D(C.Dom).outdet
                    val update :
                      'a in_val ->
                      'a in_val ->
                      'a in_val ->
                      'a in_val ->
                      ('a in_val -> ('a, unit) Direct.abstract) ->
                      ('a, out_val ref) Direct.abstract ->
                      ('a, unit, 'b, 'c) cmonad
                    val update_det :
                      'a in_val ->
                      ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                      ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                      ('a, unit, 'b, 'c) cmonad
                    val upd_kind : Ge.update_kind
                  end
          end
    module DivisionUpdate :
      functor
        (C : sig
               module Dom :
                 sig
                   type v
                   type kind = Kinds.domain_is_field
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
               type contr
               type 'a vc = ('a, contr) Direct.abstract
               type 'a vo = ('a, Dom.v) Direct.abstract
               val getL :
                 'a vc ->
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract -> 'a vo
               val dim1 : 'a vc -> ('a, int) Direct.abstract
               val dim2 : 'a vc -> ('a, int) Direct.abstract
               val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
               val copy : 'a vc -> 'a vc
               val init :
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract -> 'a vc
               val identity :
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract -> 'a vc
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
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract -> 'a vo
               val col_head_set :
                 'a vc ->
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract ->
                 'a vo -> ('a, unit) Direct.abstract
             end) ->
        functor (Det : DETF) ->
          sig
            module Dom :
              sig
                type v = C.Dom.v
                type kind = Kinds.domain_is_field
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
            type 'a in_val = 'a Dom.vc
            type out_val = Det(C.Dom).outdet
            val update :
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              ('a Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
            val update_det : 'a -> 'b -> ('a -> 'c) -> 'c
            val upd_kind : Ge.update_kind
          end
    module FractionFreeUpdate :
      functor (Ctr : D.CONTAINER2D) ->
        functor (Det : DETF) ->
          sig
            module Dom :
              sig
                type v = Ctr.Dom.v
                type kind = Ctr.Dom.kind
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
            type 'a in_val = ('a, Dom.v) Direct.abstract
            type out_val = Det(Ctr.Dom).outdet
            val update :
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              ('a Dom.vc -> 'b) ->
              ('a, Dom.v ref) Direct.abstract -> 'c -> ('c -> 'b -> 'd) -> 'd
            val update_det : 'a -> ('a -> 'b) -> 'c -> 'b
            val upd_kind : Ge.update_kind
          end
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
              val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
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
                ('a list -> 'b option -> 'c) -> 'c
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
                ('a list -> 'b option -> 'c) -> 'c
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
              val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
          module OutProxy :
            functor (C0 : D.CONTAINER2D) ->
              functor (Det0 : DETF) ->
                sig
                  module DD :
                    sig
                      type indet = C0.Dom.v
                      type outdet = Det0(C0.Dom).outdet
                      type tdet = outdet ref
                      type 'a lstate = 'a Det0(C0.Dom).lstate
                      type 'a tag_lstate = [ `TDet of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val upd_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                      val zero_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val acc :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val get :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                      val set :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                    end
                  module type S =
                    functor
                      (C1 : sig
                              module Dom :
                                sig
                                  type v
                                  type kind = C0.Dom.kind
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
                                    ('a vc ->
                                     'a vc -> ('a, bool) Direct.abstract)
                                    option
                                  val normalizerL : ('a vc -> 'a vc) option
                                end
                              type contr
                              type 'a vc = ('a, contr) Direct.abstract
                              type 'a vo = ('a, Dom.v) Direct.abstract
                              val getL :
                                'a vc ->
                                ('a, int) Direct.abstract ->
                                ('a, int) Direct.abstract -> 'a vo
                              val dim1 : 'a vc -> ('a, int) Direct.abstract
                              val dim2 : 'a vc -> ('a, int) Direct.abstract
                              val mapper :
                                ('a vo -> 'a vo) option -> 'a vc -> 'a vc
                              val copy : 'a vc -> 'a vc
                              val init :
                                ('a, int) Direct.abstract ->
                                ('a, int) Direct.abstract -> 'a vc
                              val identity :
                                ('a, int) Direct.abstract ->
                                ('a, int) Direct.abstract -> 'a vc
                              val swap_rows_stmt :
                                'a vc ->
                                ('a, int) Direct.abstract ->
                                ('a, int) Direct.abstract ->
                                ('a, unit) Direct.abstract
                              val swap_cols_stmt :
                                'a vc ->
                                ('a, int) Direct.abstract ->
                                ('a, int) Direct.abstract ->
                                ('a, unit) Direct.abstract
                              val row_head :
                                'a vc ->
                                ('a, int) Direct.abstract ->
                                ('a, int) Direct.abstract -> 'a vo
                              val col_head_set :
                                'a vc ->
                                ('a, int) Direct.abstract ->
                                ('a, int) Direct.abstract ->
                                'a vo -> ('a, unit) Direct.abstract
                            end) ->
                      functor
                        (Det : sig
                                 type indet
                                 type outdet = DD.outdet
                                 type tdet = outdet ref
                                 type 'a lstate = 'a DD.lstate
                                 type 'a tag_lstate = [ `TDet of 'a lstate ]
                                 type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                                   constraint 'a =
                                     'c * ([> 'c tag_lstate ] as 'd) * 'e
                                 type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                                   constraint 'a =
                                     'c * ([> 'c tag_lstate ] as 'd) * 'e
                                 val decl :
                                   unit ->
                                   ('a * [> 'a tag_lstate ] * 'b, unit) lm
                                 val upd_sign :
                                   unit ->
                                   ('a * [> 'a tag_lstate ] * 'b, unit) om
                                 val zero_sign :
                                   unit ->
                                   ('a * [> 'a tag_lstate ] * 'b, unit) lm
                                 val acc :
                                   ('a, indet) Direct.abstract ->
                                   ('a * [> 'a tag_lstate ] * 'b, unit) lm
                                 val get :
                                   unit ->
                                   ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                                 val set :
                                   ('a, indet) Direct.abstract ->
                                   ('a * [> 'a tag_lstate ] * 'b, unit) lm
                                 val fin :
                                   unit ->
                                   ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                               end) ->
                        functor (PK : PIVOTKIND) ->
                          sig
                            type res
                            module R : TrackRank.RANK
                            module P :
                              sig
                                type idx_rep = int
                                type flip_rep = PK.flip_rep
                                type perm_rep = PK.perm_rep
                                type 'a ira = ('a, idx_rep) Direct.abstract
                                type 'a fra = ('a, flip_rep) Direct.abstract
                                type 'a pra = ('a, perm_rep) Direct.abstract
                                type 'a lstate =
                                    ('a, perm_rep ref) Direct.abstract
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
                                  ('a pra option, [> 'a tag_lstate ] list,
                                   ('a, 'b) Direct.abstract)
                                  StateCPSMonad.monad
                              end
                            module L : LOWER
                            val make_result :
                              'a wmatrix ->
                              ('a, res,
                               [> `TDet of 'a Det.lstate
                                | `TLower of 'a L.lstate
                                | `TPivot of 'a P.lstate
                                | `TRan of 'a TrackRank.lstate ],
                               'b)
                              cmonad
                          end
                end
          module OutJustMatrix :
            functor (C0 : D.CONTAINER2D) ->
              functor (Det : DETERMINANT) ->
                functor (PK : PIVOTKIND) ->
                  sig
                    type res = C.contr
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
                          'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
                      end
                    module P :
                      sig
                        type idx_rep = PK.idx_rep
                        type flip_rep = PK.flip_rep
                        type perm_rep = PK.perm_rep
                        type 'a ira = ('a, idx_rep) Direct.abstract
                        type 'a fra = ('a, flip_rep) Direct.abstract
                        type 'a pra = ('a, perm_rep) Direct.abstract
                        type 'a lstate =
                            ('a, PK.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                        val wants_pack : bool
                      end
                    val make_result :
                      'a wmatrix -> 'b -> ('b -> 'a C.vc -> 'c) -> 'c
                  end
          module OutDet :
            functor (C0 : D.CONTAINER2D) ->
              functor (Det : DETERMINANT) ->
                functor (PK : PIVOTKIND) ->
                  sig
                    type res = C.contr * Det.outdet
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
                          'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
                      end
                    module P :
                      sig
                        type idx_rep = PK.idx_rep
                        type flip_rep = PK.flip_rep
                        type perm_rep = PK.perm_rep
                        type 'a ira = ('a, idx_rep) Direct.abstract
                        type 'a fra = ('a, flip_rep) Direct.abstract
                        type 'a pra = ('a, perm_rep) Direct.abstract
                        type 'a lstate =
                            ('a, PK.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                        val wants_pack : bool
                      end
                    val make_result :
                      'a wmatrix ->
                      ([> 'a Det.tag_lstate ] as 'b) list ->
                      ('b list ->
                       ('a, C.contr * Det.outdet) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
                      ('a, 'c) Direct.abstract
                  end
          module OutRank :
            functor (C0 : D.CONTAINER2D) ->
              functor (Det : DETERMINANT) ->
                functor (PK : PIVOTKIND) ->
                  sig
                    type res = C.contr * int
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
                        type idx_rep = PK.idx_rep
                        type flip_rep = PK.flip_rep
                        type perm_rep = PK.perm_rep
                        type 'a ira = ('a, idx_rep) Direct.abstract
                        type 'a fra = ('a, flip_rep) Direct.abstract
                        type 'a pra = ('a, perm_rep) Direct.abstract
                        type 'a lstate =
                            ('a, PK.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                        val wants_pack : bool
                      end
                    val make_result :
                      'a wmatrix ->
                      ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
                      ('b list -> ('a, C.contr * 'c) Direct.abstract -> 'd) ->
                      'd
                  end
          module OutDetRank :
            functor (C0 : D.CONTAINER2D) ->
              functor (Det : DETERMINANT) ->
                functor (PK : PIVOTKIND) ->
                  sig
                    type res = C.contr * Det.outdet * int
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
                        type idx_rep = PK.idx_rep
                        type flip_rep = PK.flip_rep
                        type perm_rep = PK.perm_rep
                        type 'a ira = ('a, idx_rep) Direct.abstract
                        type 'a fra = ('a, flip_rep) Direct.abstract
                        type 'a pra = ('a, perm_rep) Direct.abstract
                        type 'a lstate =
                            ('a, PK.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val decl :
                          'a ->
                          'b ->
                          ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                        val wants_pack : bool
                      end
                    val make_result :
                      'a wmatrix ->
                      ([> `TDet of 'a Det.lstate
                        | `TRan of ('a, 'c ref) Direct.abstract ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, C.contr * Det.outdet * 'c) Direct.abstract ->
                       ('a, 'd) Direct.abstract) ->
                      ('a, 'd) Direct.abstract
                  end
          module OutDetRankPivot :
            functor (C0 : D.CONTAINER2D) ->
              functor (Det : DETERMINANT) ->
                functor (PK : PIVOTKIND) ->
                  sig
                    type res = C.contr * Det.outdet * int * PK.perm_rep
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
                        type idx_rep = PK.idx_rep
                        type flip_rep = PK.flip_rep
                        type perm_rep = PK.perm_rep
                        type 'a ira = ('a, idx_rep) Direct.abstract
                        type 'a fra = ('a, flip_rep) Direct.abstract
                        type 'a pra = ('a, perm_rep) Direct.abstract
                        type 'a lstate =
                            ('a, PK.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
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
                          'a PK.ira ->
                          ([> `TPivot of
                                ('a, PK.perm_rep ref) Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Direct.abstract ->
                           ('a, 'd) Direct.abstract) ->
                          ('a, 'd) Direct.abstract
                        val add :
                          'a PK.fra ->
                          ([> `TPivot of
                                ('a, PK.perm_rep ref) Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                          'c
                        val fin :
                          unit ->
                          ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract option -> 'd) ->
                          'd
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
                        val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                        val wants_pack : bool
                      end
                    val make_result :
                      'a wmatrix ->
                      ([> `TDet of 'a Det.lstate
                        | `TPivot of ('a, 'c ref) Direct.abstract
                        | `TRan of ('a, 'd ref) Direct.abstract ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, C.contr * Det.outdet * 'd * 'c) Direct.abstract ->
                       ('a, 'e) Direct.abstract) ->
                      ('a, 'e) Direct.abstract
                  end
          module Out_L_U :
            functor
              (C0 : sig
                      module Dom :
                        sig
                          type v
                          type kind = Kinds.domain_is_field
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
                            ('a vc -> 'a vc -> ('a, bool) Direct.abstract)
                            option
                          val normalizerL : ('a vc -> 'a vc) option
                        end
                      type contr
                      type 'a vc = ('a, contr) Direct.abstract
                      type 'a vo = ('a, Dom.v) Direct.abstract
                      val getL :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vo
                      val dim1 : 'a vc -> ('a, int) Direct.abstract
                      val dim2 : 'a vc -> ('a, int) Direct.abstract
                      val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
                      val copy : 'a vc -> 'a vc
                      val init :
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vc
                      val identity :
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vc
                      val swap_rows_stmt :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        ('a, unit) Direct.abstract
                      val swap_cols_stmt :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        ('a, unit) Direct.abstract
                      val row_head :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vo
                      val col_head_set :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a vo -> ('a, unit) Direct.abstract
                    end) ->
              functor (Det : DETERMINANT) ->
                functor (PK : PIVOTKIND) ->
                  sig
                    type res = C.contr * C.contr * PK.perm_rep
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
                        type idx_rep = PK.idx_rep
                        type flip_rep = PK.flip_rep
                        type perm_rep = PK.perm_rep
                        type 'a ira = ('a, idx_rep) Direct.abstract
                        type 'a fra = ('a, flip_rep) Direct.abstract
                        type 'a pra = ('a, perm_rep) Direct.abstract
                        type 'a lstate =
                            ('a, PK.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
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
                          'a PK.ira ->
                          ([> `TPivot of
                                ('a, PK.perm_rep ref) Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Direct.abstract ->
                           ('a, 'd) Direct.abstract) ->
                          ('a, 'd) Direct.abstract
                        val add :
                          'a PK.fra ->
                          ([> `TPivot of
                                ('a, PK.perm_rep ref) Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                          'c
                        val fin :
                          unit ->
                          ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract option -> 'd) ->
                          'd
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
                          ('a list -> 'b option -> 'c) -> 'c
                        val wants_pack : bool
                      end
                    val make_result :
                      'a wmatrix ->
                      ([> `TLower of ('a, 'c) Direct.abstract
                        | `TPivot of ('a, 'd ref) Direct.abstract ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, C.contr * 'c * 'd) Direct.abstract -> 'e) ->
                      'e
                  end
          module Out_LU_Packed :
            functor
              (C0 : sig
                      module Dom :
                        sig
                          type v
                          type kind = Kinds.domain_is_field
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
                            ('a vc -> 'a vc -> ('a, bool) Direct.abstract)
                            option
                          val normalizerL : ('a vc -> 'a vc) option
                        end
                      type contr
                      type 'a vc = ('a, contr) Direct.abstract
                      type 'a vo = ('a, Dom.v) Direct.abstract
                      val getL :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vo
                      val dim1 : 'a vc -> ('a, int) Direct.abstract
                      val dim2 : 'a vc -> ('a, int) Direct.abstract
                      val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
                      val copy : 'a vc -> 'a vc
                      val init :
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vc
                      val identity :
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vc
                      val swap_rows_stmt :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        ('a, unit) Direct.abstract
                      val swap_cols_stmt :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        ('a, unit) Direct.abstract
                      val row_head :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract -> 'a vo
                      val col_head_set :
                        'a vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a vo -> ('a, unit) Direct.abstract
                    end) ->
              functor (Det : DETERMINANT) ->
                functor (PK : PIVOTKIND) ->
                  sig
                    type res = C.contr * PK.perm_rep
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
                        type idx_rep = PK.idx_rep
                        type flip_rep = PK.flip_rep
                        type perm_rep = PK.perm_rep
                        type 'a ira = ('a, idx_rep) Direct.abstract
                        type 'a fra = ('a, flip_rep) Direct.abstract
                        type 'a pra = ('a, perm_rep) Direct.abstract
                        type 'a lstate =
                            ('a, PK.perm_rep ref) Direct.abstract
                        type 'a tag_lstate = [ `TPivot of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                        val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
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
                          'a PK.ira ->
                          ([> `TPivot of
                                ('a, PK.perm_rep ref) Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list ->
                           ('c, unit) Direct.abstract ->
                           ('a, 'd) Direct.abstract) ->
                          ('a, 'd) Direct.abstract
                        val add :
                          'a PK.fra ->
                          ([> `TPivot of
                                ('a, PK.perm_rep ref) Direct.abstract ]
                           as 'b)
                          list ->
                          ('b list -> ('a, unit) Direct.abstract option -> 'c) ->
                          'c
                        val fin :
                          unit ->
                          ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a)
                          list ->
                          ('a list -> ('b, 'c) Direct.abstract option -> 'd) ->
                          'd
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
                          ('a list -> 'b option -> 'c) -> 'c
                        val wants_pack : bool
                      end
                    val make_result :
                      'a ->
                      ([> `TLower of ('c, 'd) Direct.abstract
                        | `TPivot of ('c, 'e ref) Direct.abstract ]
                       as 'b)
                      list ->
                      ('b list -> ('c, 'd * 'e) Direct.abstract -> 'f) -> 'f
                  end
          module type PIVOT =
            functor (D : DETF) ->
              functor (P : TRACKPIVOT) ->
                sig
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    ('a, C.Dom.v option,
                     [> `TDet of 'a D(C.Dom).lstate | `TPivot of 'a P.lstate ],
                     'b)
                    cmonad
                end
          module RowPivot :
            functor (Det : DETF) ->
              functor (P : TRACKPIVOT) ->
                sig
                  module D :
                    sig
                      type indet = C.Dom.v
                      type outdet = Det(C.Dom).outdet
                      type tdet = outdet ref
                      type 'a lstate = 'a Det(C.Dom).lstate
                      type 'a tag_lstate = [ `TDet of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val upd_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                      val zero_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val acc :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val get :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                      val set :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                    end
                  module I :
                    sig
                      val row_iter :
                        'a ->
                        'b ->
                        ('c, int) Direct.abstract ->
                        ('c, int) Direct.abstract ->
                        ('a ->
                         ('c, int) Direct.abstract ->
                         'b -> ('c, 'd) Direct.abstract) ->
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
                         'd ->
                         'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                        'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                    end
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b)
                    list ->
                    ('b list ->
                     ('a, C.Dom.v option) Direct.abstract ->
                     ('a, 'c) Direct.abstract) ->
                    ('a, 'c) Direct.abstract
                end
          module FullPivot :
            functor (Det : DETF) ->
              functor (P : TRACKPIVOT) ->
                sig
                  module D :
                    sig
                      type indet = C.Dom.v
                      type outdet = Det(C.Dom).outdet
                      type tdet = outdet ref
                      type 'a lstate = 'a Det(C.Dom).lstate
                      type 'a tag_lstate = [ `TDet of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val upd_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                      val zero_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val acc :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val get :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                      val set :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                    end
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b)
                    list ->
                    ('b list ->
                     ('a, C.Dom.v option) Direct.abstract ->
                     ('a, 'c) Direct.abstract) ->
                    ('a, 'c) Direct.abstract
                end
          module NoPivot :
            functor (Det : DETF) ->
              functor (P : TRACKPIVOT) ->
                sig
                  module D :
                    sig
                      type indet = C.Dom.v
                      type outdet = Det(C.Dom).outdet
                      type tdet = outdet ref
                      type 'a lstate = 'a Det(C.Dom).lstate
                      type 'a tag_lstate = [ `TDet of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val decl :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val upd_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                      val zero_sign :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val acc :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val get :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                      val set :
                        ('a, indet) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                    end
                  val findpivot :
                    'a wmatrix ->
                    'a curpos ->
                    'b ->
                    ('b -> ('a, C.Dom.v option) Direct.abstract -> 'c) -> 'c
                end
          module GenGE :
            functor (PivotF : PIVOT) ->
              functor (PK : PIVOTKIND) ->
                functor (Detf : DETF) ->
                  functor (Update : UpdateProxy(C)(Detf).S) ->
                    functor (In : INPUT) ->
                      functor (Out : OutProxy(C)(Detf).S) ->
                        sig
                          module Det :
                            sig
                              type indet = C.Dom.v
                              type outdet = Detf(C.Dom).outdet
                              type tdet = outdet ref
                              type 'a lstate = 'a Detf(C.Dom).lstate
                              type 'a tag_lstate = [ `TDet of 'a lstate ]
                              type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                                constraint 'a =
                                  'c * ([> 'c tag_lstate ] as 'd) * 'e
                              type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                                constraint 'a =
                                  'c * ([> 'c tag_lstate ] as 'd) * 'e
                              val decl :
                                unit ->
                                ('a * [> 'a tag_lstate ] * 'b, unit) lm
                              val upd_sign :
                                unit ->
                                ('a * [> 'a tag_lstate ] * 'b, unit) om
                              val zero_sign :
                                unit ->
                                ('a * [> 'a tag_lstate ] * 'b, unit) lm
                              val acc :
                                ('a, indet) Direct.abstract ->
                                ('a * [> 'a tag_lstate ] * 'b, unit) lm
                              val get :
                                unit ->
                                ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                              val set :
                                ('a, indet) Direct.abstract ->
                                ('a * [> 'a tag_lstate ] * 'b, unit) lm
                              val fin :
                                unit ->
                                ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                            end
                          module U :
                            sig
                              type 'a in_val = 'a C.Dom.vc
                              type out_val = Detf(C.Dom).outdet
                              val update :
                                'a in_val ->
                                'a in_val ->
                                'a in_val ->
                                'a in_val ->
                                ('a in_val -> ('a, unit) Direct.abstract) ->
                                ('a, out_val ref) Direct.abstract ->
                                ('a, unit, 'b, 'c) cmonad
                              val update_det :
                                'a in_val ->
                                ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                                ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                                ('a, unit, 'b, 'c) cmonad
                              val upd_kind : Ge.update_kind
                            end
                          module Input :
                            sig
                              type inp = In.inp
                              val get_input :
                                ('a, inp) Direct.abstract ->
                                (('a, C.contr) Direct.abstract *
                                 ('a, int) Direct.abstract * bool, 'b,
                                 ('a, 'c) Direct.abstract)
                                StateCPSMonad.monad
                            end
                          module Output :
                            sig
                              type res = Out(C)(Det)(PK).res
                              module R :
                                sig
                                  type 'a tag_lstate =
                                      'a TrackRank.tag_lstate_
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
                                    ('a * [> 'a TrackRank.tag_lstate ] * 'b,
                                     unit)
                                    TrackRank.lm
                                  val fin :
                                    unit ->
                                    ('a * [> 'a TrackRank.tag_lstate ] * 'b,
                                     int)
                                    TrackRank.lm
                                end
                              module P :
                                sig
                                  type idx_rep = int
                                  type flip_rep = PK.flip_rep
                                  type perm_rep = PK.perm_rep
                                  type 'a ira = ('a, idx_rep) Direct.abstract
                                  type 'a fra =
                                      ('a, flip_rep) Direct.abstract
                                  type 'a pra =
                                      ('a, perm_rep) Direct.abstract
                                  type 'a lstate =
                                      ('a, perm_rep ref) Direct.abstract
                                  type 'a tag_lstate =
                                      [ `TPivot of 'a lstate ]
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
                                    ('a pra option, [> 'a tag_lstate ] list,
                                     ('a, 'b) Direct.abstract)
                                    StateCPSMonad.monad
                                end
                              module L :
                                sig
                                  type 'a lstate =
                                      ('a, C.contr) Direct.abstract
                                  type 'a tag_lstate =
                                      [ `TLower of 'a lstate ]
                                  type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                                    constraint 'a =
                                      'c * ([> 'c tag_lstate ] as 'd) * 'e
                                  type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                                    constraint 'a =
                                      'c * ([> 'c tag_lstate ] as 'd) * 'e
                                  val mfetch :
                                    unit ->
                                    ('a * [> 'a tag_lstate ] * 'b, C.contr)
                                    lm
                                  val decl :
                                    ('a, C.contr) Direct.abstract ->
                                    ('a * [> 'a tag_lstate ] * 'b, C.contr)
                                    lm
                                  val updt :
                                    'a C.vc ->
                                    ('a, int) Direct.abstract ->
                                    ('a, int) Direct.abstract ->
                                    'a C.vo ->
                                    'a C.Dom.vc ->
                                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                                    option
                                  val fin :
                                    unit ->
                                    ('a * [> 'a tag_lstate ] * 'b, C.contr)
                                    om
                                  val wants_pack : bool
                                end
                              val make_result :
                                'a wmatrix ->
                                ('a, res,
                                 [> `TDet of 'a Det.lstate
                                  | `TLower of 'a L.lstate
                                  | `TPivot of 'a P.lstate
                                  | `TRan of 'a TrackRank.lstate ],
                                 'b)
                                cmonad
                            end
                          module Pivot :
                            sig
                              val findpivot :
                                'a wmatrix ->
                                'a curpos ->
                                ('a, C.Dom.v option,
                                 [> `TDet of 'a Detf(C.Dom).lstate
                                  | `TPivot of 'a Output.P.lstate ],
                                 'b)
                                cmonad
                            end
                          module I :
                            sig
                              val row_iter :
                                'a ->
                                'b ->
                                ('c, int) Direct.abstract ->
                                ('c, int) Direct.abstract ->
                                ('a ->
                                 ('c, int) Direct.abstract ->
                                 'b -> ('c, 'd) Direct.abstract) ->
                                (('c, int) Direct.abstract ->
                                 ('c, 'd) Direct.abstract ->
                                 'e ->
                                 ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                                'e ->
                                ('e -> ('c, unit) Direct.abstract -> 'i) ->
                                'i
                              val col_iter :
                                'a ->
                                'b ->
                                ('c, int) Direct.abstract ->
                                ('c, int) Direct.abstract ->
                                ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                                (('c, int) Direct.abstract ->
                                 'd ->
                                 'e ->
                                 ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                                'e ->
                                ('e -> ('c, unit) Direct.abstract -> 'i) ->
                                'i
                            end
                          val gen :
                            ('a, Input.inp) Direct.abstract ->
                            ([> `TDet of 'a Det.lstate
                              | `TLower of 'a Output.L.lstate
                              | `TPivot of 'a Output.P.lstate
                              | `TRan of 'a TrackRank.lstate ]
                             as 'b)
                            list ->
                            ('b list ->
                             ('a, Output.res) Direct.abstract ->
                             ('a, 'c) Direct.abstract) ->
                            ('a, 'c) Direct.abstract
                        end
        end
  end
val instantiate :
  ((unit -> 'a) -> 'b list -> ('c -> 'd -> 'd) -> unit -> 'e) -> 'a -> 'e =
  <fun>
#   type 'a pr = { pf : 'a; }
# val runit : 'a pr -> 'a = <fun>
#   * * * * * * * * *     module FDet :
  sig
    type indet = Domains_direct.FloatDomainL.v
    type outdet = Domains_direct.FloatDomainL.v
    type tdet = outdet ref
    type 'a lstate =
        ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
    type 'a tag_lstate = [ `TDet of 'a lstate ]
    type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
      constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
    type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
      constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
    val fetch_iter : [> `TDet of 'a ] list -> 'a
    val dfetch :
      unit -> ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
    val dstore :
      'a -> ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
    val decl :
      unit ->
      ([> `TDet of
            ('b, int ref) Direct.abstract *
            ('b, Domains_direct.FloatDomainL.v ref) Direct.abstract ]
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
      'a Domains_direct.FloatDomainL.vc ->
      ([> `TDet of
            'c * ('a, Domains_direct.FloatDomainL.v ref) Direct.abstract ]
       as 'b)
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
            ('b, Domains_direct.FloatDomainL.v ref) Direct.abstract ]
       as 'a)
      list ->
      ('a list -> ('b, Domains_direct.FloatDomainL.v) Direct.abstract -> 'c) ->
      'c
  end
module IDet :
  sig
    type indet = Domains_direct.IntegerDomainL.v
    type outdet = Domains_direct.IntegerDomainL.v
    type tdet = outdet ref
    type 'a lstate =
        ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
    type 'a tag_lstate = [ `TDet of 'a lstate ]
    type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
      constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
    type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
      constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
    val fetch_iter : [> `TDet of 'a ] list -> 'a
    val dfetch :
      unit -> ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
    val dstore :
      'a -> ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
    val decl :
      unit ->
      ([> `TDet of
            ('b, int ref) Direct.abstract *
            ('b, Domains_direct.IntegerDomainL.v ref) Direct.abstract ]
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
      'a Domains_direct.IntegerDomainL.vc ->
      ([> `TDet of
            'c * ('a, Domains_direct.IntegerDomainL.v ref) Direct.abstract ]
       as 'b)
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
            ('b, Domains_direct.IntegerDomainL.v ref) Direct.abstract ]
       as 'a)
      list ->
      ('a list -> ('b, Domains_direct.IntegerDomainL.v) Direct.abstract -> 'c) ->
      'c
  end
module RDet :
  sig
    type indet = Domains_direct.RationalDomainL.v
    type outdet = Domains_direct.RationalDomainL.v
    type tdet = outdet ref
    type 'a lstate =
        ('a, int ref) Direct.abstract * ('a, tdet) Direct.abstract
    type 'a tag_lstate = [ `TDet of 'a lstate ]
    type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
      constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
    type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
      constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
    val fetch_iter : [> `TDet of 'a ] list -> 'a
    val dfetch :
      unit -> ([> `TDet of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
    val dstore :
      'a -> ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
    val decl :
      unit ->
      ([> `TDet of
            ('b, int ref) Direct.abstract *
            ('b, Domains_direct.RationalDomainL.v ref) Direct.abstract ]
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
      'a Domains_direct.RationalDomainL.vc ->
      ([> `TDet of
            'c * ('a, Domains_direct.RationalDomainL.v ref) Direct.abstract ]
       as 'b)
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
            ('b, Domains_direct.RationalDomainL.v ref) Direct.abstract ]
       as 'a)
      list ->
      ('a list ->
       ('b, Domains_direct.RationalDomainL.v) Direct.abstract -> 'c) ->
      'c
  end
module Z3 :
  sig
    type v = int
    type kind = Kinds.domain_is_field
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
    type kind = Kinds.domain_is_field
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
        type kind = Domains_direct.FloatDomainL.kind
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
        type kind = Domains_direct.FloatDomainL.kind
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
        type kind = Domains_direct.IntegerDomainL.kind
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
        type kind = Domains_direct.IntegerDomainL.kind
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
        type kind = Domains_direct.RationalDomainL.kind
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
        type kind = Z3.kind
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
        type kind = Z19.kind
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
File "test-ge-common.ml", line 33, characters 15-37:
Unbound module FortranVectorContainer
# 
