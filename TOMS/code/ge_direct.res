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
                ('a C.vc ->
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract ->
                 'a C.vo -> ('a, unit) Direct.abstract) ->
                'a C.vc ->
                ('a, int) Direct.abstract ->
                ('a, int) Direct.abstract ->
                'a C.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
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
              val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val decl :
                ('a, C.contr) Direct.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val updt :
                ('a C.vc ->
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract ->
                 'a C.vo -> ('a, unit) Direct.abstract) ->
                'a C.vc ->
                ('a, int) Direct.abstract ->
                ('a, int) Direct.abstract ->
                'a C.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
              val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
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
              val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val decl :
                ('a, C.contr) Direct.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val updt :
                ('a C.vc ->
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract ->
                 'a C.vo -> ('a, unit) Direct.abstract) ->
                'a C.vc ->
                ('a, int) Direct.abstract ->
                ('a, int) Direct.abstract ->
                'a C.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
              val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
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
              val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val decl :
                ('a, C.contr) Direct.abstract ->
                ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
              val updt :
                ('a C.vc ->
                 ('a, int) Direct.abstract ->
                 ('a, int) Direct.abstract ->
                 'a C.vo -> ('a, unit) Direct.abstract) ->
                'a C.vc ->
                ('a, int) Direct.abstract ->
                ('a, int) Direct.abstract ->
                'a C.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
              val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
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
            functor (Det0 : DETF) ->
              sig
                module DD :
                  sig
                    type indet = C.Dom.v
                    type outdet = Det0(C.Dom).outdet
                    type tdet = outdet ref
                    type 'a lstate = 'a Det0(C.Dom).lstate
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
                    val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                    val set :
                      ('a, indet) Direct.abstract ->
                      ('a * [> 'a tag_lstate ] * 'b, unit) lm
                    val fin :
                      unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                  end
                module type S =
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
                      type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val decl :
                        'a ->
                        'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                      val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                      val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, C.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val decl :
                        ('a, C.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val updt :
                        ('a C.vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a C.vo -> ('a, unit) Direct.abstract) ->
                        'a C.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a C.vo ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
                      val wants_pack : bool
                    end
                  val make_result :
                    'a wmatrix -> 'b -> ('b -> 'a C.vc -> 'c) -> 'c
                end
          module OutDet :
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
                      type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val decl :
                        'a ->
                        'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                      val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                      val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, C.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val decl :
                        ('a, C.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val updt :
                        ('a C.vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a C.vo -> ('a, unit) Direct.abstract) ->
                        'a C.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a C.vo ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
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
                      type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val decl :
                        'a ->
                        'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                      val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                      val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, C.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val decl :
                        ('a, C.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val updt :
                        ('a C.vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a C.vo -> ('a, unit) Direct.abstract) ->
                        'a C.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a C.vo ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
                      val wants_pack : bool
                    end
                  val make_result :
                    'a wmatrix ->
                    ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
                    ('b list -> ('a, C.contr * 'c) Direct.abstract -> 'd) ->
                    'd
                end
          module OutDetRank :
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
                      type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                      val decl :
                        'a ->
                        'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                      val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                      val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
                    end
                  module L :
                    sig
                      type 'a lstate = ('a, C.contr) Direct.abstract
                      type 'a tag_lstate = [ `TLower of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val decl :
                        ('a, C.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val updt :
                        ('a C.vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a C.vo -> ('a, unit) Direct.abstract) ->
                        'a C.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a C.vo ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
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
                      type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                        ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                         as 'b)
                        list ->
                        ('b list ->
                         ('c, unit) Direct.abstract ->
                         ('a, 'd) Direct.abstract) ->
                        ('a, 'd) Direct.abstract
                      val add :
                        'a PK.fra ->
                        ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
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
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val decl :
                        ('a, C.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val updt :
                        ('a C.vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a C.vo -> ('a, unit) Direct.abstract) ->
                        'a C.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a C.vo ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
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
                      type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                        ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                         as 'b)
                        list ->
                        ('b list ->
                         ('c, unit) Direct.abstract ->
                         ('a, 'd) Direct.abstract) ->
                        ('a, 'd) Direct.abstract
                      val add :
                        'a PK.fra ->
                        ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
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
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val decl :
                        ('a, C.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val updt :
                        ('a C.vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a C.vo -> ('a, unit) Direct.abstract) ->
                        'a C.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a C.vo ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
                      val wants_pack : bool
                    end
                  val make_result :
                    'a wmatrix ->
                    ([> `TLower of 'a L.lstate
                      | `TPivot of ('a, 'c ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('a, C.contr * C.contr * 'c) Direct.abstract ->
                     ('a, 'd) Direct.abstract) ->
                    ('a, 'd) Direct.abstract
                end
          module Out_LU_Packed :
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
                      type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                      type 'a tag_lstate = [ `TPivot of 'a lstate ]
                      type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                        ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                         as 'b)
                        list ->
                        ('b list ->
                         ('c, unit) Direct.abstract ->
                         ('a, 'd) Direct.abstract) ->
                        ('a, 'd) Direct.abstract
                      val add :
                        'a PK.fra ->
                        ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
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
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                        constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                      val mfetch :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val decl :
                        ('a, C.contr) Direct.abstract ->
                        ('a * [> 'a tag_lstate ] * 'b, C.contr) lm
                      val updt :
                        ('a C.vc ->
                         ('a, int) Direct.abstract ->
                         ('a, int) Direct.abstract ->
                         'a C.vo -> ('a, unit) Direct.abstract) ->
                        'a C.vc ->
                        ('a, int) Direct.abstract ->
                        ('a, int) Direct.abstract ->
                        'a C.vo ->
                        ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                      val fin :
                        unit -> ('a * [> 'a tag_lstate ] * 'b, C.contr) om
                      val wants_pack : bool
                    end
                  val make_result :
                    'a ->
                    ([> `TLower of 'c L.lstate
                      | `TPivot of ('c, 'd ref) Direct.abstract ]
                     as 'b)
                    list ->
                    ('b list ->
                     ('c, C.contr * 'd) Direct.abstract ->
                     ('c, 'e) Direct.abstract) ->
                    ('c, 'e) Direct.abstract
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
                      functor (Out : OutProxy(Detf).S) ->
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
                              type res = Out(Det)(PK).res
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
                                    ('a C.vc ->
                                     ('a, int) Direct.abstract ->
                                     ('a, int) Direct.abstract ->
                                     'a C.vo -> ('a, unit) Direct.abstract) ->
                                    'a C.vc ->
                                    ('a, int) Direct.abstract ->
                                    ('a, int) Direct.abstract ->
                                    'a C.vo ->
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
          ('a GAC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val decl :
          ('a, GAC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val updt :
          ('a GAC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val decl :
          ('a, GAC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val updt :
          ('a GAC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val decl :
          ('a, GAC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
        val updt :
          ('a GAC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
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
    module OutProxy :
      functor (Det0 : GEF.DETF) ->
        sig
          module DD :
            sig
              type indet = GAC_F.Dom.v
              type outdet = Det0(GAC_F.Dom).outdet
              type tdet = outdet ref
              type 'a lstate = 'a Det0(GAC_F.Dom).lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
          module type S =
            functor
              (Det : sig
                       type indet
                       type outdet = DD.outdet
                       type tdet = outdet ref
                       type 'a lstate = 'a DD.lstate
                       type 'a tag_lstate = [ `TDet of 'a lstate ]
                       type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                         constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                       type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                     end) ->
              functor (PK : GEF.PIVOTKIND) ->
                sig
                  type res
                  module R : GEF.TrackRank.RANK
                  module P :
                    sig
                      type idx_rep = int
                      type flip_rep = PK.flip_rep
                      type perm_rep = PK.perm_rep
                      type 'a ira = ('a, idx_rep) Direct.abstract
                      type 'a fra = ('a, flip_rep) Direct.abstract
                      type 'a pra = ('a, perm_rep) Direct.abstract
                      type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
                      | `TRan of 'a GEF.TrackRank.lstate ],
                     'b)
                    GEF.cmonad
                end
        end
    module OutJustMatrix :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_F.contr
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix -> 'b -> ('b -> 'a GAC_F.vc -> 'c) -> 'c
          end
    module OutDet :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_F.contr * Det.outdet
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> 'a Det.tag_lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_F.contr * Det.outdet) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module OutRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_F.contr * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
              ('b list -> ('a, GAC_F.contr * 'c) Direct.abstract -> 'd) -> 'd
          end
    module OutDetRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_F.contr * Det.outdet * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TDet of 'a Det.lstate
                | `TRan of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GAC_F.contr * Det.outdet * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module OutDetRankPivot :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_F.contr * Det.outdet * int * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
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
               ('a, GAC_F.contr * Det.outdet * 'd * 'c) Direct.abstract ->
               ('a, 'e) Direct.abstract) ->
              ('a, 'e) Direct.abstract
          end
    module Out_L_U :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_F.contr * GAC_F.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TLower of 'a L.lstate
                | `TPivot of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GAC_F.contr * GAC_F.contr * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module Out_LU_Packed :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_F.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a ->
              ([> `TLower of 'c L.lstate
                | `TPivot of ('c, 'd ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('c, GAC_F.contr * 'd) Direct.abstract ->
               ('c, 'e) Direct.abstract) ->
              ('c, 'e) Direct.abstract
          end
    module type PIVOT =
      functor (D : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GAC_F.Dom.v option,
               [> `TDet of 'a D(GAC_F.Dom).lstate | `TPivot of 'a P.lstate ],
               'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_F.Dom.v
                type outdet = Det(GAC_F.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_F.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                   'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                  'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
              end
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_F.Dom.v
                type outdet = Det(GAC_F.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_F.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_F.Dom.v
                type outdet = Det(GAC_F.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_F.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_F.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module GenGE :
      functor (PivotF : PIVOT) ->
        functor (PK : GEF.PIVOTKIND) ->
          functor (Detf : GEF.DETF) ->
            functor (Update : GEF.UpdateProxy(GAC_F)(Detf).S) ->
              functor (In : INPUT) ->
                functor (Out : OutProxy(Detf).S) ->
                  sig
                    module Det :
                      sig
                        type indet = GAC_F.Dom.v
                        type outdet = Detf(GAC_F.Dom).outdet
                        type tdet = outdet ref
                        type 'a lstate = 'a Detf(GAC_F.Dom).lstate
                        type 'a tag_lstate = [ `TDet of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                    module U :
                      sig
                        type 'a in_val = 'a GAC_F.Dom.vc
                        type out_val = Detf(GAC_F.Dom).outdet
                        val update :
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          ('a in_val -> ('a, unit) Direct.abstract) ->
                          ('a, out_val ref) Direct.abstract ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val upd_kind : Ge.update_kind
                      end
                    module Input :
                      sig
                        type inp = In.inp
                        val get_input :
                          ('a, inp) Direct.abstract ->
                          (('a, GAC_F.contr) Direct.abstract *
                           ('a, int) Direct.abstract * bool, 'b,
                           ('a, 'c) Direct.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(Det)(PK).res
                        module R :
                          sig
                            type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               unit)
                              GEF.TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int)
                              GEF.TrackRank.lm
                          end
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
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
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
                                ('a, GAC_F.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                            val decl :
                              ('a, GAC_F.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) lm
                            val updt :
                              ('a GAC_F.vc ->
                               ('a, int) Direct.abstract ->
                               ('a, int) Direct.abstract ->
                               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
                              'a GAC_F.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a GAC_F.vo ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
                            val wants_pack : bool
                          end
                        val make_result :
                          'a wmatrix ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TLower of 'a L.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a GEF.TrackRank.lstate ],
                           'b)
                          GEF.cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a wmatrix ->
                          'a curpos ->
                          ('a, GAC_F.Dom.v option,
                           [> `TDet of 'a Detf(GAC_F.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          GEF.cmonad
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
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                        val col_iter :
                          'a ->
                          'b ->
                          ('c, int) Direct.abstract ->
                          ('c, int) Direct.abstract ->
                          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                          (('c, int) Direct.abstract ->
                           'd ->
                           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                      end
                    val gen :
                      ('a, Input.inp) Direct.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TLower of 'a Output.L.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a GEF.TrackRank.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
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
          ('a GVC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val decl :
          ('a, GVC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val updt :
          ('a GVC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val decl :
          ('a, GVC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val updt :
          ('a GVC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val decl :
          ('a, GVC_F.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
        val updt :
          ('a GVC_F.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_F.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
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
    module OutProxy :
      functor (Det0 : GEF.DETF) ->
        sig
          module DD :
            sig
              type indet = GVC_F.Dom.v
              type outdet = Det0(GVC_F.Dom).outdet
              type tdet = outdet ref
              type 'a lstate = 'a Det0(GVC_F.Dom).lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
          module type S =
            functor
              (Det : sig
                       type indet
                       type outdet = DD.outdet
                       type tdet = outdet ref
                       type 'a lstate = 'a DD.lstate
                       type 'a tag_lstate = [ `TDet of 'a lstate ]
                       type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                         constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                       type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                     end) ->
              functor (PK : GEF.PIVOTKIND) ->
                sig
                  type res
                  module R : GEF.TrackRank.RANK
                  module P :
                    sig
                      type idx_rep = int
                      type flip_rep = PK.flip_rep
                      type perm_rep = PK.perm_rep
                      type 'a ira = ('a, idx_rep) Direct.abstract
                      type 'a fra = ('a, flip_rep) Direct.abstract
                      type 'a pra = ('a, perm_rep) Direct.abstract
                      type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
                      | `TRan of 'a GEF.TrackRank.lstate ],
                     'b)
                    GEF.cmonad
                end
        end
    module OutJustMatrix :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_F.contr
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix -> 'b -> ('b -> 'a GVC_F.vc -> 'c) -> 'c
          end
    module OutDet :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_F.contr * Det.outdet
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> 'a Det.tag_lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_F.contr * Det.outdet) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module OutRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_F.contr * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
              ('b list -> ('a, GVC_F.contr * 'c) Direct.abstract -> 'd) -> 'd
          end
    module OutDetRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_F.contr * Det.outdet * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TDet of 'a Det.lstate
                | `TRan of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_F.contr * Det.outdet * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module OutDetRankPivot :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_F.contr * Det.outdet * int * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
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
               ('a, GVC_F.contr * Det.outdet * 'd * 'c) Direct.abstract ->
               ('a, 'e) Direct.abstract) ->
              ('a, 'e) Direct.abstract
          end
    module Out_L_U :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_F.contr * GVC_F.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TLower of 'a L.lstate
                | `TPivot of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_F.contr * GVC_F.contr * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module Out_LU_Packed :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_F.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_F.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_F.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_F.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a ->
              ([> `TLower of 'c L.lstate
                | `TPivot of ('c, 'd ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('c, GVC_F.contr * 'd) Direct.abstract ->
               ('c, 'e) Direct.abstract) ->
              ('c, 'e) Direct.abstract
          end
    module type PIVOT =
      functor (D : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_F.Dom.v option,
               [> `TDet of 'a D(GVC_F.Dom).lstate | `TPivot of 'a P.lstate ],
               'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_F.Dom.v
                type outdet = Det(GVC_F.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_F.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                   'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                  'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
              end
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_F.Dom.v
                type outdet = Det(GVC_F.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_F.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_F.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_F.Dom.v
                type outdet = Det(GVC_F.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_F.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_F.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module GenGE :
      functor (PivotF : PIVOT) ->
        functor (PK : GEF.PIVOTKIND) ->
          functor (Detf : GEF.DETF) ->
            functor (Update : GEF.UpdateProxy(GVC_F)(Detf).S) ->
              functor (In : INPUT) ->
                functor (Out : OutProxy(Detf).S) ->
                  sig
                    module Det :
                      sig
                        type indet = GVC_F.Dom.v
                        type outdet = Detf(GVC_F.Dom).outdet
                        type tdet = outdet ref
                        type 'a lstate = 'a Detf(GVC_F.Dom).lstate
                        type 'a tag_lstate = [ `TDet of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                    module U :
                      sig
                        type 'a in_val = 'a GVC_F.Dom.vc
                        type out_val = Detf(GVC_F.Dom).outdet
                        val update :
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          ('a in_val -> ('a, unit) Direct.abstract) ->
                          ('a, out_val ref) Direct.abstract ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val upd_kind : Ge.update_kind
                      end
                    module Input :
                      sig
                        type inp = In.inp
                        val get_input :
                          ('a, inp) Direct.abstract ->
                          (('a, GVC_F.contr) Direct.abstract *
                           ('a, int) Direct.abstract * bool, 'b,
                           ('a, 'c) Direct.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(Det)(PK).res
                        module R :
                          sig
                            type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               unit)
                              GEF.TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int)
                              GEF.TrackRank.lm
                          end
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
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
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
                                ('a, GVC_F.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                            val decl :
                              ('a, GVC_F.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) lm
                            val updt :
                              ('a GVC_F.vc ->
                               ('a, int) Direct.abstract ->
                               ('a, int) Direct.abstract ->
                               'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
                              'a GVC_F.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a GVC_F.vo ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
                            val wants_pack : bool
                          end
                        val make_result :
                          'a wmatrix ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TLower of 'a L.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a GEF.TrackRank.lstate ],
                           'b)
                          GEF.cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a wmatrix ->
                          'a curpos ->
                          ('a, GVC_F.Dom.v option,
                           [> `TDet of 'a Detf(GVC_F.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          GEF.cmonad
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
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                        val col_iter :
                          'a ->
                          'b ->
                          ('c, int) Direct.abstract ->
                          ('c, int) Direct.abstract ->
                          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                          (('c, int) Direct.abstract ->
                           'd ->
                           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                      end
                    val gen :
                      ('a, Input.inp) Direct.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TLower of 'a Output.L.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a GEF.TrackRank.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
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
          ('a GAC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val decl :
          ('a, GAC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val updt :
          ('a GAC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val decl :
          ('a, GAC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val updt :
          ('a GAC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val decl :
          ('a, GAC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
        val updt :
          ('a GAC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
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
    module OutProxy :
      functor (Det0 : GEF.DETF) ->
        sig
          module DD :
            sig
              type indet = GAC_I.Dom.v
              type outdet = Det0(GAC_I.Dom).outdet
              type tdet = outdet ref
              type 'a lstate = 'a Det0(GAC_I.Dom).lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
          module type S =
            functor
              (Det : sig
                       type indet
                       type outdet = DD.outdet
                       type tdet = outdet ref
                       type 'a lstate = 'a DD.lstate
                       type 'a tag_lstate = [ `TDet of 'a lstate ]
                       type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                         constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                       type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                     end) ->
              functor (PK : GEF.PIVOTKIND) ->
                sig
                  type res
                  module R : GEF.TrackRank.RANK
                  module P :
                    sig
                      type idx_rep = int
                      type flip_rep = PK.flip_rep
                      type perm_rep = PK.perm_rep
                      type 'a ira = ('a, idx_rep) Direct.abstract
                      type 'a fra = ('a, flip_rep) Direct.abstract
                      type 'a pra = ('a, perm_rep) Direct.abstract
                      type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
                      | `TRan of 'a GEF.TrackRank.lstate ],
                     'b)
                    GEF.cmonad
                end
        end
    module OutJustMatrix :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_I.contr
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix -> 'b -> ('b -> 'a GAC_I.vc -> 'c) -> 'c
          end
    module OutDet :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_I.contr * Det.outdet
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> 'a Det.tag_lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_I.contr * Det.outdet) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module OutRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_I.contr * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
              ('b list -> ('a, GAC_I.contr * 'c) Direct.abstract -> 'd) -> 'd
          end
    module OutDetRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_I.contr * Det.outdet * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TDet of 'a Det.lstate
                | `TRan of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GAC_I.contr * Det.outdet * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module OutDetRankPivot :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_I.contr * Det.outdet * int * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
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
               ('a, GAC_I.contr * Det.outdet * 'd * 'c) Direct.abstract ->
               ('a, 'e) Direct.abstract) ->
              ('a, 'e) Direct.abstract
          end
    module Out_L_U :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_I.contr * GAC_I.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TLower of 'a L.lstate
                | `TPivot of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GAC_I.contr * GAC_I.contr * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module Out_LU_Packed :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_I.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a ->
              ([> `TLower of 'c L.lstate
                | `TPivot of ('c, 'd ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('c, GAC_I.contr * 'd) Direct.abstract ->
               ('c, 'e) Direct.abstract) ->
              ('c, 'e) Direct.abstract
          end
    module type PIVOT =
      functor (D : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GAC_I.Dom.v option,
               [> `TDet of 'a D(GAC_I.Dom).lstate | `TPivot of 'a P.lstate ],
               'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_I.Dom.v
                type outdet = Det(GAC_I.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_I.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                   'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                  'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
              end
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_I.Dom.v
                type outdet = Det(GAC_I.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_I.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_I.Dom.v
                type outdet = Det(GAC_I.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_I.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_I.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module GenGE :
      functor (PivotF : PIVOT) ->
        functor (PK : GEF.PIVOTKIND) ->
          functor (Detf : GEF.DETF) ->
            functor (Update : GEF.UpdateProxy(GAC_I)(Detf).S) ->
              functor (In : INPUT) ->
                functor (Out : OutProxy(Detf).S) ->
                  sig
                    module Det :
                      sig
                        type indet = GAC_I.Dom.v
                        type outdet = Detf(GAC_I.Dom).outdet
                        type tdet = outdet ref
                        type 'a lstate = 'a Detf(GAC_I.Dom).lstate
                        type 'a tag_lstate = [ `TDet of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                    module U :
                      sig
                        type 'a in_val = 'a GAC_I.Dom.vc
                        type out_val = Detf(GAC_I.Dom).outdet
                        val update :
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          ('a in_val -> ('a, unit) Direct.abstract) ->
                          ('a, out_val ref) Direct.abstract ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val upd_kind : Ge.update_kind
                      end
                    module Input :
                      sig
                        type inp = In.inp
                        val get_input :
                          ('a, inp) Direct.abstract ->
                          (('a, GAC_I.contr) Direct.abstract *
                           ('a, int) Direct.abstract * bool, 'b,
                           ('a, 'c) Direct.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(Det)(PK).res
                        module R :
                          sig
                            type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               unit)
                              GEF.TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int)
                              GEF.TrackRank.lm
                          end
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
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
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
                                ('a, GAC_I.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                            val decl :
                              ('a, GAC_I.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) lm
                            val updt :
                              ('a GAC_I.vc ->
                               ('a, int) Direct.abstract ->
                               ('a, int) Direct.abstract ->
                               'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
                              'a GAC_I.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a GAC_I.vo ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
                            val wants_pack : bool
                          end
                        val make_result :
                          'a wmatrix ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TLower of 'a L.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a GEF.TrackRank.lstate ],
                           'b)
                          GEF.cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a wmatrix ->
                          'a curpos ->
                          ('a, GAC_I.Dom.v option,
                           [> `TDet of 'a Detf(GAC_I.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          GEF.cmonad
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
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                        val col_iter :
                          'a ->
                          'b ->
                          ('c, int) Direct.abstract ->
                          ('c, int) Direct.abstract ->
                          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                          (('c, int) Direct.abstract ->
                           'd ->
                           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                      end
                    val gen :
                      ('a, Input.inp) Direct.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TLower of 'a Output.L.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a GEF.TrackRank.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
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
          ('a GVC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val decl :
          ('a, GVC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val updt :
          ('a GVC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val decl :
          ('a, GVC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val updt :
          ('a GVC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val decl :
          ('a, GVC_I.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
        val updt :
          ('a GVC_I.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_I.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
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
    module OutProxy :
      functor (Det0 : GEF.DETF) ->
        sig
          module DD :
            sig
              type indet = GVC_I.Dom.v
              type outdet = Det0(GVC_I.Dom).outdet
              type tdet = outdet ref
              type 'a lstate = 'a Det0(GVC_I.Dom).lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
          module type S =
            functor
              (Det : sig
                       type indet
                       type outdet = DD.outdet
                       type tdet = outdet ref
                       type 'a lstate = 'a DD.lstate
                       type 'a tag_lstate = [ `TDet of 'a lstate ]
                       type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                         constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                       type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                     end) ->
              functor (PK : GEF.PIVOTKIND) ->
                sig
                  type res
                  module R : GEF.TrackRank.RANK
                  module P :
                    sig
                      type idx_rep = int
                      type flip_rep = PK.flip_rep
                      type perm_rep = PK.perm_rep
                      type 'a ira = ('a, idx_rep) Direct.abstract
                      type 'a fra = ('a, flip_rep) Direct.abstract
                      type 'a pra = ('a, perm_rep) Direct.abstract
                      type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
                      | `TRan of 'a GEF.TrackRank.lstate ],
                     'b)
                    GEF.cmonad
                end
        end
    module OutJustMatrix :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_I.contr
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix -> 'b -> ('b -> 'a GVC_I.vc -> 'c) -> 'c
          end
    module OutDet :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_I.contr * Det.outdet
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> 'a Det.tag_lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_I.contr * Det.outdet) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module OutRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_I.contr * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
              ('b list -> ('a, GVC_I.contr * 'c) Direct.abstract -> 'd) -> 'd
          end
    module OutDetRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_I.contr * Det.outdet * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TDet of 'a Det.lstate
                | `TRan of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_I.contr * Det.outdet * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module OutDetRankPivot :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_I.contr * Det.outdet * int * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
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
               ('a, GVC_I.contr * Det.outdet * 'd * 'c) Direct.abstract ->
               ('a, 'e) Direct.abstract) ->
              ('a, 'e) Direct.abstract
          end
    module Out_L_U :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_I.contr * GVC_I.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TLower of 'a L.lstate
                | `TPivot of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_I.contr * GVC_I.contr * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module Out_LU_Packed :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_I.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_I.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_I.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_I.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a ->
              ([> `TLower of 'c L.lstate
                | `TPivot of ('c, 'd ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('c, GVC_I.contr * 'd) Direct.abstract ->
               ('c, 'e) Direct.abstract) ->
              ('c, 'e) Direct.abstract
          end
    module type PIVOT =
      functor (D : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_I.Dom.v option,
               [> `TDet of 'a D(GVC_I.Dom).lstate | `TPivot of 'a P.lstate ],
               'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_I.Dom.v
                type outdet = Det(GVC_I.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_I.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                   'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                  'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
              end
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_I.Dom.v
                type outdet = Det(GVC_I.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_I.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_I.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_I.Dom.v
                type outdet = Det(GVC_I.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_I.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_I.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module GenGE :
      functor (PivotF : PIVOT) ->
        functor (PK : GEF.PIVOTKIND) ->
          functor (Detf : GEF.DETF) ->
            functor (Update : GEF.UpdateProxy(GVC_I)(Detf).S) ->
              functor (In : INPUT) ->
                functor (Out : OutProxy(Detf).S) ->
                  sig
                    module Det :
                      sig
                        type indet = GVC_I.Dom.v
                        type outdet = Detf(GVC_I.Dom).outdet
                        type tdet = outdet ref
                        type 'a lstate = 'a Detf(GVC_I.Dom).lstate
                        type 'a tag_lstate = [ `TDet of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                    module U :
                      sig
                        type 'a in_val = 'a GVC_I.Dom.vc
                        type out_val = Detf(GVC_I.Dom).outdet
                        val update :
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          ('a in_val -> ('a, unit) Direct.abstract) ->
                          ('a, out_val ref) Direct.abstract ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val upd_kind : Ge.update_kind
                      end
                    module Input :
                      sig
                        type inp = In.inp
                        val get_input :
                          ('a, inp) Direct.abstract ->
                          (('a, GVC_I.contr) Direct.abstract *
                           ('a, int) Direct.abstract * bool, 'b,
                           ('a, 'c) Direct.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(Det)(PK).res
                        module R :
                          sig
                            type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               unit)
                              GEF.TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int)
                              GEF.TrackRank.lm
                          end
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
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
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
                                ('a, GVC_I.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                            val decl :
                              ('a, GVC_I.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) lm
                            val updt :
                              ('a GVC_I.vc ->
                               ('a, int) Direct.abstract ->
                               ('a, int) Direct.abstract ->
                               'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
                              'a GVC_I.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a GVC_I.vo ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
                            val wants_pack : bool
                          end
                        val make_result :
                          'a wmatrix ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TLower of 'a L.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a GEF.TrackRank.lstate ],
                           'b)
                          GEF.cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a wmatrix ->
                          'a curpos ->
                          ('a, GVC_I.Dom.v option,
                           [> `TDet of 'a Detf(GVC_I.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          GEF.cmonad
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
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                        val col_iter :
                          'a ->
                          'b ->
                          ('c, int) Direct.abstract ->
                          ('c, int) Direct.abstract ->
                          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                          (('c, int) Direct.abstract ->
                           'd ->
                           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                      end
                    val gen :
                      ('a, Input.inp) Direct.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TLower of 'a Output.L.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a GEF.TrackRank.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
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
          ('a GAC_R.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_R.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val decl :
          ('a, GAC_R.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val updt :
          ('a GAC_R.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_R.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val decl :
          ('a, GAC_R.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val updt :
          ('a GAC_R.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_R.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val decl :
          ('a, GAC_R.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
        val updt :
          ('a GAC_R.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
          'a GAC_R.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
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
    module OutProxy :
      functor (Det0 : GEF.DETF) ->
        sig
          module DD :
            sig
              type indet = GAC_R.Dom.v
              type outdet = Det0(GAC_R.Dom).outdet
              type tdet = outdet ref
              type 'a lstate = 'a Det0(GAC_R.Dom).lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
          module type S =
            functor
              (Det : sig
                       type indet
                       type outdet = DD.outdet
                       type tdet = outdet ref
                       type 'a lstate = 'a DD.lstate
                       type 'a tag_lstate = [ `TDet of 'a lstate ]
                       type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                         constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                       type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                     end) ->
              functor (PK : GEF.PIVOTKIND) ->
                sig
                  type res
                  module R : GEF.TrackRank.RANK
                  module P :
                    sig
                      type idx_rep = int
                      type flip_rep = PK.flip_rep
                      type perm_rep = PK.perm_rep
                      type 'a ira = ('a, idx_rep) Direct.abstract
                      type 'a fra = ('a, flip_rep) Direct.abstract
                      type 'a pra = ('a, perm_rep) Direct.abstract
                      type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
                      | `TRan of 'a GEF.TrackRank.lstate ],
                     'b)
                    GEF.cmonad
                end
        end
    module OutJustMatrix :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_R.contr
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_R.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix -> 'b -> ('b -> 'a GAC_R.vc -> 'c) -> 'c
          end
    module OutDet :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_R.contr * Det.outdet
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_R.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> 'a Det.tag_lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_R.contr * Det.outdet) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module OutRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_R.contr * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_R.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
              ('b list -> ('a, GAC_R.contr * 'c) Direct.abstract -> 'd) -> 'd
          end
    module OutDetRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_R.contr * Det.outdet * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GAC_R.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TDet of 'a Det.lstate
                | `TRan of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GAC_R.contr * Det.outdet * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module OutDetRankPivot :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_R.contr * Det.outdet * int * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_R.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
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
               ('a, GAC_R.contr * Det.outdet * 'd * 'c) Direct.abstract ->
               ('a, 'e) Direct.abstract) ->
              ('a, 'e) Direct.abstract
          end
    module Out_L_U :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_R.contr * GAC_R.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_R.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TLower of 'a L.lstate
                | `TPivot of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GAC_R.contr * GAC_R.contr * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module Out_LU_Packed :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GAC_R.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GAC_R.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                  'a GAC_R.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GAC_R.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a ->
              ([> `TLower of 'c L.lstate
                | `TPivot of ('c, 'd ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('c, GAC_R.contr * 'd) Direct.abstract ->
               ('c, 'e) Direct.abstract) ->
              ('c, 'e) Direct.abstract
          end
    module type PIVOT =
      functor (D : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GAC_R.Dom.v option,
               [> `TDet of 'a D(GAC_R.Dom).lstate | `TPivot of 'a P.lstate ],
               'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_R.Dom.v
                type outdet = Det(GAC_R.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_R.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                   'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                  'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
              end
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_R.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_R.Dom.v
                type outdet = Det(GAC_R.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_R.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GAC_R.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GAC_R.Dom.v
                type outdet = Det(GAC_R.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GAC_R.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GAC_R.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module GenGE :
      functor (PivotF : PIVOT) ->
        functor (PK : GEF.PIVOTKIND) ->
          functor (Detf : GEF.DETF) ->
            functor (Update : GEF.UpdateProxy(GAC_R)(Detf).S) ->
              functor (In : INPUT) ->
                functor (Out : OutProxy(Detf).S) ->
                  sig
                    module Det :
                      sig
                        type indet = GAC_R.Dom.v
                        type outdet = Detf(GAC_R.Dom).outdet
                        type tdet = outdet ref
                        type 'a lstate = 'a Detf(GAC_R.Dom).lstate
                        type 'a tag_lstate = [ `TDet of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                    module U :
                      sig
                        type 'a in_val = 'a GAC_R.Dom.vc
                        type out_val = Detf(GAC_R.Dom).outdet
                        val update :
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          ('a in_val -> ('a, unit) Direct.abstract) ->
                          ('a, out_val ref) Direct.abstract ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val upd_kind : Ge.update_kind
                      end
                    module Input :
                      sig
                        type inp = In.inp
                        val get_input :
                          ('a, inp) Direct.abstract ->
                          (('a, GAC_R.contr) Direct.abstract *
                           ('a, int) Direct.abstract * bool, 'b,
                           ('a, 'c) Direct.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(Det)(PK).res
                        module R :
                          sig
                            type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               unit)
                              GEF.TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int)
                              GEF.TrackRank.lm
                          end
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
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
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
                                ('a, GAC_R.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                            val decl :
                              ('a, GAC_R.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) lm
                            val updt :
                              ('a GAC_R.vc ->
                               ('a, int) Direct.abstract ->
                               ('a, int) Direct.abstract ->
                               'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
                              'a GAC_R.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a GAC_R.vo ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
                            val wants_pack : bool
                          end
                        val make_result :
                          'a wmatrix ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TLower of 'a L.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a GEF.TrackRank.lstate ],
                           'b)
                          GEF.cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a wmatrix ->
                          'a curpos ->
                          ('a, GAC_R.Dom.v option,
                           [> `TDet of 'a Detf(GAC_R.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          GEF.cmonad
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
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                        val col_iter :
                          'a ->
                          'b ->
                          ('c, int) Direct.abstract ->
                          ('c, int) Direct.abstract ->
                          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                          (('c, int) Direct.abstract ->
                           'd ->
                           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                      end
                    val gen :
                      ('a, Input.inp) Direct.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TLower of 'a Output.L.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a GEF.TrackRank.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
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
          ('a GVC_Z3.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z3.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z3.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val decl :
          ('a, GVC_Z3.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val updt :
          ('a GVC_Z3.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z3.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z3.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val decl :
          ('a, GVC_Z3.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val updt :
          ('a GVC_Z3.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z3.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z3.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val decl :
          ('a, GVC_Z3.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
        val updt :
          ('a GVC_Z3.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z3.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z3.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
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
    module OutProxy :
      functor (Det0 : GEF.DETF) ->
        sig
          module DD :
            sig
              type indet = GVC_Z3.Dom.v
              type outdet = Det0(GVC_Z3.Dom).outdet
              type tdet = outdet ref
              type 'a lstate = 'a Det0(GVC_Z3.Dom).lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
          module type S =
            functor
              (Det : sig
                       type indet
                       type outdet = DD.outdet
                       type tdet = outdet ref
                       type 'a lstate = 'a DD.lstate
                       type 'a tag_lstate = [ `TDet of 'a lstate ]
                       type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                         constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                       type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                     end) ->
              functor (PK : GEF.PIVOTKIND) ->
                sig
                  type res
                  module R : GEF.TrackRank.RANK
                  module P :
                    sig
                      type idx_rep = int
                      type flip_rep = PK.flip_rep
                      type perm_rep = PK.perm_rep
                      type 'a ira = ('a, idx_rep) Direct.abstract
                      type 'a fra = ('a, flip_rep) Direct.abstract
                      type 'a pra = ('a, perm_rep) Direct.abstract
                      type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
                      | `TRan of 'a GEF.TrackRank.lstate ],
                     'b)
                    GEF.cmonad
                end
        end
    module OutJustMatrix :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z3.contr
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z3.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix -> 'b -> ('b -> 'a GVC_Z3.vc -> 'c) -> 'c
          end
    module OutDet :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z3.contr * Det.outdet
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z3.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> 'a Det.tag_lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_Z3.contr * Det.outdet) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module OutRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z3.contr * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z3.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
              ('b list -> ('a, GVC_Z3.contr * 'c) Direct.abstract -> 'd) ->
              'd
          end
    module OutDetRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z3.contr * Det.outdet * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z3.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TDet of 'a Det.lstate
                | `TRan of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z3.contr * Det.outdet * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module OutDetRankPivot :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z3.contr * Det.outdet * int * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_Z3.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
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
               ('a, GVC_Z3.contr * Det.outdet * 'd * 'c) Direct.abstract ->
               ('a, 'e) Direct.abstract) ->
              ('a, 'e) Direct.abstract
          end
    module Out_L_U :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z3.contr * GVC_Z3.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_Z3.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TLower of 'a L.lstate
                | `TPivot of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z3.contr * GVC_Z3.contr * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module Out_LU_Packed :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z3.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_Z3.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z3.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z3.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a ->
              ([> `TLower of 'c L.lstate
                | `TPivot of ('c, 'd ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('c, GVC_Z3.contr * 'd) Direct.abstract ->
               ('c, 'e) Direct.abstract) ->
              ('c, 'e) Direct.abstract
          end
    module type PIVOT =
      functor (D : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_Z3.Dom.v option,
               [> `TDet of 'a D(GVC_Z3.Dom).lstate | `TPivot of 'a P.lstate ],
               'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_Z3.Dom.v
                type outdet = Det(GVC_Z3.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_Z3.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                   'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                  'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
              end
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_Z3.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_Z3.Dom.v
                type outdet = Det(GVC_Z3.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_Z3.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_Z3.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_Z3.Dom.v
                type outdet = Det(GVC_Z3.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_Z3.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_Z3.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module GenGE :
      functor (PivotF : PIVOT) ->
        functor (PK : GEF.PIVOTKIND) ->
          functor (Detf : GEF.DETF) ->
            functor (Update : GEF.UpdateProxy(GVC_Z3)(Detf).S) ->
              functor (In : INPUT) ->
                functor (Out : OutProxy(Detf).S) ->
                  sig
                    module Det :
                      sig
                        type indet = GVC_Z3.Dom.v
                        type outdet = Detf(GVC_Z3.Dom).outdet
                        type tdet = outdet ref
                        type 'a lstate = 'a Detf(GVC_Z3.Dom).lstate
                        type 'a tag_lstate = [ `TDet of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                    module U :
                      sig
                        type 'a in_val = 'a GVC_Z3.Dom.vc
                        type out_val = Detf(GVC_Z3.Dom).outdet
                        val update :
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          ('a in_val -> ('a, unit) Direct.abstract) ->
                          ('a, out_val ref) Direct.abstract ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val upd_kind : Ge.update_kind
                      end
                    module Input :
                      sig
                        type inp = In.inp
                        val get_input :
                          ('a, inp) Direct.abstract ->
                          (('a, GVC_Z3.contr) Direct.abstract *
                           ('a, int) Direct.abstract * bool, 'b,
                           ('a, 'c) Direct.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(Det)(PK).res
                        module R :
                          sig
                            type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               unit)
                              GEF.TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int)
                              GEF.TrackRank.lm
                          end
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
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
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
                                ('a, GVC_Z3.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                            val decl :
                              ('a, GVC_Z3.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) lm
                            val updt :
                              ('a GVC_Z3.vc ->
                               ('a, int) Direct.abstract ->
                               ('a, int) Direct.abstract ->
                               'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
                              'a GVC_Z3.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a GVC_Z3.vo ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
                            val wants_pack : bool
                          end
                        val make_result :
                          'a wmatrix ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TLower of 'a L.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a GEF.TrackRank.lstate ],
                           'b)
                          GEF.cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a wmatrix ->
                          'a curpos ->
                          ('a, GVC_Z3.Dom.v option,
                           [> `TDet of 'a Detf(GVC_Z3.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          GEF.cmonad
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
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                        val col_iter :
                          'a ->
                          'b ->
                          ('c, int) Direct.abstract ->
                          ('c, int) Direct.abstract ->
                          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                          (('c, int) Direct.abstract ->
                           'd ->
                           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                      end
                    val gen :
                      ('a, Input.inp) Direct.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TLower of 'a Output.L.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a GEF.TrackRank.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
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
          ('a GVC_Z19.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z19.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z19.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val decl :
          ('a, GVC_Z19.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val updt :
          ('a GVC_Z19.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z19.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z19.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val decl :
          ('a, GVC_Z19.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val updt :
          ('a GVC_Z19.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z19.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z19.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
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
        val mfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val decl :
          ('a, GVC_Z19.contr) Direct.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) lm
        val updt :
          ('a GVC_Z19.vc ->
           ('a, int) Direct.abstract ->
           ('a, int) Direct.abstract ->
           'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
          'a GVC_Z19.vc ->
          ('a, int) Direct.abstract ->
          ('a, int) Direct.abstract ->
          'a GVC_Z19.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
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
    module OutProxy :
      functor (Det0 : GEF.DETF) ->
        sig
          module DD :
            sig
              type indet = GVC_Z19.Dom.v
              type outdet = Det0(GVC_Z19.Dom).outdet
              type tdet = outdet ref
              type 'a lstate = 'a Det0(GVC_Z19.Dom).lstate
              type 'a tag_lstate = [ `TDet of 'a lstate ]
              type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
              type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
          module type S =
            functor
              (Det : sig
                       type indet
                       type outdet = DD.outdet
                       type tdet = outdet ref
                       type 'a lstate = 'a DD.lstate
                       type 'a tag_lstate = [ `TDet of 'a lstate ]
                       type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                         constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                       type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                     end) ->
              functor (PK : GEF.PIVOTKIND) ->
                sig
                  type res
                  module R : GEF.TrackRank.RANK
                  module P :
                    sig
                      type idx_rep = int
                      type flip_rep = PK.flip_rep
                      type perm_rep = PK.perm_rep
                      type 'a ira = ('a, idx_rep) Direct.abstract
                      type 'a fra = ('a, flip_rep) Direct.abstract
                      type 'a pra = ('a, perm_rep) Direct.abstract
                      type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
                      | `TRan of 'a GEF.TrackRank.lstate ],
                     'b)
                    GEF.cmonad
                end
        end
    module OutJustMatrix :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z19.contr
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z19.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix -> 'b -> ('b -> 'a GVC_Z19.vc -> 'c) -> 'c
          end
    module OutDet :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z19.contr * Det.outdet
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit -> 'a -> ('a -> ('b, int) Direct.abstract -> 'c) -> 'c
              end
            module P :
              sig
                type idx_rep = PK.idx_rep
                type flip_rep = PK.flip_rep
                type perm_rep = PK.perm_rep
                type 'a ira = ('a, idx_rep) Direct.abstract
                type 'a fra = ('a, flip_rep) Direct.abstract
                type 'a pra = ('a, perm_rep) Direct.abstract
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z19.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> 'a Det.tag_lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_Z19.contr * Det.outdet) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module OutRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z19.contr * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z19.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TRan of ('a, 'c ref) Direct.abstract ] as 'b) list ->
              ('b list -> ('a, GVC_Z19.contr * 'c) Direct.abstract -> 'd) ->
              'd
          end
    module OutDetRank :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z19.contr * Det.outdet * int
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rowrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val colrep : 'a PK.ira -> 'a PK.ira -> 'a PK.fra
                val decl :
                  'a -> 'b -> ('b -> ('c, unit) Direct.abstract -> 'd) -> 'd
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
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
                  ('a GVC_Z19.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TDet of 'a Det.lstate
                | `TRan of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z19.contr * Det.outdet * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module OutDetRankPivot :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z19.contr * Det.outdet * int * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_Z19.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
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
               ('a, GVC_Z19.contr * Det.outdet * 'd * 'c) Direct.abstract ->
               ('a, 'e) Direct.abstract) ->
              ('a, 'e) Direct.abstract
          end
    module Out_L_U :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z19.contr * GVC_Z19.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_Z19.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a wmatrix ->
              ([> `TLower of 'a L.lstate
                | `TPivot of ('a, 'c ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('a, GVC_Z19.contr * GVC_Z19.contr * 'c) Direct.abstract ->
               ('a, 'd) Direct.abstract) ->
              ('a, 'd) Direct.abstract
          end
    module Out_LU_Packed :
      functor (Det : GEF.DETERMINANT) ->
        functor (PK : GEF.PIVOTKIND) ->
          sig
            type res = GVC_Z19.contr * PK.perm_rep
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
                   ('b, int ref) Direct.abstract -> ('b, 'c) Direct.abstract) ->
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
                    val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
                  end
                val fin :
                  unit ->
                  ([> `TRan of ('b, 'c ref) Direct.abstract ] as 'a) list ->
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
                type 'a lstate = ('a, PK.perm_rep ref) Direct.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list ->
                   ('c, unit) Direct.abstract -> ('a, 'd) Direct.abstract) ->
                  ('a, 'd) Direct.abstract
                val add :
                  'a PK.fra ->
                  ([> `TPivot of ('a, PK.perm_rep ref) Direct.abstract ]
                   as 'b)
                  list ->
                  ('b list -> ('a, unit) Direct.abstract option -> 'c) -> 'c
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Direct.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Direct.abstract option -> 'd) -> 'd
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
                  ('a GVC_Z19.vc ->
                   ('a, int) Direct.abstract ->
                   ('a, int) Direct.abstract ->
                   'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                  'a GVC_Z19.vc ->
                  ('a, int) Direct.abstract ->
                  ('a, int) Direct.abstract ->
                  'a GVC_Z19.vo ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                val fin :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
                val wants_pack : bool
              end
            val make_result :
              'a ->
              ([> `TLower of 'c L.lstate
                | `TPivot of ('c, 'd ref) Direct.abstract ]
               as 'b)
              list ->
              ('b list ->
               ('c, GVC_Z19.contr * 'd) Direct.abstract ->
               ('c, 'e) Direct.abstract) ->
              ('c, 'e) Direct.abstract
          end
    module type PIVOT =
      functor (D : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ('a, GVC_Z19.Dom.v option,
               [> `TDet of 'a D(GVC_Z19.Dom).lstate | `TPivot of 'a P.lstate ],
               'b)
              GEF.cmonad
          end
    module RowPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_Z19.Dom.v
                type outdet = Det(GVC_Z19.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_Z19.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
                   'd -> 'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                  'e -> ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
              end
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_Z19.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module FullPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_Z19.Dom.v
                type outdet = Det(GVC_Z19.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_Z19.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'b) list ->
              ('b list ->
               ('a, GVC_Z19.Dom.v option) Direct.abstract ->
               ('a, 'c) Direct.abstract) ->
              ('a, 'c) Direct.abstract
          end
    module NoPivot :
      functor (Det : GEF.DETF) ->
        functor (P : GEF.TRACKPIVOT) ->
          sig
            module D :
              sig
                type indet = GVC_Z19.Dom.v
                type outdet = Det(GVC_Z19.Dom).outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det(GVC_Z19.Dom).lstate
                type 'a tag_lstate = [ `TDet of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
            val findpivot :
              'a wmatrix ->
              'a curpos ->
              'b ->
              ('b -> ('a, GVC_Z19.Dom.v option) Direct.abstract -> 'c) -> 'c
          end
    module GenGE :
      functor (PivotF : PIVOT) ->
        functor (PK : GEF.PIVOTKIND) ->
          functor (Detf : GEF.DETF) ->
            functor (Update : GEF.UpdateProxy(GVC_Z19)(Detf).S) ->
              functor (In : INPUT) ->
                functor (Out : OutProxy(Detf).S) ->
                  sig
                    module Det :
                      sig
                        type indet = GVC_Z19.Dom.v
                        type outdet = Detf(GVC_Z19.Dom).outdet
                        type tdet = outdet ref
                        type 'a lstate = 'a Detf(GVC_Z19.Dom).lstate
                        type 'a tag_lstate = [ `TDet of 'a lstate ]
                        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
                        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                          constraint 'a =
                            'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                    module U :
                      sig
                        type 'a in_val = 'a GVC_Z19.Dom.vc
                        type out_val = Detf(GVC_Z19.Dom).outdet
                        val update :
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          'a in_val ->
                          ('a in_val -> ('a, unit) Direct.abstract) ->
                          ('a, out_val ref) Direct.abstract ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
                          ('a, unit, 'b, 'c) GEF.cmonad
                        val upd_kind : Ge.update_kind
                      end
                    module Input :
                      sig
                        type inp = In.inp
                        val get_input :
                          ('a, inp) Direct.abstract ->
                          (('a, GVC_Z19.contr) Direct.abstract *
                           ('a, int) Direct.abstract * bool, 'b,
                           ('a, 'c) Direct.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(Det)(PK).res
                        module R :
                          sig
                            type 'a tag_lstate = 'a GEF.TrackRank.tag_lstate_
                            val rfetch :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val decl :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int ref)
                              GEF.TrackRank.lm
                            val succ :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               unit)
                              GEF.TrackRank.lm
                            val fin :
                              unit ->
                              ('a * [> 'a GEF.TrackRank.tag_lstate ] * 'b,
                               int)
                              GEF.TrackRank.lm
                          end
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
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
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
                                ('a, GVC_Z19.contr) Direct.abstract
                            type 'a tag_lstate = [ `TLower of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val mfetch :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr)
                              lm
                            val decl :
                              ('a, GVC_Z19.contr) Direct.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr)
                              lm
                            val updt :
                              ('a GVC_Z19.vc ->
                               ('a, int) Direct.abstract ->
                               ('a, int) Direct.abstract ->
                               'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
                              'a GVC_Z19.vc ->
                              ('a, int) Direct.abstract ->
                              ('a, int) Direct.abstract ->
                              'a GVC_Z19.vo ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm option
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr)
                              om
                            val wants_pack : bool
                          end
                        val make_result :
                          'a wmatrix ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TLower of 'a L.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a GEF.TrackRank.lstate ],
                           'b)
                          GEF.cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a wmatrix ->
                          'a curpos ->
                          ('a, GVC_Z19.Dom.v option,
                           [> `TDet of 'a Detf(GVC_Z19.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          GEF.cmonad
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
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                        val col_iter :
                          'a ->
                          'b ->
                          ('c, int) Direct.abstract ->
                          ('c, int) Direct.abstract ->
                          ('a -> 'b -> ('c, int) Direct.abstract -> 'd) ->
                          (('c, int) Direct.abstract ->
                           'd ->
                           'e -> ('f -> 'g -> 'g) -> ('c, 'h) Direct.abstract) ->
                          'e ->
                          ('e -> ('c, unit) Direct.abstract -> 'i) -> 'i
                      end
                    val gen :
                      ('a, Input.inp) Direct.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TLower of 'a Output.L.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a GEF.TrackRank.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Direct.abstract ->
                       ('a, 'c) Direct.abstract) ->
                      ('a, 'c) Direct.abstract
                  end
  end
module GenFA1 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutJustMatrix(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA2 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDet(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA3 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA4 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA11 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutJustMatrix(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA12 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDet(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA13 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA14 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA24 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDetRankPivot(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA25 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDetRankPivot(Det)(GEF.RowVectorPerm).res
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
            type idx_rep = int
            type flip_rep = GEF.RowVectorPerm.flip_rep
            type perm_rep = GEF.RowVectorPerm.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA26 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutJustMatrix(Det)(GEF.RowVectorPerm).res
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
            type idx_rep = int
            type flip_rep = GEF.RowVectorPerm.flip_rep
            type perm_rep = GEF.RowVectorPerm.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA5 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpMatrixMargin.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutJustMatrix(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA6 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.AbstractDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.AbstractDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpMatrixMargin.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDet(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA7 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpMatrixMargin.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA8 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpMatrixMargin.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFA9 :
  sig
    module Det :
      sig
        type indet = GAC_F.Dom.v
        type outdet = GEF.NoDet(GAC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GAC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_F.Dom.vc
        type out_val = GEF.NoDet(GAC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_F.Out_LU_Packed(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_F.wmatrix ->
          'a G_GAC_F.curpos ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV1 :
  sig
    module Det :
      sig
        type indet = GVC_F.Dom.v
        type outdet = GEF.NoDet(GVC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GVC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_F.Dom.vc
        type out_val = GEF.NoDet(GVC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_F.OutJustMatrix(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_F.wmatrix ->
          'a G_GVC_F.curpos ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV2 :
  sig
    module Det :
      sig
        type indet = GVC_F.Dom.v
        type outdet = GEF.AbstractDet(GVC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_F.Dom.vc
        type out_val = GEF.AbstractDet(GVC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_F.OutDet(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_F.wmatrix ->
          'a G_GVC_F.curpos ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV3 :
  sig
    module Det :
      sig
        type indet = GVC_F.Dom.v
        type outdet = GEF.NoDet(GVC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.NoDet(GVC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_F.Dom.vc
        type out_val = GEF.NoDet(GVC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_F.OutRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_F.wmatrix ->
          'a G_GVC_F.curpos ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV4 :
  sig
    module Det :
      sig
        type indet = GVC_F.Dom.v
        type outdet = GEF.AbstractDet(GVC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_F.Dom.vc
        type out_val = GEF.AbstractDet(GVC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_F.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_F.wmatrix ->
          'a G_GVC_F.curpos ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenFV5 :
  sig
    module Det :
      sig
        type indet = GVC_F.Dom.v
        type outdet = GEF.AbstractDet(GVC_F.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_F.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_F.Dom.vc
        type out_val = GEF.AbstractDet(GVC_F.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_F.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_F.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_F.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_F.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_F.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_F.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_F.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_F.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_F.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_F.wmatrix ->
          'a G_GVC_F.curpos ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA1 :
  sig
    module Det :
      sig
        type indet = GAC_I.Dom.v
        type outdet = GEF.AbstractDet(GAC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_I.Dom.vc
        type out_val = GEF.AbstractDet(GAC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_I.OutJustMatrix(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_I.wmatrix ->
          'a G_GAC_I.curpos ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA2 :
  sig
    module Det :
      sig
        type indet = GAC_I.Dom.v
        type outdet = GEF.AbstractDet(GAC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_I.Dom.vc
        type out_val = GEF.AbstractDet(GAC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_I.OutDet(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_I.wmatrix ->
          'a G_GAC_I.curpos ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA3 :
  sig
    module Det :
      sig
        type indet = GAC_I.Dom.v
        type outdet = GEF.AbstractDet(GAC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_I.Dom.vc
        type out_val = GEF.AbstractDet(GAC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_I.OutRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_I.wmatrix ->
          'a G_GAC_I.curpos ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA4 :
  sig
    module Det :
      sig
        type indet = GAC_I.Dom.v
        type outdet = GEF.AbstractDet(GAC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_I.Dom.vc
        type out_val = GEF.AbstractDet(GAC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_I.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_I.wmatrix ->
          'a G_GAC_I.curpos ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA5 :
  sig
    module Det :
      sig
        type indet = GAC_I.Dom.v
        type outdet = GEF.AbstractDet(GAC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_I.Dom.vc
        type out_val = GEF.AbstractDet(GAC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_I.Out_L_U(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_I.wmatrix ->
          'a G_GAC_I.curpos ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIA6 :
  sig
    module Det :
      sig
        type indet = GAC_I.Dom.v
        type outdet = GEF.AbstractDet(GAC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_I.Dom.vc
        type out_val = GEF.AbstractDet(GAC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_I.Out_LU_Packed(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_I.wmatrix ->
          'a G_GAC_I.curpos ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV1 :
  sig
    module Det :
      sig
        type indet = GVC_I.Dom.v
        type outdet = GEF.AbstractDet(GVC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_I.Dom.vc
        type out_val = GEF.AbstractDet(GVC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_I.OutJustMatrix(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_I.wmatrix ->
          'a G_GVC_I.curpos ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV2 :
  sig
    module Det :
      sig
        type indet = GVC_I.Dom.v
        type outdet = GEF.AbstractDet(GVC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_I.Dom.vc
        type out_val = GEF.AbstractDet(GVC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_I.OutDet(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_I.wmatrix ->
          'a G_GVC_I.curpos ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV3 :
  sig
    module Det :
      sig
        type indet = GVC_I.Dom.v
        type outdet = GEF.AbstractDet(GVC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_I.Dom.vc
        type out_val = GEF.AbstractDet(GVC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_I.OutRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_I.wmatrix ->
          'a G_GVC_I.curpos ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV4 :
  sig
    module Det :
      sig
        type indet = GVC_I.Dom.v
        type outdet = GEF.AbstractDet(GVC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_I.Dom.vc
        type out_val = GEF.AbstractDet(GVC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_I.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_I.wmatrix ->
          'a G_GVC_I.curpos ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV5 :
  sig
    module Det :
      sig
        type indet = GVC_I.Dom.v
        type outdet = GEF.AbstractDet(GVC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_I.Dom.vc
        type out_val = GEF.AbstractDet(GVC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_I.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_I.wmatrix ->
          'a G_GVC_I.curpos ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenIV6 :
  sig
    module Det :
      sig
        type indet = GVC_I.Dom.v
        type outdet = GEF.AbstractDet(GVC_I.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_I.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_I.Dom.vc
        type out_val = GEF.AbstractDet(GVC_I.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_I.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_I.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_I.OutDetRankPivot(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_I.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_I.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_I.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_I.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_I.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_I.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_I.wmatrix ->
          'a G_GVC_I.curpos ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA1 :
  sig
    module Det :
      sig
        type indet = GAC_R.Dom.v
        type outdet = GEF.AbstractDet(GAC_R.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_R.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_R.Dom.vc
        type out_val = GEF.AbstractDet(GAC_R.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_R.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_R.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_R.OutJustMatrix(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_R.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_R.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_R.wmatrix ->
          'a G_GAC_R.curpos ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA2 :
  sig
    module Det :
      sig
        type indet = GAC_R.Dom.v
        type outdet = GEF.AbstractDet(GAC_R.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_R.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_R.Dom.vc
        type out_val = GEF.AbstractDet(GAC_R.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_R.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_R.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_R.OutDet(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_R.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_R.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_R.wmatrix ->
          'a G_GAC_R.curpos ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA3 :
  sig
    module Det :
      sig
        type indet = GAC_R.Dom.v
        type outdet = GEF.AbstractDet(GAC_R.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_R.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_R.Dom.vc
        type out_val = GEF.AbstractDet(GAC_R.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_R.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_R.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_R.OutRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_R.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_R.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_R.wmatrix ->
          'a G_GAC_R.curpos ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenRA4 :
  sig
    module Det :
      sig
        type indet = GAC_R.Dom.v
        type outdet = GEF.AbstractDet(GAC_R.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GAC_R.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GAC_R.Dom.vc
        type out_val = GEF.AbstractDet(GAC_R.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GAC_R.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GAC_R.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GAC_R.OutDetRank(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GAC_R.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GAC_R.vo -> ('a, unit) Direct.abstract) ->
              'a GAC_R.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GAC_R.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GAC_R.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GAC_R.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GAC_R.wmatrix ->
          'a G_GAC_R.curpos ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenZp3 :
  sig
    module Det :
      sig
        type indet = GVC_Z3.Dom.v
        type outdet = GEF.AbstractDet(GVC_Z3.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_Z3.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_Z3.Dom.vc
        type out_val = GEF.AbstractDet(GVC_Z3.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_Z3.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_Z3.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_Z3.OutDetRankPivot(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_Z3.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_Z3.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_Z3.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_Z3.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z3.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_Z3.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_Z3.wmatrix ->
          'a G_GVC_Z3.curpos ->
          ('a, GVC_Z3.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_Z3.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
module GenZp19 :
  sig
    module Det :
      sig
        type indet = GVC_Z19.Dom.v
        type outdet = GEF.AbstractDet(GVC_Z19.Dom).outdet
        type tdet = outdet ref
        type 'a lstate = 'a GEF.AbstractDet(GVC_Z19.Dom).lstate
        type 'a tag_lstate = [ `TDet of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
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
    module U :
      sig
        type 'a in_val = 'a GVC_Z19.Dom.vc
        type out_val = GEF.AbstractDet(GVC_Z19.Dom).outdet
        val update :
          'a in_val ->
          'a in_val ->
          'a in_val ->
          'a in_val ->
          ('a in_val -> ('a, unit) Direct.abstract) ->
          ('a, out_val ref) Direct.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
        val upd_kind : Ge.update_kind
      end
    module Input :
      sig
        type inp = G_GVC_Z19.InpJustMatrix.inp
        val get_input :
          ('a, inp) Direct.abstract ->
          (('a, GVC_Z19.contr) Direct.abstract * ('a, int) Direct.abstract *
           bool, 'b, ('a, 'c) Direct.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = G_GVC_Z19.OutDetRankPivot(Det)(GEF.PermList).res
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
            type idx_rep = int
            type flip_rep = GEF.PermList.flip_rep
            type perm_rep = GEF.PermList.perm_rep
            type 'a ira = ('a, idx_rep) Direct.abstract
            type 'a fra = ('a, flip_rep) Direct.abstract
            type 'a pra = ('a, perm_rep) Direct.abstract
            type 'a lstate = ('a, perm_rep ref) Direct.abstract
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
              (('a, unit) Direct.abstract option, [> 'a tag_lstate ] list,
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
              ('a GVC_Z19.vc ->
               ('a, int) Direct.abstract ->
               ('a, int) Direct.abstract ->
               'a GVC_Z19.vo -> ('a, unit) Direct.abstract) ->
              'a GVC_Z19.vc ->
              ('a, int) Direct.abstract ->
              ('a, int) Direct.abstract ->
              'a GVC_Z19.vo -> ('a * [> 'a tag_lstate ] * 'b, unit) lm option
            val fin :
              unit -> ('a * [> 'a tag_lstate ] * 'b, GVC_Z19.contr) om
            val wants_pack : bool
          end
        val make_result :
          'a G_GVC_Z19.wmatrix ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TLower of 'a L.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a GEF.TrackRank.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a G_GVC_Z19.wmatrix ->
          'a G_GVC_Z19.curpos ->
          ('a, GVC_Z19.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_Z19.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
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
    val gen :
      ('a, Input.inp) Direct.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TLower of 'a Output.L.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a GEF.TrackRank.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) Direct.abstract -> ('a, 'c) Direct.abstract) ->
      ('a, 'c) Direct.abstract
  end
val resFA1 : GenFA1.Input.inp -> GenFA1.Output.res = <fun>
val resFA2 : GenFA2.Input.inp -> GenFA2.Output.res = <fun>
val resFA3 : GenFA3.Input.inp -> GenFA3.Output.res = <fun>
val resFA4 : GenFA4.Input.inp -> GenFA4.Output.res = <fun>
val resFV1 : GenFV1.Input.inp -> GenFV1.Output.res = <fun>
val resFV2 : GenFV2.Input.inp -> GenFV2.Output.res = <fun>
val resFV3 : GenFV3.Input.inp -> GenFV3.Output.res = <fun>
val resFV4 : GenFV4.Input.inp -> GenFV4.Output.res = <fun>
val resFV5 : GenFV5.Input.inp -> GenFV5.Output.res = <fun>
val resIA1 : GenIA1.Input.inp -> GenIA1.Output.res = <fun>
val resIA2 : GenIA2.Input.inp -> GenIA2.Output.res = <fun>
val resIA3 : GenIA3.Input.inp -> GenIA3.Output.res = <fun>
val resIA4 : GenIA4.Input.inp -> GenIA4.Output.res = <fun>
val resIA5 : GenIA5.Input.inp -> GenIA5.Output.res = <fun>
val resIV1 : GenIV1.Input.inp -> GenIV1.Output.res = <fun>
val resIV2 : GenIV2.Input.inp -> GenIV2.Output.res = <fun>
val resIV3 : GenIV3.Input.inp -> GenIV3.Output.res = <fun>
val resIV4 : GenIV4.Input.inp -> GenIV4.Output.res = <fun>
val resIV5 : GenIV5.Input.inp -> GenIV5.Output.res = <fun>
val resIV6 : GenIV6.Input.inp -> GenIV6.Output.res = <fun>
val resFA11 : GenFA11.Input.inp -> GenFA11.Output.res = <fun>
val resFA12 : GenFA12.Input.inp -> GenFA12.Output.res = <fun>
val resFA13 : GenFA13.Input.inp -> GenFA13.Output.res = <fun>
val resFA14 : GenFA14.Input.inp -> GenFA14.Output.res = <fun>
val resFA24 : GenFA24.Input.inp -> GenFA24.Output.res = <fun>
val resFA25 : GenFA25.Input.inp -> GenFA25.Output.res = <fun>
val resFA26 : GenFA26.Input.inp -> GenFA26.Output.res = <fun>
val resRA1 : GenRA1.Input.inp -> GenRA1.Output.res = <fun>
val resRA2 : GenRA2.Input.inp -> GenRA2.Output.res = <fun>
val resRA3 : GenRA3.Input.inp -> GenRA3.Output.res = <fun>
val resRA4 : GenRA4.Input.inp -> GenRA4.Output.res = <fun>
val resFA5 : GenFA5.Input.inp -> GenFA5.Output.res = <fun>
val resFA6 : GenFA6.Input.inp -> GenFA6.Output.res = <fun>
val resFA7 : GenFA7.Input.inp -> GenFA7.Output.res = <fun>
val resFA8 : GenFA8.Input.inp -> GenFA8.Output.res = <fun>
val resFA9 : GenFA9.Input.inp -> GenFA9.Output.res = <fun>
val resZp3 : GenZp3.Input.inp -> GenZp3.Output.res = <fun>
val resZp19 : GenZp19.Input.inp -> GenZp19.Output.res = <fun>
val rFA1 : GenFA1.Input.inp -> GenFA1.Output.res = <fun>
val rFA2 : GenFA2.Input.inp -> GenFA2.Output.res = <fun>
val rFA3 : GenFA3.Input.inp -> GenFA3.Output.res = <fun>
val rFA4 : GenFA4.Input.inp -> GenFA4.Output.res = <fun>
val rFV1 : GenFV1.Input.inp -> GenFV1.Output.res = <fun>
val rFV2 : GenFV2.Input.inp -> GenFV2.Output.res = <fun>
val rFV3 : GenFV3.Input.inp -> GenFV3.Output.res = <fun>
val rFV4 : GenFV4.Input.inp -> GenFV4.Output.res = <fun>
val rFV5 : GenFV5.Input.inp -> GenFV5.Output.res = <fun>
val rIA1 : GenIA1.Input.inp -> GenIA1.Output.res = <fun>
val rIA2 : GenIA2.Input.inp -> GenIA2.Output.res = <fun>
val rIA3 : GenIA3.Input.inp -> GenIA3.Output.res = <fun>
val rIA4 : GenIA4.Input.inp -> GenIA4.Output.res = <fun>
val rIA5 : GenIA5.Input.inp -> GenIA5.Output.res = <fun>
val rIV1 : GenIV1.Input.inp -> GenIV1.Output.res = <fun>
val rIV2 : GenIV2.Input.inp -> GenIV2.Output.res = <fun>
val rIV3 : GenIV3.Input.inp -> GenIV3.Output.res = <fun>
val rIV4 : GenIV4.Input.inp -> GenIV4.Output.res = <fun>
val rIV5 : GenIV5.Input.inp -> GenIV5.Output.res = <fun>
val rIV6 : GenIV6.Input.inp -> GenIV6.Output.res = <fun>
val rFA11 : GenFA11.Input.inp -> GenFA11.Output.res = <fun>
val rFA12 : GenFA12.Input.inp -> GenFA12.Output.res = <fun>
val rFA13 : GenFA13.Input.inp -> GenFA13.Output.res = <fun>
val rFA14 : GenFA14.Input.inp -> GenFA14.Output.res = <fun>
val rFA24 : GenFA24.Input.inp -> GenFA24.Output.res = <fun>
val rFA25 : GenFA25.Input.inp -> GenFA25.Output.res = <fun>
val rFA26 : GenFA26.Input.inp -> GenFA26.Output.res = <fun>
val rRA1 : GenRA1.Input.inp -> GenRA1.Output.res = <fun>
val rRA2 : GenRA2.Input.inp -> GenRA2.Output.res = <fun>
val rRA3 : GenRA3.Input.inp -> GenRA3.Output.res = <fun>
val rRA4 : GenRA4.Input.inp -> GenRA4.Output.res = <fun>
val rFA5 : GenFA5.Input.inp -> GenFA5.Output.res = <fun>
val rFA6 : GenFA6.Input.inp -> GenFA6.Output.res = <fun>
val rFA7 : GenFA7.Input.inp -> GenFA7.Output.res = <fun>
val rFA8 : GenFA8.Input.inp -> GenFA8.Output.res = <fun>
val rFA9 : GenFA9.Input.inp -> GenFA9.Output.res = <fun>
val rZp3 : GenZp3.Input.inp -> GenZp3.Output.res = <fun>
val rZp19 : GenZp19.Input.inp -> GenZp19.Output.res = <fun>
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
val resI11 : GenIA1.Output.res list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|]]
val resI12 : GenIA2.Output.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0)]
val resI13 : GenIA3.Output.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 2)]
val resI14 : GenIA4.Output.res list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0, 2)]
val resI15 : GenIA5.Output.res list =
  [([|[|1|]|], [|[|0|]|], []);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|],
    [|[|0; 0; 0|]; [|0; 0; 0|]; [|0; 0; 0|]|], []);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|],
    [|[|0; 0; 0|]; [|0; 0; 0|]; [|0; 0; 0|]|], []);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|],
    [|[|0; 0; 0; 0|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|], 
    []);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|],
    [|[|0; 0; 0|]; [|0; 0; 0|]; [|0; 0; 0|]|], [Direct.RowSwap (2, 1)])]
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
val resI21 : GenIV1.Output.res list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}]
val resI22 : GenIV2.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0)]
val resI23 : GenIV3.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 2)]
val resI24 : GenIV4.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
val resI25 : GenIV5.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
val resI26 : GenIV6.Output.res list =
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
val resFA9 : GenFA9.Output.res =
  ([|[|4.; 13.; 5.|]; [|-1.; 6.25; 1.25|]; [|1.; -1.25; 2.|]|],
   [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)])
- : unit = ()
- : unit = ()
- : unit = ()
val resF1 : GenFA1.Output.res list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
- : unit = ()
- : unit = ()
- : unit = ()
val a2v : 'a array array -> 'a Domains_direct.container2dfromvector = <fun>
val xxx : GAC_F.Dom.v Domains_direct.container2dfromvector list =
  [{arr = [|1.|]; n = 1; m = 1};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.|]; n = 3; m = 3};
   {arr = [|1.; 2.; 3.; 0.; 4.; 13.; 5.; 0.; -1.; 3.; 0.; 0.|]; n = 3; m = 4};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.; 0.; 0.; 0.|]; n = 4; m = 3};
   {arr = [|0.; 2.; 3.; 0.; 10.; 5.; 0.; 3.; 0.|]; n = 3; m = 3}]
- : unit = ()
val resFV5 : GenFV5.Output.res list =
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
val resF11 : GenFA11.Output.res list =
  [[|[|1.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]|];
   [|[|13.; 5.; 4.; 0.|];
     [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
     [|0.; 0.; -1.72413793103448287; 0.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|];
   [|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|]]
val resF12 : GenFA12.Output.res list =
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
val resF13 : GenFA13.Output.res list =
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
val resF14 : GenFA14.Output.res list =
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
val resF24 : GenFA24.Output.res list =
  [([|[|1.|]|], 1., 1, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [Direct.RowSwap (2, 1); Direct.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2,
    [Direct.RowSwap (1, 0)])]
val resF25 : GenFA25.Output.res list =
  [([|[|1.|]|], 1., 1, [|0|]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [|1; 2; 0|]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [|1; 2; 0|]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [|1; 2; 0; 3|]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2, [|1; 0; 2|])]
val resF26 : GenFA26.Output.res list =
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
val resR11 : GenRA1.Output.res list =
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
val resR12 : GenRA2.Output.res list =
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
val resR13 : GenRA3.Output.res list =
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
val resR14 : GenRA4.Output.res list =
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
