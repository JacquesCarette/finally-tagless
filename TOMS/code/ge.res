        Objective Caml version 3.09.1

#               module GEF :
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
    module Iters :
      functor (C : D.CONTAINER2D) ->
        sig
          val row_iter :
            'a C.vc ->
            ('a, int) Code.abstract ->
            ('a, int) Code.abstract ->
            ('a, int) Code.abstract ->
            (('a, int) Code.abstract ->
             ('a, C.Dom.v) Code.abstract ->
             'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
            'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
          val col_iter :
            'a C.vc ->
            ('a, int) Code.abstract ->
            ('a, int) Code.abstract ->
            ('a, int) Code.abstract ->
            (('a, int) Code.abstract ->
             'a C.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
            'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        end
    type ('a, 'b, 'c, 'd) cmonad =
        (('a, 'b) Code.abstract, 'c list, ('a, 'd) Code.abstract)
        StateCPSMonad.monad
    type ('a, 'b, 'c, 'd) omonad =
        (('a, 'b) Code.abstract option, 'c list, ('a, 'd) Code.abstract)
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
            ('a, indet) Code.abstract ->
            ('a * [> 'a tag_lstate ] * 'b, unit) lm
          val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
          val set :
            ('a, indet) Code.abstract ->
            ('a * [> 'a tag_lstate ] * 'b, unit) lm
          val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
        end
    module type RANK =
      sig
        type 'a lstate = ('a, int ref) Code.abstract
        type 'a tag_lstate = [ `TRan of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
      end
    module TrackRank :
      sig
        type 'a lstate = ('a, int ref) Code.abstract
        type 'a tag_lstate = [ `TRan of 'a lstate ]
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
      end
    module Rank :
      sig
        type 'a lstate = ('a, int ref) Code.abstract
        type 'a tag_lstate = [ `TRan of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
      end
    module NoRank :
      sig
        type 'a lstate = ('a, int ref) Code.abstract
        type 'a tag_lstate = [ `TRan of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
      end
    module NoDet :
      functor (Dom : D.DOMAINL) ->
        sig
          type indet = Dom.v
          type outdet = unit
          type tdet = outdet ref
          type 'a lstate = unit
          val decl :
            unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
          val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
          val zero_sign :
            unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
          val acc : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
          val get :
            unit -> 'a -> ('a -> ('b, unit ref) Code.abstract -> 'c) -> 'c
          val set : 'a -> 'b -> ('b -> ('c, unit) Code.abstract -> 'd) -> 'd
          val fin :
            unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
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
            ([> `TDet of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
          val decl :
            unit ->
            ([> `TDet of
                  ('b, int ref) Code.abstract * ('b, Dom.v ref) Code.abstract ]
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
            'a Dom.vc ->
            ([> `TDet of 'c * ('a, Dom.v ref) Code.abstract ] as 'b) list ->
            ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
          val get :
            unit ->
            ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
          val set :
            ('a, 'b) Code.abstract ->
            ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
            ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
          val fin :
            unit ->
            ([> `TDet of
                  ('b, int ref) Code.abstract * ('b, Dom.v ref) Code.abstract ]
             as 'a)
            list -> ('a list -> ('b, Dom.v) Code.abstract -> 'c) -> 'c
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
                    ('a, indet) Code.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) Code.abstract ->
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
                           type 'a vc = ('a, v) Code.abstract
                           val zeroL : 'a vc
                           val oneL : 'a vc
                           val ( +^ ) : 'a vc -> 'a vc -> 'a vc
                           val ( *^ ) : 'a vc -> 'a vc -> 'a vc
                           val ( -^ ) : 'a vc -> 'a vc -> 'a vc
                           val uminusL : 'a vc -> 'a vc
                           val divL : 'a vc -> 'a vc -> 'a vc
                           val better_thanL :
                             ('a vc -> 'a vc -> ('a, bool) Code.abstract)
                             option
                           val normalizerL : ('a vc -> 'a vc) option
                         end
                       type contr
                       type 'a vc = ('a, contr) Code.abstract
                       type 'a vo = ('a, Dom.v) Code.abstract
                       val getL :
                         'a vc ->
                         ('a, int) Code.abstract ->
                         ('a, int) Code.abstract -> 'a vo
                       val dim1 : 'a vc -> ('a, int) Code.abstract
                       val dim2 : 'a vc -> ('a, int) Code.abstract
                       val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
                       val copy : 'a vc -> 'a vc
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
                         ('a, int) Code.abstract ->
                         ('a, int) Code.abstract -> 'a vo
                       val col_head_set :
                         'a vc ->
                         ('a, int) Code.abstract ->
                         ('a, int) Code.abstract ->
                         'a vo -> ('a, unit) Code.abstract
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
                      ('a in_val -> ('a, unit) Code.abstract) ->
                      ('a, out_val ref) Code.abstract ->
                      ('a, unit, 'b, 'c) cmonad
                    val update_det :
                      'a in_val ->
                      ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                      ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                      ('a, unit, 'b, 'c) cmonad
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
            type 'a in_val = 'a Dom.vc
            type out_val = Det(C.Dom).outdet
            val update :
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              ('a Dom.vc -> 'b) -> 'c -> 'd -> ('d -> 'b -> 'e) -> 'e
            val update_det : 'a -> 'b -> ('a -> 'c) -> 'c
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
            type 'a in_val = ('a, Dom.v) Code.abstract
            type out_val = Det(Ctr.Dom).outdet
            val update :
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              ('a Dom.vc -> 'b) ->
              ('a, Dom.v ref) Code.abstract -> 'c -> ('c -> 'b -> 'd) -> 'd
            val update_det : 'a -> ('a -> 'b) -> 'c -> 'b
          end
    module type TRACKPIVOT =
      sig
        type 'a lstate
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val add :
          ('a, Code.perm) Code.abstract ->
          (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
           ('a, 'b) Code.abstract)
          StateCPSMonad.monad
        val fin :
          unit ->
          (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list, 'b)
          StateCPSMonad.monad
      end
    module TrackPivot :
      sig
        type 'a lstate = ('a, Code.perm list ref) Code.abstract
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TPivot of 'a ] list -> 'a
        val pfetch :
          unit ->
          ([> `TPivot of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val pstore :
          'a ->
          ([> `TPivot of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TPivot of ('b, Code.perm list ref) Code.abstract ] as 'a) list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val add :
          ('a, 'b) Code.abstract ->
          ([> `TPivot of ('a, 'b list ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract option -> 'd) -> 'd
      end
    module KeepPivot :
      sig
        type 'a lstate = ('a, Code.perm list ref) Code.abstract
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val fetch_iter : [> `TPivot of 'a ] list -> 'a
        val pfetch :
          unit ->
          ([> `TPivot of 'b ] as 'a) list -> ('a list -> 'b -> 'c) -> 'c
        val pstore :
          'a ->
          ([> `TPivot of 'a ] as 'b) list -> ('b list -> unit -> 'c) -> 'c
        val decl :
          unit ->
          ([> `TPivot of ('b, Code.perm list ref) Code.abstract ] as 'a) list ->
          ('a list -> ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
          ('b, 'd) Code.abstract
        val add :
          ('a, 'b) Code.abstract ->
          ([> `TPivot of ('a, 'b list ref) Code.abstract ] as 'c) list ->
          ('c list -> ('a, unit) Code.abstract option -> 'd) -> 'd
        val fin :
          unit ->
          ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
          ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd
      end
    module DiscardPivot :
      sig
        type 'a lstate = ('a, Code.perm list ref) Code.abstract
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
        val fin :
          unit -> 'a -> ('a -> ('b, 'c list) Code.abstract -> 'd) -> 'd
      end
    module type INPUT =
      functor (C : D.CONTAINER2D) ->
        sig
          type inp
          val get_input :
            ('a, inp) Code.abstract ->
            (('a, C.contr) Code.abstract * ('a, int) Code.abstract * bool,
             'b, ('a, 'c) Code.abstract)
            StateCPSMonad.monad
        end
    module InpJustMatrix :
      functor (C : D.CONTAINER2D) ->
        sig
          type inp = C.contr
          val get_input :
            'a C.vc ->
            'b ->
            ('b -> 'a C.vc * ('a, int) Code.abstract * bool -> 'c) -> 'c
        end
    module InpMatrixMargin :
      functor (C : D.CONTAINER2D) ->
        sig
          type inp = C.contr * int
          val get_input :
            ('a, 'b * 'c) Code.abstract ->
            'd ->
            ('d ->
             ('a, 'b) Code.abstract * ('a, 'c) Code.abstract * bool -> 'e) ->
            'e
        end
    module OutProxy :
      functor (C0 : D.CONTAINER2D) ->
        functor (Det0 : DETF) ->
          sig
            module D :
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
                val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val upd_sign :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                val zero_sign :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val acc :
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module type S =
              functor (C : D.CONTAINER2D) ->
                functor
                  (Det : sig
                           type indet
                           type outdet = D.outdet
                           type tdet = outdet ref
                           type 'a lstate
                           type 'a tag_lstate = [ `TDet of 'a lstate ]
                           type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                             constraint 'a =
                               'c * ([> 'c tag_lstate ] as 'd) * 'e
                           type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                             constraint 'a =
                               'c * ([> 'c tag_lstate ] as 'd) * 'e
                           val decl :
                             unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                           val upd_sign :
                             unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                           val zero_sign :
                             unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                           val acc :
                             ('a, indet) Code.abstract ->
                             ('a * [> 'a tag_lstate ] * 'b, unit) lm
                           val get :
                             unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                           val set :
                             ('a, indet) Code.abstract ->
                             ('a * [> 'a tag_lstate ] * 'b, unit) lm
                           val fin :
                             unit ->
                             ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                         end) ->
                  sig
                    type res
                    module D : DETERMINANT
                    module R : RANK
                    module P : TRACKPIVOT
                    val make_result :
                      ('a, C.contr) Code.abstract ->
                      ('a, res,
                       [> `TDet of 'a Det.lstate
                        | `TPivot of 'a P.lstate
                        | `TRan of 'a R.lstate ],
                       'b)
                      cmonad
                  end
          end
    module OutJustMatrix :
      functor (C : D.CONTAINER2D) ->
        functor (Det : DETERMINANT) ->
          sig
            type res = C.contr
            module D :
              sig
                type indet = Det.indet
                type outdet = Det.outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det.lstate
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
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) Code.abstract
                type 'a tag_lstate = [ `TRan of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
              end
            module P :
              sig
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit ->
                  'a -> ('a -> ('b, 'c list) Code.abstract -> 'd) -> 'd
              end
            val make_result : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
          end
    module OutDet :
      functor (C : D.CONTAINER2D) ->
        functor (Det : DETERMINANT) ->
          sig
            type res = C.contr * Det.outdet
            module D :
              sig
                type indet = Det.indet
                type outdet = Det.outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det.lstate
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
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) Code.abstract
                type 'a tag_lstate = [ `TRan of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
              end
            module P :
              sig
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit ->
                  'a -> ('a -> ('b, 'c list) Code.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) Code.abstract ->
              ([> 'a D.tag_lstate ] as 'c) list ->
              ('c list ->
               ('a, 'b * D.outdet) Code.abstract -> ('a, 'd) Code.abstract) ->
              ('a, 'd) Code.abstract
          end
    module OutRank :
      functor (C : D.CONTAINER2D) ->
        functor (Det : DETERMINANT) ->
          sig
            type res = C.contr * int
            module D :
              sig
                type indet = Det.indet
                type outdet = Det.outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det.lstate
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
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) Code.abstract
                type 'a tag_lstate = [ `TRan of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
              end
            module P :
              sig
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit ->
                  'a -> ('a -> ('b, 'c list) Code.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) Code.abstract ->
              ([> 'a R.tag_lstate ] as 'c) list ->
              ('c list ->
               ('a, 'b * int) Code.abstract -> ('a, 'd) Code.abstract) ->
              ('a, 'd) Code.abstract
          end
    module OutDetRank :
      functor (C : D.CONTAINER2D) ->
        functor (Det : DETERMINANT) ->
          sig
            type res = C.contr * Det.outdet * int
            module D :
              sig
                type indet = Det.indet
                type outdet = Det.outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det.lstate
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
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) Code.abstract
                type 'a tag_lstate = [ `TRan of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
              end
            module P :
              sig
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) Code.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit ->
                  'a -> ('a -> ('b, 'c list) Code.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) Code.abstract ->
              ([> `TDet of 'a D.lstate | `TRan of 'a R.lstate ] as 'c) list ->
              ('c list ->
               ('a, 'b * D.outdet * int) Code.abstract ->
               ('a, 'd) Code.abstract) ->
              ('a, 'd) Code.abstract
          end
    module OutDetRankPivot :
      functor (C : D.CONTAINER2D) ->
        functor (Det : DETERMINANT) ->
          sig
            type res = C.contr * Det.outdet * int * Code.perm list
            module D :
              sig
                type indet = Det.indet
                type outdet = Det.outdet
                type tdet = outdet ref
                type 'a lstate = 'a Det.lstate
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
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) Code.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) Code.abstract
                type 'a tag_lstate = [ `TRan of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val rfetch :
                  unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
                val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
              end
            module P :
              sig
                type 'a lstate = ('a, Code.perm list ref) Code.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                  unit ->
                  ([> `TPivot of ('b, Code.perm list ref) Code.abstract ]
                   as 'a)
                  list ->
                  ('a list ->
                   ('c, unit) Code.abstract -> ('b, 'd) Code.abstract) ->
                  ('b, 'd) Code.abstract
                val add :
                  ('a, 'b) Code.abstract ->
                  ([> `TPivot of ('a, 'b list ref) Code.abstract ] as 'c)
                  list ->
                  ('c list -> ('a, unit) Code.abstract option -> 'd) -> 'd
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) Code.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) Code.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) Code.abstract ->
              ([> `TDet of 'a D.lstate
                | `TPivot of ('a, 'd ref) Code.abstract
                | `TRan of 'a R.lstate ]
               as 'c)
              list ->
              ('c list ->
               ('a, 'b * D.outdet * int * 'd) Code.abstract ->
               ('a, 'e) Code.abstract) ->
              ('a, 'e) Code.abstract
          end
    module type PIVOT =
      functor (C : D.CONTAINER2D) ->
        functor (D : DETF) ->
          functor (P : TRACKPIVOT) ->
            sig
              val findpivot :
                'a C.vc ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                ('a, C.Dom.v option,
                 [> `TDet of 'a D(C.Dom).lstate | `TPivot of 'a P.lstate ],
                 'b)
                cmonad
            end
    module RowPivot :
      functor (Ctr : D.CONTAINER2D) ->
        functor (Det : DETF) ->
          functor (P : TRACKPIVOT) ->
            sig
              module D :
                sig
                  type indet = Ctr.Dom.v
                  type outdet = Det(Ctr.Dom).outdet
                  type tdet = outdet ref
                  type 'a lstate = 'a Det(Ctr.Dom).lstate
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
                    ('a, indet) Code.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) Code.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                end
              module I :
                sig
                  val row_iter :
                    'a Ctr.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    (('a, int) Code.abstract ->
                     ('a, Ctr.Dom.v) Code.abstract ->
                     'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
                    'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
                  val col_iter :
                    'a Ctr.vc ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    ('a, int) Code.abstract ->
                    (('a, int) Code.abstract ->
                     'a Ctr.vo ->
                     'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
                    'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
                end
              val findpivot :
                'a Ctr.vc ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                'b ->
                ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'c)
                list ->
                ('c list ->
                 ('a, Ctr.Dom.v option) Code.abstract ->
                 ('a, 'd) Code.abstract) ->
                ('a, 'd) Code.abstract
            end
    module FullPivot :
      functor (C : D.CONTAINER2D) ->
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
                  val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val upd_sign :
                    unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                  val zero_sign :
                    unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val acc :
                    ('a, indet) Code.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) Code.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                end
              val findpivot :
                'a C.vc ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                ('a, int) Code.abstract ->
                ([> 'a D.tag_lstate ] as 'b) list ->
                ('b list ->
                 ('a, C.Dom.v option) Code.abstract -> ('a, 'c) Code.abstract) ->
                ('a, 'c) Code.abstract
            end
    module NoPivot :
      functor (C : D.CONTAINER2D) ->
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
                  val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val upd_sign :
                    unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                  val zero_sign :
                    unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val acc :
                    ('a, indet) Code.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) Code.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                end
              val findpivot :
                'a C.vc ->
                ('a, int) Code.abstract ->
                'b ->
                ('a, int) Code.abstract ->
                'c ->
                'd -> ('d -> ('a, C.Dom.v option) Code.abstract -> 'e) -> 'e
            end
    module Gen :
      functor (C : D.CONTAINER2D) ->
        functor (PivotF : PIVOT) ->
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
                          unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                        val upd_sign :
                          unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                        val zero_sign :
                          unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                        val acc :
                          ('a, indet) Code.abstract ->
                          ('a * [> 'a tag_lstate ] * 'b, unit) lm
                        val get :
                          unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                        val set :
                          ('a, indet) Code.abstract ->
                          ('a * [> 'a tag_lstate ] * 'b, unit) lm
                        val fin :
                          unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
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
                          ('a in_val -> ('a, unit) Code.abstract) ->
                          ('a, out_val ref) Code.abstract ->
                          ('a, unit, 'b, 'c) cmonad
                        val update_det :
                          'a in_val ->
                          ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                          ('a in_val -> ('a, unit, 'b, 'c) cmonad) ->
                          ('a, unit, 'b, 'c) cmonad
                      end
                    module Input :
                      sig
                        type inp = In(C).inp
                        val get_input :
                          ('a, inp) Code.abstract ->
                          (('a, C.contr) Code.abstract *
                           ('a, int) Code.abstract * bool, 'b,
                           ('a, 'c) Code.abstract)
                          StateCPSMonad.monad
                      end
                    module Output :
                      sig
                        type res = Out(C)(Det).res
                        module D :
                          sig
                            type indet = Out(C)(Det).D.indet
                            type outdet = Out(C)(Det).D.outdet
                            type tdet = outdet ref
                            type 'a lstate = 'a Out(C)(Det).D.lstate
                            type 'a tag_lstate = [ `TDet of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val decl :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val upd_sign :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
                            val zero_sign :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val acc :
                              ('a, indet) Code.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val get :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                            val set :
                              ('a, indet) Code.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                          end
                        module R :
                          sig
                            type 'a lstate = ('a, int ref) Code.abstract
                            type 'a tag_lstate = [ `TRan of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
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
                        module P :
                          sig
                            type 'a lstate = 'a Out(C)(Det).P.lstate
                            type 'a tag_lstate = [ `TPivot of 'a lstate ]
                            type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                              constraint 'a =
                                'c * ([> 'c tag_lstate ] as 'd) * 'e
                            val decl :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val add :
                              ('a, Code.perm) Code.abstract ->
                              (('a, unit) Code.abstract option,
                               [> 'a tag_lstate ] list,
                               ('a, 'b) Code.abstract)
                              StateCPSMonad.monad
                            val fin :
                              unit ->
                              (('a, Code.perm list) Code.abstract,
                               [> 'a tag_lstate ] list, 'b)
                              StateCPSMonad.monad
                          end
                        val make_result :
                          ('a, C.contr) Code.abstract ->
                          ('a, res,
                           [> `TDet of 'a Det.lstate
                            | `TPivot of 'a P.lstate
                            | `TRan of 'a R.lstate ],
                           'b)
                          cmonad
                      end
                    module Pivot :
                      sig
                        val findpivot :
                          'a C.vc ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          ('a, C.Dom.v option,
                           [> `TDet of 'a Detf(C.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          cmonad
                      end
                    module I :
                      sig
                        val row_iter :
                          'a C.vc ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          (('a, int) Code.abstract ->
                           ('a, C.Dom.v) Code.abstract ->
                           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
                          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
                        val col_iter :
                          'a C.vc ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          ('a, int) Code.abstract ->
                          (('a, int) Code.abstract ->
                           'a C.vo ->
                           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
                          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
                      end
                    val gen :
                      ('a, Input.inp) Code.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a Output.R.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) Code.abstract ->
                       ('a, 'c) Code.abstract) ->
                      ('a, 'c) Code.abstract
                  end
  end
type 'a pr = { pf : 'b. ('b, 'a) code; }
# val instantiate :
  (('a, 'b) code -> 'c list -> ('d -> 'e -> 'e) -> ('a, 'f) code) ->
  ('a, 'b -> 'f) code = <fun>
# val runit : 'a pr -> 'a = <fun>
#   * * * * * * * * *     module FDet :
  sig
    type indet = Domains_code.FloatDomainL.v
    type outdet = Domains_code.FloatDomainL.v
    type tdet = outdet ref
    type 'a lstate = ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
            ('b, int ref) Code.abstract *
            ('b, Domains_code.FloatDomainL.v ref) Code.abstract ]
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
      'a Domains_code.FloatDomainL.vc ->
      ([> `TDet of 'c * ('a, Domains_code.FloatDomainL.v ref) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
    val get :
      unit ->
      ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
    val set :
      ('a, 'b) Code.abstract ->
      ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
      ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
    val fin :
      unit ->
      ([> `TDet of
            ('b, int ref) Code.abstract *
            ('b, Domains_code.FloatDomainL.v ref) Code.abstract ]
       as 'a)
      list ->
      ('a list -> ('b, Domains_code.FloatDomainL.v) Code.abstract -> 'c) ->
      'c
  end
module IDet :
  sig
    type indet = Domains_code.IntegerDomainL.v
    type outdet = Domains_code.IntegerDomainL.v
    type tdet = outdet ref
    type 'a lstate = ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
            ('b, int ref) Code.abstract *
            ('b, Domains_code.IntegerDomainL.v ref) Code.abstract ]
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
      'a Domains_code.IntegerDomainL.vc ->
      ([> `TDet of 'c * ('a, Domains_code.IntegerDomainL.v ref) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
    val get :
      unit ->
      ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
    val set :
      ('a, 'b) Code.abstract ->
      ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
      ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
    val fin :
      unit ->
      ([> `TDet of
            ('b, int ref) Code.abstract *
            ('b, Domains_code.IntegerDomainL.v ref) Code.abstract ]
       as 'a)
      list ->
      ('a list -> ('b, Domains_code.IntegerDomainL.v) Code.abstract -> 'c) ->
      'c
  end
module RDet :
  sig
    type indet = Domains_code.RationalDomainL.v
    type outdet = Domains_code.RationalDomainL.v
    type tdet = outdet ref
    type 'a lstate = ('a, int ref) Code.abstract * ('a, tdet) Code.abstract
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
            ('b, int ref) Code.abstract *
            ('b, Domains_code.RationalDomainL.v ref) Code.abstract ]
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
      'a Domains_code.RationalDomainL.vc ->
      ([> `TDet of
            'c * ('a, Domains_code.RationalDomainL.v ref) Code.abstract ]
       as 'b)
      list -> ('b list -> ('a, unit) Code.abstract -> 'd) -> 'd
    val get :
      unit ->
      ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
    val set :
      ('a, 'b) Code.abstract ->
      ([> `TDet of 'd * ('a, 'b ref) Code.abstract ] as 'c) list ->
      ('c list -> ('a, unit) Code.abstract -> 'e) -> 'e
    val fin :
      unit ->
      ([> `TDet of
            ('b, int ref) Code.abstract *
            ('b, Domains_code.RationalDomainL.v ref) Code.abstract ]
       as 'a)
      list ->
      ('a list -> ('b, Domains_code.RationalDomainL.v) Code.abstract -> 'c) ->
      'c
  end
module GAC_F :
  sig
    module Dom :
      sig
        type v = Domains_code.FloatDomainL.v
        type kind = Domains_code.FloatDomainL.kind
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
        type kind = Domains_code.FloatDomainL.kind
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
        type kind = Domains_code.IntegerDomainL.kind
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
        type kind = Domains_code.IntegerDomainL.kind
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
        type kind = Domains_code.RationalDomainL.kind
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutJustMatrix(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutJustMatrix(GAC_F)(Det).D.indet
            type outdet = GEF.OutJustMatrix(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDet(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDet(GAC_F)(Det).D.indet
            type outdet = GEF.OutDet(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDet(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDet(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutRank(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutRank(GAC_F)(Det).D.indet
            type outdet = GEF.OutRank(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutRank(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutRank(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GAC_F)(Det).D.indet
            type outdet = GEF.OutDetRank(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutJustMatrix(GVC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutJustMatrix(GVC_F)(Det).D.indet
            type outdet = GEF.OutJustMatrix(GVC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutJustMatrix(GVC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutJustMatrix(GVC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDet(GVC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDet(GVC_F)(Det).D.indet
            type outdet = GEF.OutDet(GVC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDet(GVC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDet(GVC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutRank(GVC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutRank(GVC_F)(Det).D.indet
            type outdet = GEF.OutRank(GVC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutRank(GVC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutRank(GVC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GVC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GVC_F)(Det).D.indet
            type outdet = GEF.OutDetRank(GVC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GVC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GVC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GVC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GVC_F)(Det).D.indet
            type outdet = GEF.OutDetRank(GVC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GVC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GVC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutJustMatrix(GAC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutJustMatrix(GAC_I)(Det).D.indet
            type outdet = GEF.OutJustMatrix(GAC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDet(GAC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutDet(GAC_I)(Det).D.indet
            type outdet = GEF.OutDet(GAC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDet(GAC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDet(GAC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutRank(GAC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutRank(GAC_I)(Det).D.indet
            type outdet = GEF.OutRank(GAC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutRank(GAC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutRank(GAC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GAC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GAC_I)(Det).D.indet
            type outdet = GEF.OutDetRank(GAC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GAC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GAC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutJustMatrix(GVC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutJustMatrix(GVC_I)(Det).D.indet
            type outdet = GEF.OutJustMatrix(GVC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutJustMatrix(GVC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutJustMatrix(GVC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDet(GVC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutDet(GVC_I)(Det).D.indet
            type outdet = GEF.OutDet(GVC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDet(GVC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDet(GVC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutRank(GVC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutRank(GVC_I)(Det).D.indet
            type outdet = GEF.OutRank(GVC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutRank(GVC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutRank(GVC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GVC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GVC_I)(Det).D.indet
            type outdet = GEF.OutDetRank(GVC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GVC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GVC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GVC_I).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GVC_I.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GVC_I)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GVC_I)(Det).D.indet
            type outdet = GEF.OutDetRank(GVC_I)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GVC_I)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GVC_I)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GVC_I.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GVC_I.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GVC_I.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutJustMatrix(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutJustMatrix(GAC_F)(Det).D.indet
            type outdet = GEF.OutJustMatrix(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDet(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDet(GAC_F)(Det).D.indet
            type outdet = GEF.OutDet(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDet(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDet(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutRank(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutRank(GAC_F)(Det).D.indet
            type outdet = GEF.OutRank(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutRank(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutRank(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GAC_F)(Det).D.indet
            type outdet = GEF.OutDetRank(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRankPivot(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRankPivot(GAC_F)(Det).D.indet
            type outdet = GEF.OutDetRankPivot(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRankPivot(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRankPivot(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_R).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_R.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutJustMatrix(GAC_R)(Det).res
        module D :
          sig
            type indet = GEF.OutJustMatrix(GAC_R)(Det).D.indet
            type outdet = GEF.OutJustMatrix(GAC_R)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_R)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_R)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_R.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_R.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_R).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_R.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDet(GAC_R)(Det).res
        module D :
          sig
            type indet = GEF.OutDet(GAC_R)(Det).D.indet
            type outdet = GEF.OutDet(GAC_R)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDet(GAC_R)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDet(GAC_R)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_R.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_R.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_R).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_R.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutRank(GAC_R)(Det).res
        module D :
          sig
            type indet = GEF.OutRank(GAC_R)(Det).D.indet
            type outdet = GEF.OutRank(GAC_R)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutRank(GAC_R)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutRank(GAC_R)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_R.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_R.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpJustMatrix(GAC_R).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_R.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GAC_R)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GAC_R)(Det).D.indet
            type outdet = GEF.OutDetRank(GAC_R)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GAC_R)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GAC_R)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_R.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_R.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_R.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpMatrixMargin(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutJustMatrix(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutJustMatrix(GAC_F)(Det).D.indet
            type outdet = GEF.OutJustMatrix(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutJustMatrix(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpMatrixMargin(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDet(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDet(GAC_F)(Det).D.indet
            type outdet = GEF.OutDet(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDet(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDet(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpMatrixMargin(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutRank(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutRank(GAC_F)(Det).D.indet
            type outdet = GEF.OutRank(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutRank(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutRank(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
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
          ('a, indet) Code.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) Code.abstract ->
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
          ('a in_val -> ('a, unit) Code.abstract) ->
          ('a, out_val ref) Code.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
        val update_det :
          'a in_val ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a in_val -> ('a, unit, 'b, 'c) GEF.cmonad) ->
          ('a, unit, 'b, 'c) GEF.cmonad
      end
    module Input :
      sig
        type inp = GEF.InpMatrixMargin(GAC_F).inp
        val get_input :
          ('a, inp) Code.abstract ->
          (('a, GAC_F.contr) Code.abstract * ('a, int) Code.abstract * bool,
           'b, ('a, 'c) Code.abstract)
          StateCPSMonad.monad
      end
    module Output :
      sig
        type res = GEF.OutDetRank(GAC_F)(Det).res
        module D :
          sig
            type indet = GEF.OutDetRank(GAC_F)(Det).D.indet
            type outdet = GEF.OutDetRank(GAC_F)(Det).D.outdet
            type tdet = outdet ref
            type 'a lstate = 'a GEF.OutDetRank(GAC_F)(Det).D.lstate
            type 'a tag_lstate = [ `TDet of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            type ('a, 'b) om = ('c, 'b, 'd, 'e) GEF.omonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val upd_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) om
            val zero_sign : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val acc :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) Code.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) Code.abstract
            type 'a tag_lstate = [ `TRan of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
            val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
          end
        module P :
          sig
            type 'a lstate = 'a GEF.OutDetRank(GAC_F)(Det).P.lstate
            type 'a tag_lstate = [ `TPivot of 'a lstate ]
            type ('a, 'b) lm = ('c, 'b, 'd, 'e) GEF.cmonad
              constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
            val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val add :
              ('a, Code.perm) Code.abstract ->
              (('a, unit) Code.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) Code.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, Code.perm list) Code.abstract, [> 'a tag_lstate ] list,
               'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) Code.abstract ->
          ('a, res,
           [> `TDet of 'a Det.lstate
            | `TPivot of 'a P.lstate
            | `TRan of 'a R.lstate ],
           'b)
          GEF.cmonad
      end
    module Pivot :
      sig
        val findpivot :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    module I :
      sig
        val row_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           ('a, GAC_F.Dom.v) Code.abstract ->
           'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
        val col_iter :
          'a GAC_F.vc ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          ('a, int) Code.abstract ->
          (('a, int) Code.abstract ->
           'a GAC_F.vo -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) Code.abstract) ->
          'b -> ('b -> ('a, unit) Code.abstract -> 'f) -> 'f
      end
    val gen :
      ('a, Input.inp) Code.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list -> ('a, Output.res) Code.abstract -> ('a, 'c) Code.abstract) ->
      ('a, 'c) Code.abstract
  end
val resFA1 : ('a, GenFA1.Input.inp -> GenFA1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_16 =
     begin
      for j_13 = t_8 to (t_7 - 1) do
       let t_14 = (t_5.(j_13)).(t_9) in
       if (t_14 <> 0.) then
        (match (! t_10) with
         | Some (i_15) ->
            if ((abs_float (snd i_15)) < (abs_float t_14)) then
             (t_10 := (Some (j_13, t_14)))
            else ()
         | None -> (t_10 := (Some (j_13, t_14))))
       else ()
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((fst i_11) <> t_8) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst i_11);
           t_5.(fst i_11) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_16 with
     | Some (i_17) ->
        begin
         for j_18 = (t_8 + 1) to (t_7 - 1) do
          let t_19 = (t_5.(j_18)).(t_9) in
          if (t_19 <> 0.) then begin
           for j_20 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_18)).(j_20) <-
             ((t_5.(j_18)).(j_20) -. ((t_19 /. i_17) *. (t_5.(t_8)).(j_20)))
           done;
           (t_5.(j_18)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resFA2 : ('a, GenFA2.Input.inp -> GenFA2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
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
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 := ((! t_8) *. i_19))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)))>.
val resFA3 : ('a, GenFA3.Input.inp -> GenFA3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_16 =
     begin
      for j_13 = t_8 to (t_7 - 1) do
       let t_14 = (t_5.(j_13)).(t_9) in
       if (t_14 <> 0.) then
        (match (! t_10) with
         | Some (i_15) ->
            if ((abs_float (snd i_15)) < (abs_float t_14)) then
             (t_10 := (Some (j_13, t_14)))
            else ()
         | None -> (t_10 := (Some (j_13, t_14))))
       else ()
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((fst i_11) <> t_8) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst i_11);
           t_5.(fst i_11) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_16 with
     | Some (i_17) ->
        begin
         for j_18 = (t_8 + 1) to (t_7 - 1) do
          let t_19 = (t_5.(j_18)).(t_9) in
          if (t_19 <> 0.) then begin
           for j_20 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_18)).(j_20) <-
             ((t_5.(j_18)).(j_20) -. ((t_19 /. i_17) *. (t_5.(t_8)).(j_20)))
           done;
           (t_5.(j_18)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resFA4 : ('a, GenFA4.Input.inp -> GenFA4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
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
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 := ((! t_8) *. i_19))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2))>.
val resFV1 : ('a, GenFV1.Input.inp -> GenFV1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (! t_2) in
    let t_8 = (! t_3) in
    let t_9 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_7 to (t_6 - 1) do
       let t_18 = (t_4.arr).((j_17 * t_4.m) + t_8) in
       if (t_18 <> 0.) then
        (match (! t_9) with
         | Some (i_19) ->
            if ((abs_float (snd i_19)) < (abs_float t_18)) then
             (t_9 := (Some (j_17, t_18)))
            else ()
         | None -> (t_9 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_9) with
       | Some (i_10) ->
          if ((fst i_10) <> t_7) then
           let a_11 = t_4.arr
           and m_12 = t_4.m in
           let i1_13 = (t_7 * m_12)
           and i2_14 = ((fst i_10) * m_12) in
           for i_15 = 0 to (m_12 - 1) do
            let t_16 = a_11.(i1_13 + i_15) in
            a_11.(i1_13 + i_15) <- a_11.(i2_14 + i_15);
            a_11.(i2_14 + i_15) <- t_16
           done
          else ();
          (Some (snd i_10))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_7 + 1) to (t_6 - 1) do
          let t_23 = (t_4.arr).((j_22 * t_4.m) + t_8) in
          if (t_23 <> 0.) then begin
           for j_24 = (t_8 + 1) to (t_5 - 1) do
            (t_4.arr).((j_22 * t_4.m) + j_24) <-
             ((t_4.arr).((j_22 * t_4.m) + j_24) -.
               ((t_23 /. i_21) *. (t_4.arr).((t_7 * t_4.m) + j_24)))
           done;
           (t_4.arr).((j_22 * t_4.m) + t_8) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
val resFV2 : ('a, GenFV2.Input.inp -> GenFV2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0.) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs_float (snd i_21)) < (abs_float t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0.) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((t_4.arr).((j_24 * t_4.m) + j_26) -.
               ((t_25 /. i_23) *. (t_4.arr).((t_9 * t_4.m) + j_26)))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_23))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)))>.
val resFV3 : ('a, GenFV3.Input.inp -> GenFV3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (! t_2) in
    let t_8 = (! t_3) in
    let t_9 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_7 to (t_6 - 1) do
       let t_18 = (t_4.arr).((j_17 * t_4.m) + t_8) in
       if (t_18 <> 0.) then
        (match (! t_9) with
         | Some (i_19) ->
            if ((abs_float (snd i_19)) < (abs_float t_18)) then
             (t_9 := (Some (j_17, t_18)))
            else ()
         | None -> (t_9 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_9) with
       | Some (i_10) ->
          if ((fst i_10) <> t_7) then
           let a_11 = t_4.arr
           and m_12 = t_4.m in
           let i1_13 = (t_7 * m_12)
           and i2_14 = ((fst i_10) * m_12) in
           for i_15 = 0 to (m_12 - 1) do
            let t_16 = a_11.(i1_13 + i_15) in
            a_11.(i1_13 + i_15) <- a_11.(i2_14 + i_15);
            a_11.(i2_14 + i_15) <- t_16
           done
          else ();
          (Some (snd i_10))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_7 + 1) to (t_6 - 1) do
          let t_23 = (t_4.arr).((j_22 * t_4.m) + t_8) in
          if (t_23 <> 0.) then begin
           for j_24 = (t_8 + 1) to (t_5 - 1) do
            (t_4.arr).((j_22 * t_4.m) + j_24) <-
             ((t_4.arr).((j_22 * t_4.m) + j_24) -.
               ((t_23 /. i_21) *. (t_4.arr).((t_7 * t_4.m) + j_24)))
           done;
           (t_4.arr).((j_22 * t_4.m) + t_8) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
val resFV4 : ('a, GenFV4.Input.inp -> GenFV4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0.) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs_float (snd i_21)) < (abs_float t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0.) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((t_4.arr).((j_24 * t_4.m) + j_26) -.
               ((t_25 /. i_23) *. (t_4.arr).((t_9 * t_4.m) + j_26)))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_23))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
val resFV5 : ('a, GenFV5.Input.inp -> GenFV5.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_30 =
     begin
      for j_26 = t_9 to (t_6 - 1) do
       for j_27 = t_10 to (t_5 - 1) do
        let t_28 = (t_4.arr).((j_26 * t_4.m) + j_27) in
        if (t_28 <> 0.) then
         (match (! t_11) with
          | Some (i_29) ->
             if ((abs_float (snd i_29)) < (abs_float t_28)) then
              (t_11 := (Some ((j_26, j_27), t_28)))
             else ()
          | None -> (t_11 := (Some ((j_26, j_27), t_28))))
        else ()
       done
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((snd (fst i_12)) <> t_10) then begin
           let a_19 = t_4.arr
           and nm_20 = (t_4.n * t_4.m)
           and m_21 = t_4.m in
           let rec loop_22 =
            fun i1_23 ->
             fun i2_24 ->
              if (i2_24 < nm_20) then
               let t_25 = a_19.(i1_23) in
               a_19.(i1_23) <- a_19.(i2_24);
               a_19.(i2_24) <- t_25;
               (loop_22 (i1_23 + m_21) (i2_24 + m_21))
              else () in
           (loop_22 t_10 (snd (fst i_12)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_12)) <> t_10) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((snd (fst i_12)) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_30 with
     | Some (i_31) ->
        begin
         for j_32 = (t_9 + 1) to (t_6 - 1) do
          let t_33 = (t_4.arr).((j_32 * t_4.m) + t_10) in
          if (t_33 <> 0.) then begin
           for j_34 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_32 * t_4.m) + j_34) <-
             ((t_4.arr).((j_32 * t_4.m) + j_34) -.
               ((t_33 /. i_31) *. (t_4.arr).((t_9 * t_4.m) + j_34)))
           done;
           (t_4.arr).((j_32 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_31))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
val resIA1 : ('a, GenIA1.Input.inp -> GenIA1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resIA2 : ('a, GenIA2.Input.inp -> GenIA2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0
    else if ((! t_9) = 1) then (! t_8)
    else (~- (! t_8)))>.
val resIA3 : ('a, GenIA3.Input.inp -> GenIA3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resIA4 : ('a, GenIA4.Input.inp -> GenIA4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0
    else if ((! t_9) = 1) then (! t_8)
    else (~- (! t_8)), (! t_2))>.
val resIV1 : ('a, GenIV1.Input.inp -> GenIV1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
val resIV2 : ('a, GenIV2.Input.inp -> GenIV2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)))>.
val resIV3 : ('a, GenIV3.Input.inp -> GenIV3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
val resIV4 : ('a, GenIV4.Input.inp -> GenIV4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
val resIV5 : ('a, GenIV5.Input.inp -> GenIV5.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_30 =
     begin
      for j_26 = t_9 to (t_6 - 1) do
       for j_27 = t_10 to (t_5 - 1) do
        let t_28 = (t_4.arr).((j_26 * t_4.m) + j_27) in
        if (t_28 <> 0) then
         (match (! t_11) with
          | Some (i_29) ->
             if ((abs (snd i_29)) > (abs t_28)) then
              (t_11 := (Some ((j_26, j_27), t_28)))
             else ()
          | None -> (t_11 := (Some ((j_26, j_27), t_28))))
        else ()
       done
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((snd (fst i_12)) <> t_10) then begin
           let a_19 = t_4.arr
           and nm_20 = (t_4.n * t_4.m)
           and m_21 = t_4.m in
           let rec loop_22 =
            fun i1_23 ->
             fun i2_24 ->
              if (i2_24 < nm_20) then
               let t_25 = a_19.(i1_23) in
               a_19.(i1_23) <- a_19.(i2_24);
               a_19.(i2_24) <- t_25;
               (loop_22 (i1_23 + m_21) (i2_24 + m_21))
              else () in
           (loop_22 t_10 (snd (fst i_12)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_12)) <> t_10) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((snd (fst i_12)) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_30 with
     | Some (i_31) ->
        begin
         for j_32 = (t_9 + 1) to (t_6 - 1) do
          let t_33 = (t_4.arr).((j_32 * t_4.m) + t_10) in
          if (t_33 <> 0) then begin
           for j_34 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_32 * t_4.m) + j_34) <-
             ((((t_4.arr).((j_32 * t_4.m) + j_34) * i_31) -
                ((t_4.arr).((t_9 * t_4.m) + j_34) * t_33)) / (! t_7))
           done;
           (t_4.arr).((j_32 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_31)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
val resFA11 : ('a, GenFA11.Input.inp -> GenFA11.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_19 =
     begin
      for j_15 = t_8 to (t_7 - 1) do
       for j_16 = t_9 to (t_6 - 1) do
        let t_17 = (t_5.(j_15)).(j_16) in
        if (t_17 <> 0.) then
         (match (! t_10) with
          | Some (i_18) ->
             if ((abs_float (snd i_18)) < (abs_float t_17)) then
              (t_10 := (Some ((j_15, j_16), t_17)))
             else ()
          | None -> (t_10 := (Some ((j_15, j_16), t_17))))
        else ()
       done
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((snd (fst i_11)) <> t_9) then
           for r_13 = 0 to ((Array.length t_5) - 1) do
            let t_14 = (t_5.(r_13)).(t_9) in
            (t_5.(r_13)).(t_9) <- (t_5.(r_13)).(snd (fst i_11));
            (t_5.(r_13)).(snd (fst i_11)) <- t_14
           done
          else ();
          if ((fst (fst i_11)) <> t_9) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(snd (fst i_11));
           t_5.(snd (fst i_11)) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_8 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_9) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_8)).(j_23)))
           done;
           (t_5.(j_21)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resFA12 : ('a, GenFA12.Input.inp -> GenFA12.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
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
          if ((snd (fst i_13)) <> t_11) then begin
           for r_15 = 0 to ((Array.length t_5) - 1) do
            let t_16 = (t_5.(r_15)).(t_11) in
            (t_5.(r_15)).(t_11) <- (t_5.(r_15)).(snd (fst i_13));
            (t_5.(r_15)).(snd (fst i_13)) <- t_16
           done;
           (t_9 := (~- (! t_9)))
          end else ();
          if ((fst (fst i_13)) <> t_11) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(snd (fst i_13));
           t_5.(snd (fst i_13)) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 := ((! t_8) *. i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)))>.
val resFA13 : ('a, GenFA13.Input.inp -> GenFA13.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_19 =
     begin
      for j_15 = t_8 to (t_7 - 1) do
       for j_16 = t_9 to (t_6 - 1) do
        let t_17 = (t_5.(j_15)).(j_16) in
        if (t_17 <> 0.) then
         (match (! t_10) with
          | Some (i_18) ->
             if ((abs_float (snd i_18)) < (abs_float t_17)) then
              (t_10 := (Some ((j_15, j_16), t_17)))
             else ()
          | None -> (t_10 := (Some ((j_15, j_16), t_17))))
        else ()
       done
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((snd (fst i_11)) <> t_9) then
           for r_13 = 0 to ((Array.length t_5) - 1) do
            let t_14 = (t_5.(r_13)).(t_9) in
            (t_5.(r_13)).(t_9) <- (t_5.(r_13)).(snd (fst i_11));
            (t_5.(r_13)).(snd (fst i_11)) <- t_14
           done
          else ();
          if ((fst (fst i_11)) <> t_9) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(snd (fst i_11));
           t_5.(snd (fst i_11)) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_8 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_9) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_8)).(j_23)))
           done;
           (t_5.(j_21)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resFA14 : ('a, GenFA14.Input.inp -> GenFA14.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
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
          if ((snd (fst i_13)) <> t_11) then begin
           for r_15 = 0 to ((Array.length t_5) - 1) do
            let t_16 = (t_5.(r_15)).(t_11) in
            (t_5.(r_15)).(t_11) <- (t_5.(r_15)).(snd (fst i_13));
            (t_5.(r_15)).(snd (fst i_13)) <- t_16
           done;
           (t_9 := (~- (! t_9)))
          end else ();
          if ((fst (fst i_13)) <> t_11) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(snd (fst i_13));
           t_5.(snd (fst i_13)) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 := ((! t_8) *. i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2))>.
val resFA24 : ('a, GenFA24.Input.inp -> GenFA24.Output.res) code =
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
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_19 =
     begin
      for j_16 = t_11 to (t_7 - 1) do
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
          if ((fst i_14) <> t_11) then begin
           begin
            let t_15 = t_5.(t_11) in
            t_5.(t_11) <- t_5.(fst i_14);
            t_5.(fst i_14) <- t_15;
            (t_9 := (~- (! t_9)))
           end;
           (t_10 := ((RowSwap ((fst i_14), t_11)) :: (! t_10)))
          end else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_11 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_12) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_12 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_11)).(j_23)))
           done;
           (t_5.(j_21)).(t_12) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2), (! t_10))>.
val resRA1 : ('a, GenRA1.Input.inp -> GenRA1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
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
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resRA2 : ('a, GenRA2.Input.inp -> GenRA2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
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
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
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
val resRA3 : ('a, GenRA3.Input.inp -> GenRA3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
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
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resRA4 : ('a, GenRA4.Input.inp -> GenRA4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
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
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
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
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
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
val resFA5 : ('a, GenFA5.Input.inp -> GenFA5.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_8 - 1) do
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
          if ((fst i_12) <> t_9) then
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13
          else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_8 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then begin
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done;
           (t_5.(j_19)).(t_10) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
val resFA6 : ('a, GenFA6.Input.inp -> GenFA6.Output.res) code =
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
          if ((fst i_14) <> t_11) then begin
           let t_15 = t_5.(t_11) in
           t_5.(t_11) <- t_5.(fst i_14);
           t_5.(fst i_14) <- t_15;
           (t_10 := (~- (! t_10)))
          end else ();
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
         (t_9 := ((! t_9) *. i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_10 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_10) = 0) then 0.
    else if ((! t_10) = 1) then (! t_9)
    else (~-. (! t_9)))>.
val resFA7 : ('a, GenFA7.Input.inp -> GenFA7.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_8 - 1) do
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
          if ((fst i_12) <> t_9) then
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13
          else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_8 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then begin
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done;
           (t_5.(j_19)).(t_10) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
val resFA8 : ('a, GenFA8.Input.inp -> GenFA8.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_8 - 1) do
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
          if ((fst i_12) <> t_9) then
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13
          else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_8 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then begin
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done;
           (t_5.(j_19)).(t_10) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (), (! t_2))>.
val rFA1 : GenFA1.Input.inp -> GenFA1.Output.res = <fun>
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
val rIA2 : GAC_I.Dom.v array array -> GAC_I.contr * int = <fun>
val rIA3 : GAC_I.Dom.v array array -> GAC_I.contr * int = <fun>
val rIA4 : GAC_I.Dom.v array array -> GAC_I.contr * int * int = <fun>
val rIV1 :
  GVC_I.Dom.v Domains_code.container2dfromvector ->
  GVC_I.Dom.v Domains_code.container2dfromvector = <fun>
val rIV2 :
  GVC_I.Dom.v Domains_code.container2dfromvector -> GVC_I.contr * int = <fun>
val rIV3 :
  GVC_I.Dom.v Domains_code.container2dfromvector -> GVC_I.contr * int = <fun>
val rIV4 :
  GVC_I.Dom.v Domains_code.container2dfromvector -> GVC_I.contr * int * int =
  <fun>
val rIV5 :
  GVC_I.Dom.v Domains_code.container2dfromvector -> GVC_I.contr * int * int =
  <fun>
val rFA11 : GAC_F.Dom.v array array -> GAC_F.Dom.v array array = <fun>
val rFA12 : GAC_F.Dom.v array array -> GAC_F.contr * float = <fun>
val rFA13 : GAC_F.Dom.v array array -> GAC_F.contr * int = <fun>
val rFA14 : GAC_F.Dom.v array array -> GAC_F.contr * float * int = <fun>
val rFA24 :
  GAC_F.Dom.v array array -> GAC_F.contr * float * int * Code.perm list =
  <fun>
val rRA1 : GAC_R.Dom.v array array -> GAC_R.Dom.v array array = <fun>
val rRA2 : GAC_R.Dom.v array array -> GAC_R.contr * Num.num = <fun>
val rRA3 : GAC_R.Dom.v array array -> GAC_R.contr * int = <fun>
val rRA4 : GAC_R.Dom.v array array -> GAC_R.contr * Num.num * int = <fun>
val rFA5 : GAC_F.contr * int -> GAC_F.Dom.v array array = <fun>
val rFA6 : GAC_F.contr * int -> GAC_F.contr * float = <fun>
val rFA7 : GAC_F.contr * int -> GAC_F.contr * int = <fun>
val rFA8 : GAC_F.contr * int -> GAC_F.contr * unit * int = <fun>
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
val resI12 : (GAC_I.contr * int) list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0)]
val resI13 : (GAC_I.contr * int) list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 2)]
val resI14 : (GAC_I.contr * int * int) list =
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
val resI21 : GVC_I.Dom.v Domains_code.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}]
val resI22 : (GVC_I.contr * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0)]
val resI23 : (GVC_I.contr * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 2)]
val resI24 : (GVC_I.contr * int * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
val resI25 : (GVC_I.contr * int * int) list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
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
val resF1 : GenFA1.Output.res list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
- : unit = ()
- : unit = ()
- : unit = ()
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
val resF11 : float array array list =
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
val resF14 : (GAC_F.contr * float * int) list =
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
val resF24 : (GAC_F.contr * float * int * Code.perm list) list =
  [([|[|1.|]|], 1., 1, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [Code.RowSwap (2, 1); Code.RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2,
    [Code.RowSwap (1, 0)])]
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
val resR12 : (GAC_R.contr * Num.num) list =
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
val resR14 : (GAC_R.contr * Num.num * int) list =
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