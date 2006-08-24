        Objective Caml version 3.09.1

#                                                                                                                                           * * * * * * *                                                                                                                                                                                                                                                                                 module CC : sig type ('a, 'b) abstract = unit -> 'b end
module GEF :
  sig
    module Infra :
      sig
        type ('a, 'b) rep = ('a, 'b) CC.abstract
        module TheCode :
          functor (AR : Abstractrep.T) ->
            sig
              type ('a, 'b) abstract = ('a, 'b) CC.abstract
              val retN :
                ('a, 'b) abstract ->
                'c ->
                ('c -> ('a, 'b) abstract -> ('a, 'd) abstract) ->
                ('a, 'd) abstract
              val seq :
                ('a, 'b) abstract -> ('a, 'c) abstract -> ('a, 'c) abstract
              val seqL :
                ('a, 'b) abstract ->
                ('a, 'c) abstract ->
                'd -> ('d -> ('a, 'c) abstract -> 'e) -> 'e
              val seqM :
                ('a -> ('b -> 'c -> 'c) -> ('d, 'e) abstract) ->
                ('a -> ('f -> 'g -> 'g) -> ('d, 'h) abstract) ->
                'a -> ('a -> ('d, 'h) abstract -> 'i) -> 'i
              val optSeq :
                ('a, 'b) abstract ->
                ('a, 'b) abstract option -> ('a, 'b) abstract
              val ifM :
                ('a, bool) abstract ->
                ('b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
                ('b -> ('f -> 'g -> 'g) -> ('a, 'e) abstract) ->
                'b -> ('b -> ('a, 'e) abstract -> 'h) -> 'h
              val rshiftM : ('a -> 'b) -> 'a -> ('a -> 'b -> 'c) -> 'c
              val whenM :
                ('a, bool) abstract ->
                ('b -> ('c -> 'd -> 'd) -> ('a, unit) abstract) ->
                'b -> ('b -> ('a, unit) abstract -> 'e) -> 'e
              val loopM :
                ('a, int) abstract ->
                ('a, int) abstract ->
                (('a, int) abstract ->
                 'b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
                'b -> ('b -> ('a, unit) abstract -> 'f) -> 'f
              val whileM :
                ('a, bool) abstract ->
                ('b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
                'b -> ('b -> ('a, unit) abstract -> 'f) -> 'f
              val matchM :
                ('a, 'b option) abstract ->
                (('a, 'b) abstract ->
                 'c -> ('d -> 'e -> 'e) -> ('a, 'f) abstract) ->
                ('c -> ('g -> 'h -> 'h) -> ('a, 'f) abstract) ->
                'c -> ('c -> ('a, 'f) abstract -> 'i) -> 'i
              val genrecloop :
                (('a, 'b -> 'c) abstract ->
                 ('a, 'b) abstract ->
                 'd -> ('e -> 'f -> 'f) -> ('a, 'c) abstract) ->
                ('a, 'b) abstract ->
                'd -> ('d -> ('a, 'c) abstract -> 'g) -> 'g
              val lift : 'a -> ('b, 'a) abstract
              val liftRef : ('a, 'b) abstract -> ('a, 'b ref) abstract
              val liftGet : ('a, 'b ref) abstract -> ('a, 'b) abstract
              val unitL : 'a -> ('a -> ('b, unit) abstract -> 'c) -> 'c
              val liftPair :
                ('a, 'b * 'c) abstract ->
                ('a, 'b) abstract * ('a, 'c) abstract
              val liftPPair :
                ('a, ('b * 'c) * 'd) abstract ->
                ('a, 'b) abstract * ('a, 'c) abstract * ('a, 'd) abstract
              module Logic :
                sig
                  val notL : ('a, bool) abstract -> ('a, bool) abstract
                  val equalL :
                    ('a, 'b) abstract ->
                    ('a, 'b) abstract -> ('a, bool) abstract
                  val notequalL :
                    ('a, 'b) abstract ->
                    ('a, 'b) abstract -> ('a, bool) abstract
                  val andL :
                    ('a, bool) abstract ->
                    ('a, bool) abstract -> ('a, bool) abstract
                end
              module Idx :
                sig
                  val zero : ('a, int) abstract
                  val one : ('a, int) abstract
                  val minusone : ('a, int) abstract
                  val succ : ('a, int) abstract -> ('a, int) abstract
                  val pred : ('a, int) abstract -> ('a, int) abstract
                  val less :
                    ('a, 'b) abstract ->
                    ('a, 'b) abstract -> ('a, bool) abstract
                  val uminus : ('a, int) abstract -> ('a, int) abstract
                  val minusoneL :
                    'a -> ('a -> ('b, int) abstract -> 'c) -> 'c
                end
              module Maybe :
                sig
                  val just : ('a, 'b) abstract -> ('a, 'b option) abstract
                  val none : ('a, 'b option) abstract
                end
              val applyMaybe : ('a -> 'a) option -> 'a -> 'a
              module Tuple :
                sig
                  val tup2 :
                    ('a, 'b) abstract ->
                    ('a, 'c) abstract -> ('a, 'b * 'c) abstract
                  val tup3 :
                    ('a, 'b) abstract ->
                    ('a, 'c) abstract ->
                    ('a, 'd) abstract -> ('a, 'b * 'c * 'd) abstract
                  val tup4 :
                    ('a, 'b) abstract ->
                    ('a, 'c) abstract ->
                    ('a, 'd) abstract ->
                    ('a, 'e) abstract -> ('a, 'b * 'c * 'd * 'e) abstract
                end
              module CList :
                sig
                  val nil : ('a, 'b list) abstract
                  val cons :
                    ('a, 'b) abstract ->
                    ('a, 'b list) abstract -> ('a, 'b list) abstract
                end
              val cunit : ('a, unit) abstract
              val update :
                ('a, 'b ref) abstract ->
                (('a, 'b) abstract -> ('a, 'b) abstract) ->
                ('a, unit) abstract
              val assign :
                ('a, 'b ref) abstract ->
                ('a, 'b) abstract -> ('a, unit) abstract
              val apply :
                ('a, 'b -> 'c) abstract ->
                ('a, 'b) abstract -> ('a, 'c) abstract
              val updateM :
                ('a, 'b ref) abstract ->
                (('a, 'b) abstract -> ('a, 'b) abstract) ->
                'c -> ('c -> ('a, unit) abstract -> 'd) -> 'd
              val assignM :
                ('a, 'b ref) abstract ->
                ('a, 'b) abstract ->
                'c -> ('c -> ('a, unit) abstract -> 'd) -> 'd
              val applyM :
                ('a, 'b -> 'c) abstract ->
                ('a, 'b) abstract ->
                'd -> ('d -> ('a, 'c) abstract -> 'e) -> 'e
              module Transformers :
                sig
                  val full_unroll :
                    int ->
                    int ->
                    (int -> ('a, unit) abstract) -> ('a, unit) abstract
                end
            end
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
            type 'a vc = ('a, v) rep
            val zeroL : 'a vc
            val oneL : 'a vc
            val ( +^ ) : 'a vc -> 'a vc -> 'a vc
            val ( *^ ) : 'a vc -> 'a vc -> 'a vc
            val ( -^ ) : 'a vc -> 'a vc -> 'a vc
            val uminusL : 'a vc -> 'a vc
            val divL : 'a vc -> 'a vc -> 'a vc
            val better_thanL : ('a vc -> 'a vc -> ('a, bool) rep) option
            val normalizerL : ('a vc -> 'a vc) option
          end
        type domain_is_field = Infra_direct.Make(CC).domain_is_field
        type domain_is_ring = Infra_direct.Make(CC).domain_is_ring
        module FloatDomain :
          sig
            type v = float
            type kind = domain_is_field
            val zero : float
            val one : float
            val plus : float -> float -> float
            val times : float -> float -> float
            val minus : float -> float -> float
            val uminus : float -> float
            val div : float -> float -> float
            val normalizer : 'a option
            val better_than : (float -> float -> bool) option
          end
        module FloatDomainL :
          sig
            type v = float
            type kind = domain_is_field
            val zero : float
            val one : float
            val plus : float -> float -> float
            val times : float -> float -> float
            val minus : float -> float -> float
            val uminus : float -> float
            val div : float -> float -> float
            val normalizer : 'a option
            val better_than : (float -> float -> bool) option
            type 'a vc = ('a, v) rep
            val zeroL : ('a, float) rep
            val oneL : ('a, float) rep
            val ( +^ ) :
              ('a, float) rep -> ('a, float) rep -> ('a, float) rep
            val ( *^ ) :
              ('a, float) rep -> ('a, float) rep -> ('a, float) rep
            val ( -^ ) :
              ('a, float) rep -> ('a, float) rep -> ('a, float) rep
            val uminusL : ('a, float) rep -> ('a, float) rep
            val divL : ('a, float) rep -> ('a, float) rep -> ('a, float) rep
            val normalizerL : 'a option
            val better_thanL :
              (('a, float) rep -> ('a, float) rep -> ('a, bool) rep) option
          end
        module IntegerDomain :
          sig
            type v = int
            type kind = domain_is_ring
            val zero : int
            val one : int
            val plus : int -> int -> int
            val times : int -> int -> int
            val minus : int -> int -> int
            val uminus : int -> int
            val div : int -> int -> int
            val normalizer : 'a option
            val better_than : (int -> int -> bool) option
          end
        module IntegerDomainL :
          sig
            type v = int
            type kind = domain_is_ring
            val zero : int
            val one : int
            val plus : int -> int -> int
            val times : int -> int -> int
            val minus : int -> int -> int
            val uminus : int -> int
            val div : int -> int -> int
            val normalizer : 'a option
            val better_than : (int -> int -> bool) option
            type 'a vc = ('a, v) rep
            val zeroL : ('a, int) rep
            val oneL : ('a, int) rep
            val ( +^ ) : ('a, int) rep -> ('a, int) rep -> ('a, int) rep
            val ( *^ ) : ('a, int) rep -> ('a, int) rep -> ('a, int) rep
            val ( -^ ) : ('a, int) rep -> ('a, int) rep -> ('a, int) rep
            val uminusL : ('a, int) rep -> ('a, int) rep
            val divL : ('a, int) rep -> ('a, int) rep -> ('a, int) rep
            val normalizerL : 'a option
            val better_thanL :
              (('a, int) rep -> ('a, int) rep -> ('a, bool) rep) option
          end
        module RationalDomain :
          sig
            type v = Num.num
            type kind = domain_is_field
            val zero : Num.num
            val one : Num.num
            val plus : Num.num -> Num.num -> Num.num
            val times : Num.num -> Num.num -> Num.num
            val minus : Num.num -> Num.num -> Num.num
            val uminus : Num.num -> Num.num
            val div : Num.num -> Num.num -> Num.num
            val normalizer : 'a option
            val better_than : 'a option
          end
        module RationalDomainL :
          sig
            type v = Num.num
            type kind = domain_is_field
            val zero : Num.num
            val one : Num.num
            val plus : Num.num -> Num.num -> Num.num
            val times : Num.num -> Num.num -> Num.num
            val minus : Num.num -> Num.num -> Num.num
            val uminus : Num.num -> Num.num
            val div : Num.num -> Num.num -> Num.num
            val normalizer : 'a option
            val better_than : 'a option
            type 'a vc = ('a, v) rep
            val zeroL : ('a, Num.num) rep
            val oneL : ('a, Num.num) rep
            val ( +^ ) :
              ('a, Num.num) rep -> ('a, Num.num) rep -> ('a, Num.num) rep
            val ( *^ ) :
              ('a, Num.num) rep -> ('a, Num.num) rep -> ('a, Num.num) rep
            val ( -^ ) :
              ('a, Num.num) rep -> ('a, Num.num) rep -> ('a, Num.num) rep
            val uminusL : ('a, Num.num) rep -> ('a, Num.num) rep
            val divL :
              ('a, Num.num) rep -> ('a, Num.num) rep -> ('a, Num.num) rep
            val normalizerL : 'a option
            val better_thanL : 'a option
          end
        module type CONTAINER2D =
          sig
            module Dom : DOMAINL
            type contr
            type 'a vc = ('a, contr) rep
            type 'a vo = ('a, Dom.v) rep
            val getL : 'a vc -> ('a, int) rep -> ('a, int) rep -> 'a vo
            val dim1 : 'a vc -> ('a, int) rep
            val dim2 : 'a vc -> ('a, int) rep
            val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
            val copy : 'a vc -> 'a vc
            val swap_rows_stmt :
              'a vc -> ('a, int) rep -> ('a, int) rep -> ('a, unit) rep
            val swap_cols_stmt :
              'a vc -> ('a, int) rep -> ('a, int) rep -> ('a, unit) rep
            val row_head : 'a vc -> ('a, int) rep -> ('a, int) rep -> 'a vo
            val row_iter :
              'a vc ->
              ('a, int) rep ->
              ('a, int) rep ->
              ('a, int) rep ->
              (('a, int) rep ->
               'a vo -> 'b -> ('b -> 'c -> 'c) -> ('a, 'd) rep) ->
              (('a, unit) rep, 'b, 'e) StateCPSMonad.monad
            val col_head_set :
              'a vc ->
              ('a, int) rep -> ('a, int) rep -> 'a vo -> ('a, unit) rep
            val col_iter :
              'a vc ->
              ('a, int) rep ->
              ('a, int) rep ->
              ('a, int) rep ->
              (('a, int) rep ->
               'a vo -> 'b -> ('b -> 'c -> 'c) -> ('a, 'd) rep) ->
              (('a, unit) rep, 'b, 'e) StateCPSMonad.monad
          end
        module GenericArrayContainer :
          functor (Dom : DOMAINL) ->
            sig
              module Dom :
                sig
                  type v = Dom.v
                  type kind = Dom.kind
                  val zero : v
                  val one : v
                  val plus : v -> v -> v
                  val times : v -> v -> v
                  val minus : v -> v -> v
                  val uminus : v -> v
                  val div : v -> v -> v
                  val better_than : (v -> v -> bool) option
                  val normalizer : (v -> v) option
                  type 'a vc = ('a, v) rep
                  val zeroL : 'a vc
                  val oneL : 'a vc
                  val ( +^ ) : 'a vc -> 'a vc -> 'a vc
                  val ( *^ ) : 'a vc -> 'a vc -> 'a vc
                  val ( -^ ) : 'a vc -> 'a vc -> 'a vc
                  val uminusL : 'a vc -> 'a vc
                  val divL : 'a vc -> 'a vc -> 'a vc
                  val better_thanL :
                    ('a vc -> 'a vc -> ('a, bool) rep) option
                  val normalizerL : ('a vc -> 'a vc) option
                end
              type contr = Dom.v array array
              type 'a vc = ('a, contr) rep
              type 'a vo = ('a, Dom.v) rep
              val getL :
                ('a, 'b array array) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, 'b) rep
              val dim2 : ('a, 'b array) rep -> ('a, int) rep
              val dim1 : ('a, 'b array array) rep -> ('a, int) rep
              val mapper :
                ('a vo -> 'a vo) option ->
                ('a, Dom.v array array) rep -> ('a, Dom.v array array) rep
              val copy : ('a, 'b array array) rep -> ('a, 'b array array) rep
              val swap_rows_stmt :
                ('a, 'b array) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, unit) rep
              val swap_cols_stmt :
                ('a, 'b array array) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, unit) rep
              val row_head :
                ('a, 'b array array) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, 'b) rep
              val row_iter :
                ('a, 'b array array) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                (('a, int) rep ->
                 ('a, 'b) rep -> 'c -> ('d -> 'e -> 'e) -> ('a, 'f) rep) ->
                'c -> ('c -> ('a, unit) rep -> 'g) -> 'g
              val col_head_set :
                ('a, 'b array array) rep ->
                ('a, int) rep ->
                ('a, int) rep -> ('a, 'b) rep -> ('a, unit) rep
              val col_iter :
                ('a, 'b array array) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                (('a, int) rep ->
                 ('a, 'b) rep -> 'c -> ('d -> 'e -> 'e) -> ('a, 'f) rep) ->
                'c -> ('c -> ('a, unit) rep -> 'g) -> 'g
            end
        type 'a container2dfromvector =
          'a Infra_direct.Make(CC).container2dfromvector = {
          arr : 'a array;
          n : int;
          m : int;
        }
        module GenericVectorContainer :
          functor (Dom : DOMAINL) ->
            sig
              module Dom :
                sig
                  type v = Dom.v
                  type kind = Dom.kind
                  val zero : v
                  val one : v
                  val plus : v -> v -> v
                  val times : v -> v -> v
                  val minus : v -> v -> v
                  val uminus : v -> v
                  val div : v -> v -> v
                  val better_than : (v -> v -> bool) option
                  val normalizer : (v -> v) option
                  type 'a vc = ('a, v) rep
                  val zeroL : 'a vc
                  val oneL : 'a vc
                  val ( +^ ) : 'a vc -> 'a vc -> 'a vc
                  val ( *^ ) : 'a vc -> 'a vc -> 'a vc
                  val ( -^ ) : 'a vc -> 'a vc -> 'a vc
                  val uminusL : 'a vc -> 'a vc
                  val divL : 'a vc -> 'a vc -> 'a vc
                  val better_thanL :
                    ('a vc -> 'a vc -> ('a, bool) rep) option
                  val normalizerL : ('a vc -> 'a vc) option
                end
              type contr = Dom.v container2dfromvector
              type 'a vc = ('a, contr) rep
              type 'a vo = ('a, Dom.v) rep
              val getL :
                ('a, 'b container2dfromvector) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, 'b) rep
              val dim2 : ('a, 'b container2dfromvector) rep -> ('a, int) rep
              val dim1 : ('a, 'b container2dfromvector) rep -> ('a, int) rep
              val mapper :
                (('a, 'b) rep -> ('a, 'b) rep) option ->
                ('a, 'b container2dfromvector) rep ->
                ('a, 'b container2dfromvector) rep
              val copy :
                ('a, 'b container2dfromvector) rep ->
                ('a, 'b container2dfromvector) rep
              val swap_rows_stmt :
                ('a, 'b container2dfromvector) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, unit) rep
              val swap_cols_stmt :
                ('a, 'b container2dfromvector) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, unit) rep
              val row_head :
                ('a, 'b container2dfromvector) rep ->
                ('a, int) rep -> ('a, int) rep -> ('a, 'b) rep
              val row_iter :
                ('a, 'b container2dfromvector) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                (('a, int) rep ->
                 ('a, 'b) rep -> 'c -> ('d -> 'e -> 'e) -> ('a, 'f) rep) ->
                'c -> ('c -> ('a, unit) rep -> 'g) -> 'g
              val col_head_set :
                ('a, 'b container2dfromvector) rep ->
                ('a, int) rep ->
                ('a, int) rep -> ('a, 'b) rep -> ('a, unit) rep
              val col_iter :
                ('a, 'b container2dfromvector) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                ('a, int) rep ->
                (('a, int) rep ->
                 ('a, 'b) rep -> 'c -> ('d -> 'e -> 'e) -> ('a, 'f) rep) ->
                'c -> ('c -> ('a, unit) rep -> 'g) -> 'g
            end
        type 'a svect = (int * 'a) list
        type 'a container2dsparse =
          'a Infra_direct.Make(CC).container2dsparse = {
          sarr : 'a svect array;
          mm : int;
        }
        module Array1D :
          sig
            val getL : 'a array -> ('b, int) rep -> ('b, 'a) rep
            val setL :
              'a array -> ('b, int) rep -> ('b, 'a) rep -> ('b, unit) rep
            val dim1 : 'a array -> ('b, int) rep
            val mapper :
              ('a, 'b -> 'b) rep option ->
              ('a, 'b array) rep -> ('a, 'b array) rep
          end
        module CArray1D :
          sig
            val getL : ('a, 'b array) rep -> int -> ('a, 'b) rep
            val setL :
              ('a, 'b array) rep -> int -> ('a, 'b) rep -> ('a, unit) rep
            val dim1 : ('a, 'b array) rep -> ('a, int) rep
          end
        module Array2D :
          sig
            val getL :
              'a array array ->
              ('b, int) rep -> ('b, int) rep -> ('b, 'a) rep
            val setL :
              'a array array ->
              ('b, int) rep ->
              ('b, int) rep -> ('b, 'a) rep -> ('b, unit) rep
            val dim2 : 'a array -> ('b, int) rep
            val dim1 : 'a array array -> ('b, int) rep
          end
        type perm =
          Infra_direct.Make(CC).perm =
            RowSwap of (int * int)
          | ColSwap of (int * int)
        val liftRowSwap : ('a, int) rep -> ('a, int) rep -> ('a, perm) rep
        val liftColSwap : ('a, int) rep -> ('a, int) rep -> ('a, perm) rep
      end
    module TC :
      sig
        type ('a, 'b) abstract = ('a, 'b) CC.abstract
        val retN :
          ('a, 'b) abstract ->
          'c ->
          ('c -> ('a, 'b) abstract -> ('a, 'd) abstract) -> ('a, 'd) abstract
        val seq : ('a, 'b) abstract -> ('a, 'c) abstract -> ('a, 'c) abstract
        val seqL :
          ('a, 'b) abstract ->
          ('a, 'c) abstract -> 'd -> ('d -> ('a, 'c) abstract -> 'e) -> 'e
        val seqM :
          ('a -> ('b -> 'c -> 'c) -> ('d, 'e) abstract) ->
          ('a -> ('f -> 'g -> 'g) -> ('d, 'h) abstract) ->
          'a -> ('a -> ('d, 'h) abstract -> 'i) -> 'i
        val optSeq :
          ('a, 'b) abstract -> ('a, 'b) abstract option -> ('a, 'b) abstract
        val ifM :
          ('a, bool) abstract ->
          ('b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
          ('b -> ('f -> 'g -> 'g) -> ('a, 'e) abstract) ->
          'b -> ('b -> ('a, 'e) abstract -> 'h) -> 'h
        val rshiftM : ('a -> 'b) -> 'a -> ('a -> 'b -> 'c) -> 'c
        val whenM :
          ('a, bool) abstract ->
          ('b -> ('c -> 'd -> 'd) -> ('a, unit) abstract) ->
          'b -> ('b -> ('a, unit) abstract -> 'e) -> 'e
        val loopM :
          ('a, int) abstract ->
          ('a, int) abstract ->
          (('a, int) abstract -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
          'b -> ('b -> ('a, unit) abstract -> 'f) -> 'f
        val whileM :
          ('a, bool) abstract ->
          ('b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
          'b -> ('b -> ('a, unit) abstract -> 'f) -> 'f
        val matchM :
          ('a, 'b option) abstract ->
          (('a, 'b) abstract -> 'c -> ('d -> 'e -> 'e) -> ('a, 'f) abstract) ->
          ('c -> ('g -> 'h -> 'h) -> ('a, 'f) abstract) ->
          'c -> ('c -> ('a, 'f) abstract -> 'i) -> 'i
        val genrecloop :
          (('a, 'b -> 'c) abstract ->
           ('a, 'b) abstract -> 'd -> ('e -> 'f -> 'f) -> ('a, 'c) abstract) ->
          ('a, 'b) abstract -> 'd -> ('d -> ('a, 'c) abstract -> 'g) -> 'g
        val lift : 'a -> ('b, 'a) abstract
        val liftRef : ('a, 'b) abstract -> ('a, 'b ref) abstract
        val liftGet : ('a, 'b ref) abstract -> ('a, 'b) abstract
        val unitL : 'a -> ('a -> ('b, unit) abstract -> 'c) -> 'c
        val liftPair :
          ('a, 'b * 'c) abstract -> ('a, 'b) abstract * ('a, 'c) abstract
        val liftPPair :
          ('a, ('b * 'c) * 'd) abstract ->
          ('a, 'b) abstract * ('a, 'c) abstract * ('a, 'd) abstract
        module Logic :
          sig
            val notL : ('a, bool) abstract -> ('a, bool) abstract
            val equalL :
              ('a, 'b) abstract -> ('a, 'b) abstract -> ('a, bool) abstract
            val notequalL :
              ('a, 'b) abstract -> ('a, 'b) abstract -> ('a, bool) abstract
            val andL :
              ('a, bool) abstract ->
              ('a, bool) abstract -> ('a, bool) abstract
          end
        module Idx :
          sig
            val zero : ('a, int) abstract
            val one : ('a, int) abstract
            val minusone : ('a, int) abstract
            val succ : ('a, int) abstract -> ('a, int) abstract
            val pred : ('a, int) abstract -> ('a, int) abstract
            val less :
              ('a, 'b) abstract -> ('a, 'b) abstract -> ('a, bool) abstract
            val uminus : ('a, int) abstract -> ('a, int) abstract
            val minusoneL : 'a -> ('a -> ('b, int) abstract -> 'c) -> 'c
          end
        module Maybe :
          sig
            val just : ('a, 'b) abstract -> ('a, 'b option) abstract
            val none : ('a, 'b option) abstract
          end
        val applyMaybe : ('a -> 'a) option -> 'a -> 'a
        module Tuple :
          sig
            val tup2 :
              ('a, 'b) abstract ->
              ('a, 'c) abstract -> ('a, 'b * 'c) abstract
            val tup3 :
              ('a, 'b) abstract ->
              ('a, 'c) abstract ->
              ('a, 'd) abstract -> ('a, 'b * 'c * 'd) abstract
            val tup4 :
              ('a, 'b) abstract ->
              ('a, 'c) abstract ->
              ('a, 'd) abstract ->
              ('a, 'e) abstract -> ('a, 'b * 'c * 'd * 'e) abstract
          end
        module CList :
          sig
            val nil : ('a, 'b list) abstract
            val cons :
              ('a, 'b) abstract ->
              ('a, 'b list) abstract -> ('a, 'b list) abstract
          end
        val cunit : ('a, unit) abstract
        val update :
          ('a, 'b ref) abstract ->
          (('a, 'b) abstract -> ('a, 'b) abstract) -> ('a, unit) abstract
        val assign :
          ('a, 'b ref) abstract -> ('a, 'b) abstract -> ('a, unit) abstract
        val apply :
          ('a, 'b -> 'c) abstract -> ('a, 'b) abstract -> ('a, 'c) abstract
        val updateM :
          ('a, 'b ref) abstract ->
          (('a, 'b) abstract -> ('a, 'b) abstract) ->
          'c -> ('c -> ('a, unit) abstract -> 'd) -> 'd
        val assignM :
          ('a, 'b ref) abstract ->
          ('a, 'b) abstract -> 'c -> ('c -> ('a, unit) abstract -> 'd) -> 'd
        val applyM :
          ('a, 'b -> 'c) abstract ->
          ('a, 'b) abstract -> 'd -> ('d -> ('a, 'c) abstract -> 'e) -> 'e
        module Transformers :
          sig
            val full_unroll :
              int ->
              int -> (int -> ('a, unit) abstract) -> ('a, unit) abstract
          end
      end
    type ('a, 'b, 'c, 'd) cmonad =
        (('a, 'b) TC.abstract, 'c list, ('a, 'd) TC.abstract)
        StateCPSMonad.monad
    type ('a, 'b, 'c, 'd) omonad =
        (('a, 'b) TC.abstract option, 'c list, ('a, 'd) TC.abstract)
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
          ('a, indet) TC.abstract -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) TC.abstract -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
      end
    module type DETF =
      functor (D : Infra.DOMAINL) ->
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
            ('a, indet) TC.abstract ->
            ('a * [> 'a tag_lstate ] * 'b, unit) lm
          val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
          val set :
            ('a, indet) TC.abstract ->
            ('a * [> 'a tag_lstate ] * 'b, unit) lm
          val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
        end
    module type RANK =
      sig
        type 'a lstate = ('a, int ref) TC.abstract
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
        type 'a lstate = ('a, int ref) TC.abstract
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
          ([> `TRan of ('b, int ref) TC.abstract ] as 'a) list ->
          ('a list -> ('b, int ref) TC.abstract -> ('b, 'c) TC.abstract) ->
          ('b, 'c) TC.abstract
        val succ :
          unit ->
          ([> `TRan of ('b, int ref) TC.abstract ] as 'a) list ->
          ('a list -> ('b, unit) TC.abstract -> 'c) -> 'c
      end
    module Rank :
      sig
        type 'a lstate = ('a, int ref) TC.abstract
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
        type 'a lstate = ('a, int ref) TC.abstract
        type 'a tag_lstate = [ `TRan of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val rfetch : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val decl : unit -> ('a * [> 'a tag_lstate ] * 'b, int ref) lm
        val succ : unit -> ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, int) lm
      end
    module NoDet :
      functor (Dom : Infra.DOMAINL) ->
        sig
          type indet = Dom.v
          type outdet = unit
          type tdet = outdet ref
          type 'a lstate = unit
          val decl : unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
          val upd_sign : unit -> 'a -> ('a -> 'b option -> 'c) -> 'c
          val zero_sign :
            unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
          val acc : 'a -> 'b -> ('b -> ('c, unit) TC.abstract -> 'd) -> 'd
          val get :
            unit -> 'a -> ('a -> ('b, unit ref) TC.abstract -> 'c) -> 'c
          val set : 'a -> 'b -> ('b -> ('c, unit) TC.abstract -> 'd) -> 'd
          val fin : unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
          type 'a tag_lstate = [ `TDet of 'a lstate ]
          type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
            constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
          type ('a, 'b) om = ('c, 'b, 'd, 'e) omonad
            constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        end
    module AbstractDet :
      functor (Dom : Infra.DOMAINL) ->
        sig
          type indet = Dom.v
          type outdet = Dom.v
          type tdet = outdet ref
          type 'a lstate = ('a, int ref) TC.abstract * ('a, tdet) TC.abstract
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
                  ('b, int ref) TC.abstract * ('b, Dom.v ref) TC.abstract ]
             as 'a)
            list ->
            ('a list -> ('c, unit) TC.abstract -> ('b, 'd) TC.abstract) ->
            ('b, 'd) TC.abstract
          val upd_sign :
            unit ->
            ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
            ('a list -> ('b, unit) TC.abstract option -> 'd) -> 'd
          val zero_sign :
            unit ->
            ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
            ('a list -> ('b, unit) TC.abstract -> 'd) -> 'd
          val acc :
            'a Dom.vc ->
            ([> `TDet of 'c * ('a, Dom.v ref) TC.abstract ] as 'b) list ->
            ('b list -> ('a, unit) TC.abstract -> 'd) -> 'd
          val get :
            unit ->
            ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
          val set :
            ('a, 'b) TC.abstract ->
            ([> `TDet of 'd * ('a, 'b ref) TC.abstract ] as 'c) list ->
            ('c list -> ('a, unit) TC.abstract -> 'e) -> 'e
          val fin :
            unit ->
            ([> `TDet of
                  ('b, int ref) TC.abstract * ('b, Dom.v ref) TC.abstract ]
             as 'a)
            list -> ('a list -> ('b, Dom.v) TC.abstract -> 'c) -> 'c
        end
    module UpdateProxy :
      functor (C0 : Infra.CONTAINER2D) ->
        functor (D0 : DETF) ->
          sig
            module type T =
              functor (D1 : Infra.DOMAINL) ->
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
                    ('a, indet) TC.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) TC.abstract ->
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
                           type 'a vc = ('a, v) Infra.rep
                           val zeroL : 'a vc
                           val oneL : 'a vc
                           val ( +^ ) : 'a vc -> 'a vc -> 'a vc
                           val ( *^ ) : 'a vc -> 'a vc -> 'a vc
                           val ( -^ ) : 'a vc -> 'a vc -> 'a vc
                           val uminusL : 'a vc -> 'a vc
                           val divL : 'a vc -> 'a vc -> 'a vc
                           val better_thanL :
                             ('a vc -> 'a vc -> ('a, bool) Infra.rep) option
                           val normalizerL : ('a vc -> 'a vc) option
                         end
                       type contr
                       type 'a vc = ('a, contr) Infra.rep
                       type 'a vo = ('a, Dom.v) Infra.rep
                       val getL :
                         'a vc ->
                         ('a, int) Infra.rep -> ('a, int) Infra.rep -> 'a vo
                       val dim1 : 'a vc -> ('a, int) Infra.rep
                       val dim2 : 'a vc -> ('a, int) Infra.rep
                       val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
                       val copy : 'a vc -> 'a vc
                       val swap_rows_stmt :
                         'a vc ->
                         ('a, int) Infra.rep ->
                         ('a, int) Infra.rep -> ('a, unit) Infra.rep
                       val swap_cols_stmt :
                         'a vc ->
                         ('a, int) Infra.rep ->
                         ('a, int) Infra.rep -> ('a, unit) Infra.rep
                       val row_head :
                         'a vc ->
                         ('a, int) Infra.rep -> ('a, int) Infra.rep -> 'a vo
                       val row_iter :
                         'a vc ->
                         ('a, int) Infra.rep ->
                         ('a, int) Infra.rep ->
                         ('a, int) Infra.rep ->
                         (('a, int) Infra.rep ->
                          'a vo ->
                          'b -> ('b -> 'c -> 'c) -> ('a, 'd) Infra.rep) ->
                         (('a, unit) Infra.rep, 'b, 'e) StateCPSMonad.monad
                       val col_head_set :
                         'a vc ->
                         ('a, int) Infra.rep ->
                         ('a, int) Infra.rep -> 'a vo -> ('a, unit) Infra.rep
                       val col_iter :
                         'a vc ->
                         ('a, int) Infra.rep ->
                         ('a, int) Infra.rep ->
                         ('a, int) Infra.rep ->
                         (('a, int) Infra.rep ->
                          'a vo ->
                          'b -> ('b -> 'c -> 'c) -> ('a, 'd) Infra.rep) ->
                         (('a, unit) Infra.rep, 'b, 'e) StateCPSMonad.monad
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
                      ('a in_val -> ('a, unit) TC.abstract) ->
                      ('a, out_val ref) TC.abstract ->
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
                   type kind = Infra.domain_is_field
                   val zero : v
                   val one : v
                   val plus : v -> v -> v
                   val times : v -> v -> v
                   val minus : v -> v -> v
                   val uminus : v -> v
                   val div : v -> v -> v
                   val better_than : (v -> v -> bool) option
                   val normalizer : (v -> v) option
                   type 'a vc = ('a, v) Infra.rep
                   val zeroL : 'a vc
                   val oneL : 'a vc
                   val ( +^ ) : 'a vc -> 'a vc -> 'a vc
                   val ( *^ ) : 'a vc -> 'a vc -> 'a vc
                   val ( -^ ) : 'a vc -> 'a vc -> 'a vc
                   val uminusL : 'a vc -> 'a vc
                   val divL : 'a vc -> 'a vc -> 'a vc
                   val better_thanL :
                     ('a vc -> 'a vc -> ('a, bool) Infra.rep) option
                   val normalizerL : ('a vc -> 'a vc) option
                 end
               type contr
               type 'a vc = ('a, contr) Infra.rep
               type 'a vo = ('a, Dom.v) Infra.rep
               val getL :
                 'a vc -> ('a, int) Infra.rep -> ('a, int) Infra.rep -> 'a vo
               val dim1 : 'a vc -> ('a, int) Infra.rep
               val dim2 : 'a vc -> ('a, int) Infra.rep
               val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
               val copy : 'a vc -> 'a vc
               val swap_rows_stmt :
                 'a vc ->
                 ('a, int) Infra.rep ->
                 ('a, int) Infra.rep -> ('a, unit) Infra.rep
               val swap_cols_stmt :
                 'a vc ->
                 ('a, int) Infra.rep ->
                 ('a, int) Infra.rep -> ('a, unit) Infra.rep
               val row_head :
                 'a vc -> ('a, int) Infra.rep -> ('a, int) Infra.rep -> 'a vo
               val row_iter :
                 'a vc ->
                 ('a, int) Infra.rep ->
                 ('a, int) Infra.rep ->
                 ('a, int) Infra.rep ->
                 (('a, int) Infra.rep ->
                  'a vo -> 'b -> ('b -> 'c -> 'c) -> ('a, 'd) Infra.rep) ->
                 (('a, unit) Infra.rep, 'b, 'e) StateCPSMonad.monad
               val col_head_set :
                 'a vc ->
                 ('a, int) Infra.rep ->
                 ('a, int) Infra.rep -> 'a vo -> ('a, unit) Infra.rep
               val col_iter :
                 'a vc ->
                 ('a, int) Infra.rep ->
                 ('a, int) Infra.rep ->
                 ('a, int) Infra.rep ->
                 (('a, int) Infra.rep ->
                  'a vo -> 'b -> ('b -> 'c -> 'c) -> ('a, 'd) Infra.rep) ->
                 (('a, unit) Infra.rep, 'b, 'e) StateCPSMonad.monad
             end) ->
        functor (Det : DETF) ->
          sig
            module Dom :
              sig
                type v = C.Dom.v
                type kind = Infra.domain_is_field
                val zero : v
                val one : v
                val plus : v -> v -> v
                val times : v -> v -> v
                val minus : v -> v -> v
                val uminus : v -> v
                val div : v -> v -> v
                val better_than : (v -> v -> bool) option
                val normalizer : (v -> v) option
                type 'a vc = ('a, v) Infra.rep
                val zeroL : 'a vc
                val oneL : 'a vc
                val ( +^ ) : 'a vc -> 'a vc -> 'a vc
                val ( *^ ) : 'a vc -> 'a vc -> 'a vc
                val ( -^ ) : 'a vc -> 'a vc -> 'a vc
                val uminusL : 'a vc -> 'a vc
                val divL : 'a vc -> 'a vc -> 'a vc
                val better_thanL :
                  ('a vc -> 'a vc -> ('a, bool) Infra.rep) option
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
      functor (Ctr : Infra.CONTAINER2D) ->
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
                type 'a vc = ('a, v) Infra.rep
                val zeroL : 'a vc
                val oneL : 'a vc
                val ( +^ ) : 'a vc -> 'a vc -> 'a vc
                val ( *^ ) : 'a vc -> 'a vc -> 'a vc
                val ( -^ ) : 'a vc -> 'a vc -> 'a vc
                val uminusL : 'a vc -> 'a vc
                val divL : 'a vc -> 'a vc -> 'a vc
                val better_thanL :
                  ('a vc -> 'a vc -> ('a, bool) Infra.rep) option
                val normalizerL : ('a vc -> 'a vc) option
              end
            type 'a in_val = ('a, Dom.v) TC.abstract
            type out_val = Det(Ctr.Dom).outdet
            val update :
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              'a Dom.vc ->
              ('a Dom.vc -> 'b) ->
              ('a, Dom.v ref) TC.abstract -> 'c -> ('c -> 'b -> 'd) -> 'd
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
          ('a, Infra.perm) TC.abstract ->
          (('a, unit) TC.abstract option, [> 'a tag_lstate ] list,
           ('a, 'b) TC.abstract)
          StateCPSMonad.monad
        val fin :
          unit ->
          (('a, Infra.perm list) TC.abstract, [> 'a tag_lstate ] list, 'b)
          StateCPSMonad.monad
      end
    module TrackPivot :
      sig
        type 'a lstate = ('a, Infra.perm list ref) TC.abstract
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
          ([> `TPivot of ('b, Infra.perm list ref) TC.abstract ] as 'a) list ->
          ('a list -> ('c, unit) TC.abstract -> ('b, 'd) TC.abstract) ->
          ('b, 'd) TC.abstract
        val add :
          ('a, 'b) TC.abstract ->
          ([> `TPivot of ('a, 'b list ref) TC.abstract ] as 'c) list ->
          ('c list -> ('a, unit) TC.abstract option -> 'd) -> 'd
      end
    module KeepPivot :
      sig
        type 'a lstate = ('a, Infra.perm list ref) TC.abstract
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
          ([> `TPivot of ('b, Infra.perm list ref) TC.abstract ] as 'a) list ->
          ('a list -> ('c, unit) TC.abstract -> ('b, 'd) TC.abstract) ->
          ('b, 'd) TC.abstract
        val add :
          ('a, 'b) TC.abstract ->
          ([> `TPivot of ('a, 'b list ref) TC.abstract ] as 'c) list ->
          ('c list -> ('a, unit) TC.abstract option -> 'd) -> 'd
        val fin :
          unit ->
          ([> `TPivot of ('b, 'c ref) TC.abstract ] as 'a) list ->
          ('a list -> ('b, 'c) TC.abstract -> 'd) -> 'd
      end
    module DiscardPivot :
      sig
        type 'a lstate = ('a, Infra.perm list ref) TC.abstract
        type 'a tag_lstate = [ `TPivot of 'a lstate ]
        type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
          constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
        val decl : unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
        val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
        val fin : unit -> 'a -> ('a -> ('b, 'c list) TC.abstract -> 'd) -> 'd
      end
    module type INPUT =
      functor (C : Infra.CONTAINER2D) ->
        sig
          type inp
          val get_input :
            ('a, inp) TC.abstract ->
            (('a, C.contr) TC.abstract * ('a, int) TC.abstract * bool, 'b,
             ('a, 'c) TC.abstract)
            StateCPSMonad.monad
        end
    module InpJustMatrix :
      functor (C : Infra.CONTAINER2D) ->
        sig
          type inp = C.contr
          val get_input :
            'a C.vc ->
            'b -> ('b -> 'a C.vc * ('a, int) Infra.rep * bool -> 'c) -> 'c
        end
    module InpMatrixMargin :
      functor (C : Infra.CONTAINER2D) ->
        sig
          type inp = C.contr * int
          val get_input :
            ('a, 'b * 'c) TC.abstract ->
            'd ->
            ('d -> ('a, 'b) TC.abstract * ('a, 'c) TC.abstract * bool -> 'e) ->
            'e
        end
    module OutProxy :
      functor (C0 : Infra.CONTAINER2D) ->
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
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module type S =
              functor (C : Infra.CONTAINER2D) ->
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
                             ('a, indet) TC.abstract ->
                             ('a * [> 'a tag_lstate ] * 'b, unit) lm
                           val get :
                             unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                           val set :
                             ('a, indet) TC.abstract ->
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
                      ('a, C.contr) TC.abstract ->
                      ('a, res,
                       [> `TDet of 'a Det.lstate
                        | `TPivot of 'a P.lstate
                        | `TRan of 'a R.lstate ],
                       'b)
                      cmonad
                  end
          end
    module OutJustMatrix :
      functor (C : Infra.CONTAINER2D) ->
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
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) TC.abstract
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
                type 'a lstate = ('a, Infra.perm list ref) TC.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit -> 'a -> ('a -> ('b, 'c list) TC.abstract -> 'd) -> 'd
              end
            val make_result : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
          end
    module OutDet :
      functor (C : Infra.CONTAINER2D) ->
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
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) TC.abstract
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
                type 'a lstate = ('a, Infra.perm list ref) TC.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit -> 'a -> ('a -> ('b, 'c list) TC.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) TC.abstract ->
              ([> 'a D.tag_lstate ] as 'c) list ->
              ('c list ->
               ('a, 'b * D.outdet) TC.abstract -> ('a, 'd) TC.abstract) ->
              ('a, 'd) TC.abstract
          end
    module OutRank :
      functor (C : Infra.CONTAINER2D) ->
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
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) TC.abstract
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
                type 'a lstate = ('a, Infra.perm list ref) TC.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit -> 'a -> ('a -> ('b, 'c list) TC.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) TC.abstract ->
              ([> 'a R.tag_lstate ] as 'c) list ->
              ('c list -> ('a, 'b * int) TC.abstract -> ('a, 'd) TC.abstract) ->
              ('a, 'd) TC.abstract
          end
    module OutDetRank :
      functor (C : Infra.CONTAINER2D) ->
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
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) TC.abstract
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
                type 'a lstate = ('a, Infra.perm list ref) TC.abstract
                type 'a tag_lstate = [ `TPivot of 'a lstate ]
                type ('a, 'b) lm = ('c, 'b, 'd, 'e) cmonad
                  constraint 'a = 'c * ([> 'c tag_lstate ] as 'd) * 'e
                val decl :
                  unit -> 'a -> ('a -> ('b, unit) TC.abstract -> 'c) -> 'c
                val add : 'a -> 'b -> ('b -> 'c option -> 'd) -> 'd
                val fin :
                  unit -> 'a -> ('a -> ('b, 'c list) TC.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) TC.abstract ->
              ([> `TDet of 'a D.lstate | `TRan of 'a R.lstate ] as 'c) list ->
              ('c list ->
               ('a, 'b * D.outdet * int) TC.abstract -> ('a, 'd) TC.abstract) ->
              ('a, 'd) TC.abstract
          end
    module OutDetRankPivot :
      functor (C : Infra.CONTAINER2D) ->
        functor (Det : DETERMINANT) ->
          sig
            type res = C.contr * Det.outdet * int * Infra.perm list
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
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                val set :
                  ('a, indet) TC.abstract ->
                  ('a * [> 'a tag_lstate ] * 'b, unit) lm
                val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
              end
            module R :
              sig
                type 'a lstate = ('a, int ref) TC.abstract
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
                type 'a lstate = ('a, Infra.perm list ref) TC.abstract
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
                  ([> `TPivot of ('b, Infra.perm list ref) TC.abstract ]
                   as 'a)
                  list ->
                  ('a list -> ('c, unit) TC.abstract -> ('b, 'd) TC.abstract) ->
                  ('b, 'd) TC.abstract
                val add :
                  ('a, 'b) TC.abstract ->
                  ([> `TPivot of ('a, 'b list ref) TC.abstract ] as 'c) list ->
                  ('c list -> ('a, unit) TC.abstract option -> 'd) -> 'd
                val fin :
                  unit ->
                  ([> `TPivot of ('b, 'c ref) TC.abstract ] as 'a) list ->
                  ('a list -> ('b, 'c) TC.abstract -> 'd) -> 'd
              end
            val make_result :
              ('a, 'b) TC.abstract ->
              ([> `TDet of 'a D.lstate
                | `TPivot of ('a, 'd ref) TC.abstract
                | `TRan of 'a R.lstate ]
               as 'c)
              list ->
              ('c list ->
               ('a, 'b * D.outdet * int * 'd) TC.abstract ->
               ('a, 'e) TC.abstract) ->
              ('a, 'e) TC.abstract
          end
    module FDet :
      sig
        type indet = Infra.FloatDomainL.v
        type outdet = Infra.FloatDomainL.v
        type tdet = outdet ref
        type 'a lstate = ('a, int ref) TC.abstract * ('a, tdet) TC.abstract
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
                ('b, int ref) TC.abstract *
                ('b, Infra.FloatDomainL.v ref) TC.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) TC.abstract -> ('b, 'd) TC.abstract) ->
          ('b, 'd) TC.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) TC.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) TC.abstract -> 'd) -> 'd
        val acc :
          'a Infra.FloatDomainL.vc ->
          ([> `TDet of 'c * ('a, Infra.FloatDomainL.v ref) TC.abstract ]
           as 'b)
          list -> ('b list -> ('a, unit) TC.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) TC.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) TC.abstract ] as 'c) list ->
          ('c list -> ('a, unit) TC.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) TC.abstract *
                ('b, Infra.FloatDomainL.v ref) TC.abstract ]
           as 'a)
          list ->
          ('a list -> ('b, Infra.FloatDomainL.v) TC.abstract -> 'c) -> 'c
      end
    module IDet :
      sig
        type indet = Infra.IntegerDomainL.v
        type outdet = Infra.IntegerDomainL.v
        type tdet = outdet ref
        type 'a lstate = ('a, int ref) TC.abstract * ('a, tdet) TC.abstract
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
                ('b, int ref) TC.abstract *
                ('b, Infra.IntegerDomainL.v ref) TC.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) TC.abstract -> ('b, 'd) TC.abstract) ->
          ('b, 'd) TC.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) TC.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) TC.abstract -> 'd) -> 'd
        val acc :
          'a Infra.IntegerDomainL.vc ->
          ([> `TDet of 'c * ('a, Infra.IntegerDomainL.v ref) TC.abstract ]
           as 'b)
          list -> ('b list -> ('a, unit) TC.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) TC.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) TC.abstract ] as 'c) list ->
          ('c list -> ('a, unit) TC.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) TC.abstract *
                ('b, Infra.IntegerDomainL.v ref) TC.abstract ]
           as 'a)
          list ->
          ('a list -> ('b, Infra.IntegerDomainL.v) TC.abstract -> 'c) -> 'c
      end
    module RDet :
      sig
        type indet = Infra.RationalDomainL.v
        type outdet = Infra.RationalDomainL.v
        type tdet = outdet ref
        type 'a lstate = ('a, int ref) TC.abstract * ('a, tdet) TC.abstract
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
                ('b, int ref) TC.abstract *
                ('b, Infra.RationalDomainL.v ref) TC.abstract ]
           as 'a)
          list ->
          ('a list -> ('c, unit) TC.abstract -> ('b, 'd) TC.abstract) ->
          ('b, 'd) TC.abstract
        val upd_sign :
          unit ->
          ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) TC.abstract option -> 'd) -> 'd
        val zero_sign :
          unit ->
          ([> `TDet of ('b, int ref) TC.abstract * 'c ] as 'a) list ->
          ('a list -> ('b, unit) TC.abstract -> 'd) -> 'd
        val acc :
          'a Infra.RationalDomainL.vc ->
          ([> `TDet of 'c * ('a, Infra.RationalDomainL.v ref) TC.abstract ]
           as 'b)
          list -> ('b list -> ('a, unit) TC.abstract -> 'd) -> 'd
        val get :
          unit ->
          ([> `TDet of 'b * 'c ] as 'a) list -> ('a list -> 'c -> 'd) -> 'd
        val set :
          ('a, 'b) TC.abstract ->
          ([> `TDet of 'd * ('a, 'b ref) TC.abstract ] as 'c) list ->
          ('c list -> ('a, unit) TC.abstract -> 'e) -> 'e
        val fin :
          unit ->
          ([> `TDet of
                ('b, int ref) TC.abstract *
                ('b, Infra.RationalDomainL.v ref) TC.abstract ]
           as 'a)
          list ->
          ('a list -> ('b, Infra.RationalDomainL.v) TC.abstract -> 'c) -> 'c
      end
    module type PIVOT =
      functor (C : Infra.CONTAINER2D) ->
        functor (D : DETF) ->
          functor (P : TRACKPIVOT) ->
            sig
              val findpivot :
                'a C.vc ->
                ('a, int) TC.abstract ->
                ('a, int) TC.abstract ->
                ('a, int) TC.abstract ->
                ('a, int) TC.abstract ->
                ('a, C.Dom.v option,
                 [> `TDet of 'a D(C.Dom).lstate | `TPivot of 'a P.lstate ],
                 'b)
                cmonad
            end
    module RowPivot :
      functor (Ctr : Infra.CONTAINER2D) ->
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
                    ('a, indet) TC.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) TC.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                end
              val findpivot :
                'a Ctr.vc ->
                ('a, int) Infra.rep ->
                ('a, int) TC.abstract ->
                ('a, int) Infra.rep ->
                'b ->
                ([> `TDet of 'a D.lstate | `TPivot of 'a P.lstate ] as 'c)
                list ->
                ('c list ->
                 ('a, Ctr.Dom.v option) TC.abstract -> ('a, 'd) TC.abstract) ->
                ('a, 'd) TC.abstract
            end
    module FullPivot :
      functor (C : Infra.CONTAINER2D) ->
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
                    ('a, indet) TC.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) TC.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                end
              val findpivot :
                'a C.vc ->
                ('a, int) TC.abstract ->
                ('a, int) TC.abstract ->
                ('a, int) TC.abstract ->
                ('a, int) TC.abstract ->
                ([> 'a D.tag_lstate ] as 'b) list ->
                ('b list ->
                 ('a, C.Dom.v option) TC.abstract -> ('a, 'c) TC.abstract) ->
                ('a, 'c) TC.abstract
            end
    module NoPivot :
      functor (C : Infra.CONTAINER2D) ->
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
                    ('a, indet) TC.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                  val set :
                    ('a, indet) TC.abstract ->
                    ('a * [> 'a tag_lstate ] * 'b, unit) lm
                  val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                end
              val findpivot :
                'a C.vc ->
                ('a, int) Infra.rep ->
                'b ->
                ('a, int) Infra.rep ->
                'c ->
                'd -> ('d -> ('a, C.Dom.v option) TC.abstract -> 'e) -> 'e
            end
    module Gen :
      functor (C : Infra.CONTAINER2D) ->
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
                          ('a, indet) TC.abstract ->
                          ('a * [> 'a tag_lstate ] * 'b, unit) lm
                        val get :
                          unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                        val set :
                          ('a, indet) TC.abstract ->
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
                          ('a in_val -> ('a, unit) TC.abstract) ->
                          ('a, out_val ref) TC.abstract ->
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
                          ('a, inp) TC.abstract ->
                          (('a, C.contr) TC.abstract *
                           ('a, int) TC.abstract * bool, 'b,
                           ('a, 'c) TC.abstract)
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
                              ('a, indet) TC.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val get :
                              unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
                            val set :
                              ('a, indet) TC.abstract ->
                              ('a * [> 'a tag_lstate ] * 'b, unit) lm
                            val fin :
                              unit ->
                              ('a * [> 'a tag_lstate ] * 'b, outdet) lm
                          end
                        module R :
                          sig
                            type 'a lstate = ('a, int ref) TC.abstract
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
                              ('a, Infra.perm) TC.abstract ->
                              (('a, unit) TC.abstract option,
                               [> 'a tag_lstate ] list, ('a, 'b) TC.abstract)
                              StateCPSMonad.monad
                            val fin :
                              unit ->
                              (('a, Infra.perm list) TC.abstract,
                               [> 'a tag_lstate ] list, 'b)
                              StateCPSMonad.monad
                          end
                        val make_result :
                          ('a, C.contr) TC.abstract ->
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
                          ('a, int) TC.abstract ->
                          ('a, int) TC.abstract ->
                          ('a, int) TC.abstract ->
                          ('a, int) TC.abstract ->
                          ('a, C.Dom.v option,
                           [> `TDet of 'a Detf(C.Dom).lstate
                            | `TPivot of 'a Output.P.lstate ],
                           'b)
                          cmonad
                      end
                    val gen :
                      ('a, Input.inp) TC.abstract ->
                      ([> `TDet of 'a Det.lstate
                        | `TPivot of 'a Output.P.lstate
                        | `TRan of 'a Output.R.lstate ]
                       as 'b)
                      list ->
                      ('b list ->
                       ('a, Output.res) TC.abstract -> ('a, 'c) TC.abstract) ->
                      ('a, 'c) TC.abstract
                  end
  end
module GAC_F :
  sig
    module Dom :
      sig
        type v = GEF.Infra.FloatDomainL.v
        type kind = GEF.Infra.FloatDomainL.kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) GEF.Infra.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) GEF.Infra.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) GEF.Infra.rep
    type 'a vo = ('a, Dom.v) GEF.Infra.rep
    val getL :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val dim2 : ('a, 'b array) GEF.Infra.rep -> ('a, int) GEF.Infra.rep
    val dim1 : ('a, 'b array array) GEF.Infra.rep -> ('a, int) GEF.Infra.rep
    val mapper :
      ('a vo -> 'a vo) option ->
      ('a, Dom.v array array) GEF.Infra.rep ->
      ('a, Dom.v array array) GEF.Infra.rep
    val copy :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, 'b array array) GEF.Infra.rep
    val swap_rows_stmt :
      ('a, 'b array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val swap_cols_stmt :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val row_head :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val row_iter :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
    val col_head_set :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, 'b) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val col_iter :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
  end
module GVC_F :
  sig
    module Dom :
      sig
        type v = GEF.Infra.FloatDomainL.v
        type kind = GEF.Infra.FloatDomainL.kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) GEF.Infra.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) GEF.Infra.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v GEF.Infra.container2dfromvector
    type 'a vc = ('a, contr) GEF.Infra.rep
    type 'a vo = ('a, Dom.v) GEF.Infra.rep
    val getL :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val dim2 :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep
    val dim1 :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep
    val mapper :
      (('a, 'b) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep) option ->
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep
    val copy :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep
    val swap_rows_stmt :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val swap_cols_stmt :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val row_head :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val row_iter :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
    val col_head_set :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, 'b) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val col_iter :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
  end
module GAC_I :
  sig
    module Dom :
      sig
        type v = GEF.Infra.IntegerDomainL.v
        type kind = GEF.Infra.IntegerDomainL.kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) GEF.Infra.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) GEF.Infra.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) GEF.Infra.rep
    type 'a vo = ('a, Dom.v) GEF.Infra.rep
    val getL :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val dim2 : ('a, 'b array) GEF.Infra.rep -> ('a, int) GEF.Infra.rep
    val dim1 : ('a, 'b array array) GEF.Infra.rep -> ('a, int) GEF.Infra.rep
    val mapper :
      ('a vo -> 'a vo) option ->
      ('a, Dom.v array array) GEF.Infra.rep ->
      ('a, Dom.v array array) GEF.Infra.rep
    val copy :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, 'b array array) GEF.Infra.rep
    val swap_rows_stmt :
      ('a, 'b array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val swap_cols_stmt :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val row_head :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val row_iter :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
    val col_head_set :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, 'b) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val col_iter :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
  end
module GVC_I :
  sig
    module Dom :
      sig
        type v = GEF.Infra.IntegerDomainL.v
        type kind = GEF.Infra.IntegerDomainL.kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) GEF.Infra.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) GEF.Infra.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v GEF.Infra.container2dfromvector
    type 'a vc = ('a, contr) GEF.Infra.rep
    type 'a vo = ('a, Dom.v) GEF.Infra.rep
    val getL :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val dim2 :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep
    val dim1 :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep
    val mapper :
      (('a, 'b) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep) option ->
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep
    val copy :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep
    val swap_rows_stmt :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val swap_cols_stmt :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val row_head :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val row_iter :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
    val col_head_set :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, 'b) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val col_iter :
      ('a, 'b GEF.Infra.container2dfromvector) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
  end
module GAC_R :
  sig
    module Dom :
      sig
        type v = GEF.Infra.RationalDomainL.v
        type kind = GEF.Infra.RationalDomainL.kind
        val zero : v
        val one : v
        val plus : v -> v -> v
        val times : v -> v -> v
        val minus : v -> v -> v
        val uminus : v -> v
        val div : v -> v -> v
        val better_than : (v -> v -> bool) option
        val normalizer : (v -> v) option
        type 'a vc = ('a, v) GEF.Infra.rep
        val zeroL : 'a vc
        val oneL : 'a vc
        val ( +^ ) : 'a vc -> 'a vc -> 'a vc
        val ( *^ ) : 'a vc -> 'a vc -> 'a vc
        val ( -^ ) : 'a vc -> 'a vc -> 'a vc
        val uminusL : 'a vc -> 'a vc
        val divL : 'a vc -> 'a vc -> 'a vc
        val better_thanL :
          ('a vc -> 'a vc -> ('a, bool) GEF.Infra.rep) option
        val normalizerL : ('a vc -> 'a vc) option
      end
    type contr = Dom.v array array
    type 'a vc = ('a, contr) GEF.Infra.rep
    type 'a vo = ('a, Dom.v) GEF.Infra.rep
    val getL :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val dim2 : ('a, 'b array) GEF.Infra.rep -> ('a, int) GEF.Infra.rep
    val dim1 : ('a, 'b array array) GEF.Infra.rep -> ('a, int) GEF.Infra.rep
    val mapper :
      ('a vo -> 'a vo) option ->
      ('a, Dom.v array array) GEF.Infra.rep ->
      ('a, Dom.v array array) GEF.Infra.rep
    val copy :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, 'b array array) GEF.Infra.rep
    val swap_rows_stmt :
      ('a, 'b array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val swap_cols_stmt :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val row_head :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep -> ('a, 'b) GEF.Infra.rep
    val row_iter :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
    val col_head_set :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, 'b) GEF.Infra.rep -> ('a, unit) GEF.Infra.rep
    val col_iter :
      ('a, 'b array array) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      ('a, int) GEF.Infra.rep ->
      (('a, int) GEF.Infra.rep ->
       ('a, 'b) GEF.Infra.rep ->
       'c -> ('d -> 'e -> 'e) -> ('a, 'f) GEF.Infra.rep) ->
      'c -> ('c -> ('a, unit) GEF.Infra.rep -> 'g) -> 'g
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GVC_I.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GVC_I.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GVC_I.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GVC_I.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_R.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_R.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_R.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_R.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_R.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_R.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_R.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.AbstractDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
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
          ('a, indet) GEF.TC.abstract ->
          ('a * [> 'a tag_lstate ] * 'b, unit) lm
        val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
        val set :
          ('a, indet) GEF.TC.abstract ->
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
          ('a in_val -> ('a, unit) GEF.TC.abstract) ->
          ('a, out_val ref) GEF.TC.abstract -> ('a, unit, 'b, 'c) GEF.cmonad
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
          ('a, inp) GEF.TC.abstract ->
          (('a, GAC_F.contr) GEF.TC.abstract * ('a, int) GEF.TC.abstract *
           bool, 'b, ('a, 'c) GEF.TC.abstract)
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
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val get : unit -> ('a * [> 'a tag_lstate ] * 'b, tdet) lm
            val set :
              ('a, indet) GEF.TC.abstract ->
              ('a * [> 'a tag_lstate ] * 'b, unit) lm
            val fin : unit -> ('a * [> 'a tag_lstate ] * 'b, outdet) lm
          end
        module R :
          sig
            type 'a lstate = ('a, int ref) GEF.TC.abstract
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
              ('a, GEF.Infra.perm) GEF.TC.abstract ->
              (('a, unit) GEF.TC.abstract option, [> 'a tag_lstate ] list,
               ('a, 'b) GEF.TC.abstract)
              StateCPSMonad.monad
            val fin :
              unit ->
              (('a, GEF.Infra.perm list) GEF.TC.abstract,
               [> 'a tag_lstate ] list, 'b)
              StateCPSMonad.monad
          end
        val make_result :
          ('a, GAC_F.contr) GEF.TC.abstract ->
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
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, int) GEF.TC.abstract ->
          ('a, GAC_F.Dom.v option,
           [> `TDet of 'a GEF.NoDet(GAC_F.Dom).lstate
            | `TPivot of 'a Output.P.lstate ],
           'b)
          GEF.cmonad
      end
    val gen :
      ('a, Input.inp) GEF.TC.abstract ->
      ([> `TDet of 'a Det.lstate
        | `TPivot of 'a Output.P.lstate
        | `TRan of 'a Output.R.lstate ]
       as 'b)
      list ->
      ('b list ->
       ('a, Output.res) GEF.TC.abstract -> ('a, 'c) GEF.TC.abstract) ->
      ('a, 'c) GEF.TC.abstract
  end
val instantiate :
  ((unit -> 'a) -> 'b list -> ('c -> 'd -> 'd) -> unit -> 'e) -> 'a -> 'e =
  <fun>
#   val resFA1 : GenFA1.Input.inp -> GenFA1.Output.res = <fun>
# val resFA2 : GenFA2.Input.inp -> GenFA2.Output.res = <fun>
# val resFA3 : GenFA3.Input.inp -> GenFA3.Output.res = <fun>
# val resFA4 : GenFA4.Input.inp -> GenFA4.Output.res = <fun>
# val resFV1 : GenFV1.Input.inp -> GenFV1.Output.res = <fun>
# val resFV2 : GenFV2.Input.inp -> GenFV2.Output.res = <fun>
# val resFV3 : GenFV3.Input.inp -> GenFV3.Output.res = <fun>
# val resFV4 : GenFV4.Input.inp -> GenFV4.Output.res = <fun>
# val resFV5 : GenFV5.Input.inp -> GenFV5.Output.res = <fun>
# val resIA1 : GenIA1.Input.inp -> GenIA1.Output.res = <fun>
# val resIA2 : GenIA2.Input.inp -> GenIA2.Output.res = <fun>
# val resIA3 : GenIA3.Input.inp -> GenIA3.Output.res = <fun>
# val resIA4 : GenIA4.Input.inp -> GenIA4.Output.res = <fun>
# val resIV1 : GenIV1.Input.inp -> GenIV1.Output.res = <fun>
# val resIV2 : GenIV2.Input.inp -> GenIV2.Output.res = <fun>
# val resIV3 : GenIV3.Input.inp -> GenIV3.Output.res = <fun>
# val resIV4 : GenIV4.Input.inp -> GenIV4.Output.res = <fun>
# val resIV5 : GenIV5.Input.inp -> GenIV5.Output.res = <fun>
# val resFA11 : GenFA11.Input.inp -> GenFA11.Output.res = <fun>
# val resFA12 : GenFA12.Input.inp -> GenFA12.Output.res = <fun>
# val resFA13 : GenFA13.Input.inp -> GenFA13.Output.res = <fun>
# val resFA14 : GenFA14.Input.inp -> GenFA14.Output.res = <fun>
# val resFA24 : GenFA24.Input.inp -> GenFA24.Output.res = <fun>
# val resRA1 : GenRA1.Input.inp -> GenRA1.Output.res = <fun>
# val resRA2 : GenRA2.Input.inp -> GenRA2.Output.res = <fun>
# val resRA3 : GenRA3.Input.inp -> GenRA3.Output.res = <fun>
# val resRA4 : GenRA4.Input.inp -> GenRA4.Output.res = <fun>
# val resFA5 : GenFA5.Input.inp -> GenFA5.Output.res = <fun>
# val resFA6 : GenFA6.Input.inp -> GenFA6.Output.res = <fun>
# val resFA7 : GenFA7.Input.inp -> GenFA7.Output.res = <fun>
# val resFA8 : GenFA8.Input.inp -> GenFA8.Output.res = <fun>
#   val rFA1 : GenFA1.Input.inp -> GenFA1.Output.res = <fun>
# val rFA2 : GenFA2.Input.inp -> GenFA2.Output.res = <fun>
# val rFA3 : GenFA3.Input.inp -> GenFA3.Output.res = <fun>
# val rFA4 : GenFA4.Input.inp -> GenFA4.Output.res = <fun>
# val rFV1 : GenFV1.Input.inp -> GenFV1.Output.res = <fun>
# val rFV2 : GenFV2.Input.inp -> GenFV2.Output.res = <fun>
# val rFV3 : GenFV3.Input.inp -> GenFV3.Output.res = <fun>
# val rFV4 : GenFV4.Input.inp -> GenFV4.Output.res = <fun>
# val rFV5 : GenFV5.Input.inp -> GenFV5.Output.res = <fun>
# val rIA1 : GenIA1.Input.inp -> GenIA1.Output.res = <fun>
# val rIA2 : GenIA2.Input.inp -> GenIA2.Output.res = <fun>
# val rIA3 : GenIA3.Input.inp -> GenIA3.Output.res = <fun>
# val rIA4 : GenIA4.Input.inp -> GenIA4.Output.res = <fun>
# val rIV1 : GenIV1.Input.inp -> GenIV1.Output.res = <fun>
# val rIV2 : GenIV2.Input.inp -> GenIV2.Output.res = <fun>
# val rIV3 : GenIV3.Input.inp -> GenIV3.Output.res = <fun>
# val rIV4 : GenIV4.Input.inp -> GenIV4.Output.res = <fun>
# val rIV5 : GenIV5.Input.inp -> GenIV5.Output.res = <fun>
# val rFA11 : GenFA11.Input.inp -> GenFA11.Output.res = <fun>
# val rFA12 : GenFA12.Input.inp -> GenFA12.Output.res = <fun>
# val rFA13 : GenFA13.Input.inp -> GenFA13.Output.res = <fun>
# val rFA14 : GenFA14.Input.inp -> GenFA14.Output.res = <fun>
# val rFA24 : GenFA24.Input.inp -> GenFA24.Output.res = <fun>
# val rRA1 : GenRA1.Input.inp -> GenRA1.Output.res = <fun>
# val rRA2 : GenRA2.Input.inp -> GenRA2.Output.res = <fun>
# val rRA3 : GenRA3.Input.inp -> GenRA3.Output.res = <fun>
# val rRA4 : GenRA4.Input.inp -> GenRA4.Output.res = <fun>
# val rFA5 : GenFA5.Input.inp -> GenFA5.Output.res = <fun>
# val rFA6 : GenFA6.Input.inp -> GenFA6.Output.res = <fun>
# val rFA7 : GenFA7.Input.inp -> GenFA7.Output.res = <fun>
# val rFA8 : GenFA8.Input.inp -> GenFA8.Output.res = <fun>
#                                                   val ia0 : GAC_I.Dom.v array array = [|[|1|]|]
val ia1 : GAC_I.Dom.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|]
val ia2 : GAC_I.Dom.v array array =
  [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|]
val ia3 : GAC_I.Dom.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|]
val ia4 : GAC_I.Dom.v array array =
  [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]
val ia5 : GAC_I.Dom.v array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|];
   [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|];
   [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]]
val resI11 : GenIA1.Output.res list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|]]
# val resI12 : GenIA2.Output.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0)]
# val resI13 : GenIA3.Output.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 2)]
# val resI14 : GenIA4.Output.res list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0, 2)]
#                 val iv0 : GVC_I.Dom.v GEF.Infra.container2dfromvector =
  {arr = [|1|]; n = 1; m = 1}
val iv1 : GVC_I.Dom.v GEF.Infra.container2dfromvector =
  {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3}
val iv2 : GVC_I.Dom.v GEF.Infra.container2dfromvector =
  {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4}
val iv4 : GVC_I.Dom.v GEF.Infra.container2dfromvector =
  {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}
val iv5 : GVC_I.Dom.v GEF.Infra.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}]
val resI21 : GenIV1.Output.res list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}]
# val resI22 : GenIV2.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0)]
# val resI23 : GenIV3.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 2)]
# val resI24 : GenIV4.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
# val resI25 : GenIV5.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
#   val fa0 : float array array = [|[|1.|]|]
#         val fa1 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|]
#         val fa2 : float array array =
  [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|]
#           val fa3 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|]
#           val fa4 : float array array =
  [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]
#                                                       * *     val fa5 : GAC_F.Dom.v array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]]
val fa6 : GAC_F.Dom.v array array = [|[|1.; 1.|]|]
val fa7 : float array array =
  [|[|1.; 2.; 3.; 1.; 0.; 0.|]; [|4.; 13.; 5.; 0.; 1.; 0.|];
    [|-1.; 3.; 0.; 0.; 0.; 1.|]|]
val resF1 : GenFA1.Output.res list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
#                                                                                                 val a2v : 'a array array -> 'a GEF.Infra.container2dfromvector = <fun>
val xxx : GAC_F.Dom.v GEF.Infra.container2dfromvector list =
  [{arr = [|1.|]; n = 1; m = 1};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.|]; n = 3; m = 3};
   {arr = [|1.; 2.; 3.; 0.; 4.; 13.; 5.; 0.; -1.; 3.; 0.; 0.|]; n = 3; m = 4};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.; 0.; 0.; 0.|]; n = 4; m = 3};
   {arr = [|0.; 2.; 3.; 0.; 10.; 5.; 0.; 3.; 0.|]; n = 3; m = 3}]
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
# val resF12 : GenFA12.Output.res list =
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
# val resF13 : GenFA13.Output.res list =
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
# val resF14 : GenFA14.Output.res list =
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
# val resF24 : GenFA24.Output.res list =
  [([|[|1.|]|], 1., 1, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [RowSwap (2, 1); RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [RowSwap (2, 1); RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [RowSwap (2, 1); RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2,
    [RowSwap (1, 0)])]
#   * * * * * * * * *     val ra0 : Num.num array array = [|[|Num.Int 1|]|]
#           val ra1 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
    [|Num.Int 4; Num.Int 13; Num.Int 5|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0|]|]
#           val ra2 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
    [|Num.Int 4; Num.Int 13; Num.Int 5; Num.Int 0|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0; Num.Int 0|]|]
#             val ra3 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
    [|Num.Int 4; Num.Int 13; Num.Int 5|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0|];
    [|Num.Int 0; Num.Int 0; Num.Int 0|]|]
#           val ra4 : Num.num array array =
  [|[|Num.Int 0; Num.Int 2; Num.Int 3|];
    [|Num.Int 0; Num.Int 13; Num.Int 5|];
    [|Num.Int 0; Num.Int 3; Num.Int 0|]|]
# val ra5 : Num.num array array list =
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
#   val resR11 : GenRA1.Output.res list =
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
# val resR12 : GenRA2.Output.res list =
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
# val resR13 : GenRA3.Output.res list =
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
# val resR14 : GenRA4.Output.res list =
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
