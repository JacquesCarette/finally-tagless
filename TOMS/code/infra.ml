module Proxy(R: Abstractrep.T) = struct

module type T = functor(R1: Abstractrep.T) -> sig

type ('a, 'b) rep = ('a, 'b) R.abstract

module TheCode : functor(AR:Abstractrep.T) -> 
    Coderep.T with type ('a, 'b) abstract = ('a, 'b) R.abstract

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
type domain_is_field
type domain_is_ring
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
    val ( +^ ) : ('a, float) rep -> ('a, float) rep -> ('a, float) rep
    val ( *^ ) : ('a, float) rep -> ('a, float) rep -> ('a, float) rep
    val ( -^ ) : ('a, float) rep -> ('a, float) rep -> ('a, float) rep
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
    val divL : ('a, Num.num) rep -> ('a, Num.num) rep -> ('a, Num.num) rep
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
      (('a, int) rep -> 'a vo -> 'b -> ('b -> 'c -> 'c) -> ('a, 'd) rep) ->
      (('a, unit) rep, 'b, 'e) StateCPSMonad.monad
    val col_head_set :
      'a vc -> ('a, int) rep -> ('a, int) rep -> 'a vo -> ('a, unit) rep
    val col_iter :
      'a vc ->
      ('a, int) rep ->
      ('a, int) rep ->
      ('a, int) rep ->
      (('a, int) rep -> 'a vo -> 'b -> ('b -> 'c -> 'c) -> ('a, 'd) rep) ->
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
          val better_thanL : ('a vc -> 'a vc -> ('a, bool) rep) option
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
        ('a, int) rep -> ('a, int) rep -> ('a, 'b) rep -> ('a, unit) rep
      val col_iter :
        ('a, 'b array array) rep ->
        ('a, int) rep ->
        ('a, int) rep ->
        ('a, int) rep ->
        (('a, int) rep ->
         ('a, 'b) rep -> 'c -> ('d -> 'e -> 'e) -> ('a, 'f) rep) ->
        'c -> ('c -> ('a, unit) rep -> 'g) -> 'g
    end
type 'a container2dfromvector = { arr : 'a array; n : int; m : int; }
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
          val better_thanL : ('a vc -> 'a vc -> ('a, bool) rep) option
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
        ('a, int) rep -> ('a, int) rep -> ('a, 'b) rep -> ('a, unit) rep
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
type 'a container2dsparse = { sarr : 'a svect array; mm : int; }
module Array1D :
  sig
    val getL : 'a array -> ('b, int) rep -> ('b, 'a) rep
    val setL : 'a array -> ('b, int) rep -> ('b, 'a) rep -> ('b, unit) rep
    val dim1 : 'a array -> ('b, int) rep
    val mapper :
      ('a, 'b -> 'b) rep option ->
      ('a, 'b array) rep -> ('a, 'b array) rep
  end
module CArray1D :
  sig
    val getL : ('a, 'b array) rep -> int -> ('a, 'b) rep
    val setL : ('a, 'b array) rep -> int -> ('a, 'b) rep -> ('a, unit) rep
    val dim1 : ('a, 'b array) rep -> ('a, int) rep
  end
module Array2D :
  sig
    val getL :
      'a array array -> ('b, int) rep -> ('b, int) rep -> ('b, 'a) rep
    val setL :
      'a array array ->
      ('b, int) rep -> ('b, int) rep -> ('b, 'a) rep -> ('b, unit) rep
    val dim2 : 'a array -> ('b, int) rep
    val dim1 : 'a array array -> ('b, int) rep
  end


type perm = RowSwap of (int * int) | ColSwap of (int*int)
val liftRowSwap : ('a, int) rep -> ('a, int) rep -> ('a, perm) rep
val liftColSwap : ('a, int) rep -> ('a, int) rep -> ('a, perm) rep
end

end
