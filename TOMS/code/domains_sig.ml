(* Declarations of domains and containers *)

module S(T:
  sig
	(* Representation type of values,  to be specified *)
    type ('a, 'b) rep
  end) = struct

open T

(* The kind of the domain: a ring or a field *)
type domain_is_field (* abstract *)
type domain_is_ring  (* abstract *)

module type DOMAIN = sig
  type v
  type kind (* Field or Ring ? *)
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


(* Lift *)
module type DOMAINL = sig
  include DOMAIN
  type 'a vc = ('a,v) rep
  val zeroL : 'a vc
  val oneL : 'a vc
  val ( +^ ) : 'a vc -> 'a vc -> 'a vc
  val ( *^ ) : 'a vc -> 'a vc -> 'a vc
  val ( -^ ) : 'a vc -> 'a vc -> 'a vc
  val uminusL : 'a vc -> 'a vc
  val divL : 'a vc -> 'a vc -> 'a vc
  val better_thanL : ('a vc -> 'a vc -> ('a,bool) rep) option
  val normalizerL : ('a vc -> 'a vc) option
end 

module type CONTAINER2D = sig
  module Dom:DOMAINL
  type contr
  type 'a vc = ('a,contr) rep
  type 'a vo = ('a,Dom.v) rep
  val getL : 'a vc -> ('a,int) rep -> ('a,int) rep -> 'a vo
  val dim1 : 'a vc -> ('a,int) rep
  val dim2 : 'a vc -> ('a,int) rep
  val mapper : ('a vo -> 'a vo) option -> 'a vc -> 'a vc
  val copy : 'a vc -> 'a vc
  val swap_rows_stmt : 'a vc -> ('a, int) rep -> ('a, int) rep -> 
                       ('a,unit) rep
  val swap_cols_stmt : 'a vc -> ('a, int) rep -> ('a, int) rep -> 
                       ('a,unit) rep
  val row_head : 'a vc -> ('a, int) rep -> ('a, int) rep -> 'a vo
  val col_head_set : 'a vc -> ('a,int) rep -> ('a,int) rep -> 'a vo -> 
            ('a,unit) rep
end

end
