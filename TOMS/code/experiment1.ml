type domain_is_field (* abstract *)
type domain_is_ring  (* abstract *)

type ('a,'v) rep = 'v

module type DOMAIN = sig
  type v
  module type DOMAINKIND
  module DOMAINKind : DOMAINKIND
  module type RINGKIND
  module type FIELDKIND
  val zero : v
end
;;


module type CONTAINER2D = sig
  module Dom:DOMAIN
  type contr
  type 'a vc = ('a,contr) rep
  type 'a vo = ('a,Dom.v) rep
  val getL : 'a vc -> ('a,int) rep -> 'a vo
end;;

module CA(D: DOMAIN) = struct
  module Dom = D
  type contr = Dom.v array
  type 'a vc = ('a,contr) rep
  type 'a vo = ('a,Dom.v) rep
  let getL c i = Array.get c i
end;;

module LA(C: CONTAINER2D) = struct

  module type UPDATE = sig val update : C.contr -> 'a C.vo end

  module UPDATEFF(C0: CONTAINER2D) = struct
    module Foo:C0.Dom.RINGKIND = C.Dom.DOMAINKind

    let update c  = C.getL c 0
  end

  module Gen(U:UPDATE) = struct
    let doit c = U.update c = C.Dom.zero
  end
end;;



module Int = struct
  type v = int
  module type RINGKIND = sig type ring end
  module type FIELDKIND = sig type ring type field end
  module type DOMAINKIND = RINGKIND
  module DOMAINKind = struct type ring end
  type kind = domain_is_ring
  let zero = 0
end;;

module Float = struct
  type v = float
  module type DOMAINKIND = sig type kind = domain_is_field end
  let zero = 0.0
end;;

module CAI = CA(Int);;
module CAF = CA(Float);;

module LAI = LA(CAI);;
module LAIFF = LAI.Gen(LAI.UPDATEFF);;
