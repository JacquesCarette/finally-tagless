module Sig = struct
  type domain_is_field
  type domain_is_ring
  module type DOMAIN = sig
    type kind
    type v
    val z : v
  end
  module type COLL = sig
    module Dom : DOMAIN
    type coll
  end
end

module Doms = struct
  open Sig
  module FDomain = struct
    type kind = domain_is_field
    type v = float
    let z = 0.0
  end
  module IDomain = struct
    type kind = domain_is_ring
    type v = int
    let z = 0
  end
  module GColl(Dom:DOMAIN) =
    struct
      module Dom = Dom
      type coll = Dom.v list
    end
end ;;

module GEF = struct
  open Sig
  module DivisionUpdate 
      (C:COLL with type Dom.kind = domain_is_field) = struct
	let update x = x
  end
  module Gen(C: COLL)
      (Update: functor(C:COLL with type Dom.kind = C.Dom.kind)
               -> sig val update : C.Dom.v -> C.Dom.v end) =
    struct
      module U = Update(C)
      let foo = U.update(C.Dom.z)
    end
end;;

module Test = GEF.Gen(Doms.GColl(Doms.FDomain))(GEF.DivisionUpdate);;
let test = Test.foo;;

module C_F = Doms.GColl(Doms.FDomain);;
module Test1 = GEF.Gen(C_F)(GEF.DivisionUpdate);;

module C_I = Doms.GColl(Doms.IDomain);;
module Test2 = GEF.Gen(C_I)(GEF.DivisionUpdate);; (* reports an error *)

