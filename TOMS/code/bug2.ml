(* this works *)
module type DOMAIN = sig
    type kind
    type foo
    val  upd : foo -> foo
end

type domain_is_field

module Rational = struct
    type kind = domain_is_field
    type foo  = int * int
    let  upd (x,y) = (x-1, y+1)
end

module Integer = struct
    type kind
    type foo  = int
    let  upd x = x-1
end

module type UPDATE = sig
    type obj
    val update : obj -> obj
end

module DivisionUpdate(D:DOMAIN with type kind = domain_is_field) = struct
    type obj = D.foo
    let update a = D.upd a
end

module BadUpdate(D:DOMAIN) = struct
    type obj = D.foo
    let update a = D.upd a
end

(* works, as expected *)
module A = DivisionUpdate(Rational)
(* correctly generates an error 
module A = DivisionUpdate(Integer)
*)

(* However, if we go higher order: *)
module type UPDATE2 =
    functor(D:DOMAIN) -> sig
    type obj = D.foo
    val update : obj -> obj
end

module Bar(D:DOMAIN)(U:UPDATE2) = struct
    module U = U(D)
    let update x = U.update x
end

(* works as there are no restrictions *)
module T3 = Bar(Integer)(BadUpdate) ;;

(* and now this does not work?!?! *)
module T2 = Bar(Rational)(DivisionUpdate) ;;
