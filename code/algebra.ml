(* set up some very general algebra that can be reused though not
   so much in the current code (yet?) *)
type ('a,'b) abstract = Ground of 'b | Code of ('a, 'b) code
let concretize = function
    | Ground x -> .<x>.
    | Code x   -> x

module type MONOID = sig
    type values
    val uunit : values
    val binop : values -> values -> values
    val binopC : ('a,values) code -> ('a,values) code -> ('a,values) code
end

module type LIFTEDMONOID = sig
    type t
    type ('a,'b) liftedvalues
    val lbinop : ('a,t) liftedvalues -> ('a,t) liftedvalues -> ('a,t) liftedvalues
    val toconcrete : ('a,t) liftedvalues -> ('a,t) code
end

(* The lifted version is not commutative ! *)
module LiftCommutativeMonoid(M:MONOID) = struct
    type t = M.values 
    type ('a,'b) liftedvalues = ('a, t) abstract
    let generalize x = Ground x
    let mixedop x y =
        if x=M.uunit then y else Code (M.binopC .<x>. (concretize y))
    let toconcrete (x: ('a,M.values) liftedvalues) = concretize x
    let lbinop x y =
        match (x,y) with
        | (Ground a, Ground b) -> Ground (M.binop a b)
        | (Ground a, b)        -> mixedop a b
        | (a, Ground b)        -> mixedop b a
        | (a, b) -> Code (M.binopC (concretize a) (concretize b))
end

module OrMonoid = struct
    type values = bool
    let uunit = false
    let binop x y = ( x ||  y)
    let binopC x y = .< .~x || .~y >.
end
module OrLMonoid = LiftCommutativeMonoid(OrMonoid)

module AndMonoid = struct
    type values = bool
    let uunit = true
    let binop x y = ( x &&  y)
    let binopC x y = .< .~x && .~y >.
end
module AndLMonoid = LiftCommutativeMonoid(AndMonoid)

module LogicGen(Or:LIFTEDMONOID)(And:LIFTEDMONOID) = 
  struct
    let mcode_or a b = mdo { 
        x <-- a;
        y <-- b;
        ret (Or.toconcrete (Or.lbinop x y)) }
    let mcode_and a b = mdo { 
        x <-- a;
        y <-- b;
        ret (And.toconcrete (And.lbinop x y)) }
end
module Logic = LogicGen(OrLMonoid)(AndLMonoid)
