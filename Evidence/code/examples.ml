module type ALL = sig 
  type 'a b 
  val bop : 'a b -> 'a b -> 'a b
  val neutral : 'a b
end
module type VALUEPROP = sig
  type 'a v
  type prop
  type 'a vp = 'a v * prop
  val value : 'a vp -> 'a v
  val prop : 'a vp -> prop
  val attach : 'a v -> prop -> 'a vp
end

module type ABSTRACTCODE = sig
  type 'b b
  type ('a,'b) abstract = Ground of 'b b | Code of ('a, 'b b) code
  val concretize : ('a,'b) abstract -> ('a,'b b) code
  val inject_value : 'b b -> ('a, 'b) abstract
  val inject_code : ('a,'b b) code -> ('a, 'b) abstract
  val binaryop : ('a,'b) abstract -> ('a,'b) abstract -> ('a,'b) abstract
  val simplop : 'b b -> ('a,'b) abstract -> ('a,'b) abstract
  val bop : 'b b -> 'b b -> 'b b
end

module Code(B:ALL) = struct
  type 'b b = 'b B.b
  type ('a,'b) abstract = Ground of 'b B.b | Code of ('a, 'b B.b) code
  let concretize = function
    | Ground x -> .<x>.
    | Code x   -> x
  let inject_value x = Ground x
  let inject_code x = Code x
  let simplop x y = if x=B.neutral then y else Code .< B.bop x .~(concretize y) >.
  let binaryop a b = match (a,b) with
      | (Ground x, Ground y) -> Ground (B.bop x y)
      | (Ground x, Code y)   -> simplop x b
      | (Code x, Ground y)   -> simplop y a
      | (Code x, Code y)   -> Code .< B.bop .~x .~y >.
  let bop a b = B.bop a b
end

module AbstractVector(B:ALL) = struct
  type 'a v = ('a B.b) list
  type prop = int
  type 'a vp = 'a v * prop
  let value (v,p) = v
  let prop (v,p) = p
  let attach v p = (v,p)
end

module LVector(B:ALL) = struct
  module VP = AbstractVector(B)
  let lnil = ([], 0)
  let lcons (h:'a B.b) ((t:'a VP.v),p2) = VP.attach (h::t) (1+p2)

  let head = function 
      | (v,p) when p>0 -> List.hd v
      | _              -> failwith "head on null list"
  let tail = function 
      | (v,p) when p>0 -> (List.tl v, p-1)
      | _              -> failwith "tail on null list"
  let concat (a1,l1) (a2,l2) = (a1@a2, l1+l2)
end

(* of type ALL but we don't want the types to be abstract *)
module T = struct 
  type 'a b = string 
  let bop x y = x ^ y
  let neutral = ""
end

module LV = LVector(T)

(* of type ALL but we don't want the types to be abstract *)
module TC = struct 
  type 'a b=('a,string) code 
  let bop x y = .< .~x ^ .~ y >.
  let neutral = .< "" >.
end
module LVC = LVector(TC)

module AbstractVector2(B:ABSTRACTCODE) = struct
  type ('a,'b) v = ('a, 'b B.b) B.abstract
  type prop = int
  type ('a,'b) vp = ('a,'b) v * prop
  let value (v,p) = v
  let prop (v,p) = p
  let attach v p = (v,p)
end

(* deal with values & code-values evenly in spec-time lists *)
module LVector2(B:ABSTRACTCODE ) = struct
  module VP = AbstractVector2(B)
  let lnil = ([], 0)
  let lcons_v h (t,p2) = VP.attach ((B.inject_value h)::t) (1+p2)
  let lcons_c h (t,p2) = VP.attach ((B.inject_code h)::t) (1+p2)

  let head = function 
      | (v,p) when p>0 -> List.hd v
      | _              -> failwith "head on null list"
  let tail = function 
      | (v,p) when p>0 -> (List.tl v, p-1)
      | _              -> failwith "tail on null list"
  let concat (a1,l1) (a2,l2) = (a1@a2, l1+l2)
  let dotprod (v1,l1) = function 
      | (v2,l2) when l1=l2 -> (List.map2 B.binaryop v1 v2, l1)
      | _                  -> failwith "product of 2 unequal vectors"
  let concretize (v,l) = 
      let rec tocode = function
          | []  -> .< [] >.
          | x::t -> .< .~x :: .~(tocode t) >. in
		.< .~(tocode (List.map B.concretize v)) >.
end

module TC2 = Code(T)
module LVC2 = LVector2(TC2)

(* of type ALL but we don't want the types to be abstract *)
module T3 = struct 
  type 'a b=int 
  let bop x y = x + y
  let neutral = 0
end

module TC3 = Code(T3)
module LVC3 = LVector2(TC3)

module T4 = struct 
  type 'a b=('a,int) code
  let bop x y = .< .~x + .~y >.
  let neutral = .< 0 >.
end
module TC4 = Code(T4)
module LVC4 = LVector2(TC4)

(* deal with values & code-values evenly in run-time lists *)
(* we are starting to get intensional here *)
module LVector3(B:ALL ) = struct
  let lnil = ( .<[]>. , 0)
  let lcons h (t,p2) = (.<(h:: .~t)>. , (1+p2))

  let head = function 
      | (v,p) when p>0 -> .< List.hd .~v>.
      | _              -> failwith "head on null list"
  let tail = function 
      | (v,p) when p>0 -> ( .< List.tl .~v >. , p-1)
      | _              -> failwith "tail on null list"
  let concat (a1,l1) (a2,l2) = (.< .~a1@ .~a2>. , l1+l2)
  let dotprod (v1,l1) = function 
      | (v2,l2) when l1=l2 -> ( .<List.map2 B.bop .~v1 .~v2>. , l1)
      | _                  -> failwith "product of 2 unequal vectors"
  let concretize (v,l) = v
end

module T5 = struct 
  type 'a b=int 
  let bop x y = x * y
  let neutral = 1
end

module LVC5 = LVector3(T5)

(* that's all very cute, but that is mostly a very difficult way
   to do dynamic typing and produce boring residual code.  What 
   about something more interesting?  Well, we need some more 
   interesting properties! *)

(* *linear* multivariate polynomials stored as a0*[(as,s)],
   however, they are assumed to be sorted lexicographically in s.
   representation fully abstract.
   Only need add *)
module Polyn : sig
    type polyn
    val add : polyn -> polyn -> polyn
    end 
    = struct
    type polyn = int * ((int*string) list)
    let add (a0,l0) (a1,l1) =
        let rec merge = function
        | ([], []) -> []
        | (x, []) -> x
        | ([], y)  -> y
        | ((x0,s0)::r0, (x1,s1)::r1) when s0=s1 -> ((x0+x1),s0) :: (merge (r0,r1))
        | ((x0,s0)::r0, ((x1,s1)::r1 as t)) when s0<s1 -> (x0,s0) :: (merge (r0,t))
        | (t, (x1,s1)::r1) -> (x1,s1) :: (merge (t,r1))
        in (a0+a1, merge (l0,l1))
end 

(*
module AbstractVector3(B:ABSTRACTCODE) = struct
  type ('a,'b) v = ('a, 'b B.b) B.abstract
  type prop = Polyn.polyn
  type ('a,'b) vp = ('a,'b) v * prop
  let value (v,p) = v
  let prop (v,p) = p
  let attach v p = (v,p)
end

module LVector4(B:ABSTRACTCODE ) = struct
  module VP = AbstractVector2(B)
  let s = "s"
  let one = ([1],s)
  let lnil = ([],[])
  let lcons_v h (t,p2) = VP.attach ((B.inject_value h)::t) (Polyn.add one p2)
  let lcons_c h (t,p2) = VP.attach ((B.inject_code h)::t) (Polyn.add one p2)

  let head = function 
      | (v,([p],s)) when p>0 -> List.hd v
      | _                    -> failwith "head on null list"
  let tail = function 
      | (v,([p],s)) when p>0 -> (List.tl v, ([p-1],s))
      | _                    -> failwith "tail on null list"
  let concat (a1,l1) (a2,l2) = (a1@a2, Polyn.add l1 l2)
  let dotprod (v1,l1) = function 
      | (v2,l2) when l1=l2 -> (List.map2 B.binaryop v1 v2, l1)
      | _                  -> failwith "product of 2 unequal vectors"
  let concretize (v,l) = 
      let rec tocode = function
          | []  -> .< [] >.
          | x::t -> .< .~x :: .~(tocode t) >. in
		.< .~(tocode (List.map B.concretize v)) >.
end

module LVC6 = LVector4(TC3)
*)
