module type Atype = sig
  type v
  val f : v -> v
  val x : bool
end 

module type Btype = sig
  type w
  val g : w -> w
end 

module A1 = struct 
    type v = float
    let f = fun x -> x
    let x = true
end

module A2 = struct 
    type v = int
    let f = fun x -> x + 1
    let x = true
end

module B1 = struct 
    type w = int
    let g = fun x -> x
end

module BB(TT:Btype) = struct
    let h = TT.g
end

module CC(TT:Atype) = struct
    let h = TT.f
end

module XX( A:Atype)(B:Btype with type w=A.v) = 
  struct
    let k = 
      if A.x then
        let module C = BB(B) in
            C.h
      else
        let module C = CC(A) in
            C.h
  end

(*  correctly does not work
module YY = XX(A1)(B1) ;; *)
(* but this does *)
module YY = XX(A2)(B1) ;;
YY.k(5) ;;
