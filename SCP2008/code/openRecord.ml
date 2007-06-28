(* Open Record: an extensible collection of arbitrarily typed items
  with first-class `labels'. One can at any point generate a new label
  value, which is guranteed to be unique.
  Our original approach, with extensible polymorphic variants, was
  one implementation of that idea. Alas, that approach had drawbacks:
  there is no gurantee that tags must be unique; the types of polymorphic
  variants don't mesh well with modudes (polymorphic variants can't be
  easily put into signatures) -- and that typing still didn't prevent an
  error that a particular list of polymorphic variants may lack the desired
  slot.

  In the present version, our extensible record is a list of values of
  the universal type. The typing layer can be added later on, via a 
  state-changing monad and phantom extensible record type.

  It seems that our approach is very close to that of PropertyLists of
  MLton (modulo the differences between OCaml and SML).

  http://mlton.org/PropertyList

 $Id$
*)

type univ = exn				(* The universal type *)
;;

module UGen0(S: sig type t end) = struct
  exception E of S.t
  let inj x = E x
  let prj x = match x with E x -> Some x | _ -> None
end;;

(* The reason for this sharade is to ensure that the argument of
   UGen0 is an anonymous signature. That ensures generativity.
*)
module UGen(S: sig type t end) = UGen0(struct type t = S.t end);;


type openrec = univ list

let new_openrec () = []

let add x (r:openrec) = x::r

let rec find prj = function
  | []     -> raise Not_found
  | (h::t) -> (match prj h with 
                Some x -> x
              | None -> find prj t)
;;


let test1 = 
      let module MINT = struct type t = int end in
      let module M1 = UGen(MINT) in
      let module M2 = UGen(MINT) in
      let module M3 = UGen(struct type t = float end) in
      let r = add (M3.inj 10.0)
	       (add (M2.inj 42)
		 (add (M1.inj 5)
		   (new_openrec ()))) in
      let v1 = find M1.prj r and
	  v2 = find M2.prj r and
	  v3 = find M3.prj r in
      (v1,v2,v3)
;;

      
