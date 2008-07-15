(* Open Record: an extensible collection of arbitrarily typed items
  with first-class `labels'. One can at any point generate a new label
  value, which is guranteed to be unique.
*)

type univ				(* Universal type *)

module UGen : functor (S:sig type t end) ->
                 sig val inj : S.t -> univ
		     val prj : univ -> S.t option
		 end

type openrec				(* abstract *)

val new_openrec : unit -> openrec

val add : univ -> openrec -> openrec

val find : (univ -> 'a option) -> openrec -> 'a

