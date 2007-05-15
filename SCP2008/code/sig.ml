(* Various experiments regarding signatures *)

module M1(S: sig type a end) = struct
  type a = S.a
  type d = (a * a) list
  module type M1S = sig
    type a
    val make  : unit -> d
    val add   : a -> d -> d
    val count : d -> int
  end
end;;

module M1Int = struct
  include M1(struct type a = int end)
  let make () = [(4,2)]
  let add x d = (x,x) :: d
  let count = function [] -> 0 | (x,y)::_ -> x
end;;

module MMain(MP: sig type a end)(M: M1(MP).M1S) = struct
  let res = M.count (M.make ())
end;;

let res = let module I = MMain(M1Int)(M1Int) in I.res;;


module type S1 = sig type a type b = a list end;;

module type S2 = sig
  include S1 with type a = int
end;;

type ('v,'s,'w) mnd = 'v * 's list;;
type ev = private [>]
type ('b,'v) lm = ('v,ev,'w) mnd
    constraint 'b = 'w
;;

module type DET = sig
  type outdet
  type state
  val decl : unit -> ('b,outdet) lm
end;;

module DET1 = struct
  type outdet = float
  type state = int * int
  (* let decl () = ((1.0, [`TDet (1,2)]) :> ('b,outdet) lm) *)
  let decl () = (1.0, [`TDet (1,2)])
end;;

module Gen(D:DET) = struct
  let gen () = begin D.decl (); () end
end;;

module GenD = Gen(DET1);;


type ('a,'b,'c,'d) cmonad = M of 'a * 'b * 'c * 'd;;


module type COMMON = sig
  type 'a tag_lstate = private [>]
  type ('b,'v) lm = ('a,'v,'a tag_lstate,'w) cmonad
    constraint 'b = 'a  * 'w
  val foo : unit -> ('a,unit) lm
end;;


module type DETERMINANT = sig
  type 'a lstate
  include COMMON with type ('a) tag_lstate = private [> `TDet of 'a lstate ]
end;;

(*

module type COMMON = sig
  type ('a,+'s) tag_lstate = 's constraint 's = [> ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 'b = 'a * ('a,'s) tag_lstate * 'w
  val foo : unit -> ('a,unit) lm
end;;


module type DETERMINANT = sig
  type 'a lstate
  include COMMON with type ('a,'s) tag_lstate = 's constraint 's = [> `TDet of 'a lstate ]
end;;

module type RANK = sig
  type 'a lstate = ('a, int ref) code
  type 'a tag_lstate = [`TRan of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
  ...
end
module type TRACKPIVOT = sig
  type 'a lstate
  type 'a tag_lstate = [`TPivot of 'a lstate ]
  type ('b,'v) lm = ('a,'v,'s,'w) cmonad
    constraint 's = [> 'a tag_lstate]
    constraint 'b = 'a * 's * 'w
 ...
end
*)
