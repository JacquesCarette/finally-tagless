(* Staged functorial code with a do notation *)
(*
 $Id$
*)

type ('a,'v) state = ('a,'v) code list ;;

type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k) ;;
let ret = retS

(* state monad morphism: get the value *)
let fetch s k = k s (List.hd s)
let store v s k = k (v::s) ()

(* another non-trivial morphism: generate a bit of code *)
let codegen v cf = fun s k -> cf (k s v)

module type DOMAIN = sig
  type v
  type 'a vc = ('a,v) code
  val zero : 'a vc
  val one  : 'a vc
  val plus : 'a vc -> 'a vc -> ('a vc,'s,'w) monad
end;;

module Domain = struct
  type v = int
  type 'a vc = ('a,v) code
  let zero = .< 0 >.
  let one = .< 1 >.
  let plus x y = ret .< .~x + .~y>.
end;;

module type OUTPUT = sig
  type res
  type out
  type tdet
  type 'a lstate = ('a,tdet) state
  val decl_det : unit -> (unit,'a lstate,('a,'w) code) monad
  val acc_det : ('a,out) code -> (unit, 'a lstate,('a,'w) code) monad
  val fin_det : ('a,out) code -> (('a,res) code,'a lstate,'w) monad
end;;

module NoDetOUTPUT(Dom: DOMAIN) =
 struct
   type out = Dom.v
   type tdet = Dom.v
   type res = Dom.v
   type 'a lstate = ('a,tdet) state
   let decl_det () = ret () (* () to avoid Monomorphic restriction! *)
   let acc_det v   = ret ()
   let fin_det result = mdo {ret result}
end;;

module Gen (Dom: DOMAIN)
(Out: OUTPUT with type out = Dom.v) =
 struct
   type v = Dom.v
   let gen s k =
    let dogen a = mdo {
      ()  <-- Out.decl_det ();
      x   <-- retN Dom.zero;
      y   <-- retN Dom.one;
      ()  <-- Out.acc_det x;
      xy  <-- Dom.plus x y;
      res <-- Dom.plus xy a;
      res <-- Out.fin_det res;
      ret res } in
   .<fun a -> .~(dogen .<a>. s k)>.
end;;

module Gen1 = Gen(Domain)(NoDetOUTPUT(Domain));;
(* Gen1.gen [] (fun s v -> v);; *)

module DetOUTPUT(Dom: DOMAIN) =
 struct
   type out = Dom.v
   type tdet = Dom.v ref
   type res = out * Dom.v (* Now return a tuple; the second comp.
is the determinant *)
   type 'a lstate = ('a,tdet) state
   let decl_det ()  = mdo {
     ddecl <-- retN .<ref ( .~(Dom.one))>.;
     store ddecl }
   let acc_det v = mdo {
     det <-- fetch;
     r   <-- Dom.plus .<! .~det>. v;
     codegen () (fun x -> .<begin .~det := .~r; .~x end>. ) }
   let fin_det result = mdo {
     det <-- fetch;
     ret .< (.~result,! .~det)>. }
end;;

module Gen2 = Gen(Domain)(DetOUTPUT(Domain));;
(* Gen2.gen [] (fun s v -> v);; *)
