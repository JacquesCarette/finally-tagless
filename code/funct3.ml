(* Staged functorial code with a do notation *)

type ('a,'v) state = ('a,'v) code list ;;

let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k) ;;
let ret = retS

(* state monad morphism: get the value *)
let fetch s k = k s (List.hd s)

(* another non-trivial morphism: generate a bit of code *)
let codegen v cf = fun s k -> cf (k s v)

module type DOMAIN = sig
  type v
  val zero : ('a,v) code
  val one  : ('a,v) code
  val plus : ('a,v) code -> ('a,v) code -> 's ->
             ('s -> ('a,v) code -> 'w) -> 'w
end;;

module Domain = struct
  type v = int
  let zero = .< 0 >.
  let one = .< 1 >.
  let plus x y s k = k s .< .~x + .~y>.
end;;

module type OUTPUT = sig
  type res
  type out
  type tdet
  val decl_det : ('a,tdet) state ->
     (('a,tdet) state -> unit -> ('a,'w) code )
     ->('a,'w) code
  val acc_det : ('a,out) code -> ('a,tdet) state ->
     (('a,tdet) state -> unit -> ('a,'w) code)
     -> ('a,'w) code
  val fin_det : ('a,out) code -> ('a,tdet) state ->
     (('a,tdet) state -> ('a,res) code -> 'w) -> 'w
end;;

module NoDetOUTPUT(Dom: DOMAIN) =
 struct
   type out = Dom.v
   type tdet = Dom.v
   type res = Dom.v
   let decl_det s k = k s () (* Monomorphic restriction! *)
   let acc_det v = retS ()
   let fin_det result = mdo {ret result}
end;;

module Gen (Dom: DOMAIN)
(Out: OUTPUT with type out = Dom.v) =
 struct
   type v = Dom.v
   let gen s k =
    let dogen a = mdo {
      ()  <-- Out.decl_det;
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
   let decl_det s k = .<let det = ref ( .~(Dom.one))
                        in .~(k ((.<det>.)::s) ())>.
   let acc_det v = mdo {
     det <-- fetch;
     r   <-- Dom.plus .<! .~det>. v;
     r   <-- codegen () (fun x -> .<begin .~det := .~r; .~x end>. );
     ret r}
   let fin_det result = mdo {
     det <-- fetch;
     ret .< (.~result,! .~det)>. }
end;;

module Gen2 = Gen(Domain)(DetOUTPUT(Domain));;
(* Gen2.gen [] (fun s v -> v);; *)
