let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k) ;;

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
  let plus x y s k = k s .<.~x + .~y>.
end;;

type ('a,'v) state = ('a,'v) code list ;;

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
   let decl_det s k = k s ()
   let acc_det v s k = k s ()
   let fin_det result s k = k s result
end;;

module Gen (Dom: DOMAIN)
(Out: OUTPUT with type out = Dom.v) =
 struct
   type v = Dom.v
   let gen s k =
    let dogen a =
    bind (Out.decl_det) (fun () ->
    bind (retN Dom.zero) (fun x ->
    bind (retN Dom.one) (fun y ->
    bind (Out.acc_det x) (fun () ->
    bind (Dom.plus x y) (fun xy ->
    bind (Dom.plus xy a) (fun res ->
    Out.fin_det res
    ))))))
in
.<fun a -> .~(dogen .<a>. s k)>.
end;;

module Gen1 = Gen(Domain)(NoDetOUTPUT(Domain));;
Gen1.gen [] (fun s v -> v);;

module DetOUTPUT(Dom: DOMAIN) =
 struct
   type out = Dom.v
   type tdet = Dom.v ref
   type res = out * Dom.v (* Now return a tuple; the second comp.
is the determinant *)
   let decl_det s k = .<let det = ref .~(Dom.one) in .~(k (.<det>.::s) ())>.
   let acc_det v s k = let det = List.hd s in
    bind (Dom.plus .<! .~det>. v) (fun r s k ->
    .<begin .~det := .~r;
    .~(k s ()) end>.) s k
       let fin_det result s k = let det = List.hd s in
    k s .<(.~result,! .~det)>.
end;;

module Gen2 = Gen(Domain)(DetOUTPUT(Domain));;
Gen2.gen [] (fun s v -> v);;
