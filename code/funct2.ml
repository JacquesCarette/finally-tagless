(* Edited to use camlp4 friendly syntax and to test the do notation *)
module type DOMAIN = sig
  type v
  val zero : ('a,v) code
  val one  : ('a,v) code
  val plus : ('a,v) code -> ('a,v) code -> 's ->
             ('s -> ('a,v) code -> 'w) -> 'w
end;;

module Domain = struct
  type v = int
  let zero = brackets 0
  let one = brackets 1
  let plus x y s k = k s (brackets ((escape x) + (escape y)))
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

let retS a = fun s k -> k s a
let retN a = fun s k -> brackets let t = escape a in escape (k s (brackets t))
let bind a f = fun s k -> a s (fun s' b -> f b s' k) ;;

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
brackets fun a -> escape (dogen (brackets a) s k)
end;;

module Gen1 = Gen(Domain)(NoDetOUTPUT(Domain));;

module DetOUTPUT(Dom: DOMAIN) =
 struct
   type out = Dom.v
   type tdet = Dom.v ref
   type res = out * Dom.v (* Now return a tuple; the second comp.
is the determinant *)
   let decl_det s k = brackets let det = ref (escape Dom.one) in (escape (k ((brackets det)::s) ()))
   let acc_det v s k = let det = List.hd s in
    bind (Dom.plus (brackets ! (escape det)) v) (fun r s k ->
    brackets begin (escape det) := (escape r);
    escape (k s ()) end) s k
       let fin_det result s k = let det = List.hd s in
    k s (brackets ((escape result),! (escape det)))
end;;

module Gen2 = Gen(Domain)(DetOUTPUT(Domain));;
