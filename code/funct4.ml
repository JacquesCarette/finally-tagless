
(* Base monad type, to be used throughout *)
type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k)
let ret = retS
let liftRef x = .< ref .~x >. 
let liftGet x = .< ! .~x >. 

(* Monad lifting functions *)
let l1 f = fun x -> mdo { t <-- x; f t}
let l2 f = fun x y -> mdo { tx <-- x; ty <-- y; f tx ty}
let l3 f = fun x y z -> mdo { tx <-- x; ty <-- y; tz <-- z; f tx ty tz}

(* Simple state representation for now - upgrades later *)
type ('a,'u, 'v) state = (('a,'u) code * ('a,'v) code) list
(* and 2 morphisms *)
let fetch s k = k s s
let store v s k = k (v::s) ()

(* another non-trivial morphism: generate a bit of code *)
let codegen v cf = fun s k -> cf (k s v)

(* loops actually bind a value *)
let retLoopM low high body = fun s k -> 
    k s .< for j = .~low to .~high do .~(body .<j>. s (fun s v -> v)) done >.

(* while ``loops'' do not naturally bind a value *)
let retWhileM cond body = fun s k -> 
    k s .< while .~cond do .~(body s (fun s v -> v)) done >.

(* sequencing *)
let seq a b = ret .< begin .~a ; .~b end >.

(* conditional *)
let lif test th el = ret .< if .~test then .~th else .~el >.

(* match *)
let retMatchM x som non = fun s k ->
    k s .< match .~x with
           | Some i -> .~(som .<i>. s (fun s v -> v))
           | None   -> .~(non s (fun s v -> v)) >.

(* nothing *)
let retUnit = fun s k -> k s .< () >.

(* the following function should not exist ? *)
let dapply1 g c1 = match g with
  | Some f -> f c1
  | None   -> c1 ;;

(* Define the actual module types and instances *)
module type DOMAIN = sig
  type v
  type 'a vc = ('a,v) code
  val zero : 'a vc
  val one : 'a vc
  val minusone : 'a vc
  val plus : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val times : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val minus : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val div : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val smaller_than : 'a vc -> 'a vc -> (('a,bool) code, 's, 'w) monad 
  val normalizerf : (('a,v -> v) code ) option
  val normalizerg : ('a vc -> 'a vc ) option
end 

module FloatDomain = 
  struct
    type v = float
    type 'a vc = ('a,float) code
    let zero = .< 0. >.  
    let one = .< 1. >. 
    let minusone = .< -1. >. 
    let plus x y = ret .<.~x +. .~y>. 
    let times x y = ret .<.~x *. .~y>.
    let minus x y = ret .<.~x -. .~y>.
    let div x y = ret .<.~x /. .~y>. 
    let smaller_than x y = retS .<abs_float .~x < abs_float .~y >.
    let normalizerf = None 
    let normalizerg = None
end

module IntegerDomain = 
  struct
    type v = int
    type 'a vc = ('a,int) code
    let zero = .< 0 >.  
    let one = .< 1 >. 
    let minusone = .< -1 >. 
    let plus x y = ret .<.~x + .~y>. 
    let times x y = ret .<.~x * .~y>.
    let minus x y = ret .<.~x - .~y>.
    let div x y = ret .<.~x / .~y>. 
    let smaller_than x y = retS .<abs .~x < abs .~y >.
    let normalizerf = None 
    let normalizerg = None
end

module type CONTAINER2D = sig
  type obj
  type contr
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,obj) code
  val get : 'a vc -> ('a,int) code -> ('a,int) code -> ('a vo,'s,'w) monad
  val set : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo -> 
            (('a,unit) code, 's, 'w) monad
  val dim1 : 'a vc -> ('a,int) code
  val dim2 : 'a vc -> ('a,int) code
  val mapper : ('a, obj->obj) code option -> 'a vc -> 'a vc
  val copy : 'a vc -> 'a vc
  val swap_rows_stmt : 'a vc -> ('a, int) code -> ('a, int) code -> 
                       ('a,unit) code
  (* val swap_cols : 'a vc -> ('a, int) code -> ('a, int) code -> 'a vc *)
end

module GenericArrayContainer(Dom:DOMAIN) =
  struct
  type obj = Dom.v
  type contr = obj array array
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,obj) code
  let get x n m = retS .< (.~x).(.~n).(.~m) >.
  let set x n m y = retS .< (.~x).(.~n).(.~m) <- .~y >.
  let dim2 x = .< Array.length .~x >.
  let dim1 x = .< Array.length (.~x).(0) >.
  let mapper g a = match g with
      | Some f -> .< Array.map (fun x -> Array.map .~f x) .~a >.
      | None   -> a
  let copy = (fun a -> .<Array.map (fun x -> Array.copy x) 
                       (Array.copy .~a) >. )
  (* this can be optimized with a swap_rows_from if it is known that
     everything before that is already = *)
  let swap_rows_stmt a r1 r2 =
      .< let t = (.~a).(.~r1) in
         begin 
             (.~a).(.~r1) <- (.~a).(.~r2);
             (.~a).(.~r2) <- t
         end >.
end

module GenericVectorContainer(Dom:DOMAIN) =
  struct
  type obj = Dom.v
  type contr = {arr:(obj array); n:int; m:int}
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,obj) code
  let get x n m = retS .< ((.~x).arr).(.~n* (.~x).n + .~m) >.
  let set x n m y = retS .< ((.~x).arr).(.~n* (.~x).n + .~m) <- .~y >.
  let dim2 x = .< (.~x).n >.
  let dim1 x = .< (.~x).m >.
  let mapper g a = match g with
      | Some f -> .< { (.~a) with arr = Array.map .~f (.~a).arr} >.
      | None   -> a
  let copy a = .< { (.~a) with arr = Array.copy (.~a).arr} >.
  let swap_rows_stmt b r1 r2 = .<
      let a = (.~b).arr and n = (.~b).n and m = (.~b).m in
      let i1 = .~r1*n and i2 = .~r2*n in
      for i = 0 to m -1 do
          let t = a.(i1 + i) in
          begin 
              a.(i2 + i) <- a.(i1 + i);
              a.(i1 + i) <- t
          end
      done  >.
end

module FArrayContainer = GenericArrayContainer(FloatDomain)
module IArrayContainer = GenericArrayContainer(IntegerDomain)
module FVectorContainer = GenericVectorContainer(FloatDomain)
module IVectorContainer = GenericVectorContainer(IntegerDomain)

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

module type LIFTEDCOMMUTATIVEMONOID = sig
    type t
    type ('a,'b) liftedvalues
    val lbinop : ('a,t) liftedvalues -> ('a,t) liftedvalues -> ('a,t) liftedvalues
    val toconcrete : ('a,t) liftedvalues -> ('a,t) code
end

module LiftCommutativeMonoid(M:MONOID) = struct
    type t = M.values 
    type ('a,'b) liftedvalues = ('a, t) abstract
    let generalize x = Ground x
    let mixedop x y =
        if x=M.uunit then y else Code (M.binopC .<x>. .< .~(concretize y) >. )
    let toconcrete (x: ('a,M.values) liftedvalues) = concretize x
    let lbinop x y =
        match (x,y) with
        | (Ground a, Ground b) -> Ground (M.binop a b)
        | (Ground a, b)        -> mixedop a b
        | (a, Ground b)        -> mixedop b a
        | (a, b) -> Code .< .~(M.binopC (concretize a) (concretize b)) >.
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

module LogicGen(Or:LIFTEDCOMMUTATIVEMONOID)(And:LIFTEDCOMMUTATIVEMONOID) = 
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

module type DETERMINANT = sig
  type indet
  type outdet
  type tdet = indet ref
  type 'a lstate
  val decl_det : unit -> 
    (unit, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val upd_sign : unit -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val zero_sign : unit -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val acc_det : ('a,indet) code -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val set_det : ('a,indet) code -> 
    (('a,unit) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val fin_det : unit -> 
    (('a,outdet) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
end

module type RANK = sig
  type 'a lstate
  val rfetch : unit -> 
    (('a,int ref) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
  val decl_rank : unit -> 
    (('a,int ref) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
  val succ_rank : unit -> 
    (('a,unit) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
  val fin_rank : unit ->  
    (('a,int) code,[> `TRan of 'a lstate ] list,('a,'w) code) monad
end;;

module type OUTPUT = sig
  type contr
  type res
  module D : DETERMINANT
  module R : RANK
  val make_result : ('a,contr) code -> 
    (('a,res) code,[> `TDet of 'a D.lstate | `TRan of 'a R.lstate] list,
     ('a,'w) code) monad
end

(* we need the domain anyways to get things to type properly *)
module NoDet(Dom:DOMAIN) =
  struct
  type indet = Dom.v
  type outdet = unit
  type tdet = indet ref
  type 'a lstate
  let decl_det () = ret ()
  let upd_sign () = retUnit
  let zero_sign () = retUnit
  let acc_det v = retUnit
  let set_det v = retUnit
  let fin_det () = retUnit
end

(* Even if no rank is output, it needs to be tracked, as the rank
   is also the outer loop index! *)
module NoRank = struct
  type 'a lstate = ('a, int ref) code
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand teh :> coercion notation *)
  let coerce = function `TRan x -> `TRan x | x -> x
  let rec fetch_iter (s : [> `TRan of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TRan x -> x
    |  _ -> fetch_iter (List.tl s)
  let rfetch () = mdo { s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s) }
  let rstore v = store (`TRan v)
  let decl_rank () = mdo {
      rdecl <-- retN (liftRef .<0>.);
      rstore rdecl;
      ret rdecl }
  let succ_rank () = mdo {
   r <-- rfetch ();
   ret .<.~r := (! .~r) + 1>. }
  let fin_rank () = ret .< -1 >.
end

(* What to return *)

module OutJustMatrix(Ctr: CONTAINER2D)(Det : DETERMINANT with type indet = Ctr.obj) =
  struct
  type contr = Ctr.contr
  type res = contr
  module D = Det
  module R = NoRank
  let make_result b = ret b
end

module OutDet(Ctr: CONTAINER2D)(Det : DETERMINANT with type indet = Ctr.obj) =
  struct
  type contr = Ctr.contr
  type res = contr * Det.outdet
  module D = Det
  module R = NoRank
  let make_result b = mdo {
    det <-- Det.fin_det ();
    ret .< ( .~b, .~det ) >. }
end

module OutRank(Ctr: CONTAINER2D)(Rank : RANK)(Dom:DOMAIN) =
  struct
  type contr = Ctr.contr
  type res = contr * int
  module D = NoDet(Dom)
  module R = Rank
  let make_result b = mdo {
    rank <-- Rank.fin_rank ();
    ret .< ( .~b, .~rank ) >. }
end

module OutDetRank(Ctr: CONTAINER2D)(Det : DETERMINANT with type indet = Ctr.obj)(Rank : RANK) =
  struct
  type contr = Ctr.contr
  type res = contr * Det.outdet * int
  module D = Det
  module R = Rank
  let make_result b = mdo {
    det  <-- Det.fin_det ();
    rank <-- Rank.fin_rank ();
    ret .< ( .~b, .~det, .~rank ) >. }
end


module AbstractDet(Dom: DOMAIN) =
  struct
  type indet = Dom.v
  type outdet = indet
  type tdet = Dom.v ref
  type 'a lstate = ('a,tdet) code * ('a,tdet) code
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand the :> coercion notation *)
  let coerce = function `TDet x -> `TDet x | x -> x
  let rec fetch_iter (s : [> `TDet of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TDet x -> x
    |  _ -> fetch_iter (List.tl s)
  let dfetch () = mdo { s <-- fetch; (* unit for monomorphism restriction *)
                     ret (fetch_iter s) }
  let dstore v = store (`TDet v)
  let decl_det () = mdo {
      ddecl <-- retN (liftRef Dom.one);
      dsdecl <-- retN (liftRef Dom.one);
      dstore (dsdecl,ddecl) }
  let upd_sign () = mdo {
      det <-- dfetch ();
      det1 <-- retS (fst det);
      r <-- Dom.times (liftGet det1) Dom.minusone;
      ret .< .~det1 := .~r>. }
  let zero_sign () = mdo {
      det <-- dfetch ();
      det1 <-- retS (fst det);
      ret .<.~det1 := .~Dom.zero>. }
  let acc_det v = mdo {
      det <-- dfetch ();
      det2 <-- retS (snd det);
      r <-- Dom.times (liftGet det2) v;
      ret .<.~det2 := .~r>. }
  let set_det v = mdo {
      det <-- dfetch ();
      det2 <-- retS (snd det);
      ret .<.~det2 := .~v>.}
  let fin_det () = mdo {
      det <-- dfetch ();
      res <-- Dom.times (liftGet (fst det)) (liftGet (snd det));
      ret res}
end

module FDet = AbstractDet(FloatDomain)
module IDet = AbstractDet(IntegerDomain)

module Rank = 
  struct
  type 'a lstate = ('a, int ref) code
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand teh :> coercion notation *)
  let coerce = function `TRan x -> `TRan x | x -> x
  let rec fetch_iter (s : [> `TRan of 'a lstate] list) =
    match (coerce (List.hd s)) with
      `TRan x -> x
    |  _ -> fetch_iter (List.tl s)
  let rfetch () = mdo { s <-- fetch; (* unit for monomorphism restriction *)
                        ret (fetch_iter s) }
  let rstore v = store (`TRan v)
  let decl_rank () = mdo {
      rdecl <-- retN (liftRef .<1>.);
      rstore rdecl;
      ret rdecl }
  let succ_rank () = mdo {
   r <-- rfetch ();
   ret .<.~r := (! .~r) + 1>. }
  let fin_rank () = mdo {
   r <-- rfetch ();
   retS (liftGet r) }
end

module Gen(Dom: DOMAIN)(Ctr: CONTAINER2D with type obj = Dom.v)(Out: OUTPUT with type contr = Ctr.contr and type D.indet = Ctr.obj) = 
   struct
    type v = Dom.v
    let gen ~fracfree =
      let findpivot b r m n c = mdo {
          i <-- retN (liftRef .< -1 >. );
          let bd j = mdo {
              bjc <-- Ctr.get b j c;
              l2 (lif .< not ( .~bjc = .~Dom.zero) >.)
                 (mdo { 
                       iv <-- retS (liftGet i);
                       l3 lif 
                           (Logic.mcode_or (ret (Code .< .~iv == -1 >.))
                                (mdo { bic <-- Ctr.get b iv c;
                                       res <-- Dom.smaller_than bjc bic;
                                       ret (Code res)}))
                           (ret .< .~i := .~j >.)
                           (retUnit) })
                 (retUnit) } in;
          l2 seq (retLoopM r .<.~n-1>. bd)
                 (ret .< if (! .~i) == -1 then None else Some ! .~i >.)} 
      and zerobelow b r c m n =
        let inner_loop i = 
          let body k = 
              (if not fracfree then 
                mdo {
                  bic <-- Ctr.get b i c;
                  brc <-- Ctr.get b r c;
                  brk <-- Ctr.get b r k;
                  bik <-- Ctr.get b i k;
                  t <-- Dom.div bic brc;
                  l <-- Dom.times t brk;
                  y <-- Dom.minus l bik;
                  Ctr.set b i k (dapply1 Dom.normalizerg y) }
              else 
                mdo {
                  bic <-- Ctr.get b i c;
                  brc <-- Ctr.get b r c;
                  brk <-- Ctr.get b r k;
                  bik <-- Ctr.get b i k;
                  x <-- Dom.times bik brc;
                  y <-- Dom.times brk bic;
                  z <-- Dom.minus x y;
                  t <-- retS (dapply1 Dom.normalizerg z);
                  ov <-- Dom.div t brk;
                  Ctr.set b i k ov}) in
              mdo { retLoopM .<.~c+1>. .<.~m-1>. body } in
          let innerbody i = mdo {
              bic <-- Ctr.get b i c;
              il <-- inner_loop i;
              l3 lif (ret .< not (.~bic = .~Dom.zero) >. )
                     (l2 seq (ret il) (Ctr.set b i c Dom.zero)) 
                     (retUnit) } in
          mdo {
              let ch = 
              if fracfree then
                  mdo { 
                      brc <-- Ctr.get b r c;
                      Out.D.set_det brc }
              else
                  mdo { 
                      brc <-- Ctr.get b r c;
                      Out.D.acc_det brc } in;
              l2 seq (retLoopM .<.~r+1>. .<.~n-1>. innerbody) ch } in
      let somg b c m n = fun i -> mdo {
          r <-- Out.R.rfetch ();
          l2 seq (mdo {
                  l3 lif (ret .< .~i <> ! .~r >. )
                         (ret (Ctr.swap_rows_stmt b (liftGet r) i))
                         (retUnit) } )
                 (l2 seq (zerobelow b (liftGet r) (liftGet c) m n)
                         (Out.R.succ_rank ())) }
      and non = Out.D.zero_sign () in
      let dogen a = mdo {
          r <-- Out.R.decl_rank ();
          c <-- retN (liftRef .< 0 >.);
          b <-- retN (Ctr.mapper Dom.normalizerf (Ctr.copy a));
          m <-- retN (Ctr.dim1 a);
          n <-- retN (Ctr.dim2 a);
          () <-- Out.D.decl_det ();
          let body = mdo {
              pivot <-- l1 retN (findpivot b (liftGet r) m n (liftGet c));
              l2 seq (retMatchM pivot (somg b c m n) non)
                     (ret .< .~c := (! .~c) + 1 >. ) } in ;
          l2 seq (retWhileM .< !(.~r) < .~m && !(.~r) < .~n >.
                    body)
                 (Out.make_result b) } 
      and kk s v = v in
    .<fun a -> .~(dogen .<a>. [] kk) >.
end

module GenFA1 = Gen(FloatDomain)(FArrayContainer)(OutJustMatrix(FArrayContainer)(NoDet(FloatDomain)))
module GenFA2 = Gen(FloatDomain)(FArrayContainer)(OutDet(FArrayContainer)(FDet))
module GenFA3 = Gen(FloatDomain)(FArrayContainer)(OutRank(FArrayContainer)(Rank)(FloatDomain))
module GenFA4 = Gen(FloatDomain)(FArrayContainer)(OutDetRank(FArrayContainer)(FDet)(Rank));;
module GenFV1 = Gen(FloatDomain)(FVectorContainer)(OutJustMatrix(FVectorContainer)(NoDet(FloatDomain)))
module GenFV2 = Gen(FloatDomain)(FVectorContainer)(OutDet(FVectorContainer)(FDet))
module GenFV3 = Gen(FloatDomain)(FVectorContainer)(OutRank(FVectorContainer)(Rank)(FloatDomain))
module GenFV4 = Gen(FloatDomain)(FVectorContainer)(OutDetRank(FVectorContainer)(FDet)(Rank));;
module GenIA1 = Gen(IntegerDomain)(IArrayContainer)(OutJustMatrix(IArrayContainer)(NoDet(IntegerDomain)));;
module GenIA2 = Gen(IntegerDomain)(IArrayContainer)(OutDet(IArrayContainer)(IDet));;
module GenIA3 = Gen(IntegerDomain)(IArrayContainer)(OutRank(IArrayContainer)(Rank)(IntegerDomain));;
module GenIA4 = Gen(IntegerDomain)(IArrayContainer)(OutDetRank(IArrayContainer)(IDet)(Rank));;
module GenIV1 = Gen(IntegerDomain)(IVectorContainer)(OutJustMatrix(IVectorContainer)(NoDet(IntegerDomain)));;
module GenIV2 = Gen(IntegerDomain)(IVectorContainer)(OutDet(IVectorContainer)(IDet));;
module GenIV3 = Gen(IntegerDomain)(IVectorContainer)(OutRank(IVectorContainer)(Rank)(IntegerDomain));;
module GenIV4 = Gen(IntegerDomain)(IVectorContainer)(OutDetRank(IVectorContainer)(IDet)(Rank));;
