(* Pure translation to mdo notation.  No use of modules or functors *)

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
let retLoop low high (body:('a,'b) code -> ('a,'c) code)  = fun s k -> 
    k s .< for j = .~low to .~high do .~(body .<j>.) done >.
let retLoopM low high body = fun s k -> 
    k s .< for j = .~low to .~high do .~(body .<j>. s (fun s v -> v)) done >.

(* while ``loops'' do not naturally bind a value *)
let retWhile cond (body:('a,'b) code) = fun s k -> 
    k s .< while .~cond do .~body done >.
let retWhileM cond body = fun s k -> 
    k s .< while .~cond do .~(body s (fun s v -> v)) done >.

(* sequencing *)
let seq a b = ret .< begin .~a ; .~b end >.

(* conditional *)
let lif test th el = ret .< if .~test then .~th else .~el >.

let llor x y = ret .< .~x || .~y >.


(* match *)
let retMatchM x som non = fun s k ->
    k s .< match .~x with
           | Some i -> .~(som s (fun s v -> v))
           | None   -> .~(non s (fun s v -> v)) >.

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
end

module FArrayContainer = GenericArrayContainer(FloatDomain)
module IArrayContainer = GenericArrayContainer(IntegerDomain)

module type MONOID = sig
    type values
    val uunit : values
    val binop : values -> values -> values
end

module LiftCommutativeMonoid(M:MONOID) = struct
    type 'a lifted = Ground of M.values | Code of ('a, M.values) code
    let concretize = function
        | Ground x -> .<x>.
        | Code x   -> x
    let generalize x = Ground x
    let binop2 x y = .< M.binop .~x .~y >.
    let mixedop x y =
        if x=M.uunit then y else Code (binop2 .<x>. .< .~(concretize y) >. )
    let binop x y =
        match (x,y) with
        | (Ground a, Ground b) -> Ground (M.binop a b)
        | (Ground a, b)        -> mixedop a b
        | (a, Ground b)        -> mixedop b a
        | (a, b) -> Code .< .~(binop2 (concretize a) (concretize b)) >.
end

(*
module Logic = 
    struct
    let code_or a b = retS (if a = .< true >. then b
                      else if b = .< true >. then a
                      else .< .~a || .~b >. )
end
*)

module type DETERMINANT = sig
  type idet
  type tdet = idet ref
  type 'a lstate
  val decl_det : unit -> 
    (unit, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val upd_sign : unit -> 
    (unit, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val acc_det : ('a,idet) code -> 
    (unit, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
  val fin_det : unit -> 
    (('a,idet) code, [> `TDet of 'a lstate ] list, ('a,'b) code) monad
end

module type RANK = sig
  type 'a lstate
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

module NoDet = 
  struct
  type idet = unit
  type tdet = unit ref
  type 'a lstate
  let decl_det () = ret ()
  let upd_sign () = ret ()
  let acc_det v = ret ()
  let fin_det () = ret .<()>.
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

module OutJustMatrix(Ctr: CONTAINER2D) =
  struct
  type contr = Ctr.contr
  type res = contr
  module D = NoDet
  module R = NoRank
  let make_result b = ret b
end

module OutDet(Ctr: CONTAINER2D)(Det : DETERMINANT with type idet = Ctr.obj) =
  struct
  type contr = Ctr.contr
  type res = contr * Det.idet
  module D = Det
  module R = NoRank
  let make_result b = mdo {
    det <-- Det.fin_det ();
    ret .< ( .~b, .~det ) >. }
end

module OutRank(Ctr: CONTAINER2D)(Rank : RANK) =
  struct
  type contr = Ctr.contr
  type res = contr * int
  module D = NoDet
  module R = Rank
  let make_result b = mdo {
    rank <-- Rank.fin_rank ();
    ret .< ( .~b, .~rank ) >. }
end

module OutDetRank(Ctr: CONTAINER2D)(Det : DETERMINANT with type idet = Ctr.obj)(Rank : RANK) =
  struct
  type contr = Ctr.contr
  type res = contr * Det.idet * int
  module D = Det
  module R = Rank
  let make_result b = mdo {
    det  <-- Det.fin_det ();
    rank <-- Rank.fin_rank ();
    ret .< ( .~b, .~det, .~rank ) >. }
end


module AbstractDet(Dom: DOMAIN) =
  struct
  type idet = Dom.v
  type tdet = Dom.v ref
  type 'a lstate = ('a,tdet) code * ('a,tdet) code
        (* the purpose of this function is to make the union open.
           Alas, Camlp4 does not understand teh :> coercion notation *)
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
      codegen () (fun x -> .<begin .~det1 := .~r; .~x end>. ) }
  let acc_det v = mdo {
      det <-- dfetch ();
      det2 <-- retS (snd det);
      r <-- Dom.times (liftGet det2) v;
      codegen () (fun x -> .<begin .~det2 := .~r; .~x end>. ) }
  let fin_det () = mdo {
      det <-- dfetch ();
      ret (liftGet (snd det)) }
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


module Gen(Dom: DOMAIN)(Ctr: CONTAINER2D with type obj = Dom.v)(Out: OUTPUT with type contr = Ctr.contr) = 
   struct
    type v = Dom.v
    let gen ~fracfree:bool  =
      let findpivot b r n c = mdo {
          i <-- retN (liftRef .< -1 >. );
          let bd j = mdo {
              bjc <-- Ctr.get b j c;
              l2 (lif .< not ( .~bjc = .~Dom.zero) >.)
                 (mdo { 
                       iv <-- retS (liftGet i);
                       l3 lif 
                                 (mdo { l2 llor (ret .< .~iv == -1 >.)
                                        (mdo { bic <-- Ctr.get b iv c;
                                               Dom.smaller_than bjc bic }) })
                                 (ret .< .~i := .~j >.)
                           (ret .< () >.) })
                 (ret .< () >.) } in;
          l2 seq (retLoopM r .<.~n-1>. bd)
                 (ret .< if (! .~i) == -1 then None else Some ! .~i >.)} in
      let swapvals track b r i j = mdo {
          t <-- l1 retN (Ctr.get b i j);
          brj <-- Ctr.get b r j;
          l2 seq (Ctr.set b i j brj) (Ctr.set b r j t) } in
      let som = mdo { ret .< () >. } in
      let non = mdo { ret .< () >. } in
      let dogen a = mdo {
          r <-- Out.R.decl_rank ();
          (* r <-- retN (liftRef .< 0 >.); *)
          c <-- retN (liftRef .< 0 >.);
          b <-- retN (Ctr.mapper Dom.normalizerf (Ctr.copy a));
          m <-- retN (Ctr.dim1 a);
          n <-- retN (Ctr.dim2 a);
          () <-- Out.D.decl_det ();
          let body = mdo {
              pivot <-- l1 retN (findpivot b (liftGet r) n (liftGet c));
              l2 seq (retMatchM pivot som non)
                     (Out.R.succ_rank ()) } in ;
          cond <-- retS .< !(.~r) < .~m && !(.~r) < .~n >.;
          l2 seq (retWhileM cond body)
                 (Out.make_result b) } 
      and kk s v = v in
    .<fun a -> .~(dogen .<a>. [] kk) >.
end

(* ret (match track with
            | TrackNothing -> swap
            | TrackDet     -> t ) } in *)

module Gen1 = Gen(FloatDomain)(FArrayContainer)(OutJustMatrix(FArrayContainer))
module Gen2 = Gen(FloatDomain)(FArrayContainer)(OutDet(FArrayContainer)(FDet))
module Gen3 = Gen(IntegerDomain)(IArrayContainer)(OutRank(IArrayContainer)(Rank));;
module Gen4 = Gen(IntegerDomain)(IArrayContainer)(OutDetRank(IArrayContainer)(IDet)(Rank));;


type outchoice = JustMatrix | Rank | Det | RankDet;;
type dettrack = TrackNothing | TrackDet ;;

type ge_choices = 
  {fracfree:bool; track:dettrack; outputs:outchoice} ;;

(*
let dapply1 g c1 = match g with
  | Some f -> f c1
  | None   -> c1 ;;

let ge_state_gen dom contr findpivot swap zerobelow choice =
  let main_loop s = 
  .< while !(.~(s.c)) < .~(s.m) && !(.~(s.r)) < .~(s.n) do
    begin
    match .~(findpivot s) with
    Some i -> begin
        if i <> !(.~(s.r)) then
            for j = !(.~(s.c)) to .~(s.m)-1 do
                .~(swap .<i>. .<j>. s);
            done;
        .~(zerobelow s) ;
        (.~(s.r)) := !(.~(s.r)) + 1;
        end;
    | None -> .~(match choice.track with
        | TrackNothing -> .< () >. ;
        | TrackDet     -> .< .~(s.detsign) := .~(dom.zero) >.;
        ) ;
    end;
    .~(s.c) := !(.~(s.c)) + 1;
  done >. in


let zerobelow_gen dom contr choice =
  let inner_loop i = 
  if not choice.fracfree then 
    let body = fun ind -> mdo {
      st <-- fetch;
      t <-- dom.div
          (contr.get .<.~st.b>. i .<!(.~st.c)>. )
          (contr.get .<.~st.b>. .<!(.~st.r)>. .<!(.~st.c)>. );
      l <-- dom.times t (contr.get .<.~st.b>. .<!(.~st.r)>. ind );
      m <-- dom.minus (contr.get .<.~st.b>. i ind) l;
      r <-- retN (contr.set .<.~st.b>. .<.~i>. ind 
          (dapply1 dom.normalizerg m)) ;
      ret (r:('a,unit) code) } in
      mdo {
          st <-- fetch;
          loop <-- retLoop .<!(.~st.c)+1>. .< .~st.m-1>. body;
          ret (loop:('a,unit) code) }
  else 
    let body ind = mdo {
      st <-- fetch;
      x <-- (dom.times 
          (contr.get .<.~st.b>. .<.~i>. ind )
          (contr.get .<.~st.b>. .<! .~st.r>. .<! .~st.c>. ));
      y <-- (dom.times 
          (contr.get .<.~st.b>. .<!(.~st.r)>. ind )
          (contr.get .<.~st.b>. .<.~i>. .<! .~st.r>. ));
      z <-- dom.minus x y;
      t <-- retS (dapply1 dom.normalizerg z);
      ov <-- dom.div t .<! .~st.det>. ;
      x <-- retN (contr.set .<.~st.b>. .<.~i>. ind ov);
      ret (x:('a,unit) code) } in
      mdo {
          st <-- fetch;
          loop <-- retLoop .<!(.~st.c)+1>. .< .~st.m-1>. body;
          ret loop }
  in
  let il1 i = mdo {
    x <-- inner_loop i;
    ret (x:('a,unit) code)} 
  and ss j = mdo {
    st <-- fetch;
    x <-- retS (contr.set .<.~st.b>. j .<! .~st.c>. dom.zero);
    ret (x:('a,unit) code) } in
  let outer_loop' = mdo {
    st <-- fetch;
    r <-- (fun s k -> .< for ii = !(.~st.r)+1 to .~st.n-1 do
    if not ( .~(contr.get .<.~st.b>. .<ii>. .<! .~st.c>. ) 
             = .~dom.zero) then
      begin .~(il1 .<ii>. st k); .~(ss .<ii>. st k) end;
  done >. );
    ret r } in
  match choice.track with
  | TrackNothing -> outer_loop'
  | TrackDet     -> mdo {
      st <-- fetch;
      ol <-- outer_loop';
      rest <-- (if choice.fracfree then
          retS .< .~st.det := 
            .~(contr.get .<.~st.b>. .<! .~st.r>. .<! .~st.c>. )>.
        else
          mdo {
               xx <-- dom.times .<! .~st.det>.
                  (contr.get .< .~st.b>.  .<! .~st.r>. .<! .~st.c>. ) ;
               yy <-- retS .< .~st.det := .~xx >. ;
               ret yy} );
      r <-- seq ol rest;
      ret r }

let specializer dom container ~fracfree ~outputs =
  let t = (if fracfree || outputs == Det 
                       || outputs == RankDet then 
      TrackDet else TrackNothing ) in
  let choice = {fracfree=fracfree; track=t; outputs=outputs} in
  let gen = mdo {
      fp <-- findpivot_gen dom container orcond_gen;
      swap <-- swapr_gen dom container choice.track;
      zb <-- zerobelow_gen dom container choice;
      res <-- ge_state_gen dom container fp (swap) (zb) choice;
      return res } in
  gen [] (fun s v -> v) ;;

let ge_float3 = .! spec_ge_float ;;
*)
