(* Pure translation to mdo notation.  No use of modules or functors *)

type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k) ;;
let ret = retS

(* state monad morphisms: 
 * get the values of the state which is still a record *)
type ('a,'c,'d) state = {b:('a,'c) code; 
  r:('a,int ref) code; c:('a,int ref) code; 
  m:('a,int) code; n:('a,int) code;
  det:('a,'d ref) code; detsign:('a,'d ref) code}

let fetch s k = k s s

(* another non-trivial morphism: generate a bit of code *)
let codegen v cf = fun s k -> cf (k s v)

let retLoop l h x = fun s k -> 
    .< for j = .~l to .~h do .~(x .<j>. s k) done >.

module type DOMAIN = sig
  type v
  type 'a vc = ('a,v) code
  val zero : 'a vc
  val one : 'a vc
  val minusone : 'a vc
  val plus : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val times : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val times : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val minus : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val div : 'a vc -> 'a vc -> ('a vc, 's, 'w) monad
  val smaller_than : ('a vc -> 'a vc -> ('a,bool) code) option
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
    let smaller_than = 
       Some(fun x y -> .<abs_float .~x < abs_float .~y >.)
    let normalizerf = None 
    let normalizerg = None
end

module type CONTAINER2D = sig
  type obj
  type contr
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,obj) code
  val get : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo
  val set : 'a vc -> ('a,int) code -> ('a,int) code -> 'a vo -> ('a,unit) code
  val dim1 : 'a vc -> ('a,int) code
  val dim2 : 'a vc -> ('a,int) code
  val mapper : ('a, obj->obj) code -> 'a vc -> 'a vc
  val copy : 'a vc -> 'a vc
end

module ArrayContainer =
  struct
  type obj = float
  type contr = float array array
  type 'a vc = ('a,contr) code
  type 'a vo = ('a,obj) code
  let get = (fun x n m -> .< (.~x).(.~n).(.~m) >. )
  let set = (fun x n m y -> .< (.~x).(.~n).(.~m) <- .~y >. )
  let dim2 = (fun x -> .< Array.length .~x >.)
  let dim1 = (fun x -> .< Array.length (.~x).(0) >. )
  let mapper = (fun f a -> .< Array.map 
      (fun x -> Array.map .~f x) .~a >.)
  let copy = (fun a -> .<Array.map (fun x -> Array.copy x) 
                       (Array.copy .~a) >. )
end

module type OUTPUT = sig
  type res
  type out
  type tdet
  type ('a,'c) lstate = ('a,'c,tdet) state
  val decl_det : unit -> (unit,('a,'c) lstate,('a,'w) code) monad
  val acc_det : ('a,out) code -> (unit,('a,'c) lstate,('a,'w) code) monad
  val fin_det : ('a,out) code -> ('a,res) code -> (('a,res) code,('a,'c) lstate,'w) monad
end;;

module NoDetOUTPUT(Dom: DOMAIN)(Ctr: CONTAINER2D with type obj = Dom.v) =
  struct
  type res = Ctr.contr
  type out = Dom.v
  type tdet = Dom.v
  type ('a,'c) lstate = ('a,'c,tdet) state
  let decl_det () = ret ()
  let acc_det v = ret ()
  let fin_det det res = mdo {ret res}
end

module type RANK = sig
  val succ_rank : unit -> (unit,('a,'c,'d) state,('a,int) code) monad
  val fin_rank : unit -> (('a,int) code,('a,'c,'d) state,int) monad
end;;

module Rank = 
  struct
  let succ_rank () = mdo {
      s <-- fetch;
      res <-- retS .<(! .~s.r) + 1>. ;
      codegen () (fun x -> .<begin .~(s.r) := .~res; .~x end>. ) }
  let fin_rank () = mdo {
      s <-- fetch;
      codegen () (fun _ -> .< .~(s.r) >. ) }
end

module Gen(Dom: DOMAIN)(Ctr: CONTAINER2D with type obj = Dom.v)(Out: OUTPUT with type res = Ctr.contr and type out = Dom.v and type tdet = Dom.v) =
  struct
    type v = Dom.v
    let gen =
      let dogen a = mdo {
        () <-- Out.decl_det ();
        det <-- retN Dom.minusone;
        res <-- Out.fin_det det a;
        ret res }
      and state a = {b = a; r = .< ref 0 >.; c = .< ref 0 >.;
        m = .<0>.; n = .<0>.; det = .< ref .~Dom.one >.;
        detsign = .< ref .~Dom.one >. } 
      and k = (fun s v -> v) in
    .<fun a -> .~(dogen .<a>. (state .<a>.) k) >.
end

module Gen1 = Gen(FloatDomain)(ArrayContainer)(NoDetOUTPUT(FloatDomain)(ArrayContainer));;

type outchoice = JustMatrix | Rank | Det | RankDet;;
type dettrack = TrackNothing | TrackDet ;;

type ge_choices = 
  {fracfree:bool; track:dettrack; outputs:outchoice} ;;

let orcond_gen main_cond = function
  | Some alt_cond -> .< .~main_cond || .~alt_cond >.
  | None -> main_cond ;;

let seq a b s k = k s .< begin .~a ; .~b end >. ;;
let seq' a b  = .< begin .~a ; .~b end >. ;;

let sapply2 g c1 c2 = match g with
  | Some f -> Some (f c1 c2)
  | None   -> None ;;

let dapply1 g c1 = match g with
  | Some f -> f c1
  | None   -> c1 ;;

let mdapply1 mapper g c1 = match g with
  | Some f -> .< .~(mapper f c1) >.
  | None   -> c1 ;;

(*
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

  let gen st k = 
      let dogen a = mdo {
          r <-- retS ref 0;
          c <-- retS ref 0;
          b <-- retS (mdapply1 contr.mapper dom.normalizerf (contr.copy a));
          m <-- retS (contr.dim1 a);
          n <-- retS (contr.dim2 a);
          det <-- retS ref dom.one;
          detsign <-- retS ref dom.one;
          i <-- retS .<0>.;
          j <-- retS .<0>.;
          st <-- retS {b=b; r=r; c=c; m=m; n=n; det=det; detsign=detsign};
          code <-- codegen st (fun st ->
            seq
              (main_loop st )
              (choose_output dom st choice.outputs ) );
           ret code } in
        .<fun a -> .~(dogen .<a>. st k) >.
   in gen ;;

let swapr_gen dom contr track = fun i j -> 
  mdo {
     s <-- fetch;
     swap <-- retS 
      .< let b = .~(s.b) and r = !(.~(s.r)) and i= .~i 
         and j= .~j in
          let t = .~(contr.get .<b>. .<i>. .<j>. ) in  
            seq
                (contr.set .<b>. .<i>. .<j>. 
                   (contr.get .<b>. .<r>. .<j>. ))
                (contr.set .<b>. .<r>. .<j>. .<t>. )
      >. ;
    r <-- dom.times .<! .~(s.detsign)>. dom.minusone;
    t <-- retS .<begin .~(s.detsign) := .~r; .~swap end>. ;
    ret (match track with
    | TrackNothing -> swap
    | TrackDet     -> t ) } ;;

let findpivot_gen dom contr orcond_gen = fun s k ->
  .< let i = ref (-1) in begin 
    for j = ! .~s.r to .~s.n-1 do
      if not ( .~(contr.get .< .~s.b>. .<j>. 
                 .<!( .~s.c)>. ) = .~(dom.zero)) then 
        if (.~(orcond_gen .< !i = (-1) >.
          (sapply2 dom.smaller_than 
            .< .~(contr.get .< .~s.b>. .<j>. .<! .~s.c>. )>.
            .< .~(contr.get .< .~s.b>. .<!i>. .<! .~s.c>. )>.
            ))) then
          i := j;
      done;
      if !i == -1 then None else Some !i;
  end >. ;;

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


let spec_ge_float = specializer dom_float array_container 
                ~fracfree:false ~outputs:RankDet ;;

let ge_float3 = .! spec_ge_float ;;
*)
