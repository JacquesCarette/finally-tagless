(* Pure translation to mdo notation.  No use of modules or functors *)
let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k) ;;
let ret = retS

(* state monad morphism: get the value *)
let fetch s k = k s s

(* another non-trivial morphism: generate a bit of code *)
let codegen v cf = fun s k -> cf (k s v)

let loop_gen l h x = fun s k -> 
    k s .< for j = .~l to .~h do .~(x .<j>. s k) done >.

type ('a,'c,'d) container2d = {
  get: ('a,'c) code -> ('a,int) code -> 
       ('a,int) code -> ('a,'d) code ;
  set: ('a,'c) code -> ('a,int) code -> 
       ('a,int) code -> ('a,'d) code -> ('a,unit) code;
  dim1: ('a,'c) code -> ('a,int) code;
  dim2: ('a,'c) code -> ('a,int) code;
  mapper: ('a, 'd->'d) code -> ('a,'c) code -> 
          ('a,'c) code;
  copy: ('a, 'c) code -> ('a,'c) code
} ;;

type ('a,'c,'d) state = {b:('a,'c) code; 
  r:('a,int ref) code; c:('a,int ref) code; 
  m:('a,int) code; n:('a,int) code;
  i:('a,int) code; j:('a,int) code;
  det:('a,'d ref) code; detsign:('a,'d ref) code};;

type ('a,'b,'s,'w) cont = 's -> ('s -> ('a,'b) code -> 'w) -> 'w;;
type ('a,'b,'s,'w) binop = ('a,'b) code -> ('a,'b) code -> 's ->
             ('s -> ('a,'b) code -> 'w) -> 'w;;
type ('a,'b,'s,'w) domain = {
  zero:('a,'b) code; one:('a,'b) code;
  minusone:('a,'b) code;
  plus:('a,'b,'s,'w) binop;
  times:('a,'b,'s,'w) binop;
  times': ('a,'b) code -> ('a,'b) code -> ('a,'b) code;
  minus:('a,'b,'s,'w) binop;
  div:('a,'b,'s,'w) binop;
  smaller_than:(('a,'b) code -> ('a,'b) code -> 
                ('a,bool) code) option;
  normalizerf:(('a,'b -> 'b) code ) option;
  normalizerg:(('a,'b) code -> ('a,'b) code ) option;
} ;;

type ('b,'c) outputs = 
    Matrix of 'b
  | MatrixRank of 'b * int
  | MatrixDet of 'b * 'c
  | MatrixRankDet of 'b * int * 'c ;;

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

let choose_output dom s = function
  | JustMatrix -> .< Matrix (.~(s.b)) >.
  | Rank       -> .< MatrixRank(.~( s.b), ! .~(s.r)) >.
  | Det        -> .< MatrixDet( .~(s.b), 
     dom.times' .<! .~(s.det)>. .<! .~(s.detsign)>. )>. 
  | RankDet    -> .< MatrixRankDet( .~(s.b), (! .~(s.r)), dom.times' .<! .~(s.det)>. .<! .~(s.detsign)>. ) >.

let ge_state_gen (dom:('a,'b,('a,'c,'b) state, ('a, unit) code) domain) 
      contr findpivot swap zerobelow choice =
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
          st <-- retS {b=b; r=r; c=c; m=m; n=n; det=det; 
                       detsign=detsign; i=i; j=j};
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
          loop <-- loop_gen .<!(.~st.c)+1>. .< .~st.m-1>. body;
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
          loop <-- loop_gen .<!(.~st.c)+1>. .< .~st.m-1>. body;
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

let dom_float = { 
   zero = .< 0. >. ; 
   one = .< 1. >. ;
   minusone = .< -1. >. ;
   plus = (fun x y -> .<.~x +. .~y>.) ;
   times = (fun x y -> .<.~x *. .~y>.) ;
   minus = (fun x y -> .<.~x -. .~y>.) ;
   div = (fun x y -> .<.~x /. .~y>.) ;
   smaller_than = 
       Some(fun x y -> .<abs_float .~x < abs_float .~y >.);
   normalizerf = None ;
   normalizerg = None };;

let array_container = {
  get = (fun x n m -> .< (.~x).(.~n).(.~m) >. );
  set = (fun x n m y -> .< (.~x).(.~n).(.~m) <- .~y >. );
  dim2 = (fun x -> .< Array.length .~x >.) ;
  dim1 = (fun x -> .< Array.length (.~x).(0) >. ) ;
  mapper = (fun f a -> .< Array.map 
      (fun x -> Array.map .~f x) .~a >.);
  copy = (fun a -> .<Array.map (fun x -> Array.copy x) 
                       (Array.copy .~a) >. )
};;

let spec_ge_float = 
    specializer dom_float array_container 
                ~fracfree:false ~outputs:RankDet ;;

let ge_float3 = .! spec_ge_float ;;
