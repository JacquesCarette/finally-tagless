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
  det:('a,'d ref) code; detsign:('a,'d ref) code};;

type ('a,'b) domain = {
  zero:('a,'b) code; one:('a,'b) code;
  minusone:('a,'b) code;
  plus:('a,'b) code -> ('a,'b) code -> ('a,'b) code; 
  times:('a,'b) code -> ('a,'b) code -> ('a,'b) code;
  minus:('a,'b) code -> ('a,'b) code -> ('a,'b) code; 
  div:('a,'b) code -> ('a,'b) code ->('a,'b) code;
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

let seq a b = .< begin .~a ; .~b end >. ;;

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
  | JustMatrix -> .< Matrix (.~s.b) >.
  | Rank       -> .< MatrixRank((.~s.b), ! .~(s.r)) >.
  | Det        -> .< MatrixDet((.~s.b), 
     .~(dom.times .<! .~(s.det)>. .<! .~(s.detsign)>. ))>.
  | RankDet    -> .<MatrixRankDet((.~s.b), 
    ! .~(s.r), .~(dom.times .<! .~(s.det)>.
    .< ! .~(s.detsign) >. )) >.

let ge_state_gen dom contr findpivot swap zerobelow choice=
  let main_loop s = 
  .< while !(.~s.c) < .~s.m && !(.~s.r) < .~s.n do
    begin
    match .~(findpivot s) with
    Some i -> begin
        if i <> !(.~s.r) then
            for j = !(.~s.c) to .~s.m-1 do
                .~(swap s .<i>. .<j>.);
            done;
        .~(zerobelow s) ;
        (.~s.r) := !(.~s.r) + 1;
        end;
    | None -> .~(match choice.track with
        | TrackNothing -> .< () >. ;
        | TrackDet     -> .< .~s.detsign := .~dom.zero >.;
        ) ;
    end;
    .~s.c := !(.~s.c) + 1;
  done >. in

  .< fun a -> 
  let b= .~(mdapply1 contr.mapper dom.normalizerf 
                     (contr.copy .<a>. )) and
      r=ref 0 and c=ref 0 and
      m= .~(contr.dim1 .<a>. ) and 
      n= .~(contr.dim2 .<a>. ) and
      det=ref .~(dom.one) and detsign=ref .~(dom.one) in
  .~(let st = {b= .<b>.; r= .<r>.; c= .<c>.; m= .<m>.; 
               n= .<n>.; det= .<det>.; 
               detsign= .<detsign>.} in
    seq
      (main_loop st )
      (choose_output dom st choice.outputs ) ) 
  >. ;;

let swapr_gen dom contr track s i j =
  let swap =
  .< let b = .~(s.b) and r = !(.~s.r) and i= .~i 
     and j= .~j in
      let t = .~(contr.get .<b>. .<i>. .<j>. ) in  begin
        .~(seq
            (contr.set .<b>. .<i>. .<j>. 
               (contr.get .<b>. .<r>. .<j>. ))
            (contr.set .<b>. .<r>. .<j>. .<t>. ) );
  end >. in
  let tds = .< .~s.detsign := 
        .~(dom.times .<! .~s.detsign>. dom.minusone) >. in
  match track with
  | TrackNothing -> swap
  | TrackDet     -> seq swap tds ;;

let findpivot_gen dom contr orcond_gen = fun s ->
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

let zerobelow_gen dom contr choice s =
  let inner_loop = 
  if not choice.fracfree then fun i s ->
    .< let t = .~(dom.div 
      (contr.get .<.~s.b>. .< .~i>. .<!(.~s.c)>. )
      (contr.get .<.~s.b>. .<!(.~s.r)>. .<!(.~s.c)>. ) ) in
    for j = !(.~s.c)+1 to .~s.m-1 do
      .~(contr.set .<.~s.b>. .<.~i>. .<j>. 
          (dapply1 dom.normalizerg 
          (dom.minus (contr.get .<.~s.b>. .<.~i>. .<j>.)
          (dom.times .<t>. 
              (contr.get .<.~s.b>. .<!(.~s.r)>. .<j>. )))))
    done; >.
  else fun i s -> 
    .< for j = !(.~s.c)+1 to .~s.m-1 do
      let t = .~(dapply1 dom.normalizerg (dom.minus 
        (dom.times 
          (contr.get .<.~s.b>. .<.~i>. .<j>. )
          (contr.get .<.~s.b>. .<! .~s.r>. .<! .~s.c>. ))
        (dom.times 
          (contr.get .<.~s.b>. .<!(.~s.r)>. .<j>. )
          (contr.get .<.~s.b>. .<.~i>. .<! .~s.r>. )))) in
      .~(contr.set .<.~s.b>. .<.~i>. .<j>. 
            (dom.div .<t>. .<! .~s.det>. ))
    done >. in
  let outer_loop =
  .< for i = !(.~s.r)+1 to .~s.n-1 do
    if not ( .~(contr.get .<.~s.b>. .<i>. .<! .~s.c>. ) 
             = .~dom.zero) then
      begin
        .~(inner_loop .<i>. s );
        .~(contr.set .<.~s.b>. .<i>. .<! .~s.c>. dom.zero)
      end;
  done >. in
  match choice.track with
  | TrackNothing -> outer_loop
  | TrackDet     -> seq outer_loop
    (if choice.fracfree then
      .< .~s.det := 
        .~(contr.get .<.~s.b>. .<! .~s.r>. .<! .~s.c>. )>.
    else
      .< .~s.det := .~(dom.times .<! .~s.det>. 
          (contr.get .<.~s.b>.
              .<! .~s.r>. .<! .~s.c>. ))>. );;

let specializer dom container ~fracfree ~outputs =
  let t = (if fracfree || outputs == Det 
                       || outputs == RankDet then 
      TrackDet else TrackNothing ) in
  let choice = 
      {fracfree=fracfree; track=t; outputs=outputs} in
  let fp = findpivot_gen dom container orcond_gen in
  let swap = swapr_gen dom container choice.track in
  let zb = zerobelow_gen dom container choice in
  ge_state_gen dom container fp swap zb choice;;

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
