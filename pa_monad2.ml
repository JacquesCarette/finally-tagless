(* name:          pa_monad2.ml
 * synopsis:      Haskell-like "do" for monads
 * author:        Jacques Carette, based in part of work of Lydia E. Van Dijk
 * last revision: Mon Feb  7 2005
 * ocaml version: 3.08.0 *)


(** Conversion Rules

    - An expression [monadic pattern <-- expression_1 ; rest ]
      becomes
      [bind expression_1 (function pattern -> rest)]

    - An expression [expression_1 ; expression_2] becomes
      [bind expression_1 (function _ -> expression_2 )]

    - An expression [mret expression] 
      becomes [ret expression]

    - mdo { } surrounds/introduces a monadic sequence, which is 
    required to end with an mret. *)

type monbind = LetB of (MLast.patt * MLast.expr) list
             | Stmt of MLast.patt option * MLast.expr
type bindtype = Global | Object

(* based on pattern_eq_expression in 
  /camlp4/etc/pa_fstream.ml *)

let rec exp_to_patt loc e =
  match e with
    <:expr< $lid:b$ >> -> <:patt< $lid:b$ >>
  | <:expr< $uid:b$ >> -> <:patt< $uid:b$ >>
  | <:expr< $e1$ $e2$ >> -> 
      let p1 = exp_to_patt loc e1 and p2 = exp_to_patt loc e2 in
      <:patt< $p1$ $p2$ >>
  | <:expr< ($list:el$) >> ->
      let pl = List.map (exp_to_patt loc) el in
      <:patt< ($list:pl$) >>
  | _ -> failwith "This pattern isn't yet supported"

let process loc bt b = 
    let globbind2 x p acc =
        <:expr< bind $x$ (fun $p$ -> $acc$) >>
    and globbind1 x acc =
        <:expr< bind $x$ (fun _ -> $acc$) >>
    and objbind2 x p acc =
        <:expr< $x$ # bind (fun $p$ -> $acc$) >>
    and objbind1 x acc =
        <:expr< $x$ # bind (fun _ -> $acc$) >> 
    and ret n = <:expr< $n$ >> in
    let choose_bind = function 
        | Global -> (globbind2, globbind1)
        | Object -> (objbind2, objbind1) in
    let folder bt = let (a,b) = choose_bind bt in
        (fun accumulator y -> 
        match y with
        | Stmt(Some p,x) -> a x p accumulator
        | Stmt(None,x) -> 
	    (match x with
	      <:expr< $e1$ $lid:op$  $e3$  >> when op = "<--" 
	      -> a e3 (exp_to_patt loc e1) accumulator
	    | _ -> b x accumulator)
        | LetB(l) -> failwith "not implemented yet"
        )
    in
    match List.rev b with 
    | [] -> failwith "somehow got an empty list from a LIST1!"
    | (Stmt(None,n)::t) -> List.fold_left (folder bt) (ret n) t  
    | _ -> failwith "Does not end with a statement"

(*
      [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = expr LEVEL "top" ->
          <:expr< let $opt:o2b o$ $list:l$ in $x$ >>
*)

(*
let expand_code loc e =
  MLast.ExCod (loc, e)
*)

EXTEND
    GLOBAL: Pcaml.expr; 

    Pcaml.expr: LEVEL "expr1"
    [
(*      [ ".<"; x = SELF; ">." -> expand_code loc x ] | *)
      [ "mdo"; "{";
        bindings = LIST1 monadic_binding SEP ";"; "}" ->
            process loc Global bindings
      ]  |
      [ "odo"; "{";
        bindings = LIST1 monadic_binding SEP ";"; "}" ->
            process loc Object bindings
      ] 
    ] ;

    Pcaml.expr: BEFORE "apply"
    [ NONA
	[ e1 = SELF; "<--"; e2 = Pcaml.expr LEVEL "expr1" ->
          <:expr< $e1$ $lid:"<--"$ $e2$ >>
	] 
    ] ;

    monadic_binding:
    [ 
      [ "let"; l = LIST1 Pcaml.let_binding SEP "and" ->
	LetB(l) ]
    | 
      [ x = Pcaml.expr LEVEL "expr1" ->
        Stmt(None,x) ]
    | 
      [ p = Pcaml.patt LEVEL "simple"; "<--"; x = Pcaml.expr LEVEL "expr1" ->
        Stmt(Some p,x) ]
    ] ;

END;

