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

type monbind = Exec of MLast.expr 
             | Last of MLast.expr
             | Binding of MLast.patt * MLast.expr
type bindtype = Global | Object

let process loc bt b = 
    let globbind2 x p acc =
        <:expr< bind $x$ (fun $p$ -> $acc$) >>
    and globbind1 x acc =
        <:expr< bind $x$ (fun _ -> $acc$) >>
    and objbind2 x p acc =
        <:expr< $x$ # bind (fun $p$ -> $acc$) >>
    and objbind1 x acc =
        <:expr< $x$ # bind (fun _ -> $acc$) >> 
    and globret n = <:expr< ret $n$ >>
    and objret n = <:expr< $n$ # ret >> in
    let choose_bind = function 
        | Global -> (globbind2, globbind1)
        | Object -> (objbind2, objbind1) in
    let choose_ret = function 
        | Global -> globret
        | Object -> objret in
    let folder bt = let (a,b) = choose_bind bt in
        (fun y accumulator -> 
        match y with
        | Binding(p,x) -> a x p accumulator
        | Exec(x) -> b x accumulator
        | Last(x) -> failwith "should not have an mret in the middle")
    in
    match b with 
    | [] -> failwith "somehow got an empty list from a LIST1!"
    | l -> let v = List.rev l in
        match (List.hd v) with
        | Last(n) ->
          List.fold_right (folder bt) (List.tl v) ((choose_ret bt) n) 
        | Binding(_,_) -> failwith "ends with a binding"
        | Exec(n) -> failwith "does not end with an mret"

EXTEND
    GLOBAL: Pcaml.expr;

    Pcaml.expr: AFTER "expr1"
    [
      [ "mdo"; "{";
        bindings = LIST1 monadic_binding SEP ";"; "}" ->
            process loc Global bindings
      ]  |
      [ "odo"; "{";
        bindings = LIST1 monadic_binding SEP ";"; "}" ->
            process loc Object bindings
      ] 
    ] ;

    monadic_binding:
    [ 
      [ p = Pcaml.patt; "<--"; x = Pcaml.expr LEVEL "expr1" ->
          Binding(p,x) ]
      | 
      [ "mret"; x = Pcaml.expr LEVEL "expr1" -> Last(x) ]
      | 
      [ x = Pcaml.expr LEVEL "expr1" -> Exec(x) ]
    ] ;
END;
