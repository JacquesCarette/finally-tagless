(* name:          pa_monad2.ml
 * synopsis:      Haskell-like "do" for monads
 * author:        Jacques Carette, based in part of work of Lydia E. Van Dijk
 * $Id$
 * last revision: Mon Feb  7 2005
 * ocaml version: 3.08.0 *)


(** Conversion Rules

Grammar informally:
                 mdo { exp }
                 mdo { exp1; exp2 }
                 mdo { x <-- exp; exp }
                 mdo { let x = foo end; exp }
which is almost literally the grammar of Haskell `do' notation,
modulo `do'/`mdo' and `<-'/`<--'.

Grammar formally:

        mdo { <do-body> }
        <do-body> :: =
                "let" var = EXP ("and" var = EXP)* "end" ";" <do-body>
                EXP
                (pat <--)? EXP ";" <do-body>

Semantics (as re-writing into the core language)

        mdo { exp } ===> exp
        mdo { pat <-- exp; rest } ===> bind exp (fun pat -> mdo { rest })
        mdo { exp; rest } ===> bind exp (fun _ -> mdo { rest })
        mdo { let pat = exp end; rest } ===> let pat = exp in mdo { rest }

`odo' is a variant of `mdo' where we use a bind `method' rather than a bind
`value' (which is supposed to be in scope).

Actually, in `let pat = exp' one can use anything that is allowed
in a `let' expression, e.g., `let pat1 = exp1 and pat2 = exp2 ...'.
The reason we can't terminate the `let' expression with just a semi-colon
is because semi-colon can be a part of an expression that is bound to
the pattern.

It is possible to use `<-' instead of `<--'. In that case,
the similarity to the `do' notation of Haskell will be complete. However,
due to the parsing rules of Camlp4, we would have to accept `:=' as
an alias for `<-'. So, mdo { pat := exp1; exp2 } would be allowed too.
Perhaps that is too much.

The major difficulty with the `do' notation is that it can't truly be
parsed by an LR-grammar. Indeed, to figure out if we should start
parsing <do-body> as an expression or a pattern, we have to parse it
as a pattern and check the "<--" delimiter. If it isn't there, we should
_backtrack_ and parse it again as an expression. Furthermore, "a <-- b"
(or "a <- b") can also be parsed as an expression. However, for some patterns,
e.g. (`_ <-- exp'), that cannot be parsed as an expression. 

    *)

type monbind = BindL of (MLast.patt * MLast.expr) list
             | BindM of MLast.patt * MLast.expr
             | ExpM  of MLast.expr
type bindtype = Global | Object

(* Convert MLast.expr into MLast.patt, if we `accidentally'
   parsed a pattern as an expression.
  The code is based on pattern_eq_expression in 
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

(* The main semantic function *)
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
        | BindM(p,x) -> a x p accumulator
        | ExpM(x) -> b x accumulator
        | BindL(l) -> <:expr< let $list:l$ in $accumulator$ >>
        )
    in
    match List.rev b with 
    | [] -> failwith "somehow got an empty list from a LIST1!"
    | (ExpM(n)::t) -> List.fold_left (folder bt) (ret n) t  
    | _ -> failwith "Does not end with an expression"

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
      [ "let"; l = LIST1 Pcaml.let_binding SEP "and"; "end" ->
	BindL(l) ]
    | 
      [ x = Pcaml.expr LEVEL "expr1" ->
	(* For some patterns, "patt <-- exp" can parse
	   as an expression too. So we have to figure out which is which. *)
        match x with
              <:expr< $e1$  $lid:op$  $e3$  >> when op = "<--" 
                  -> BindM((exp_to_patt loc e1),e3)
            | _ -> ExpM(x) ]
    | 
      [ p = Pcaml.patt LEVEL "simple"; "<--"; x = Pcaml.expr LEVEL "expr1" ->
        BindM(p,x) ]
    ] ;

END;

