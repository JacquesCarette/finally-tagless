(* name:          pa_monad.ml
 * synopsis:      Haskell-like "do" for monads
 * authors:       Jacques Carette, Oleg Kiselyov, and Lydia E. Van Dijk
 * last revision: Wed September 13, 2005.
 * ocaml version: 3.08.2 *)


(** {2 Conversion Rules}

{3 Grammar informally}
{[
        doM exp
        doM exp1; exp2
        doM x <-- exp; exp
        doM let x = foo in; exp
]}
which is almost literally the grammar of Haskell [do] notation,
modulo [do]/[doM] and [<-]/[<--].

The actual bind function of the monad defaults to [bind].  To select a
different function, use the extended form of [doM]:
{[
        doM Mod in exp2
        doM Mod in exp3; exp4
        doM Mod in x <-- exp; exp
        doM Mod in let x = foo in; exp
]}

{3 Grammar formally}
{[
        "doM" [EXP "in"] <do-body>
        <do-body> ::=
                "let" var "=" EXP ("and" var "=" EXP)* "in" ";" <do-body>
                EXP
                (pat "<--")? EXP ";" <do-body>
]}

{3 Semantics (as re-writing into the core language)}
{[
        doM exp                             ===>  exp
        doM pat <-- exp; rest               ===>  bind exp (fun pat -> doM rest)
        doM bexp in pat <-- exp; rest       ===>  bexp.bind exp (fun pat -> doM rest)
        doM exp; rest                       ===>  bind exp (fun _ -> doM rest)
        doM bexp in exp; rest               ===>  bexp.bind exp (fun _ -> doM rest)
        doM let pat = exp in; rest          ===>  let pat = exp in doM rest
        doM bexp in let pat = exp in; rest  ===>  let pat = exp in doM bexp in rest
]}

Actually, in [let pat = exp] one can use anything that is allowed in a
[let] expression, for example,
{[        let pat1 = exp1 and pat2 = exp2 ...]}
The reason we cannot terminate the [let] expression with just a
semi-colon is because semi-colon can be a part of an expression that
is bound to the pattern.

It would be possible to use [<-] instead of [<--].  In that case, the
similarity to the [do] notation of Haskell will be complete.  However,
due to the parsing rules of Camlp4, we would have to accept [:=] as an
alias for [<-].  So,
{[        doM pat := exp1; exp2]}
would be allowed too.  Perhaps that is too much.

The major difficulty with the [do] notation is that it cannot truly be
parsed by an LR-grammar.  Indeed, to figure out if we should start
parsing <do-body> as an expression or a pattern, we have to parse it
as a pattern and check the [<--] delimiter.  If it is not there, we
should {e backtrack} and parse it again as an expression.
Furthermore, [a <-- b] (or [a <- b]) can also be parsed as an
expression.  However, for some patterns, for example ([_ <-- exp]),
that cannot be parsed as an expression. *)

type monbind =
    BindL of (MLast.patt * MLast.expr) list
  | BindM of MLast.patt * MLast.expr
  | ExpM  of MLast.expr

(* Convert MLast.expr into MLast.patt, if we `accidentally' parsed a
   pattern as an expression.  The code is based on
   pattern_eq_expression in pa_fstream.ml. *)
let rec exp_to_patt loc e =
  match e with
      <:expr< $lid:b$ >> -> <:patt< $lid:b$ >>
    | <:expr< $uid:b$ >> -> <:patt< $uid:b$ >>
    | <:expr< $e1$ $e2$ >> ->
      let p1 = exp_to_patt loc e1
      and p2 = exp_to_patt loc e2 in
        <:patt< $p1$ $p2$ >>
    | <:expr< ($list:el$) >> ->
      let pl = List.map (exp_to_patt loc) el in
        <:patt< ($list:pl$) >>
    | _ -> failwith "exp_to_patt: this pattern is not yet supported"

(* The main semantic function *)
let process loc b f =
  let globbind1 x acc = <:expr< $f$ $x$ (fun _ -> $acc$) >>
  and globbind2 x p acc = <:expr< $f$ $x$ (fun $p$ -> $acc$) >>
  and ret n = <:expr< $n$ >> in
  let folder =
    fun acc y ->
      match y with
          BindM (p, x) -> globbind2 x p acc
        | ExpM x -> globbind1 x acc
        | BindL l -> <:expr< let $list:l$ in $acc$ >>
  in
    match List.rev b with
        [] -> failwith "process: somehow got an empty list from a LIST1!"
      | ExpM n :: t -> List.fold_left folder (ret n) t
      | _ -> failwith "process: does not end with an expression"

EXTEND
    GLOBAL: Pcaml.expr;

    Pcaml.expr: LEVEL "expr1"
    [
      [ "doM"; bind_mod = UIDENT; "in";
        bindings = LIST1 monadic_binding SEP ";" ->
          process loc bindings (<:expr< $uid:bind_mod$.bind >>) ]
    |
      [ "doM";
        bindings = LIST1 monadic_binding SEP ";" ->
          process loc bindings (<:expr< bind >>) ]
    ] ;

    Pcaml.expr: BEFORE "apply"
    [ NONA
	[ e1 = SELF; "<--"; e2 = Pcaml.expr LEVEL "expr1" ->
            <:expr< $e1$ $lid:"<--"$ $e2$ >> ]
    ] ;

    monadic_binding:
    [
      [ "let"; l = LIST1 Pcaml.let_binding SEP "and"; "in" ->
	  BindL(l) ]
    |
      [ x = Pcaml.expr LEVEL "expr1" ->
	(* For some patterns, "patt <-- exp" can parse as an
	   expression too.  So we have to figure out which is which. *)
          match x with
              <:expr< $e1$  $lid:op$ $e3$ >> when op = "<--" ->
                    BindM (exp_to_patt loc e1, e3)
            | _ -> ExpM x ]
    |
      [ p = Pcaml.patt LEVEL "simple"; "<--"; x = Pcaml.expr LEVEL "expr1" ->
          BindM (p, x) ]
    ] ;
END;
