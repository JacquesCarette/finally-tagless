(* name:          pa_monad.ml
 * synopsis:      Haskell-like "do" for monads
 * authors:       Jacques Carette and Oleg Kiselyov,
 *                based in part of work of Lydia E. Van Dijk
 * last revision: Sun Dec 11 08:30:44 UTC 2005
 * ocaml version: 3.09.0 *)


(** {2 Syntax Extension to Support Monads}

This module extends OCaml's syntax by a Haskell-like "do"-notation is
particularly suited for the work with monads.

{2 Conversion Rules}

{3 Grammar informally}
We support four different constructs to indroduce a monadic expressions.
{[
        mdo exp
        mdo exp1; exp2
        mdo x <-- exp1; exp2
        mdo let x = foo in; exp
]}
which is almost literally the grammar of the Haskell "do"-notation,
with the differences that Haskell uses "do" and "<-" where we use
"[mdo]" and "[<--]".

The actual bind function of the monad defaults to "[bind]".  To select a
different function, use the extended form of "[mdo]":
{[
        mdo with exp1 in exp2
        mdo with exp1 in exp3; exp4
        mdo with exp1 in x <-- exp2; exp3
        mdo with exp in let x = foo in; exp
]}

{3 Grammar formally}
{[
        "mdo" ["with" EXP "in"] <do-body>
        <do-body> ::=
                  "let" var "=" EXP ("and" var "=" EXP)* "in" ";" <do-body>
                | EXP
                | (pat "<--")? EXP ";" <do-body>
]}

{3 Semantics (as re-writing into the core language)}
{[
        mdo exp                             ===>  exp
        mdo pat <-- exp; rest               ===>  bind exp (fun pat -> mdo rest)
        mdo with bexp in pat <-- exp; rest  ===>  bexp exp (fun pat -> mdo rest)
        mdo exp; rest                       ===>  bind exp (fun _ -> mdo rest)
        mdo with bexp in exp; rest          ===>  bexp exp (fun _ -> mdo rest)
        mdo let pat = exp in; rest          ===>  let pat = exp in mdo rest
]}

Actually, in [let pat = exp] one can use anything that is allowed in a
[let] expression, for example,
{[        let pat1 = exp1 and pat2 = exp2 ...]}
The reason we cannot terminate the [let] expression with just a
semi-colon is because semi-colon can be a part of an expression that
is bound to the pattern.

It would be possible to use "[<-]" instead of "[<--]".  In that case, the
similarity to the "[do]" notation of Haskell will be complete.  However,
due to the parsing rules of Camlp4, we would have to accept "[:=]" as an
alias for "[<-]".  So,
{[        mdo pat := exp1; exp2]}
would be allowed too.  Perhaps that is too much.

The major difficulty with the "[do]" notation is that it cannot truly be
parsed by an LR-grammar.  Indeed, to figure out if we should start
parsing <do-body> as an expression or a pattern, we have to parse it
as a pattern and check the "[<--]" delimiter.  If it is not there, we
should {e backtrack} and parse it again as an expression.
Furthermore, [a <-- b] (or [a <- b]) can also be parsed as an
expression.  However, for some patterns, for example ([_ <-- exp]),
that cannot be parsed as an expression. *)


(** Syntactically, we can encouter three different kinds of monadic
    bindings.  Sometimes we find out about an expression that is not a
    monadic binding, just an expression; this is the fourth case. *)
type monadic_binding =
    BindL of (MLast.patt * MLast.expr) list (** "[let]" inside "[mdo]" *)
  | BindMRef of MLast.patt * MLast.expr     (** bind ("[>>=]") using a refutable pattern *)
  | BindMIrref of MLast.patt * MLast.expr   (** bind ("[>>=]") using an irrefutable pattern *)
  | ThenM of MLast.expr                     (** "then"-binding, this is the "[>>]"-operator *)


(** Convert [an_expression] into a pattern, if we "accidentally" parse
    a pattern as an expression. *)
(*  The code is based on [pattern_eq_expression] in {i pa_fstream.ml}. *)
let rec exp_to_patt (_loc: MLast.loc) (an_expression: MLast.expr): MLast.patt =
  match an_expression with
      <:expr< $int:s$ >> -> <:patt< $int:s$ >> (* integer constant*)
    | <:expr< $chr:c$ >> -> <:patt< $chr:c$ >> (* character constant;*)
    | <:expr< $str:s$ >> -> <:patt< $str:s$ >> (* string constant *)
    | <:expr< $lid:b$ >> -> <:patt< $lid:b$ >> (* local variable *)
    | <:expr< $uid:b$ >> -> <:patt< $uid:b$ >> (* variable of other module *)
    | <:expr< $e1$ $e2$ >> ->                  (* function application *)
      let p1 = exp_to_patt _loc e1
      and p2 = exp_to_patt _loc e2 in
        <:patt< $p1$ $p2$ >>
    | <:expr< ($list:el$) >> ->                (* tuple *)
      let pl = List.map (exp_to_patt _loc) el in
        <:patt< ($list:pl$) >>
    | <:expr< { $list:eel$ } >> ->             (* record *)
      let ppl = List.map (fun (p, e) -> p, exp_to_patt _loc e) eel in
        <:patt< { $list:ppl$ } >>
    | _ -> failwith "exp_to_patt: this pattern is not yet supported"


(** Answer whether [a_pattern] is irrefutable. *)
let rec is_irrefutable_pattern (a_pattern: MLast.patt): bool =
  match a_pattern with
      <:patt< { $list:ppl$ } >> ->      (* record *)
      List.for_all (fun (_p1, p2) -> is_irrefutable_pattern p2) ppl
    | <:patt< () >> -> true             (* unit *)
    | <:patt< ( $p$ : $t$ ) >> ->       (* type constraint *)
      is_irrefutable_pattern p
    | <:patt< ( $p1$ as $p2$ ) >> ->    (* alias *)
      is_irrefutable_pattern p1
    | <:patt< ( $list:pl$ ) >> ->       (* tuple of patterns *)
      List.for_all is_irrefutable_pattern pl
    | <:patt< $lid:s$ >> -> true        (* variable *)
    | <:patt< _ >> -> true              (* wildcard *)
    | _ -> false
(** Answer whether [an_expression] represents an irrefutable pattern. *)
and is_irrefutable_expression (an_expression: MLast.expr): bool =
  match an_expression with
      <:expr< { $list:eel$ } >> ->      (* record *)
      List.for_all (fun (_p, e) -> is_irrefutable_expression e) eel
    | <:expr< () >> -> true             (* unit *)
    | <:expr< ( $exp$ : $t$ ) >> ->     (* type constraint *)
      is_irrefutable_expression exp
      (* MISSING: parenthesized expression? *)
    | <:expr< ( $list:el$ ) >> ->
      List.for_all is_irrefutable_expression el
    | <:expr< $lid:s$ >> -> true        (* variable *)
      (* MISSING: wildcard? *)
    | _ -> false


(** Convert all expressions [a_binding_list] inside "mdo" into core
    OCaml.  Use [a_bind_function] as the monad's "bind"-function. *)
let convert
    (_loc: MLast.loc)
    (a_binding_list: monadic_binding list)
    (a_bind_function: MLast.expr): MLast.expr =
  let glob_then x acc = <:expr< $a_bind_function$ $x$ (fun _ -> $acc$) >>
  and glob_bind_irrefutable x p acc = <:expr< $a_bind_function$ $x$ (fun $p$ -> $acc$) >>
  and glob_bind_refutable x p acc =
    <:expr< $a_bind_function$ $x$ (fun [$p$ -> $acc$ | _ -> fail "bind"]) >> in
  let folder =
    fun acc y ->
      match y with
          BindMIrref (p, x) -> glob_bind_irrefutable x p acc
        | BindMRef (p, x) -> glob_bind_refutable x p acc
        | ThenM x -> glob_then x acc
        | BindL l -> <:expr< let $list:l$ in $acc$ >>
  in
    match List.rev a_binding_list with
        [] -> failwith "convert: somehow got an empty list from a LIST1"
      | ThenM n :: t -> List.fold_left folder <:expr< $n$ >> t
      | _ -> failwith "convert: does not end with an expression"

(** [attach_bind an_expression]

    Append "[bind]" to the module name given in [an_expression].  Fail
    if [an_expression] is not a valid module name. *)
let append_bind _loc an_expression =
  match an_expression with
      <:expr< $e1$ . $e2$ >> as m -> <:expr< $m$ . bind >>
    | <:expr< $uid:s$ >> as m -> <:expr< $m$ . bind >>
    | _ -> failwith "append_bind: 'with module' expects a module name or module-path."


EXTEND
    GLOBAL: Pcaml.expr;

    Pcaml.expr: LEVEL "expr1"
    [
      [ "mdo"; "with"; "module"; monad_module = Pcaml.expr; "in";
        bindings = LIST1 monadic_binding SEP ";" ->
          convert _loc bindings (append_bind _loc monad_module) ]
    |
      [ "mdo"; "with"; bind_fun = Pcaml.expr; "in";
        bindings = LIST1 monadic_binding SEP ";" ->
          convert _loc bindings bind_fun ]
    |
      [ "mdo";
        bindings = LIST1 monadic_binding SEP ";" ->
          convert _loc bindings <:expr< bind >> ]
    ] ;

    Pcaml.expr: BEFORE "apply"
    [ NONA
	[ e1 = SELF; "<--"; e2 = Pcaml.expr LEVEL "expr1" ->
            <:expr< $e1$ $lid:"<--"$ $e2$ >> ]
    ] ;

    monadic_binding:
    [
      [ "let"; l = LIST1 Pcaml.let_binding SEP "and"; "in" -> BindL l ]
    |
      [ x = Pcaml.expr LEVEL "expr1" ->
	(* For some patterns, "patt <-- exp" can parse as an
	   expression too.  So we have to figure out which is
	   which. *)
          match x with
              <:expr< $e1$ $lid:op$ $e3$ >> when op = "<--" ->
                    if is_irrefutable_expression e1 then
                       BindMIrref (exp_to_patt _loc e1, e3)
                    else BindMRef (exp_to_patt _loc e1, e3)
            | _ -> ThenM x ]
    |
      [ p = ipatt; "<--"; x = Pcaml.expr LEVEL "expr1" ->
          BindMIrref (p, x)
      | p = Pcaml.patt LEVEL "simple"; "<--"; x = Pcaml.expr LEVEL "expr1" ->
          BindMRef (p, x) ]
    ] ;

    patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple" RIGHTA
      [ i = UIDENT -> <:patt< $uid:i$ >>
      | i = LIDENT -> <:patt< $lid:i$ >> ]
    ] ;

    label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> (i, p) ] ] ;

    (* irrefutable patterns (from "pa_r.ml") *)
    ipatt:
    [ [ "{"; lpl = LIST1 label_ipatt SEP ";"; "}" ->
          <:patt< { $list:lpl$ } >>            (* record *)
      | "("; ")" -> <:patt< () >>              (* unit *)
      | "("; p = SELF; ")" ->                  (* parenthesized pattern *)
          <:patt< $p$ >>
      (* MISSING: type restriction *)
      | p = SELF; "as"; p2 = SELF ->           (* alias *)
          <:patt< ($p$ as $p2$) >>
      | p = SELF; ","; ps = LIST1 ipatt SEP "," ->
          let lst = p :: ps in
            <:patt< ($list:lst$) >>            (* tuple of patterns *)
      | s = LIDENT -> <:patt< $lid:s$ >>       (* variable *)
      | "_" -> <:patt< _ >> ]                  (* wildcard *)
    ] ;
END;
