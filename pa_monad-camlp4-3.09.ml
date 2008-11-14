(* name:          pa_monad.ml
 * synopsis:      Haskell-like "do" for monads
 * authors:       Jacques Carette and Oleg Kiselyov,
 *                based in part of work of Lydia E. Van Dijk
 * last revision: Wed Mar 22 07:22:00 UTC 2006
 * ocaml version: 3.09.0
 *
 * Copyright (C) 2006-2008  J. Carette, L. E. van Dijk, O. Kiselyov
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


(** {2 Syntax Extension to Support Monads}

This module extends OCaml's syntax by a Haskell-like "do"-notation
particularly suited for the work with monads.

By the nature of the translation process (at pre-processing time,
before compilation) it cannot be guaranteed that the result code
actually obeys the three fundamental laws for all monads:
+ [bind (return x) f]  is identical to  [f x]
+ [bind m return]      is identical to  [m]
+ [bind (bind m f) g]  is identical to  [bind m (fun x -> bind (f x) g)]

where [bind] and [return] are user defined functions. Incidentally, in
Haskell, too, it is entirely the responsibility of the programmer to
make sure that [bind] and [return] implemented for a particular Monad
do indeed obey the above laws.


{2 Conversion Rules}

{3 Grammar informally}
We support four different constructs to introduce a monadic
expressions.
- [perform exp]
- [perform exp1; exp2]
- [perform x <-- exp1; exp2]
- [perform let x = foo in exp]

which is almost literally the grammar of the Haskell's "do"-notation,
with the differences that Haskell uses "do" and "<-" where we use
"[perform]" and "[<--]".

We support not only [let x = foo in ...]  expressions but arbitrarily
complex [let]-expressions, including [let rec] and [let module].


{4 Extended Forms}
The actual bind function of the monad defaults to "[bind]" and the
match-failure function to "[failwith]" (only used for refutable
patterns; see below).  To select a different function, use the
extended forms of "[perform]".

{b Expression:} Use the given expression as bind-function and apply
the default match-failure function ([failwith]) where necessary.
{[
        perform with exp1 in exp2
        perform with exp1 in exp3; exp4
        perform with exp1 in x <-- exp2; exp3
        perform with exp in let x = foo in exp
]}
Use the first given expression ([exp1]) as bind-function and the
second ([exp2]) as match-failure function.
{[
        perform with exp1 and exp2 in exp3
        perform with exp1 and exp2 in exp3; exp4
        perform with exp1 and exp2 in x <-- exp3; exp4
        perform with exp1 and exp2 in let x = foo in exp1
]}

{b Module:} Use the function named "[bind]" from module "[Mod]".  In
addition use the module's "[failwith]"-function in refutable patterns.
{[
        perform with module Mod in exp2
        perform with module Mod in exp3; exp4
        perform with module Mod in x <-- exp2; exp3
        perform with module Mod in let x = foo in exp
]}


{4 Refutable Patterns}
An irrefutable pattern is either:
- A variable,
- The wildcard "[_]",
- The constructor "[()]",
- A tuple with irrefutable patterns,
- A record with irrefutable patterns, or
- An irrefutable pattern with a type constraint.
Any other pattern is refutable.

Why do we need this distinction?  Well, the expression
{[
        perform x <-- exp1; exp2
]}
expands to
{[
        bind exp2 (fun x -> exp1)
]}
where pattern match can never fail as "[x]" can take any value.  This
is an example of an irrefutable pattern.  No catch-all branch is
required here.  Compare this with
{[
        perform 1 <-- exp1; exp2
]}
which expands to
{[
        bind exp2 (fun 1 -> exp1 | _ -> failwith "pattern match")
]}
As the match can fail -- "[1]" being a refutable pattern in this
position -- we must add a second branch that catches the remaining
values.  The user is free to override the "[failwith]" function with
her own version.

Refer to the thread on the Haskell mailing list concerning the topic
of {{:http://www.haskell.org/pipermail/haskell/2006-January/017237.html}
refutable patterns} and an excerpt from an earlier
{{:http://www.cs.chalmers.se/~rjmh/Haskell/Messages/Decision.cgi?id=2}
discussion} on the same issue.


{3 Grammar formally}
Formally the grammar of [pa_monad] can be specified as follows.
{[
        "perform" ["with" <user-function-spec> "in"] <perform-body>
        <user-function-spec> ::=
                  EXPR ["and" EXPR]
                | "module" MODULE-NAME
        <binding> ::=
                  PATTERN "<--" EXPR
        <perform-body> ::=
                  <LET-FORM> <perform-body>
                | EXPR
                | <binding> ";" <perform-body>
                | "rec" <binding> ["and" <binding> [...]] ";" <perform-body>
]}
where [EXPR] is an OCaml expression {i expr} as defined in Section 6.7
of the OCaml manual, [MODULE-NAME] a {i module-name} (Sec. 6.3),
[LET-FORM] is any of the [let], [let rec], or [let module] {i let-forms}
(Sec. 6.7), and [PATTERN] a {i pattern} (Sec. 6.6).

The "[rec]" keyword allows for a recursive binding in
{[
        "rec" PATTERN "<--" EXPR
        "and" PATTERN "<--" EXPR
        ...
        "and" PATTERN "<--" EXPR ";"
]}
The syntax extension groups all bindings in a "[rec]"-"[and]", but
it does not group consecutive "[rec]"-bindings.  This grouping is
sometimes called segmentation.

Example:
Define a recursive group of bindings consisting of three patterns
(PATTERN1-PATTERN3) and expressions (EXPR1-EXPR3), a non-recursive
binding PATTERN4/EXPR4, and finally a single recursive binding
PATTERN5/EXPR5:
{[
        "rec" PATTERN1 "<--" EXPR1
        "and" PATTERN2 "<--" EXPR2
        "and" PATTERN3 "<--" EXPR3 ";"
              PATTERN4 "<--" EXPR4 ";"
        "rec" PATTERN5 "<--" EXPR5 ";"
]}
Please consult Section 7.3 of the Manual for valid recursive
definitions of values, as the only allowed [PATTERN] in the recursive
case is a [NAME], similarly stringent restrictions apply to [EXPR].
The theoretical aspects of recursive monadic bindings can be found in
Levent Erkök, John Launchbury: "{i A Recursive do for Haskell}".


For any ['a monad] the expansion uses the functions "[bind]" and
"[failwith]" with the signatures
{[
        val bind: 'a monad -> ('a -> 'b monad) -> 'b monad
        val failwith: string -> 'a monad
]}
unless overridden by the user.  Analogously, the signatures of modules
used in the "[with module]"-form must enclose
{[
        sig
          type 'a monad
          val bind: 'a monad -> ('a -> 'b monad) -> 'b monad
          val failwith: string -> 'a monad
        end
]}


{3 Semantics (as re-writing into the core language)}
In this section, we abbreviate irrefutable patterns with [ipat] and
refutable patterns with [rpat].
{[
        perform exp1                 ===>  exp1
        perform ipat <-- exp; rest   ===>  bind exp (fun ipat -> perform rest)
        perform rpat <-- exp; rest   ===>  bind exp (fun rpat -> perform rest
                                                         | _ -> failwith "pattern match")
        perform let ... in rest      ===>  let ... in perform rest
        perform exp; rest            ===>  bind exp (fun _ -> perform rest)

        perform with bexp in body
                ===> perform body
                        where bind is substituted with bexp

        perform with bexp and fexp in body
                ===> perform body
                        where bind is substituted with bexp and
                              failwith is substituted with fexp

        perform with module Mod in body
                ===> perform body
                        where bind is substituted with Mod.bind and
                              failwith with Mod.failwith
]}


{4 Implementation Notes And Design Decisions}
It is be possible to use "[<-]" instead of "[<--]".  In that case, the
similarity to the "[do]" notation of Haskell will be complete.
However, if the program has [_ <- exp] outside of [perform], this will
be accepted by the parser (and create an (incomprehensible) error
later on.  It is better to use a dedicated symbol "[<--]", so if the
user abuses it, the error should be clear right away.

The major difficulty with the [perform] notation is that it cannot
truly be parsed by an LR-grammar.  Indeed, to figure out if we should
start parsing <perform-body> as an expression or a pattern, we have to
parse it as a pattern and check for the "[<--]" delimiter.  If it is
not there, we should {e backtrack} and parse it again as an
expression.  Furthermore, [a <-- b] (or [a <- b]) can also be parsed
as an expression.  However, some patterns, for example ([_ <-- exp]),
cannot be parsed as an expression.

It is possible (via some kind of flag) to avoid parsing [_ <-- exp]
outside of perform. But this becomes quite complex and unreliable.  To
record a particular expression [patt <-- exp] in AST, we use the node
{[
    <:expr< let [(patt, exp)] in $lid:"<--"$ >>
]}
If the construction [_ <-- exp] is used by mistake, we get an error
message about an unbound identifier "<--", which is our intention.


{2 Known Issues}

- Sum types are assumed to have more than one constructor, thus always
  yield refutable patterns.  This is, if you define
  {[
        type t = T
  ]}
  and later use
  {[
        perform T <- T; ...
  ]}
  you get "Warning U: this match case is unused." which is not deserved.

- Aliases in patterns are not supported yet.  Code like
  {[
        perform
          ((x, y, z) as tuple) <-- 1, 2, 3;
          ...
  ]}
  blows the extension out of the water.  As a matter of fact, it is
  not clear that this should be supported at all: patterns with
  aliases are not "simple patterns" (see {i pa_o.ml}).  For example,
  patterns with aliases cannot be used in [fun pattern -> ...].  Thus,
  at present monadic bindings should include only those patterns that
  are permissible in [fun].  And perhaps this is the optimal decision.

- The recursive form "[rec ... <-- ...]" is not implemented completely.
  It lacks support for a (user-sepecified) fix-point function.  See
  for example Erkök and Launchbury's
  {{:http://www.cse.ogi.edu/PacSoft/projects/rmb/recdo.ps.gz} "A
  Recursive do for Haskell"}.
 *)


(** [failure_text]

    This is the text that accompanies a match failure of a refutable
    pattern. *)
let failure_text = "pattern match"


(** [default_bind_expr _loc]

    This is the default expression for the "bind" function. *)
let default_bind_expr (_loc: MLast.loc): MLast.expr =
  <:expr< bind >>


(** [default_failure_fun_expr _loc]

    This is the expression for the default "failwith" function. *)
let default_failure_fun_expr (_loc: MLast.loc): MLast.expr =
  <:expr< failwith >>


(** [default_failure_expr _loc]

    This is the expression for the default "failwith" function
    ({!Pa_monad.default_failure_fun_expr}) after the
    {!Pa_monad.failure_text} has been applied. *)
let default_failure_expr (_loc: MLast.loc): MLast.expr =
  let fun_expr = default_failure_fun_expr _loc
  and text_expr = <:expr< $str:failure_text$ >> in
    <:expr< $fun_expr$ $text_expr$ >>


(** [exp_to_patt _loc an_expression]

    Convert [an_expression] to a (simple) pattern, if we "accidentally" parse
    a pattern as an expression. *)
(* The code is based on [pattern_eq_expression] in {i pa_fstream.ml}. *)
let rec exp_to_patt (_loc: MLast.loc) (an_expression: MLast.expr): MLast.patt =
  match an_expression with
      <:expr< $int:s$ >> -> <:patt< $int:s$ >> (* integer constant *)
    | <:expr< $chr:c$ >> -> <:patt< $chr:c$ >> (* character constant *)
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
    | <:expr< ($e$ : $t$) >> ->                (* type restriction *)
      let p = exp_to_patt _loc e in
        <:patt< ($p$ : $t$) >>
    | _ -> Stdpp.raise_with_loc _loc
             (Stream.Error "exp_to_patt: this pattern is not yet supported")


(** [patt_to_exp _loc a_pattern]

    Convert [a_pattern] to an expression, if we must reuse it an a
    different semantic position. *)
let rec patt_to_exp (_loc: MLast.loc) (a_pattern: MLast.patt): MLast.expr =
  match a_pattern with
      <:patt< $int:s$ >> -> <:expr< $int:s$ >> (* integer constant *)
    | <:patt< $chr:c$ >> -> <:expr< $chr:c$ >> (* character constant *)
    | <:patt< $str:s$ >> -> <:expr< $str:s$ >> (* string constant *)
    | <:patt< $lid:b$ >> -> <:expr< $lid:b$ >> (* local variable *)
    | <:patt< $uid:b$ >> -> <:expr< $uid:b$ >> (* variable of other module *)
    | <:patt< $e1$ $e2$ >> ->                  (* function application *)
      let p1 = patt_to_exp _loc e1
      and p2 = patt_to_exp _loc e2 in
        <:expr< $p1$ $p2$ >>
    | <:patt< ($list:el$) >> ->                (* tuple *)
      let pl = List.map (patt_to_exp _loc) el in
        <:expr< ($list:pl$) >>
    | <:patt< { $list:eel$ } >> ->             (* record *)
      let ppl = List.map (fun (p, e) -> p, patt_to_exp _loc e) eel in
        <:expr< { $list:ppl$ } >>
    | <:patt< ($e$ : $t$) >> ->                (* type restriction *)
      let p = patt_to_exp _loc e in
        <:expr< ($p$ : $t$) >>
    | _ -> Stdpp.raise_with_loc _loc
             (Stream.Error "patt_to_exp: this expression is not yet supported")



(** [is_irrefutable_pattern a_pattern]

    Answer whether [a_pattern] is irrefutable. *)
let rec is_irrefutable_pattern (a_pattern: MLast.patt): bool =
  match a_pattern with
      <:patt< { $list:ppl$ } >> ->             (* record *)
      List.for_all (fun (_p1, p2) -> is_irrefutable_pattern p2) ppl
    | <:patt< () >> -> true                    (* unit *)
    | <:patt< ( $p$ : $_t$ ) >> ->             (* type constraint *)
      is_irrefutable_pattern p
    | <:patt< ( $p1$ as $_p2$ ) >> ->          (* alias *)
      is_irrefutable_pattern p1
    | <:patt< ( $list:pl$ ) >> ->              (* tuple of patterns *)
      List.for_all is_irrefutable_pattern pl
    | <:patt< $lid:_s$ >> -> true              (* variable *)
    | <:patt< _ >> -> true                     (* wildcard *)
    | _ -> false


(** [tuplify_expr _loc an_expression_list]

    Convert [an_expression_list] to a tuple of expressions. *)
let tuplify_expr (_loc: MLast.loc) (an_expression_list: MLast.expr list): MLast.expr =
  match an_expression_list with
      x :: [] -> x
    | xs -> <:expr< ($list:xs$) >>


(** [tuplify_patt _loc a_pattern_list]

    Convert [a_pattern_list] to a tuple of patterns. *)
let tuplify_patt (_loc: MLast.loc) (a_pattern_list: MLast.patt list): MLast.patt =
  match a_pattern_list with
      x :: [] -> x
    | xs -> <:patt< ($list:xs$) >>


(** [convert _loc a_perform_body a_bind_function a_fail_function]

    Convert all expressions of [a_perform_body] inside [perform] into
    core OCaml.  Use [a_bind_function] as the monad's "bind"-function,
    and [a_fail_function] as the "failure"-function. *)
let convert
    (_loc: MLast.loc)
    (a_perform_body: MLast.expr)
    (a_bind_function: MLast.expr)
    (a_fail_function: MLast.expr): MLast.expr =
  let rec loop _loc a_perform_body =
    match a_perform_body with
        <:expr< let $opt:false$ $list:((_patt, _expr) :: [])$ in $lid:"<--"$ >> ->
          Stdpp.raise_with_loc _loc
            (Stream.Error "convert: monadic binding cannot be last a \"perform\" body")
      | <:expr< let $opt:true$ $list:((_patt, _expr) :: [])$ in $lid:"<--"$ >> ->
          Stdpp.raise_with_loc _loc
            (Stream.Error "convert: recursive monadic binding cannot be last a \"perform\" body")
      | <:expr< let $opt:false$ $list:bs$ in $body$ >> ->
          let body' = loop _loc body in
            <:expr< let $opt:false$ $list:bs$ in $body'$ >>
      | <:expr< let $opt:true$ $list:bs$ in $body$ >> ->
          let body' = loop _loc body in
            <:expr< let $opt:true$ $list:bs$ in $body'$ >>
      | <:expr< let module $m$ = $mb$ in $body$ >> ->
          let body' = loop _loc body in
            <:expr< let module $m$ = $mb$ in $body'$ >>
      | <:expr< do { $list:(b1 :: b2 :: bs)$ } >> ->
          let do_rest () =
            loop _loc
              (match bs with
                   [] -> b2
                 | _  -> <:expr< do { $list:(b2 :: bs)$ } >>)
          and do_merge a_body =
            loop _loc <:expr< do { $list:(a_body :: b2 :: bs)$ } >> in
              begin
                match b1 with
                    (* monadic binding *)
                    <:expr< let $opt:false$ $list:((p, e) :: [])$ in $lid:"<--"$ >> ->
                      if is_irrefutable_pattern p then
                        <:expr< $a_bind_function$ $e$ (fun $p$ -> $do_rest ()$) >>
                      else
                        <:expr< $a_bind_function$
                                  $e$
                                  (fun [$p$ -> $do_rest ()$
                                        | _ -> $a_fail_function$ ]) >>
                    (* recursive monadic binding *)
                  | <:expr< let $opt:true$ $list:bindings$ in $lid:"<--"$ >> ->
                      let pattern_list = List.map fst bindings in
                      let patterns = tuplify_patt _loc pattern_list
                      and patt_as_exp =
                        tuplify_expr
                          _loc
                          (List.map (fun x -> patt_to_exp _loc x) pattern_list)
                      in
                        List.iter
                          (fun p ->
                             if not (is_irrefutable_pattern p) then
                               Stdpp.raise_with_loc _loc
                                 (Stream.Error
                                    ("convert: refutable patterns like '" ^
                                       Pcaml.string_of Pcaml.pr_patt p ^
                                       "' and recursive bindings do not go together")))
                          pattern_list;
                        <:expr< let rec $list:bindings$ in
                          $a_bind_function$
                            $patt_as_exp$
                            (fun $patterns$ -> $do_rest ()$) >>
                  | (* map through the regular let *)
                    <:expr< let $opt:false$ $list:bs$ in $body$ >> ->
                      <:expr< let $opt:false$ $list:bs$ in $do_merge body$ >>
                  | <:expr< let $opt:true$ $list:bs$ in $body$ >> ->
                      <:expr< let $opt:true$ $list:bs$ in $do_merge body$ >>
                  | <:expr< let module $m$ = $mb$ in $body$ >> ->
                      <:expr< let module $m$ = $mb$ in $do_merge body$ >>
                  | _ -> <:expr< $a_bind_function$ $b1$ (fun _ -> $do_rest ()$) >>
              end
      | any_body -> any_body
  in loop _loc a_perform_body


(** [qualify _loc a_module_expression a_function_expression]

    Append [a_function_expression] to the module name given in
    [a_module_expression], this is, qualify [a_function_expression] by
    [a_module_expression].  Fail if [a_module_expression] is not a valid
    module name. *)
let qualify
    (_loc: MLast.loc)
    (a_module_expression: MLast.expr)
    (a_function_expression: MLast.expr): MLast.expr =
  match a_module_expression with
      <:expr< $_e1$ . $_e2$ >> as m -> <:expr< $m$ . $a_function_expression$ >>
    | <:expr< $uid:_s$ >> as m -> <:expr< $m$ . $a_function_expression$ >>
    | _ -> failwith "qualify: 'with module' expects a module name or module-path."


(* Here we have to do the same nasty trick that Camlp4 uses and even
 * mentions in its documentation (viz. 'horrible hack' in pa_o.ml).  We
 * see if we can expect [patt <--] succeed.  Here [patt] is a simple
 * pattern and it definitely does not parse as an expression.
 * Rather than resorting to unlimited lookahead and emulating the
 * Pcaml.patt LEVEL simple grammar, we do it the other way around: We
 * make sure that a pattern can always be parsed as an expression and
 * declare "[_]" a valid identifier!  If you attempt to use it,
 * you will get an undefined identifier anyway, so it is safe. *)

EXTEND
    GLOBAL: Pcaml.expr;

    Pcaml.expr: LEVEL "expr1"
    [
      [ "perform"; "with"; "module"; monad_module = Pcaml.expr; "in";
        perform_body = Pcaml.expr LEVEL "top" ->
          let qualified_fail_expr =
            qualify _loc monad_module (default_failure_fun_expr _loc) in
            convert _loc
              perform_body
              (qualify _loc monad_module (default_bind_expr _loc))
              <:expr< $qualified_fail_expr$ $str:failure_text$ >> ]
    |
      [ "perform"; "with"; bind_fun = Pcaml.expr;
                           fail_fun = OPT opt_failure_expr; "in";
        perform_body = Pcaml.expr LEVEL "top" ->
          convert _loc
            perform_body
            bind_fun
            (match fail_fun with
                 None -> default_failure_expr _loc
               | Some f -> <:expr< $f$ $str:failure_text$ >>) ]
    |
      [ "perform";
        perform_body = Pcaml.expr LEVEL "top" ->
          convert _loc
            perform_body
            (default_bind_expr _loc)
            (default_failure_expr _loc) ]
    ] ;

    opt_failure_expr:
    [
      [ "and"; fail_fun = Pcaml.expr -> fail_fun ]
    ] ;

    recursive_monadic_binding:
    [
      [ e1 = Pcaml.expr LEVEL "simple"; "<--"; e2 = Pcaml.expr LEVEL "expr1" ->
        (exp_to_patt _loc e1, e2) ]
    ] ;

    Pcaml.expr: BEFORE "apply"
    [ NONA
      [ "rec"; binding_list = LIST1 recursive_monadic_binding SEP "and" ->
          <:expr< let $opt:true$ $list:binding_list$ in $lid:"<--"$ >> ]
    |
      [ e1 = SELF; "<--"; e2 = Pcaml.expr LEVEL "expr1" ->
        let p1 = exp_to_patt _loc e1 in
          <:expr< let $opt:false$ $list:((p1, e2) :: [])$ in $lid:"<--"$ >> ]
    ] ;

   (* The difference between the expression and patterns is just [_].
    * So, we make [_] identifier. *)
   Pcaml.expr: LEVEL "simple"
   [
     [ "_" -> <:expr< $lid:"_"$ >> ]
   ] ;

END;
