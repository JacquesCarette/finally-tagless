(* name:          pa_monad.ml
 * synopsis:      Haskell-like "do" for monads
 * authors:       Jacques Carette and Oleg Kiselyov,
 *                based in part of work of Lydia E. Van Dijk
 * last revision: Sun Dec 18 08:43:10 UTC 2005
 * ocaml version: 3.09.0 *)


(** {2 Syntax Extension to Support Monads}

This module extends OCaml's syntax by a Haskell-like "do"-notation
particularly suited for the work with monads.

{2 Conversion Rules}

{3 Grammar informally}
We support four different constructs to introduce a monadic
expressions.
{[
        perform exp
        perform exp1; exp2
        perform x <-- exp1; exp2
        perform let x = foo in exp
]}
which is almost literally the grammar of the Haskell "do"-notation,
with the differences that Haskell uses "do" and "<-" where we use
"[perform]" and "[<--]".

The actual bind function of the monad defaults to "[bind]" and the
match-failure function to "[failwith]" (only used for refutable
patterns; see below).

{4 Extended Forms}
To select a different function, use the extended forms of "[perform]".

{b Expression:} Use the given expression as bind-function and apply
the default match-failure function ([failwith]) where necessary.
{[
        perform with exp1 in exp2
        perform with exp1 in exp3; exp4
        perform with exp1 in x <-- exp2; exp3
        perform with exp in let x = foo in exp
]}
Use the first given expression as bind-function and the second as
match-failure function.
{[
        perform with exp1 and exp2 in exp3
        perform with exp1 and exp2 in exp3; exp4
        perform with exp1 and exp2 in x <-- exp3; exp4
        perform with exp1 and exp2 in let x = foo in exp1
]}

{b Module:} Use the function named "[bind]" from module "[Mod]".  In
addition use the module's "[failwith]" function in refutable patterns.
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

{3 Grammar formally}
{[
        "perform" ["with" <user-function-spec> "in"] <perform-body>
        <user-function-spec> ::=
                  EXPR ["and" EXPR]
                | "module" MODULE-NAME
        <perform-body> ::=
                  "let" LET-BINDING "=" EXPR ("and" LET-BINDING "=" EXPR)* "in" ";" <perform-body>
                | EXPR
                | (PATTERN "<--")? EXPR ";" <perform-body>
]}
where [EXPR] is an OCaml expression {i expr} as defined in Section 6.7
of the OCaml manual, [MODULE-NAME] a {i module-name} (Sec. 6.3),
[LET-BINDING] a {i let-binding} (Sec. 6.7), and [PATTERN] a {i
pattern} (Sec. 6.6).

For any ['a monad] the expansion uses the functions "[bind]" and
"[failwith]" with the signatures
{[
        val bind: 'a monad -> ('a -> 'b monad) -> 'b monad
        val failwith: string -> 'a monad
]}
unless overridden by the user.  Analogously, the signatures of modules
used in the [with module] form must enclose
{[
        sig
          type 'a monad
          val bind: 'a monad -> ('a -> 'b monad) -> 'b monad
          val failwith: string -> 'a monad
        end
]}

{3 Semantics (as re-writing into the core language)}
In the following, we abbreviate irrefutable patterns with [ipat] and
refutable patterns with [rpat].
{[
        perform exp                                      ===>  exp
        perform ipat <-- exp; rest                       ===>  bind exp (fun ipat -> perform rest)
        perform rpat <-- exp; rest                       ===>  bind exp (fun rpat -> perform rest
                                                                           | _ -> failwith "pattern match")
        perform with bexp in ipat <-- exp; rest          ===>  bexp exp (fun ipat -> perform rest)
        perform with bexp in rpat <-- exp; rest          ===>  bexp exp (fun rpat -> perform rest
                                                                           | _ -> failwith "pattern match")
        perform with bexp and fexp in rpat <-- exp; rest ===>  bexp exp (fun rpat -> perform rest
                                                                         | _ -> fexp "pattern match")
        perform with module Mod in ipat <-- exp; rest    ===>  Mod.bind exp (fun ipat -> perform rest)
        perform with module Mod in rpat <-- exp; rest    ===>  Mod.bind exp (fun rpat -> perform rest
                                                                               | _ -> Mod.failwith "pattern match")
        perform exp; rest                                ===>  bind exp (fun _ -> perform rest)
        perform with bexp in exp; rest                   ===>  bexp exp (fun _ -> perform rest)
        perform with module Mod in exp; rest             ===>  Mod.bind exp (fun _ -> perform rest)
        perform let pat = exp in; rest                   ===>  let pat = exp in perform rest
]}

It is be possible to use "[<-]" instead of "[<--]".  In that case,
the similarity to the "[do]" notation of Haskell will be complete.
However, if the program has [_ <- exp] outside of perform, this will be 
accepted by the parser (and create an (incomprehensible) error later on.
It's better to use a dedicated symbol [<--], so if the user abuses
it, the error should be clear right away.

The major difficulty with the [perform] notation is that it cannot
truly be parsed by an LR-grammar.  Indeed, to figure out if we should
start parsing <perform-body> as an expression or a pattern, we have to
parse it as a pattern and check the "[<--]" delimiter.  If it is not
there, we should {e backtrack} and parse it again as an expression.
Furthermore, [a <-- b] (or [a <- b]) can also be parsed as an
expression.  However, some patterns, for example ([_ <-- exp]), cannot
be parsed as an expression.

It is possible (via some kind of flag) to avoid parsing [_ <-- exp]
outside of perform. But this becomes quite complex and unreliable.
To record a particular expression [patt <-- exp] in AST, we use a node
<:expr< let [(patt, exp)] in $lid:"<--"$ >>
If the construction [_ <-- exp] is used by mistake, we get an error message
about an unbound identifier "<--". That is the intention.


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
  blows the extension out of the water.
  But this is not a simple pattern. It can't be used in fun ...
- Recursive binding (like Haskell's [mdo]) is not implemented.  For
  example,
  {[
        perform
          xs <-- Some (1 :: xs);
          return xs
  ]}
  is translated by this syntax extension, but the resulting code does
  not compile.
- From an old posting of James Woodyatt:
  {i ... since Ocaml strictly evaluates all the arguments to a
     function before executing it (unless the argument is of type 'a
     Lazy.t), you really cannot define a proper (>>) operator for your
     monads.  It will not work the same as the one in Haskell, because Ocaml
     evaluates the right operand before it is needed, and you do not buy
     anything by fixing it so that it takes a Lazy.t value instead.}
  I have not encoutered a problem until now and do not know what think
  of this comment. -- Lydia  8-/
 *)


(** Syntactically, we can encounter three different kinds of monadic
    bindings.  Sometimes we find out about an expression that is not a
    monadic binding, just an expression; this is the fourth case. *)
type monadic_binding =
    BindL of (MLast.patt * MLast.expr) list (** "[let]" inside "[perform]" *)
  | BindMRef of MLast.patt * MLast.expr     (** bind ("[>>=]") using a refutable pattern *)
  | BindMIrref of MLast.patt * MLast.expr   (** bind ("[>>=]") using an irrefutable pattern *)
  | ThenM of MLast.expr                     (** "then"-binding, this is the "[>>]"-operator *)


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
(*  The code is based on [pattern_eq_expression] in {i pa_fstream.ml}. *)
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
    | _ -> failwith "exp_to_patt: this pattern is not yet supported"


(** [is_irrefutable_pattern a_pattern]

    Answer whether [a_pattern] is irrefutable. *)
let rec is_irrefutable_pattern (a_pattern: MLast.patt): bool =
  match a_pattern with
      <:patt< { $list:ppl$ } >> ->             (* record *)
      List.for_all (fun (_p1, p2) -> is_irrefutable_pattern p2) ppl
    | <:patt< () >> -> true                    (* unit *)
    | <:patt< ( $p$ : $t$ ) >> ->              (* type constraint *)
      is_irrefutable_pattern p
    | <:patt< ( $p1$ as $p2$ ) >> ->           (* alias *)
      is_irrefutable_pattern p1
    | <:patt< ( $list:pl$ ) >> ->              (* tuple of patterns *)
      List.for_all is_irrefutable_pattern pl
    | <:patt< $lid:s$ >> -> true               (* variable *)
    | <:patt< _ >> -> true                     (* wildcard *)
    | _ -> false
(** [is_irrefutable_expression an_expression]

(*
   Answer whether [an_expression] represents an irrefutable pattern. *)
(* Implementation Note: We correctly interpret parenthesized expressions
   and the wildcard ([_]).*)
and is_irrefutable_expression (an_expression: MLast.expr): bool =
  match an_expression with
      <:expr< { $list:eel$ } >> ->             (* record *)
      List.for_all (fun (_p, e) -> is_irrefutable_expression e) eel
    | <:expr< () >> -> true                    (* unit *)
    | <:expr< ( $exp$ : $t$ ) >> ->            (* type constraint *)
      is_irrefutable_expression exp
    | <:expr< ( $list:el$ ) >> ->
      List.for_all is_irrefutable_expression el
    | <:expr< $lid:s$ >> -> true               (* variable *)
    | _ -> false

*)

(** [convert _loc a_binding_list a_bind_function a_fail_function]

    Convert all expressions of [a_binding_list] inside [perform] into
    core OCaml.  Use [a_bind_function] as the monad's "bind"-function,
    and [a_fail_function] as the "failure"-function. *)
(*
let convert
    (_loc: MLast.loc)
    (a_binding_list: monadic_binding list)
    (a_bind_function: MLast.expr)
    (a_fail_function: MLast.expr): MLast.expr =
  let glob_then expr acc =
    <:expr< $a_bind_function$ $expr$ (fun _ -> $acc$) >>
  and glob_bind_irrefutable expr patt acc =
    <:expr< $a_bind_function$ $expr$ (fun $patt$ -> $acc$) >>
  and glob_bind_refutable expr patt acc =
    <:expr< $a_bind_function$ $expr$ (fun [$patt$ -> $acc$ | _ -> $a_fail_function$ ]) >> in
  let folder acc = function
      BindMIrref (patt, expr) -> glob_bind_irrefutable expr patt acc
    | BindMRef (patt, expr) -> glob_bind_refutable expr patt acc
    | ThenM expr -> glob_then expr acc
    | BindL lst -> <:expr< let $list:lst$ in $acc$ >>
  in
    match List.rev a_binding_list with
        [] -> failwith "convert: somehow got an empty list from a LIST1"
      | ThenM x :: xs -> List.fold_left folder <:expr< $x$ >> xs
      | _ -> failwith "convert: does not end with an expression"

*)

(* post-process the body of the perform expression. Essentially,
   we do fold over the expression *)
let rec convert
    (_loc: MLast.loc)
    (body: MLast.expr)
    (a_bind_function: MLast.expr)
    (a_fail_function: MLast.expr): MLast.expr =
  match body with
  | 
    <:expr< let $opt:false$ $list:((p,e) :: [])$ in $lid:"<--"$ >> ->
      Stdpp.raise_with_loc _loc 
	(Stream.Error 
	   "Monadic binding cannot be the last in thing in perform body")
  | 
    <:expr< let $opt:false$ $list:bs$ in $body$ >> ->
      let body' = convert _loc body a_bind_function a_fail_function in
      <:expr< let $opt:false$ $list:bs$ in $body'$ >>
  | 
    <:expr< let $opt:true$ $list:bs$ in $body$ >> ->
      let body' = convert _loc body a_bind_function a_fail_function in
      <:expr< let $opt:true$ $list:bs$ in $body'$ >>
  | 
    <:expr< let module $m$ = $mb$ in $body$ >> ->
      let body' = convert _loc body a_bind_function a_fail_function in
      <:expr< let module $m$ = $mb$ in $body'$ >>
  | 
    <:expr< do { $list:(b1 :: b2 :: brest)$ } >> ->
      let do_rest ()  = 
	convert _loc
	  (match brest with [] -> b2 
	                    | _ -> <:expr< do { $list:(b2 :: brest)$ } >>)
	  a_bind_function a_fail_function 
      and do_merge body =
	convert _loc
	  <:expr< do { $list:(body :: b2 :: brest)$ } >> 
          a_bind_function a_fail_function in
      (match b1 with
      |	(* monadic binding *)
	<:expr< let $opt:false$ $list:((p,e) :: [])$ in $lid:"<--"$ >> ->
	  if is_irrefutable_pattern p
	  then
	    <:expr< $a_bind_function$ $e$ (fun $p$ -> $do_rest ()$) >>
	  else
	    <:expr< $a_bind_function$ $e$ 
	       (fun [$p$ -> $do_rest ()$ | _ -> $a_fail_function$ ]) >>
      |	(* map through the regular let *)
	<:expr< let $opt:false$ $list:bs$ in $body$ >> ->
	  <:expr< let $opt:false$ $list:bs$ in $do_merge body$ >>
      |
	<:expr< let $opt:true$ $list:bs$ in $body$ >> ->
	  <:expr< let $opt:true$ $list:bs$ in $do_merge body$ >>
      | 
	<:expr< let module $m$ = $mb$ in $body$ >> ->
	  <:expr< let module $m$ = $mb$ in $do_merge body$ >>
      | _ -> 
	  <:expr< $a_bind_function$ $b1$ (fun _ -> $do_rest ()$) >>)
  | 
    body -> body




(** [qualify _loc a_module_expression a_function_expression]

    Append [a_function_expression] to the module name given in
    [a_module_expression], this is qualify [a_function_expression] by
    [a_module_expression].  Fail if [a_module_expression] is not a valid
    module name. *)
let qualify
    (_loc: MLast.loc)
    (a_module_expression: MLast.expr)
    (a_function_expression: MLast.expr): MLast.expr =
  match a_module_expression with
      <:expr< $e1$ . $e2$ >> as m -> <:expr< $m$ . $a_function_expression$ >>
    | <:expr< $uid:s$ >> as m -> <:expr< $m$ . $a_function_expression$ >>
    | _ -> failwith "qualify: 'with module' expects a module name or module-path."

(* And here we have to do the same nasty trick that Camlp4 uses and even
   mentions in its documentation 
   cf. `horrible hack' in pa_o.ml
   We see if we can expect [patt <--] succeed. Here patt is a simple
   pattern and it definitely didn't parse as an expression.
   Well, rather than resort to unlimited lookahead and emulating
   the Pcaml.patt LEVEL simple grammar, we do the other way around.
   We make sure that a pattern can always be parsed as an expression.
   We declare "_" a valid identifier!
   Well, if you attempt to use it, you'll get an undefined identifier
   anyway, so it's safe...
*)


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
              <:expr< $qualified_fail_expr$ $str:failure_text$ >>]
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

    Pcaml.expr: BEFORE "apply"
    [ NONA
	[ e1 = SELF; "<--"; e2 = Pcaml.expr LEVEL "expr1" ->
	  let p1 = exp_to_patt _loc e1 in
          <:expr< let $list:((p1,e2) :: [])$ in $lid:"<--"$ >> ]
    ] ;

   (* The difference between the expression and patterns is just [_]
      So, we make [_] identifier...
   *)
   Pcaml.expr: LEVEL "simple"
   [
   [ "_" -> <:expr< $lid:"_"$ >> ]
   ];
   
(* Alas, the following doesn't work... expressions such as [set foo (x-y)]
   [-y] is attempted to be parsed as a pattern, and so parsing fails.

    Pcaml.expr: LAST
    [
	[ p1 = Pcaml.patt LEVEL "simple"; "<--"; 
	  e2 = Pcaml.expr LEVEL "expr1" ->
   
            <:expr< let $list:((p1,e2) :: [])$ in $lid:"<--"$ >>

	]
    ] ;
*)
(*
    monadic_binding:
    [
      [ "let"; l = LIST1 Pcaml.let_binding SEP "and"; "in" -> BindL l ]
    |
      [ x = Pcaml.expr LEVEL "expr1" ->
	(* For some patterns, "patt <-- exp" can parse as an
	   expression too.  So we have to figure out which is
	   which. *)
          match x with
              <:expr< $e1$ $lid:op$ $e2$ >> when op = "<--" ->
                    if is_irrefutable_expression e1 then
                       BindMIrref (exp_to_patt _loc e1, e2)
                    else BindMRef (exp_to_patt _loc e1, e2)
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
*)
END;

