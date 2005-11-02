let uplaceholder = "Placeholder"
;;

(* Replace UID uplaceholder with that of m *)
let rec adjust'placeholder loc m e =
  match e with
    <:expr< $lid:b$ >> as e -> e
  | <:expr< $int:s$ >> as e -> e
  | <:expr< $flo:s$ >> as e -> e
  | <:expr< $str:s$ >> as e -> e
  | <:expr< $chr:s$ >> as e -> e
  | <:expr< $uid:b$ >> when b = uplaceholder -> <:expr< $uid:m$ >>
  | <:expr< $uid:b$ >> as e -> e
  | <:expr< fun [$p$ -> $e$] >> -> 
      let e = adjust'placeholder loc m e in <:expr< fun [$p$ -> $e$] >>
  | <:expr< $e1$ $e2$ >> -> 
      let e1 = adjust'placeholder loc m e1 and 
	  e2 = adjust'placeholder loc m e2 in
      <:expr< $e1$ $e2$ >>
  | <:expr< $e1$ . $e2$ >> -> 
      let e1 = adjust'placeholder loc m e1 and 
	  e2 = adjust'placeholder loc m e2 in
      <:expr< $e1$ . $e2$ >>
  | <:expr< ($list:el$) >> ->
      let es = List.map (adjust'placeholder loc m) el in
      <:expr< ($list:es$) >>
  | MLast.ExEsc (loc,e) -> 
      MLast.ExEsc (loc, adjust'placeholder loc m e)
  | MLast.ExBrk (loc,e) -> 
      MLast.ExBrk (loc, adjust'placeholder loc m e)
  | e -> e
  | _ -> failwith "This pattern isn't yet supported"
;;

let lift_simple loc e = 
  let e = MLast.ExBrk (loc, e) in
  <:expr< $uid:uplaceholder$ . $lid:"retS"$ $e$ >>
;;

let lift2 loc name e1 e2 = 
  <:expr< $uid:uplaceholder$ . $lid:name$ $e1$ $e2$ >>
;;


EXTEND
    GLOBAL: Pcaml.expr; 

    Pcaml.expr: LEVEL "expr1"
    [ 
      [ "funM"; m = UIDENT; p = Pcaml.patt LEVEL "simple"; e = myfun_def ->
	  let e = adjust'placeholder loc m e in
          MLast.ExBrk (loc, <:expr< fun [$p$ -> $e$] >>)
      ] 
    ] ;

  (* Core expressions, lifted from camlp4/etc/pa_o.ml *)
  (* myexpr must return the code value *)
  myexpr:
    [ "top" RIGHTA
      [ e1 = SELF; ";" -> e1 ]
    | "expr1"
      [ 
(*
	"let"; "rec"; l = let_binding "in";
        x = myexpr LEVEL "top" ->
          <:expr< let $opt:o2b o$ $list:l$ in $x$ >>
      | "let"; l = let_binding "in";
        x = myexpr LEVEL "top" ->
          <:expr< let $opt:o2b o$ $list:l$ in $x$ >>
      | "function"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< fun [ $list:l$ ] >>
      | "fun"; p = patt LEVEL "simple"; e = fun_def ->
          <:expr< fun [$p$ -> $e$] >>
      | "match"; e = SELF; "with"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< match $e$ with [ $list:l$ ] >>
      | "try"; e = SELF; "with"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< try $e$ with [ $list:l$ ] >>
*)
      (* all three branches must return a code value *)
       "if"; e1 = SELF; "then"; e2 = myexpr LEVEL "expr1";
        "else"; e3 = myexpr LEVEL "expr1" ->
          <:expr< $uid:uplaceholder$ . $lid:"fif"$ $e1$ $e2$ $e3$ >>
      | "if"; e1 = SELF; "then"; e2 = myexpr LEVEL "expr1" ->
	  let e3 = MLast.ExBrk (loc, <:expr< () >>) in
          <:expr< $uid:uplaceholder$ . $lid:"fif"$ $e1$ $e2$ $e3$ >>
(*
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; e = SELF; "done" ->
          <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:get_seq e$ } >>
      | "while"; e1 = SELF; "do"; e2 = SELF; "done" ->
          <:expr< while $e1$ do { $list:get_seq e2$ } >>
*)
       ]
(*
    | [ e = SELF; ","; el = LIST1 NEXT SEP "," ->
          <:expr< ( $list:[e :: el]$ ) >> ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = expr LEVEL "expr1" ->
          <:expr< $e1$.val := $e2$ >>
      | e1 = SELF; "<-"; e2 = expr LEVEL "expr1" ->
          match bigarray_set loc e1 e2 with
          [ Some e -> e
          | None -> <:expr< $e1$ := $e2$ >> ] ]
    | "||" RIGHTA
      [ e1 = SELF; "or"; e2 = SELF -> <:expr< $lid:"or"$ $e1$ $e2$ >>
      | e1 = SELF; "||"; e2 = SELF -> <:expr< $e1$ || $e2$ >> ]
    | "&&" RIGHTA
      [ e1 = SELF; "&"; e2 = SELF -> <:expr< $lid:"&"$ $e1$ $e2$ >>
      | e1 = SELF; "&&"; e2 = SELF -> <:expr< $e1$ && $e2$ >> ]
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF -> <:expr< $e1$ < $e2$ >>
      | e1 = SELF; ">"; e2 = SELF -> <:expr< $e1$ > $e2$ >>
      | e1 = SELF; "<="; e2 = SELF -> <:expr< $e1$ <= $e2$ >>
      | e1 = SELF; ">="; e2 = SELF -> <:expr< $e1$ >= $e2$ >>
      | e1 = SELF; "="; e2 = SELF -> <:expr< $e1$ = $e2$ >>
      | e1 = SELF; "<>"; e2 = SELF -> <:expr< $e1$ <> $e2$ >>
      | e1 = SELF; "=="; e2 = SELF -> <:expr< $e1$ == $e2$ >>
      | e1 = SELF; "!="; e2 = SELF -> <:expr< $e1$ != $e2$ >>
      | e1 = SELF; "$"; e2 = SELF -> <:expr< $lid:"\$"$ $e1$ $e2$ >>
      | e1 = SELF; op = infixop0; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF -> <:expr< $e1$ ^ $e2$ >>
      | e1 = SELF; "@"; e2 = SELF -> <:expr< $e1$ @ $e2$ >>
      | e1 = SELF; op = infixop1; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | RIGHTA
      [ e1 = SELF; "::"; e2 = SELF -> <:expr< [$e1$ :: $e2$] >> ]
*)
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF -> lift2 loc "plus" e1 e2
      | e1 = SELF; "-"; e2 = SELF -> lift2 loc "minus" e1 e2
(*
      | e1 = SELF; op = Pcaml.infixop2; e2 = SELF -> 
	  lift2 loc op e1 e2 
*)
      ]
(*
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF -> <:expr< $e1$ * $e2$ >>
      | e1 = SELF; "/"; e2 = SELF -> <:expr< $e1$ / $e2$ >>
      | e1 = SELF; "%"; e2 = SELF -> <:expr< $lid:"%"$ $e1$ $e2$ >>
      | e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
      | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1$ lor $e2$ >>
      | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
      | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1$ mod $e2$ >>
      | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | "**" RIGHTA
      [ e1 = SELF; "**"; e2 = SELF -> <:expr< $e1$ ** $e2$ >>
      | e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1$ asr $e2$ >>
      | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1$ lsl $e2$ >>
      | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1$ lsr $e2$ >>
      | e1 = SELF; op = infixop4; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | "unary minus" NONA
      [ "-"; e = SELF -> <:expr< $mkumin loc "-" e$ >>
      | "-."; e = SELF -> <:expr< $mkumin loc "-." e$ >> ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF ->
          match constr_expr_arity loc e1 with
          [ 1 -> <:expr< $e1$ $e2$ >>
          | _ ->
              match e2 with
              [ <:expr< ( $list:el$ ) >> ->
                  List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) e1 el
              | _ -> <:expr< $e1$ $e2$ >> ] ]
      | "assert"; e = SELF ->
          match e with
          [ <:expr< False >> -> <:expr< assert False >>
          | _ -> <:expr< assert ($e$) >> ]
      | "lazy"; e = SELF ->
          <:expr< lazy ($e$) >> ]
    | "meta" NONA                             (* XXO *)
      [ ".!"; e = SELF -> MLast.ExRun loc e   (* XXO *)
      | ".~"; e = SELF -> MLast.ExEsc loc e ] (* XXO *)
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
      | e1 = SELF; "."; "{"; e2 = SELF; "}" -> bigarray_get loc e1 e2
      | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >> ]
    | "~-" NONA
      [ "!"; e = SELF -> <:expr< $e$ . val>>
      | "~-"; e = SELF -> <:expr< ~- $e$ >>
      | "~-."; e = SELF -> <:expr< ~-. $e$ >>
      | f = prefixop; e = SELF -> <:expr< $lid:f$ $e$ >> ]
*)
    | "simple" LEFTA
      [ s = INT -> lift_simple loc <:expr< $int:s$ >>
      | s = FLOAT -> lift_simple loc <:expr< $flo:s$ >>
      | s = STRING -> lift_simple loc <:expr< $str:s$ >>
      | c = CHAR -> lift_simple loc <:expr< $chr:c$ >>
      | UIDENT "True" -> lift_simple loc <:expr< $uid:" True"$ >>
      | UIDENT "False" -> lift_simple loc <:expr< $uid:" False"$ >>
      |	i = LIDENT -> lift_simple loc <:expr< $lid:i$ >>
(*
 * Skip qualified identifiers...
      | i = expr_ident -> i
*)

      | s = "false" -> lift_simple loc <:expr< False >>
      | s = "true" -> lift_simple loc <:expr< True >>
      | "["; "]" -> lift_simple loc <:expr< [] >>
(*
      | "["; el = expr1_semi_list; "]" -> <:expr< $mklistexp loc None el$ >>
      | "[|"; "|]" -> <:expr< [| |] >>
      | "[|"; el = expr1_semi_list; "|]" -> <:expr< [| $list:el$ |] >>
      | "{"; test_label_eq; lel = lbl_expr_list; "}" ->
          <:expr< { $list:lel$ } >>
      | "{"; e = expr LEVEL "."; "with"; lel = lbl_expr_list; "}" ->
          <:expr< { ($e$) with $list:lel$ } >>
      | "("; ")" -> <:expr< () >>
      | "("; op = operator_rparen -> <:expr< $lid:op$ >>
      | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
      | "("; e = SELF; ")" -> <:expr< $e$ >>
      | ".<"; e = SELF; ">." -> MLast.ExBrk loc e  (* XXO *)
      | "begin"; e = SELF; "end" -> <:expr< $e$ >>
      | "begin"; "end" -> <:expr< () >>
      | x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              ({Lexing.pos_fname = "";
                Lexing.pos_lnum = 0;
                Lexing.pos_bol = 0;
                Lexing.pos_cnum = int_of_string (String.sub x 0 i)},
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> (Token.nowhere, x) ]
          in
          Pcaml.handle_expr_locate loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_expr_quotation loc x 
*)
      ]
  
   ]

  ;

  myfun_def:
    [ RIGHTA
      [ p = Pcaml.patt LEVEL "simple"; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "->"; e = myexpr ->
	  MLast.ExEsc (loc, e)  ] ]
  ;

END;
