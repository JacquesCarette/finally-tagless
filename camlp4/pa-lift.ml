let uplaceholder = "Placeholder"
;;

let id'counter = ref 0
let gensym () = 
  let v = !id'counter in
  let () = id'counter := v+1 in
  "g~~" ^ string_of_int v
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
  | <:expr< let $opt:o$ $list:l$ in $x$ >> ->
      let x = adjust'placeholder loc m x in
      let l = List.map (fun (p,e) -> (p,adjust'placeholder loc m e)) l in
      <:expr< let $opt:o$ $list:l$ in $x$ >>
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

let unit_expr loc = MLast.ExBrk (loc, <:expr< () >>) 
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
	   We support only a subset of OCaml let rec
	   In particular, args must be simple patterns. No "and" clauses.

	   let rec p a1 a2 = e in body
	   ==>
	   ENV.bind .<fun self a1 a2 ->
                      let p x = self x in .~(.<e>.)>.
           (fun loop ->
           .<let p = .~(ENV.ym loop) in .~(.<body>)>.)
          *)
      
        "let"; "rec"; p = Pcaml.patt LEVEL "simple";
          args = LIST1 Pcaml.patt LEVEL "simple"; "=";
          e = myexpr; "in"; body = myexpr LEVEL "top" ->
         let self = gensym () in
         let l = [(p,<:expr< $lid:self$ >>)] in
         let bind_arg1 = 
         MLast.ExBrk (loc,
            List.fold_right 
              (fun p1 e -> <:expr< fun [ $p1$ -> $e$ ] >>)
              (<:patt< $lid:self$ >> :: args)      
              <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc(loc,e)$>>) in
         let lv = gensym () in
         let lvp = <:patt< $lid:lv$ >> in
         let l = [(p,MLast.ExEsc(loc,
                  <:expr< $uid:uplaceholder$ . $lid:"ym"$ $lid:lv$ >>))] in
         let body' = 
           <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc(loc,body)$ >> in
         let bind_arg2 = 
           <:expr< fun [ $lvp$ -> $MLast.ExBrk(loc,body')$ ] >> in
        <:expr< $uid:uplaceholder$ . $lid:"bind"$ $bind_arg1$ $bind_arg2$ >>
   

         (*
	    We support only a subset of OCaml let: No "and" clauses.
	    only one pattern on the left of "=".

	    let i = e in x
	    ==>
	    ENV.bind .<e>. (fun gensym -> .<let i = .~gensym in .~(.<x>.)>.)
	 *)
      | "let"; p = Pcaml.patt LEVEL "simple"; "="; e = myexpr; "in";
           x = myexpr LEVEL "top" ->
        let p' = gensym () in
        let pp' = <:patt< $lid:p'$ >> in
        let l = [(p,(MLast.ExEsc (loc,<:expr< $lid:p'$ >>)))] in
        let fbody = MLast.ExBrk (loc, 
             <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc (loc, x)$ >>) in
        <:expr< $uid:uplaceholder$ . $lid:"bind"$ 
                  $e$ 
                  (fun [ $pp'$ -> $fbody$ ])  >>
(*
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
      | "if"; e1 = SELF; "then"; e2 = myexpr LEVEL "expr1";
        "else"; e3 = myexpr LEVEL "expr1" ->
          <:expr< $uid:uplaceholder$ . $lid:"fif"$ $e1$ $e2$ $e3$ >>
      | "if"; e1 = SELF; "then"; e2 = myexpr LEVEL "expr1" ->
          <:expr< $uid:uplaceholder$ . $lid:"fif"$ $e1$ $e2$ $unit_expr loc$ >>
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
*)
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF ->  lift2 loc "<"  e1 e2
      | e1 = SELF; ">"; e2 = SELF ->  lift2 loc ">"  e1 e2
      | e1 = SELF; "<="; e2 = SELF -> lift2 loc "<=" e1 e2
      | e1 = SELF; ">="; e2 = SELF -> lift2 loc ">=" e1 e2
      | e1 = SELF; "="; e2 = SELF ->  lift2 loc "="  e1 e2
      | e1 = SELF; "<>"; e2 = SELF -> lift2 loc "<>" e1 e2
      | e1 = SELF; "=="; e2 = SELF -> lift2 loc "==" e1 e2 
      | e1 = SELF; "!="; e2 = SELF -> lift2 loc "!=" e1 e2
      | e1 = SELF; "$"; e2 = SELF ->  lift2 loc "$"  e1 e2
   (* | e1 = SELF; op = infixop0; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> *)
     ]
(*
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF -> <:expr< $e1$ ^ $e2$ >>
      | e1 = SELF; "@"; e2 = SELF -> <:expr< $e1$ @ $e2$ >>
      | e1 = SELF; op = infixop1; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | RIGHTA
      [ e1 = SELF; "::"; e2 = SELF -> <:expr< [$e1$ :: $e2$] >> ]
*)
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF -> lift2 loc "+" e1 e2
      | e1 = SELF; "-"; e2 = SELF -> lift2 loc "-" e1 e2
   (* | e1 = SELF; op = Pcaml.infixop2; e2 = SELF -> 
	  lift2 loc op e1 e2  *)
      ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF ->    lift2 loc "*" e1 e2 
      | e1 = SELF; "/"; e2 = SELF ->    lift2 loc "/" e1 e2
      | e1 = SELF; "%"; e2 = SELF ->    lift2 loc "%" e1 e2
      | e1 = SELF; "land"; e2 = SELF -> lift2 loc "land" e1 e2
      | e1 = SELF; "lor"; e2 = SELF ->  lift2 loc "lor"  e1 e2
      | e1 = SELF; "lxor"; e2 = SELF -> lift2 loc "lxor" e1 e2
      | e1 = SELF; "mod"; e2 = SELF ->  lift2 loc "mod"  e1 e2
   (* | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >>*)
      ]
(*
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
*)
      | "("; ")" -> unit_expr loc 
(*      | "("; op = operator_rparen -> <:expr< $lid:op$ >> *)
(*      | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >> *)
      | "("; e = SELF; ")" -> <:expr< $e$ >>
(*      | ".<"; e = SELF; ">." -> MLast.ExBrk loc e  (* XXO *) *)
      | "begin"; e = SELF; "end" -> <:expr< $e$ >>
      | "begin"; "end" -> unit_expr loc
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
