(* The SUPER-REIFIER: the reifier of AST as code *)

let uplaceholder = "Placeholder"
;;

let id'counter = ref 0
let gensym () = 
  let v = !id'counter in
  let () = id'counter := v+1 in
  "g~~" ^ string_of_int v
;;


(* Replace UID uplaceholder with UID m *)
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
let lift_lit loc e = 
  let e = MLast.ExBrk (loc, e) in
  <:expr< $uid:uplaceholder$ . $lid:"retL"$ $e$ >>
;;


let lift2 loc name e1 e2 = 
  <:expr< $uid:uplaceholder$ . $lid:name$ $e1$ $e2$ >>
;;


let unit_expr loc = MLast.ExBrk (loc, <:expr< () >>) 
;;

let distinguished_1op = ["-"]
;;

let distinguished_2op = 
  ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!=";
   "$";
   "+"; "+.";
   "-"; "-.";
   "*"; "*.";
   "/"; "/.";
   "%"; "land"; "lor"; "lxor"; "mod";
  ]
;;

let lift1 loc m name e1 = 
  <:expr< $uid:m$ . $lid:name$ $e1$ >>
;;

let lift_simple loc m e = 
  let e = MLast.ExBrk (loc, e) in
  <:expr< $uid:m$ . $lid:"retS"$ $e$ >>
;;
let lift_lit loc m e = 
  let e = MLast.ExBrk (loc, e) in
  <:expr< $uid:m$ . $lid:"retL"$ $e$ >>
;;

let lift2 loc m name e1 e2 = 
  <:expr< $uid:m$ . $lid:name$ $e1$ $e2$  >>
;;

let lift3 loc m name e1 e2 e3 = 
  <:expr< $uid:m$ . $lid:name$ $e1$ $e2$ $e3$ >>
;;

let rec lift_exp loc m e = 
  match e with
  | <:expr< $int:s$ >> as e -> lift_lit loc m e
  | <:expr< $flo:s$ >> as e -> lift_lit loc m e
  | <:expr< $str:s$ >> as e -> lift_lit loc m e
  | <:expr< $chr:s$ >> as e -> lift_lit loc m e
  | <:expr< () >> as e      -> lift_lit loc m e
  | <:expr< [] >> as e      -> lift_lit loc m e
  | <:expr< True >> as e      -> lift_lit loc m e
  | <:expr< False >> as e      -> lift_lit loc m e

  | <:expr< if $e1$ then $e2$ else $e3$ >> -> 
      lift3 loc m "fif" 
	(lift_exp loc m e1) (lift_exp loc m e2) (lift_exp loc m e3)

         (*
	    We support only a subset of OCaml let: No "and" clauses.
	    only one pattern on the left of "=".

	    let i = e in x
	    ==>
	    ENV.bind .<e>. (fun gensym -> .<let i = .~gensym in .~(.<x>.)>.)
	 *)
  | <:expr< let $opt:false$ $list:l$ in $body$ >> -> 
      let do_bind p e x =
        let p' = gensym () in
        let pp' = <:patt< $lid:p'$ >> in
        let l = [(p,(MLast.ExEsc (loc,<:expr< $lid:p'$ >>)))] in
        let fbody = MLast.ExBrk (loc, 
             <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc (loc, x)$ >>) in
        <:expr< $uid:m$ . $lid:"bind"$ 
                  $e$ 
                  (fun [ $pp'$ -> $fbody$ ])  >>
	in (match l with
	| [(p,e)] -> do_bind p (lift_exp loc m e) (lift_exp loc m body)
	| _ -> failwith "only one binding is supported in let" )  

        (*
	   We support only a subset of OCaml let rec
	   In particular, args must be simple patterns. No "and" clauses.

	   let rec p a1 a2 = e in body
	   ==>
	   .<let p = .~(ENV.ym .<fun self a1 a2 -> let p = self in .~(.<e>.)>.)
             in .~(.<body>.)>.
          *)
      
  | <:expr< let $opt:true$ $list:l$ in $body$ >> -> 
      let do_openrec p e body =
         let self = gensym () in
         let self' = <:patt< $lid:self$ >> in
         let l = [(p,<:expr< $lid:self$ >>)] in
	 let rec esc_e e =
	   (match e with
	   | <:expr< fun [ $p$ -> $e$ ] >> -> 
	       let e = esc_e e in <:expr< fun [ $p$ -> $e$ ] >> 
	   | _ -> let e = lift_exp loc m e in
	       <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc(loc,e)$>>)
	   in
         let ym_arg = 
          MLast.ExBrk (loc, <:expr< fun [ $self'$ -> $esc_e e$] >>) in
         let ym_app = <:expr< $uid:m$ . $lid:"ym"$ $ym_arg$ >> in
         let l = [(p,<:expr< $MLast.ExEsc(loc,ym_app)$ >>)] in
         MLast.ExBrk(loc,
          <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc(loc,body)$ >>)
	in (match l with
	| [(p,e)] -> do_openrec p e (lift_exp loc m body)
	| _ -> failwith "only one binding is supported in let rec" )  

  | <:expr< $lid:op$ $e1$ >> when List.mem op distinguished_1op 
    -> lift1 loc m op (lift_exp loc m e1)
  | <:expr< $lid:op$ $e1$ $e2$ >> when List.mem op distinguished_2op 
    -> lift2 loc m op (lift_exp loc m e1) (lift_exp loc m e2)
  | <:expr< $lid:i$ >> as e -> lift_simple loc m e

  | <:expr< $e1$ $e2$ >> -> 
      lift2 loc m "app" (lift_exp loc m e1) (lift_exp loc m e2)

  | e -> e
;;

EXTEND
    GLOBAL: Pcaml.expr; 

    Pcaml.expr: LEVEL "expr1"
    [ 
      [ "funM"; m = UIDENT; p = Pcaml.patt LEVEL "simple"; e = fun_def ->
	  let rec lift_body e =
	    match e with
	    | <:expr< fun [$p$ -> $e$] >> ->
		let e = lift_body e in <:expr< fun [$p$ -> $e$] >>
	    | _ -> MLast.ExEsc (loc,
			 <:expr< $uid:m$ . $lid:"run"$ $lift_exp loc m e$ >>)
	  in MLast.ExBrk (loc, lift_body <:expr< fun [$p$ -> $e$] >>)
      ] 
    ] ;

  (* Literally Pcaml.fun_def *)
  fun_def:
    [ RIGHTA
      [ p = Pcaml.patt LEVEL "simple"; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "->"; e = Pcaml.expr -> <:expr< $e$ >> ] ]
  ;

END;

(*
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
  (* myexpr must return the code value, of a monadic type *)
  myexpr:
    [ "top" RIGHTA
      [ e1 = SELF; ";"; e2 = SELF ->
	<:expr< $uid:uplaceholder$ . $lid:"fseq"$ $e1$ $e2$ >>
      | e1 = SELF; ";" -> e1 ]
    | "expr1"
      [ 
         (*
	   We support only a subset of OCaml let rec
	   In particular, args must be simple patterns. No "and" clauses.

	   let rec p a1 a2 = e in body
	   ==>
	   .<let p = .~(ENV.ym .<fun self a1 a2 -> let p = self in .~(.<e>.)>.)
             in .~(.<body>.)>.
          *)
      
        "let"; "rec"; p = Pcaml.patt LEVEL "simple";
          args = LIST1 Pcaml.patt LEVEL "simple"; "=";
          e = myexpr; "in"; body = myexpr LEVEL "top" ->
         let self = gensym () in
         let l = [(p,<:expr< $lid:self$ >>)] in
         let ym_arg = 
          MLast.ExBrk (loc,
           (List.fold_right 
              (fun p1 e -> <:expr< fun [ $p1$ -> $e$ ] >>)
              (<:patt< $lid:self$ >> :: args)      
              <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc(loc,e)$>>)) in
         let ym_app = <:expr< $uid:uplaceholder$ . $lid:"ym"$ $ym_arg$ >> in
         let l = [(p,<:expr< $MLast.ExEsc(loc,ym_app)$ >>)] in
         MLast.ExBrk(loc,
          <:expr< let $opt:false$ $list:l$ in $MLast.ExEsc(loc,body)$ >>)

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
          match None (* bigarray_set loc e1 e2*) with
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
      [ e1 = SELF; "+"; e2 = SELF ->  lift2 loc "+" e1 e2
      | e1 = SELF; "-"; e2 = SELF ->  lift2 loc "-" e1 e2
      | e1 = SELF; "+."; e2 = SELF -> lift2 loc "+." e1 e2
      | e1 = SELF; "-."; e2 = SELF -> lift2 loc "-." e1 e2
   (* | e1 = SELF; op = Pcaml.infixop2; e2 = SELF -> 
	  lift2 loc op e1 e2  *)
      ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF ->    lift2 loc "*"  e1 e2 
      | e1 = SELF; "*."; e2 = SELF ->   lift2 loc "*." e1 e2
      | e1 = SELF; "/"; e2 = SELF ->    lift2 loc "/"  e1 e2
      | e1 = SELF; "%"; e2 = SELF ->    lift2 loc "%"  e1 e2
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
*)
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF ->
          <:expr< $uid:uplaceholder$ . $lid:"app"$ $e1$ $e2$ >>

(*
          match constr_expr_arity loc e1 with
          [ 1 -> <:expr< $e1$ $e2$ >>
          | _ ->
              match e2 with
              [ <:expr< ( $list:el$ ) >> ->
                  List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) e1 el
              | _ -> <:expr< $e1$ $e2$ >> ] ]
*)
(*
      | "assert"; e = SELF ->
          match e with
4           [ <:expr< False >> -> <:expr< assert False >>
          | _ -> <:expr< assert ($e$) >> ]
      | "lazy"; e = SELF ->
          <:expr< lazy ($e$) >> 
*)
   ]
(*
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
      [ s = INT ->    lift_lit loc <:expr< $int:s$ >>
      | s = FLOAT ->  lift_lit loc <:expr< $flo:s$ >>
      | s = STRING -> lift_lit loc <:expr< $str:s$ >>
      | c = CHAR ->   lift_lit loc <:expr< $chr:c$ >>
      | UIDENT "True" ->  lift_lit loc <:expr< $uid:" True"$ >>
      | UIDENT "False" -> lift_lit loc <:expr< $uid:" False"$ >>
      |	i = LIDENT ->     lift_simple loc <:expr< $lid:i$ >>
(*
 * Skip qualified identifiers...
      | i = expr_ident -> i
*)

      | s = "false" -> lift_lit loc <:expr< False >>
      | s = "true" ->  lift_lit loc <:expr< True >>
      | "["; "]" ->    lift_lit loc <:expr< [] >>
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
	  MLast.ExEsc (loc, <:expr< $uid:uplaceholder$ . $lid:"run"$ $e$ >>)  
      ] ]
  ;

END;

*)
