(* The SUPER-REIFIER: the reifier of AST as code *)

let id'counter = ref 0
let gensym () = 
  let v = !id'counter in
  let () = id'counter := v+1 in
  "g~~" ^ string_of_int v
;;

let distinguished_1op = ["-"; "ref"]
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
  | <:expr< True >> as e    -> lift_lit loc m e
  | <:expr< False >> as e   -> lift_lit loc m e

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

  | <:expr< $e1$.val >> -> lift1 loc m "!" (lift_exp loc m e1)

  | <:expr< $lid:op$ $e1$ >> when List.mem op distinguished_1op 
    -> lift1 loc m op (lift_exp loc m e1)
  | <:expr< $lid:op$ $e1$ $e2$ >> when List.mem op distinguished_2op 
    -> lift2 loc m op (lift_exp loc m e1) (lift_exp loc m e2)
  | <:expr< $lid:i$ >> as e -> lift_simple loc m e

  | <:expr< do { $list:el$ } >> -> 
      (match el with
      |	[]  -> failwith "cannot happen: empty el in `do' expr"
      |	[e] -> lift_exp loc m e
      |	[e1;e2] -> lift2 loc m "fseq" (lift_exp loc m e1) (lift_exp loc m e2)
      |	(h::t) -> lift2 loc m "fseq" (lift_exp loc m h)
	                   (lift_exp loc m  <:expr< do { $list:t$ } >>))

	(* for i = e1 to e2 in body
	   ==>
	   ENV.ffor true <e1> <e2> (fun i' -> .<let i = .~i' in .~(<body>)>.)
	 *)
  | <:expr< for $s$ = $e1$ $to:b$ $e2$ do { $list:el$ } >> ->
      let ni = gensym () in
      let ni' = <:patt< $lid:ni$ >> in
      let l = [(<:patt< $lid:s$ >>, MLast.ExEsc(loc,<:expr< $lid:ni$ >>))] in
      let body = MLast.ExEsc(loc,lift_exp loc m <:expr< do { $list:el$ } >>) in
      let body = MLast.ExBrk(loc,
                             <:expr< let $opt:false$ $list:l$ in $body$ >>) in
      let b = if b then <:expr< True >> else <:expr< False >> in
      <:expr< $uid:m$ . $lid:"ffor"$ $b$
                             $lift_exp loc m e1$
                             $lift_exp loc m e2$
                             (fun [$ni'$ -> $body$]) >>

  | <:expr< $e1$.val := $e2$ >> ->
      lift2 loc m "fass" (lift_exp loc m e1) (lift_exp loc m e2)

  | <:expr< $e1$ .( $e2$ ) >> ->
      lift2 loc m "aref" (lift_exp loc m e1) (lift_exp loc m e2)
      
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

