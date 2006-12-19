        MetaOCaml version 3.09.1 alpha 029

#       val e1 : 'a list * int = ([], 0)
# val e2 : 'a Examples.T.b list * int = (["foo"], 1)
#   val e3 : 'a list * int = ([], 0)
# val e4 : ('a, '_b) Examples.TC2.abstract list * int =
  ([Examples.TC2.Ground "foo"], 1)
# val e5 : ('a, '_b) Examples.TC2.abstract list * int =
  ([Examples.TC2.Code .<"foo">.], 1)
#   val e6 : ('a, '_b) Examples.TC3.abstract list * int =
  ([Examples.TC3.Ground 6], 1)
# val e7 : ('a, '_b) Examples.TC3.abstract list * int =
  ([Examples.TC3.Code .<7>.; Examples.TC3.Ground 6], 2)
#   val e8 : ('a, '_b) Examples.TC4.abstract list * int =
  ([Examples.TC4.Ground .<6>.], 1)
# val e9 : ('a, '_b) Examples.TC4.abstract list * int =
  ([Examples.TC4.Code .<.<7>.>.; Examples.TC4.Ground .<6>.], 2)
#   val e10 : ('a, int list) code * int =
  (.<[(* cross-stage persistent value (as id: h) *);
   (* cross-stage persistent value (as id: h) *)]>.,
   2)
#     val w1 : 'a Examples.T.b = "foo"
# val w2 : 'a Examples.T.b list * int = ([], 0)
#   val w3 : ('a, '_b) Examples.TC2.abstract list * int =
  ([Examples.TC2.Ground "foo"; Examples.TC2.Ground "foo"], 2)
# val w4 : ('a, '_b) Examples.TC2.abstract list * int =
  ([Examples.TC2.Ground "foo"; Examples.TC2.Ground "foo";
    Examples.TC2.Ground "foo"; Examples.TC2.Ground "foo"],
   4)
# val w5 : ('a, '_b) Examples.TC2.abstract list * int =
  ([Examples.TC2.Code .<"foo">.; Examples.TC2.Code .<"foo">.], 2)
# val w6 : ('a, '_b) Examples.TC2.abstract list * int =
  ([Examples.TC2.Code .<"foo">.; Examples.TC2.Code .<"foo">.;
    Examples.TC2.Code .<"foo">.; Examples.TC2.Code .<"foo">.],
   4)
# val w7 : ('a, '_b) Examples.TC2.abstract list * int =
  ([Examples.TC2.Ground "foo"; Examples.TC2.Ground "foo";
    Examples.TC2.Ground "foo"; Examples.TC2.Ground "foo";
    Examples.TC2.Code .<"foo">.; Examples.TC2.Code .<"foo">.;
    Examples.TC2.Code .<"foo">.; Examples.TC2.Code .<"foo">.],
   8)
#   val w8 : ('a, '_b) Examples.TC3.abstract list * int =
  ([Examples.TC3.Ground (-5); Examples.TC3.Code .<3>.;
    Examples.TC3.Code .<7>.; Examples.TC3.Ground 6],
   4)
# val w9 : ('a, '_b) Examples.TC3.abstract list * int =
  ([Examples.TC3.Code .<(-5)>.; Examples.TC3.Ground 3;
    Examples.TC3.Code .<7>.; Examples.TC3.Ground 6],
   4)
# val w9a : ('a, '_b) Examples.TC3.abstract list * int =
  ([Examples.TC3.Ground 0; Examples.TC3.Ground 0; Examples.TC3.Ground 0;
    Examples.TC3.Ground 0],
   4)
#   val w10 : ('a, '_b) Examples.TC3.abstract list * int =
  ([Examples.TC3.Code
     .<(((* cross-stage persistent value (as id: B.bop) *))
    (* cross-stage persistent value (as id: x) *) (-5))>.;
    Examples.TC3.Code
     .<(((* cross-stage persistent value (as id: B.bop) *))
    (* cross-stage persistent value (as id: x) *) 3)>.;
    Examples.TC3.Code
     .<(((* cross-stage persistent value (as id: B.bop) *)) 7 7)>.;
    Examples.TC3.Ground 12],
   4)
# val w10a : ('a, '_b) Examples.TC3.abstract list * int =
  ([Examples.TC3.Ground (-5); Examples.TC3.Code .<3>.;
    Examples.TC3.Code .<7>.; Examples.TC3.Ground 6],
   4)
# val w11 : ('a, '_b Examples.TC3.b list) code =
  .<[(((* cross-stage persistent value (as id: B.bop) *))
     (* cross-stage persistent value (as id: x) *) (-5));
   (((* cross-stage persistent value (as id: B.bop) *))
     (* cross-stage persistent value (as id: x) *) 3);
   (((* cross-stage persistent value (as id: B.bop) *)) 7 7);
   (* cross-stage persistent value (as id: x) *)]>.
# val w11a : ('a, '_b Examples.TC3.b list) code =
  .<[(* cross-stage persistent value (as id: x) *); 3; 7;
   (* cross-stage persistent value (as id: x) *)]>.
# val w12 : '_a Examples.TC3.b list = [-10; 6; 14; 12]
# val w12a : '_a Examples.TC3.b list = [-5; 3; 7; 6]
#   val w13 : ('a, '_b) Examples.TC4.abstract list * int =
  ([Examples.TC4.Ground .<(-5)>.; Examples.TC4.Code .<.<3>.>.;
    Examples.TC4.Code .<.<7>.>.; Examples.TC4.Ground .<6>.],
   4)
# val w14 : ('a, '_b) Examples.TC4.abstract list * int =
  ([Examples.TC4.Code .<.<(-5)>.>.; Examples.TC4.Ground .<3>.;
    Examples.TC4.Code .<.<7>.>.; Examples.TC4.Ground .<6>.],
   4)
#   val w15 : ('a, '_b) Examples.TC4.abstract list * int =
  ([Examples.TC4.Code
     .<(((* cross-stage persistent value (as id: B.bop) *))
    (* cross-stage persistent value (as id: x) *) .<(-5)>.)>.;
    Examples.TC4.Code
     .<(((* cross-stage persistent value (as id: B.bop) *))
    (* cross-stage persistent value (as id: x) *) .<3>.)>.;
    Examples.TC4.Code
     .<(((* cross-stage persistent value (as id: B.bop) *)) .<7>. .<7>.)>.;
    Examples.TC4.Ground .<(6 + 6)>.],
   4)
# val w16 : ('a, '_b Examples.TC4.b list) code =
  .<[(((* cross-stage persistent value (as id: B.bop) *))
     (* cross-stage persistent value (as id: x) *) .<(-5)>.);
   (((* cross-stage persistent value (as id: B.bop) *))
     (* cross-stage persistent value (as id: x) *) .<3>.);
   (((* cross-stage persistent value (as id: B.bop) *)) .<7>. .<7>.);
   (* cross-stage persistent value (as id: x) *)]>.
# val w18 : '_a Examples.TC4.b list =
  [.<((-5) + (-5))>.; .<(3 + 3)>.; .<(7 + 7)>.; .<(6 + 6)>.]
#   val w19 : ('a, int) code =
  .<(((* cross-stage persistent value (as id: List.hd) *))
    [(* cross-stage persistent value (as id: h) *);
     (* cross-stage persistent value (as id: h) *)])>.
# val w19a : int = -17
# val w20 : ('a, int list) code * int =
  (.<(((* cross-stage persistent value (as id: List.tl) *))
    [(* cross-stage persistent value (as id: h) *);
     (* cross-stage persistent value (as id: h) *)])>.,
   1)
# val w20a : int list = [6]
#   val w21 : ('a, 'b Examples.T5.b list) code * int =
  (.<(((* cross-stage persistent value (as id: List.map2) *))
    (* cross-stage persistent value (as id: B.bop) *)
    [(* cross-stage persistent value (as id: h) *);
     (* cross-stage persistent value (as id: h) *)]
    [(* cross-stage persistent value (as id: h) *);
     (* cross-stage persistent value (as id: h) *)])>.,
   2)
# val w21a : 'a Examples.T5.b list = [289; 36]
#           - : unit = ()
#       - : unit = ()
#       - : unit = ()
# * *   
