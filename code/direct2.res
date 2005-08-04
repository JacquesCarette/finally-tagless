        Objective Caml version 3.08.0

# #   val resFA1 :
  ('a,
   'b Direct.GenFA1.Ctr.vc ->
   Direct.OutJustMatrix(Direct.FloatDomain)(Direct.GenericArrayContainer)(Direct.NoDet(Direct.FloatDomain)).res)
  code =
  .<fun a_1 ->
   (((* cross-stage persistent value (as id: runM) *))
     (((* cross-stage persistent value (as id: dogen) *)) a_1))>.
# * * * * * * * * * * * * * * * * * * * * * * * * * *     val rFA1 :
  ('a,
   'b Direct.GenFA1.Ctr.vc ->
   Direct.OutJustMatrix(Direct.FloatDomain)(Direct.GenericArrayContainer)(Direct.NoDet(Direct.FloatDomain)).res)
  code =
  .<fun a_1 ->
   (((* cross-stage persistent value (as id: runM) *))
     (((* cross-stage persistent value (as id: dogen) *)) a_1))>.
# * * * * * * * * * * * * * * * * * * * * * * * * * *                                                       * * * * * * * * * * * * * * * * * *     val ia0 : int array array = [|[|1|]|]
val ia1 : int array array = [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|]
val ia2 : int array array =
  [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|]
val ia3 : int array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|]
val ia4 : int array array = [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]
val ia5 : int array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|];
   [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|];
   [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]]
val fa0 : float array array = [|[|1.|]|]
#         val fa1 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|]
#         val fa2 : float array array =
  [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|]
#           val fa3 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|]
#           val fa4 : float array array =
  [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]
#                                     