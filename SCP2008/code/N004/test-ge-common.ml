(* Tests of GE. first instantiate some modules, and the #use this file *)

(* testing, for the kinds problem. Please keep this.

module GAC_F = GenericArrayContainer(FloatDomainL)
module T0 = Foo(FloatDomainL);;
module T1 = DivisionUpdate(GAC_F)(NoDet);;
*)

module Z3 = ZpMakeL(struct let p = 3 end)
module Z19 = ZpMakeL(struct let p = 19 end)

(* That should fail, and we should check this. 9 is not a prime number *)
let () = assert (
          try
            let module Z9 = ZpMakeL(struct let p = 9 end)
        in false
      with Assert_failure _ -> true)

module GAC_F = GenericArrayContainer(FloatDomainL)
module GVC_F = GenericVectorContainer(FloatDomainL)
module GAC_I = GenericArrayContainer(IntegerDomainL)
module GVC_I = GenericVectorContainer(IntegerDomainL)
module GAC_R = GenericArrayContainer(RationalDomainL)
module GVC_Z3 = GenericVectorContainer(Z3)
module GVC_Z19 = GenericVectorContainer(Z19)
module GFC_F = FortranVectorContainer(FloatDomainL)

module G_GAC_F = GenLA(GAC_F)
module G_GVC_F = GenLA(GVC_F)
module G_GAC_I = GenLA(GAC_I)
module G_GVC_I = GenLA(GVC_I)
module G_GAC_R = GenLA(GAC_R)
module G_GVC_Z3 = GenLA(GVC_Z3)
module G_GVC_Z19 = GenLA(GVC_Z19)
module G_GFC_F = GenLA(GFC_F)

open G_GAC_F
open G_GAC_F.GE

module GenFA1 = GenGE(struct
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutJustMatrix end)
module GenFA2 = GenGE(struct
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDet end)
module GenFA3 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutRank end)
module GenFA4 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDetRank end)
module GenFA11 = GenGE(struct
    module Det = NoDet
    module PivotF = FullPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutJustMatrix end)
module GenFA12 = GenGE(struct
    module Det = AbstractDet
    module PivotF = FullPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDet end)
module GenFA13 = GenGE(struct 
    module Det = NoDet
    module PivotF = FullPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutRank end)
module GenFA14 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = FullPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDetRank end)
module GenFA24 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDetRankPivot end)
module GenFA25 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = RowVectorPerm
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDetRankPivot end)
module GenFA26 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = RowVectorPerm
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutJustMatrix end)
module GenFA5 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpMatrixMargin
    module Output = OutJustMatrix end)
module GenFA6 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpMatrixMargin
    module Output = OutDet end)
module GenFA7 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpMatrixMargin
    module Output = OutRank end)
module GenFA8 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpMatrixMargin
    module Output = OutDetRank end)
module GenFA9 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = Out_LU_Packed end)
module GenFA31 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = Out_L_U end)
module GenFA32 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = Out_LU_Packed end)

open G_GVC_F
open G_GVC_F.GE
module GenFV1 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutJustMatrix end)
module GenFV2 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutDet end)
module GenFV3 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutRank end)
module GenFV4 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutDetRank end)
module GenFV5 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = FullPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutDetRank end)

(* For some reason, these 2 go bonkers - worry about them later *)
(* Hmm, seem to work for me... *)
module GenFV6 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = Out_L_U end)
;;
module GenFV7 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = Out_LU_Packed end)
;;


open G_GAC_I
open G_GAC_I.GE
module GenIA1 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix
    module Output = OutJustMatrix end)

(* Check the pre-fligh tests. The test should trigger before the generation
   begins!
*)
let _ = assert (
  try
    let module GenIA1' = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutJustMatrix end)
    in
    false
  with Failure "Cannot do Division in a non-field" -> true
)
;;

module GenIA2 = GenGE(struct
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix 
    module Output = OutDet end)
module GenIA3 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix 
    module Output = OutRank end)
module GenIA4 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix 
    module Output = OutDetRank end)
(* Neither of these two 'work' as one cannot output the L matrix
   while FractionFree !*)

let _ = assert (
  try
    let module GenIA5 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix
    module Output = Out_L_U end)
    in
    false
  with Failure "Out_L_U: Can't extract the L in a non-field" -> true
)
;;

let _ = assert (
  try
    let module GenIA6 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix
    module Output = Out_LU_Packed end)
    in
    false
  with Failure "Out_LU_Packed: Can't extract the L in a non-field" -> true
)
;;

open G_GVC_I
open G_GVC_I.GE
module GenIV1 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix
    module Output = OutJustMatrix end)
module GenIV2 = GenGE(struct
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix 
    module Output = OutDet end)
module GenIV3 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix 
    module Output = OutRank end)
module GenIV4 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix 
    module Output = OutDetRank end)
module GenIV5 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = FullPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix
    module Output = OutDetRank end)
module GenIV6 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = FullPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix
    module Output = OutDetRankPivot end)

open G_GAC_R
open G_GAC_R.GE
module GenRA1 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutJustMatrix end)
module GenRA2 = GenGE(struct
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDet end)
module GenRA3 = GenGE(struct 
    module Det = NoDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutRank end)
module GenRA4 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix 
    module Output = OutDetRank end)

open G_GVC_Z3
open G_GVC_Z3.GE
module GenZp3 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = DivisionUpdate
    module Input = InpJustMatrix
    module Output = OutDetRankPivot end)

open G_GVC_Z19
open G_GVC_Z19.GE
module GenZp19 = GenGE(struct 
    module Det = AbstractDet
    module PivotF = RowPivot
    module PivotRep = PermList
    module Update = FractionFreeUpdate
    module Input = InpJustMatrix
    module Output = OutDetRankPivot end)
;;

let resFA1 = instantiate GenFA1.gen ;;
let resFA2 = instantiate GenFA2.gen ;;
let resFA3 = instantiate GenFA3.gen ;;
let resFA4 = instantiate GenFA4.gen ;;
let resFV1 = instantiate GenFV1.gen ;;
let resFV2 = instantiate GenFV2.gen ;;
let resFV3 = instantiate GenFV3.gen ;;
let resFV4 = instantiate GenFV4.gen ;;
let resFV5 = instantiate GenFV5.gen ;;
(*
let resFV6 = instantiate GenFV6.gen ;;
let resFV7 = instantiate GenFV7.gen ;;
*)
let resIA1 = instantiate GenIA1.gen ;;
let resIA2 = instantiate GenIA2.gen ;;
let resIA3 = instantiate GenIA3.gen ;;
let resIA4 = instantiate GenIA4.gen ;;
let resIV1 = instantiate GenIV1.gen ;;
let resIV2 = instantiate GenIV2.gen ;;
let resIV3 = instantiate GenIV3.gen ;;
let resIV4 = instantiate GenIV4.gen ;;
let resIV5 = instantiate GenIV5.gen ;;
let resIV6 = instantiate GenIV6.gen ;;
let resFA11 = instantiate GenFA11.gen ;;
let resFA12 = instantiate GenFA12.gen ;;
let resFA13 = instantiate GenFA13.gen ;;
let resFA14 = instantiate GenFA14.gen ;;
let resFA24 = instantiate GenFA24.gen ;;
let resFA25 = instantiate GenFA25.gen ;;
let resFA26 = instantiate GenFA26.gen ;;
let resRA1 = instantiate GenRA1.gen ;;
let resRA2 = instantiate GenRA2.gen ;;
let resRA3 = instantiate GenRA3.gen ;;
let resRA4 = instantiate GenRA4.gen ;;
let resFA5 = instantiate GenFA5.gen ;;
let resFA6 = instantiate GenFA6.gen ;;
let resFA7 = instantiate GenFA7.gen ;;
let resFA8 = instantiate GenFA8.gen ;;
let resFA9 = instantiate GenFA9.gen ;;
let resFA31 = instantiate GenFA31.gen ;;
let resFA32 = instantiate GenFA32.gen ;;
let resZp3 = instantiate GenZp3.gen ;;
let resZp19 = instantiate GenZp19.gen ;;

let rFA1 = runit {pf =  resFA1 } ;;
let rFA2 = runit {pf =  resFA2 };;
let rFA3 = runit {pf =  resFA3 };;
let rFA4 = runit {pf =  resFA4 };;
let rFV1 = runit {pf =  resFV1 };;
let rFV2 = runit {pf =  resFV2 };;
let rFV3 = runit {pf =  resFV3 };;
let rFV4 = runit {pf =  resFV4 };;
let rFV5 = runit {pf =  resFV5 };;
(*
let rFV6 = runit {pf =  resFV6 };;
let rFV7 = runit {pf =  resFV7 };;
*)
let rIA1 = runit {pf =  resIA1 };;
let rIA2 = runit {pf =  resIA2 };;
let rIA3 = runit {pf =  resIA3 };;
let rIA4 = runit {pf =  resIA4 };;
let rIV1 = runit {pf =  resIV1 };;
let rIV2 = runit {pf =  resIV2 };;
let rIV3 = runit {pf =  resIV3 };;
let rIV4 = runit {pf =  resIV4 };;
let rIV5 = runit {pf =  resIV5 };;
let rIV6 = runit {pf =  resIV6 };;
let rFA11 = runit {pf =  resFA11 };;
let rFA12 = runit {pf =  resFA12 };;
let rFA13 = runit {pf =  resFA13 };;
let rFA14 = runit {pf =  resFA14 };;
let rFA24 = runit {pf =  resFA24 };;
let rFA25 = runit {pf =  resFA25 };;
let rFA26 = runit {pf =  resFA26 };;
let rRA1 = runit {pf =  resRA1 };;
let rRA2 = runit {pf =  resRA2 };;
let rRA3 = runit {pf =  resRA3 };;
let rRA4 = runit {pf =  resRA4 };;
let rFA5 = runit {pf =  resFA5 };;
let rFA6 = runit {pf =  resFA6 };;
let rFA7 = runit {pf =  resFA7 };;
let rFA8 = runit {pf =  resFA8 };;
let rFA9 = runit {pf =  resFA9 };;
let rFA31 = runit {pf =  resFA31 };;
let rFA32 = runit {pf =  resFA32 };;
let rZp3 = runit {pf =  resZp3 };;
let rZp19 = runit {pf =  resZp19 };;

let ia0 = Array.make 1 (Array.make 1 1)
let ia1 = Array.of_list [
    Array.of_list [1 ; 2 ; 3 ] ;
    Array.of_list [4 ; 13 ; 5 ] ;
    Array.of_list [(-1) ; 3 ; 0 ]
    ]
let ia2 = Array.of_list [
    Array.of_list [1 ; 2 ; 3 ; 0] ;
    Array.of_list [4 ; 13 ; 5 ; 0] ;
    Array.of_list [(-1) ; 3 ; 0 ; 0]
    ]
let ia3 = Array.of_list [
    Array.of_list [1 ; 2 ; 3 ] ;
    Array.of_list [4 ; 13 ; 5 ] ;
    Array.of_list [(-1) ; 3 ; 0 ] ;
    Array.of_list [0 ; 0 ; 0 ]
    ]
let ia4 = Array.of_list [
    Array.of_list [0 ; 2 ; 3 ] ;
    Array.of_list [0 ; 13 ; 5 ] ;
    Array.of_list [0 ; 3 ; 0 ] ;
    ]
let ia5 = [ia0; ia1; ia2; ia3; ia4]

let resI11 = List.map rIA1 ia5;;
let resI12 = List.map rIA2 ia5;;
let resI13 = List.map rIA3 ia5;;
let resI14 = List.map rIA4 ia5;;

let iv0 = {arr=Array.make 1 1; n=1; m=1}
let iv1 = {arr=Array.of_list [ 1; 2; 3; 4; 13; 5; (-1); 3; 0]; n=3; m=3}
let iv2 = {arr=Array.of_list [ 1; 2; 3; 0; 4; 13; 5; 0; (-1); 3; 0; 0]; n=3; m=4}
(* let iv3 = {arr=Array.of_list [ 1; 2; 3; 4; 13; 5; (-1); 3; 0; 0; 0; 0]; n=4; m=3} *)
let iv4 = {arr=Array.of_list [ 0; 2; 3; 0; 13; 5; 0; 3; 0]; n=3; m=3}
let iv5 = [iv0; iv1; iv2; iv4]

let resI21 = List.map rIV1 iv5;;
let resI22 = List.map rIV2 iv5;;
let resI23 = List.map rIV3 iv5;;
let resI24 = List.map rIV4 iv5;;
let resI25 = List.map rIV5 iv5;;
let resI26 = List.map rIV6 iv5;;

let fa0 = Array.make 1 (Array.make 1 1.) ;;
let fa1 = Array.of_list [
    Array.of_list [1. ; 2. ; 3. ] ;
    Array.of_list [4. ; 13. ; 5. ] ;
    Array.of_list [(-1.) ; 3. ; 0. ]
    ] ;;
let fa2 = Array.of_list [
    Array.of_list [1. ; 2. ; 3. ; 0.] ;
    Array.of_list [4. ; 13. ; 5. ; 0.] ;
    Array.of_list [(-1.) ; 3. ; 0. ; 0.]
    ] ;;
let fa3 = Array.of_list [
    Array.of_list [1. ; 2. ; 3. ] ;
    Array.of_list [4. ; 13. ; 5. ] ;
    Array.of_list [(-1.) ; 3. ; 0. ] ;
    Array.of_list [0. ; 0. ; 0. ]
    ] ;;

let fa4 = Array.of_list [
    Array.of_list [0. ; 2. ; 3. ] ;
    Array.of_list [0. ; 10. ; 5. ] ;
    Array.of_list [0. ; 3. ; 0. ] ;
    ] ;;
let fa5 = [fa0; fa1; fa2; fa3; fa4]

let fa6 = Array.make 1 (Array.make 2 1.)
let fa7 = Array.of_list [
    Array.of_list [1. ; 2. ; 3.   ; 1. ; 0. ; 0. ] ;
    Array.of_list [4. ; 13. ; 5.  ; 0. ; 1. ; 0. ] ;
    Array.of_list [(-1.) ; 3. ; 0.; 0. ; 0. ; 1. ]
    ]

let _ = assert (rFA1 fa0 = [|[|1.0|]|])
(* permutation: 1<->2, 2<->3. Det is 50 *)
let _ = assert (rFA1 fa1 =
    [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|]
          )
let _ = assert (rFA1 fa2 =
    [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|]
          )

let _ = assert (rFA1 fa3 =
    [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]
          )
let _ = assert (rFA1 fa4 =
    [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; -0.|]|]
          )

let _ = assert( rFA5 (fa6,1) = [|[|1.0; 1.0|]|] )

let resFA9 = rFA9 fa1;;

(* should also test more of these
*)
let _ = assert( rFA6 (fa7,3) = 
     ([|[|4.; 13.; 5.; 0.; 1.; 0.|]; 
        [|0.; 6.25; 1.25; 0.; 0.25; 1.|];
        [|0.; 0.; 2.; 1.; -0.2; 0.2|]|], 50.) )
let _ = assert( rFA7 (fa7, 3) =
    ([|[|4.; 13.; 5.; 0.; 1.; 0.|]; 
       [|0.; 6.25; 1.25; 0.; 0.25; 1.|];
       [|0.; 0.; 2.; 1.; -0.2; 0.2|]|], 3) )
let _ = assert( rFA8 (fa7, 3) =
    ([|[|4.; 13.; 5.; 0.; 1.; 0.|]; [|0.; 6.25; 1.25; 0.; 0.25; 1.|];
     [|0.; 0.; 2.; 1.; -0.2; 0.2|]|], 50., 3) )

let resF1 = List.map rFA1 fa5;;
let _ = assert (List.map rFA2 fa5 =
  [([|[|1.|]|], 1.);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50.);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50.);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0.)])

let _ = assert (List.map rFA3 fa5 =
  [([|[|1.|]|], 1);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 3);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 3);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],3);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 2)])

let _ = assert (List.map rFA4 fa5 =
 [([|[|1.|]|], 1., 1);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2)])

let resFA31 = List.map rFA31 fa5;;
let resFA32 = List.map rFA32 fa5;;

let a2v arr =
  let n = Array.length arr
  and m = Array.length arr.(0)
  and r = Array.concat (Array.to_list arr)
  in {arr = r; n = n; m = m}

let xxx = List.map a2v fa5

let _ = assert (List.map (fun arr -> rFV4 (a2v arr)) fa5 =
 [({arr = [|1.|]; n = 1; m = 1}, 1., 1);
   ({arr = [|4.; 13.; 5.; 0.; 6.25; 1.25; 0.; 0.; 2.|]; n = 3; m = 3}, 50.,
    3);
   ({arr = [|4.; 13.; 5.; 0.; 0.; 6.25; 1.25; 0.; 0.; 0.; 2.; 0.|]; n = 3;
     m = 4},
    50., 3);
   ({arr = [|4.; 13.; 5.; 0.; 6.25; 1.25; 0.; 0.; 2.; 0.; 0.; 0.|]; n = 4;
     m = 3},
    50., 3);
   ({arr = [|0.; 10.; 5.; 0.; 0.; 2.; 0.; 0.; 0.|]; n = 3; m = 3}, 0., 2)])

let resFV5 = List.map (fun arr -> rFV5 (a2v arr)) fa5
(*
let resFV6 = List.map (fun arr -> rFV6 (a2v arr)) fa5
let resFV7 = List.map (fun arr -> rFV7 (a2v arr)) fa5
*)


let resF11 = List.map rFA11 fa5;;
let resF12 = List.map rFA12 fa5;;
let resF13 = List.map rFA13 fa5;;
let resF14 = List.map rFA14 fa5;;
let resF24 = List.map rFA24 fa5;;
let resF25 = List.map rFA25 fa5;;
let resF26 = List.map rFA26 fa5;;

(*
let _ = assert (List.map rFA24 fa5 =
 [([|[|1.|]|], 1., 1, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3, []);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, []);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2, [])])
*)

let ra0 = Array.make 1 (Array.make 1 (Num.num_of_int 1)) ;;
let ra1 = Array.map (fun a -> Array.map Num.num_of_int a) (
    Array.of_list [
    Array.of_list [1 ; 2 ; 3 ] ;
    Array.of_list [4 ; 13 ; 5 ] ;
    Array.of_list [(-1) ; 3 ; 0 ]
    ]) ;;
let ra2 = Array.map (fun a -> Array.map Num.num_of_int a) (
    Array.of_list [
    Array.of_list [1 ; 2 ; 3 ; 0] ;
    Array.of_list [4 ; 13 ; 5 ; 0] ;
    Array.of_list [(-1) ; 3 ; 0 ; 0]
    ]) ;;
let ra3 = Array.map (fun a -> Array.map Num.num_of_int a) (
    Array.of_list [
    Array.of_list [1 ; 2 ; 3 ] ;
    Array.of_list [4 ; 13 ; 5 ] ;
    Array.of_list [(-1) ; 3 ; 0 ] ;
    Array.of_list [0 ; 0 ; 0 ]
    ]) ;;
let ra4 = Array.map (fun a -> Array.map Num.num_of_int a) (
    Array.of_list [
    Array.of_list [0 ; 2 ; 3 ] ;
    Array.of_list [0 ; 13 ; 5 ] ;
    Array.of_list [0 ; 3 ; 0 ] ;
    ]) ;;
let ra5 = [ra0; ra1; ra2; ra3; ra4] ;;

let resR11 = List.map rRA1 ra5;;
let resR12 = List.map rRA2 ra5;;
let resR13 = List.map rRA3 ra5;;
let resR14 = List.map rRA4 ra5;;
