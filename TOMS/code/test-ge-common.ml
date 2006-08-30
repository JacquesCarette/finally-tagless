(* Tests of GE. first instantiate some modules, and the #use this file *)

(* testing, for the kinds problem. Please keep this.

module GAC_F = GenericArrayContainer(FloatDomainL)
module T0 = Foo(FloatDomainL);;
module T1 = DivisionUpdate(GAC_F)(NoDet);;
*)


module FDet = AbstractDet(FloatDomainL)
module IDet = AbstractDet(IntegerDomainL)
module RDet = AbstractDet(RationalDomainL)

module GAC_F = GenericArrayContainer(FloatDomainL)
module GVC_F = GenericVectorContainer(FloatDomainL)
module GAC_I = GenericArrayContainer(IntegerDomainL)
module GVC_I = GenericVectorContainer(IntegerDomainL)
module GAC_R = GenericArrayContainer(RationalDomainL)

module GenFA1 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
;;
module GenFA2 = Gen(GAC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenFA3 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenFA4 = Gen(GAC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFV1 = Gen(GVC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenFV2 = Gen(GVC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenFV3 = Gen(GVC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenFV4 = Gen(GVC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFV5 = Gen(GVC_F)
                   (FullPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)

(* But this is an error!
module GenIA1 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix(GAC_I)(IDet))
*)
module GenIA1 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenIA2 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenIA3 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenIA4 = Gen(GAC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenIV1 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenIV2 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenIV3 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenIV4 = Gen(GVC_I)
                   (RowPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenIV5 = Gen(GVC_I)
                   (FullPivot)
                   (AbstractDet)
                   (FractionFreeUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFA11 = Gen(GAC_F)
                   (FullPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenFA12 = Gen(GAC_F)
                   (FullPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenFA13 = Gen(GAC_F)
                   (FullPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenFA14 = Gen(GAC_F)
                   (FullPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)

module GenFA24 = Gen(GAC_F)
                    (RowPivot)
                    (AbstractDet)
                    (DivisionUpdate)
                    (InpJustMatrix)
                    (OutDetRankPivot)
module GenRA1 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
module GenRA2 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDet)
module GenRA3 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutRank)
module GenRA4 = Gen(GAC_R)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutDetRank)
module GenFA5 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutJustMatrix)

module GenFA6 = Gen(GAC_F)
                   (RowPivot)
                   (AbstractDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutDet)
module GenFA7 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutRank)
module GenFA8 = Gen(GAC_F)
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpMatrixMargin)
                   (OutDetRank)
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
let resIA1 = instantiate GenIA1.gen ;;
let resIA2 = instantiate GenIA2.gen ;;
let resIA3 = instantiate GenIA3.gen ;;
let resIA4 = instantiate GenIA4.gen ;;
let resIV1 = instantiate GenIV1.gen ;;
let resIV2 = instantiate GenIV2.gen ;;
let resIV3 = instantiate GenIV3.gen ;;
let resIV4 = instantiate GenIV4.gen ;;
let resIV5 = instantiate GenIV5.gen ;;
let resFA11 = instantiate GenFA11.gen ;;
let resFA12 = instantiate GenFA12.gen ;;
let resFA13 = instantiate GenFA13.gen ;;
let resFA14 = instantiate GenFA14.gen ;;
let resFA24 = instantiate GenFA24.gen ;;
let resRA1 = instantiate GenRA1.gen ;;
let resRA2 = instantiate GenRA2.gen ;;
let resRA3 = instantiate GenRA3.gen ;;
let resRA4 = instantiate GenRA4.gen ;;
let resFA5 = instantiate GenFA5.gen ;;
let resFA6 = instantiate GenFA6.gen ;;
let resFA7 = instantiate GenFA7.gen ;;
let resFA8 = instantiate GenFA8.gen ;;

let rFA1 = runit {pf =  resFA1 } ;;
let rFA2 = runit {pf =  resFA2 };;
let rFA3 = runit {pf =  resFA3 };;
let rFA4 = runit {pf =  resFA4 };;
let rFV1 = runit {pf =  resFV1 };;
let rFV2 = runit {pf =  resFV2 };;
let rFV3 = runit {pf =  resFV3 };;
let rFV4 = runit {pf =  resFV4 };;
let rFV5 = runit {pf =  resFV5 };;
let rIA1 = runit {pf =  resIA1 };;
let rIA2 = runit {pf =  resIA2 };;
let rIA3 = runit {pf =  resIA3 };;
let rIA4 = runit {pf =  resIA4 };;
let rIV1 = runit {pf =  resIV1 };;
let rIV2 = runit {pf =  resIV2 };;
let rIV3 = runit {pf =  resIV3 };;
let rIV4 = runit {pf =  resIV4 };;
let rIV5 = runit {pf =  resIV5 };;
let rFA11 = runit {pf =  resFA11 };;
let rFA12 = runit {pf =  resFA12 };;
let rFA13 = runit {pf =  resFA13 };;
let rFA14 = runit {pf =  resFA14 };;
let rFA24 = runit {pf =  resFA24 };;
let rRA1 = runit {pf =  resRA1 };;
let rRA2 = runit {pf =  resRA2 };;
let rRA3 = runit {pf =  resRA3 };;
let rRA4 = runit {pf =  resRA4 };;
let rFA5 = runit {pf =  resFA5 };;
let rFA6 = runit {pf =  resFA6 };;
let rFA7 = runit {pf =  resFA7 };;
let rFA8 = runit {pf =  resFA8 };;

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

(* should also test more of these
let fff = rFA6 (fa7,3);;
*)

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


let resF11 = List.map rFA11 fa5;;
let resF12 = List.map rFA12 fa5;;
let resF13 = List.map rFA13 fa5;;
let resF14 = List.map rFA14 fa5;;
let resF24 = List.map rFA24 fa5;;

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