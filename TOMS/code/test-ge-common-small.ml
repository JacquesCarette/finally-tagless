(* Tests of GE. first instantiate some modules, and the #use this file *)

(* testing, for the kinds problem. Please keep this.

module GAC_F = GenericArrayContainer(FloatDomainL)
module T0 = Foo(FloatDomainL);;
module T1 = DivisionUpdate(GAC_F)(NoDet);;
*)



module GAC_F = GenericArrayContainer(FloatDomainL)
module G_GAC_F = GenLA(GAC_F)
open G_GAC_F
module GenFA1_F = struct 
      module R = NoRank
      module P = DiscardPivot
      module L = NoLower
      module PivotF = RowPivot
      module Det = NoDet
      module Update = DivisionUpdate
      module Input = InpJustMatrix end

module GenFA1 = GenGE(GenFA1_F)(OutJustMatrix)
;;
let resFA1 = instantiate GenFA1.gen ;;
let rFA1 = runit {pf =  resFA1 } ;;

let fa2 = Array.of_list [
    Array.of_list [1. ; 2. ; 3. ; 0.] ;
    Array.of_list [4. ; 13. ; 5. ; 0.] ;
    Array.of_list [(-1.) ; 3. ; 0. ; 0.]
    ] ;;

let _ = assert (rFA1 fa2 =
    [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|]
          )

module GenFA25_F = struct 
      module R = Rank
      module P = KeepPivot(RowVectorPerm)
      module L = NoLower
      module PivotF = RowPivot
      module Det = AbstractDet
      module Update = DivisionUpdate
      module Input = InpJustMatrix end

module GenFA25 = GenGE(GenFA25_F)(OutDetRankPivot)

let resFA25 = instantiate GenFA25.gen ;;
let rFA25 = runit {pf =  resFA25 };;
let resF25 = rFA25 fa2;;
let (arr, det, rk, perm) = resF25 in 
  assert (det = 50.0 && rk =3 && perm = [|1; 2; 0|])
;;


