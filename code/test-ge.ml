open Ge;;
open Infra;; (* for some types *)

let resFA1 = GenFA1.gen ;;
let resFA2 = GenFA2.gen ;;
let resFA3 = GenFA3.gen ;;
let resFA4 = GenFA4.gen ;;
let resFV1 = GenFV1.gen ;;
let resFV2 = GenFV2.gen ;;
let resFV3 = GenFV3.gen ;;
let resFV4 = GenFV4.gen ;;
let resFV5 = GenFV5.gen ;;
let resIA1 = GenIA1.gen ;;
let resIA2 = GenIA2.gen ;;
let resIA3 = GenIA3.gen ;;
let resIA4 = GenIA4.gen ;;
let resIV1 = GenIV1.gen ;;
let resIV2 = GenIV2.gen ;;
let resIV3 = GenIV3.gen ;;
let resIV4 = GenIV4.gen ;;
let resIV5 = GenIV5.gen ;;
let resFA11 = GenFA11.gen ;;
let resFA12 = GenFA12.gen ;;
let resFA13 = GenFA13.gen ;;
let resFA14 = GenFA14.gen ;;
let resRA1 = GenRA1.gen ;;
let resRA2 = GenRA2.gen ;;
let resRA3 = GenRA3.gen ;;
let resRA4 = GenRA4.gen ;;

let rFA1 = .! resFA1 ;;
let rFA2 = .! resFA2 ;;
let rFA3 = .! resFA3 ;;
let rFA4 = .! resFA4 ;;
let rFV1 = .! resFV1 ;;
let rFV2 = .! resFV2 ;;
let rFV3 = .! resFV3 ;;
let rFV4 = .! resFV4 ;;
let rFV5 = .! resFV5 ;;
let rIA1 = .! resIA1 ;;
let rIA2 = .! resIA2 ;;
let rIA3 = .! resIA3 ;;
let rIA4 = .! resIA4 ;;
let rIV1 = .! resIV1 ;;
let rIV2 = .! resIV2 ;;
let rIV3 = .! resIV3 ;;
let rIV4 = .! resIV4 ;;
let rIV5 = .! resIV5 ;;
let rFA11 = .! resFA11 ;;
let rFA12 = .! resFA12 ;;
let rFA13 = .! resFA13 ;;
let rFA14 = .! resFA14 ;;
let rRA1 = .! resRA1 ;;
let rRA2 = .! resRA2 ;;
let rRA3 = .! resRA3 ;;
let rRA4 = .! resRA4 ;;

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
