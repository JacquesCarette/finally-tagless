open Funct4;;

let resFA1 = GenFA1.gen ;;
let resFA11 = GenFA11.gen ;;
let resFA2 = GenFA2.gen ;;
let resFA3 = GenFA3.gen ;;
let resFA4 = GenFA4.gen ;;
let resFV1 = GenFV1.gen ;;
let resFV2 = GenFV2.gen ;;
let resFV3 = GenFV3.gen ;;
let resFV4 = GenFV4.gen ;;
let resIA1 = GenIA1.gen ;;
let resIA2 = GenIA2.gen ;;
let resIA3 = GenIA3.gen ;;
let resIA4 = GenIA4.gen ;;
let resIV1 = GenIV1.gen ;;
let resIV2 = GenIV2.gen ;;
let resIV3 = GenIV3.gen ;;
let resIV4 = GenIV4.gen ;;

let rFA1 = .! resFA1 ;;
let rFA2 = .! resFA2 ;;
let rFA3 = .! resFA3 ;;
let rFA4 = .! resFA4 ;;
let rFV1 = .! resFV1 ;;
let rFV2 = .! resFV2 ;;
let rFV3 = .! resFV3 ;;
let rFV4 = .! resFV4 ;;
let rIA1 = .! resIA1 ;;
let rIA2 = .! resIA2 ;;
let rIA3 = .! resIA3 ;;
let rIA4 = .! resIA4 ;;
let rIV1 = .! resIV1 ;;
let rIV2 = .! resIV2 ;;
let rIV3 = .! resIV3 ;;
let rIV4 = .! resIV4 ;;

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

let resI1 = List.map rIA1 ia5;;
let resI2 = List.map rIA2 ia5;;
let resI3 = List.map rIA3 ia5;;
let resI4 = List.map rIA4 ia5;;

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
    Array.of_list [0. ; 13. ; 5. ] ;
    Array.of_list [0. ; 3. ; 0. ] ;
    ] ;;
let fa5 = [fa0; fa1; fa2; fa3; fa4]
let resF1 = List.map rFA1 fa5;;
let resF2 = List.map rFA2 fa5;;
let resF3 = List.map rFA3 fa5;;
let resF4 = List.map rFA4 fa5;;
