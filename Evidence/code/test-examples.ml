open Examples

(* construct some lists *)
let e1 = LV.lnil ;;
let e2 = LV.lcons "foo" e1;;

let e3 = LVC2.lnil ;;
let e4 = LVC2.lcons_v "foo" e3 ;;
let e5 = LVC2.lcons_c .<"foo">. e3 ;;

let e6 = LVC3.lcons_v 6 e3;;
let e7 = LVC3.lcons_c .<7>. e6;;

let e8 = LVC4.lcons_v .<6>. e3;;
let e9 = LVC4.lcons_c .< .<7>. >. e8;;

let e10 = LVC5.lcons (-17) (LVC5.lcons 6 LVC5.lnil);;

(* things that work *)
let w1 = LV.head e2;;
let w2 = LV.tail e2;;

let w3 = LVC2.concat e4 e4;;
let w4 = LVC2.concat w3 w3;;
let w5 = LVC2.concat e5 e5;;
let w6 = LVC2.concat w5 w5;;
let w7 = LVC2.concat w4 w6;;

let w8 = LVC3.lcons_v (-5) (LVC3.lcons_c .<3>. e7);;
let w9 = LVC3.lcons_c .< -5 >. (LVC3.lcons_v 3 e7);;
let w9a = LVC3.lcons_v 0 (LVC3.lcons_v 0 (LVC3.lcons_v 0 (LVC3.lcons_v 0 LVC3.lnil)));;

let w10 = LVC3.dotprod w8 w9;;
let w10a = LVC3.dotprod w8 w9a;;
let w11 = LVC3.concretize w10 ;;
let w11a = LVC3.concretize w10a ;;
let w12 = .! w11 ;;
let w12a = .! w11a ;;

let w13 = LVC4.lcons_v .< -5 >. (LVC4.lcons_c .< .<3>. >. e9);;
let w14 = LVC4.lcons_c .< .< -5 >. >. (LVC4.lcons_v .<3>. e9);;

let w15 = LVC4.dotprod w13 w14;;
let w16 = LVC4.concretize w15 ;;
let w18 = .! w16 ;;

let w19 = LVC5.head e10;;
let w19a = .! w19;;
let w20 = LVC5.tail e10;;
let w20a = .! (LVC5.concretize w20);;

let w21 = LVC5.dotprod e10 e10;;
let w21a = .! (fst w21);;

(* things that don't *)
let _ = assert ( 
    try
        let f1 = LV.tail e1 in false
    with Failure s -> (s = "tail on null list")  ) ;;
let _ = assert ( 
    try
        let f2 = LVC4.dotprod e9 w14 in false
    with Failure s -> (s = "product of 2 unequal vectors") ) ;;
let _ = assert ( 
    try
        let f3 = LVC5.dotprod e10 w20 in false
    with Failure s -> (s = "product of 2 unequal vectors") ) ;;
(* not even type-correct 
        let f2 = LVC3.lcons_v 5 e4 
*)
