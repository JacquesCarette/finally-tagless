open Funct4;;

let res1 = Gen1.gen ~fracfree:false;;
let res2 = Gen2.gen ~fracfree:false;;
let res3 = Gen3.gen ~fracfree:false;;
let res4 = Gen4.gen ~fracfree:true;;

let r1 = .! res1 ;;
let r2 = .! res2 ;;
let r3 = .! res3 ;;
let r4 = .! res4 ;;
