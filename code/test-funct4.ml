open Funct4;;

let res1 = Gen1.gen ~fracfree:false ~outputs:RankDet;;
let res2 = Gen2.gen ~fracfree:false ~outputs:RankDet;;
let res3 = Gen3.gen ~fracfree:true ~outputs:RankDet;;
let res4 = Gen4.gen ~fracfree:true ~outputs:RankDet;;

let r1 = .! res1 ;;
let r2 = .! res2 ;;
let r3 = .! res3 ;;
let r4 = .! res4 ;;
