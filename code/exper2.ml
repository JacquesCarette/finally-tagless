open Ge_int ;;
open Ge_final ;;

Trx.init_times () ;;
Random.init 1 ;;

let makearray i =
    Array.init i (fun j -> Array.init i (fun k -> Random.int 1000)) ;;
let xx = List.map  makearray  [10;30;50;70];;

let doit aa =
    let hand = Trx.timenew "hand code" (fun () -> ge_int aa) in
    let unstaged = Trx.timenew "generic"
        (fun () -> ((Ge_unstaged.specializer Ge_unstaged.dom_int 
             Ge_unstaged.array_container
            ~fracfree:false ~outputs:Ge_unstaged.JustMatrix) aa)) in
    let spec = Trx.timenew  "specialize"
        (fun () -> specializer dom_int array_container 
                ~fracfree:false ~outputs:JustMatrix ) in
    let compiled = Trx.timenew  "compiling"
        (fun () -> .! spec) in
    let runit = Trx.timenew  "running"
        (fun () -> (compiled aa)) in
        (hand, unstaged, spec, compiled, runit)

let res = List.map doit xx ;;
let _ = Trx.print_times () ;;
