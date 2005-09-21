Trx.init_times () ;;
Random.init 1 ;;

let makearray i =
    Array.init i (fun j -> Array.init i (fun k -> Random.float 1.0)) ;;
let xx = List.map  makearray  [10;30;50;70];;

let gen_FA1 = .! (Funct4.GenFA1.gen) 
and direct_FA1 = Direct2.GenFA1.gen ;;

let doit aa =
    let generated = Trx.timenew "generated" (fun () -> gen_FA1 aa) in
    let gen_time = Trx.timenew "generation time" (fun () -> .!
    Funct4.GenFA1.gen) in
    let horder = Trx.timenew "higher-order"
        (fun () -> (direct_FA1 aa)) in
    let _ = assert (generated = horder) in
        (generated, gen_time, horder) ;;

let res = List.map doit xx ;;
let _ = Trx.print_times () ;;
