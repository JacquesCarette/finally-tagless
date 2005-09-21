Trx.init_times () ;;
Random.init 1 ;;

let makearray i =
    Array.init i (fun j -> Array.init i (fun k -> Random.int 1000)) ;;
let xx = List.map  makearray  [10;30;50;70];;

let gen_IA4 = .! (Funct4.GenIA4.gen) 
and direct_IA4 = Direct2.GenIA4.gen ;;

let doit aa =
    let generated = Trx.timenew "generated" (fun () -> gen_IA4 aa) in
    let gen_time = Trx.timenew "generation time" (fun () -> .!
    Funct4.GenFA1.gen) in
    let horder = Trx.timenew "higher-order"
        (fun () -> (direct_IA4 aa)) in
    let _ = assert (generated = horder) in
        (generated, gen_time, horder) ;;

let res = List.map doit xx ;;
let _ = Trx.print_times () ;;
