open Infra2;;

let mloop n kn rest =
    begin
	for i = 0 to (n - 1) do
        let c = (float_of_int i /.  float_of_int (n - 1)) in
        kn.(i) <- rest i c;
    done;
    kn;
    end;;

let seq2 a b = .< begin .~a ; .~b end >. ;;
let build_knots3 num_knots = fun acode bcode -> mdo {
    xknot <-- retN ( .<Array.make num_knots 0.0 >. ) ;
    arr   <-- ret (Array.make num_knots .< () >. ) ;
    res   <-- ret (mloop num_knots arr (fun i c -> 
              .<(.~xknot).(i) <- (.~acode +. ((.~bcode -. .~acode) *. c)) >. )) ;
    res2  <-- ret (Array.fold_left seq2 .<()>. arr) ;
    seqM res2 (ret xknot)
    }
;;

(* This test should be in a separate file *)
let test3 = .< fun a b ->  .~(runM (build_knots3 10 .<a>. .<b>. )) >.;;
