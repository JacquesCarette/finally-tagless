open StateCPSMonad


module GEF = Ge.GEMake(Direct)
open GEF
open Domains_direct

let instantiate f = fun a -> runM (f (fun () -> a)) () ;;

type 'b pr = {pf : 'b};;
let runit f = f.pf;;

(* The following should raise an error! One can't apply IntegerDomain
   with the DivisionUpdate, which requires a field 
*)
module GenFA1_error = Gen(GenericVectorContainer(IntegerDomainL))
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
;;

#use "test-ge-common.ml";;
