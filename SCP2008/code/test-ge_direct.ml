open StateCPSMonad


module GEF = Ge.LAMake(Direct)
open GEF
open Domains_direct

let instantiate gen =
    fun a -> (runM (gen (fun () -> a)) []) () ;;

type 'b pr = {pf : 'b};;
let runit f = f.pf;;

(* The following should raise an error! One can't apply IntegerDomain
   with the DivisionUpdate, which requires a field 
module GenFA1_error = Gen(GenericVectorContainer(IntegerDomainL))
                   (RowPivot)
                   (NoDet)
                   (DivisionUpdate)
                   (InpJustMatrix)
                   (OutJustMatrix)
;;
*)

#use "test-ge-common.ml";;
