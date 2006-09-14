open StateCPSMonad


module GEF = Ge.GEMake(Code)
open GEF
open Domains_code

type 'b pr = {pf : 'a . ('a,'b) code};;
let instantiate (ge,lu) = 
    .<fun a -> .~(runM (ge .<a>.)) >.;;
let runit f = .! f.pf;;

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
