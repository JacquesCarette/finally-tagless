type ('a, 'b) abstract = unit -> 'b

open StateCPSMonad
open Prelude

(* This one is very special! *)
let retN a = fun s k -> fun () -> let t = a () in k s (fun () -> t) ()

(* Naming conventions: 
   Functions that end in M take monads as arguments and partially
   run them -- in a reset fashion 
   Functions that end in L always return abstract values from plain
   value -- the L is stands for ``lifted'' *)

(* sequencing *)
(* Note: the difference between `seq' and `seqM' is quite akin
   to the difference between call-by-value and call-by-name.
   Also note that `seq' can be expressed in terms of `seqM',
   but not the other way around 
*)

let seq a b = fun () -> begin a (); b () end
let seqL a b = fun s k -> k s (seq a b)

let seqM a b = fun s k -> k s (fun () ->  begin a s k0 () ; b s k0 () end )

let optSeq a = function
    | Some c -> seq a c
    | None   -> a

let optSeqM a = function
    | Some c -> seqM a c
    | None   -> a

(* conditional *)
(* Note the implicit `reset'.  Also note how the condition does not
   involve a `reset' *)
(* Note the implicit `reset' *)
let ifM test th el = fun s k ->
  k s (fun () ->  if test () then th s k0 () else el s k0 () )

let rshiftM cf = fun s k -> k s (cf s)

let whenM test th  = rshiftM (fun s -> 
  fun () ->  if test () then th s k0 () else () )

(* loops actually bind a value *)
let loopM low high body = function
  | UP -> (fun s k -> 
    k s (fun () ->  for j = low () to high () do body (fun () -> j) s k0 () done
))
  | DOWN -> (fun s k -> 
    k s (fun () ->  for j = low () downto high () do body (fun () -> j) s k0 ()
done ))

(* while ``loops'' do not naturally bind a value *)
let whileM cond body = fun s k -> 
    k s (fun () ->  while cond () do body s k0 () done )

(* match for Some/None *)
let matchM x som non = fun s k ->
    k s (fun () ->  match x () with
           | Some i -> som (fun () -> i) s k0 ()
           | None   -> non s k0 () )

(* generic loop *)
let genrecloop gen rtarg = fun s k -> k s
    (fun () -> let rec loop j = gen (fun () -> loop) (fun () -> j) s k0 () in
    loop (rtarg ()))

(* another non-trivial morphism: generate a bit of code *)
(* let codegen v cf = fun s k -> cf (k s v) *)

(* set up some very general algebra that can be reused though not
   so much in the current code (yet?) 
type ('a,'b) abstract = Ground of 'b | Code of ('a, 'b) code
let concretize = function
    | Ground x -> .<x>.
    | Code x   -> x
*)

(* We now define a lot of infrastructure.  We will be quite
   thorough, and define base abstraction and lifted abstractions,
   This will involve a lot of boilerplate code, which unfortunately 
   cannot be so easily automated in MetaOCaml -- that would require 
   introspection.  It could be done in camlp4, but that seems too 
   much as well.  This 'base' could be elided, but when we decide
   to make more use of Abstract Interpretation, we'll regret it,
   so do it now. *)

let lift x = fun () -> x

let liftRef x = fun () -> ref (x ())
let liftGet x = fun () -> ! (x ())

(* Need to use `fun' explictly to avoid monomorphising *)
let unitL = fun s k -> k s (fun () -> ())

(* To be able to deconstruct pairs in monadic code:
   perform (a,b) <-- ret (liftPair pv) *)
let liftPair x = ((fun () -> fst (x ())), fun () -> snd (x ()))
let liftPPair x = ((fun () -> fst (fst (x ()))) , (fun () -> snd (fst (x ()))),
fun () -> snd (x ()) )

(* logic code combinators - plain and monadic *)
module Logic = struct
  let notL a        = fun () -> not (a ())
  let equalL a b    = fun () -> (a ()) = (b ())
  let notequalL a b = fun () -> (a ()) <> (b ())
  let andL a b     = fun () -> (a ()) && (b ())
end

(* operations on code indices *)
module Idx = struct
  let zero = fun () -> 0
  let one = fun () -> 1
  let minusone = fun () -> -1
  let succ a = fun () -> (a ()) + 1
  let pred a = fun () -> (a ()) - 1
  let less a b = fun () -> (a ()) < (b ())
  let uminus a = fun () -> - (a ())
  let add a b = fun () -> (a ()) + (b ())

  (* need explicit fun to avoid monomorphising *)
  let minusoneL = fun s k -> k s minusone
end

(* Maybe code generator *)
module Maybe = struct
  let just x = fun () -> Some (x ())
  let none   = fun () -> None
end

let applyMaybe g x = match g with
    | Some f -> f x
    | None   -> x

(* monadic tuples *)
module Tuple = struct
  let tup2 a b = fun () -> ( (a ()), (b ()) )
  let tup3 a b c = fun () -> ( (a ()), (b ()), (c ()) )
  let tup4 a b c d = fun () -> ( (a ()), (b ()), (c ()), (d ()) )
end

(* List ops *)
module CList = struct
  let nil = fun () -> []
  let cons a b = fun () -> (a ()) :: (b ())
end

(* Some basic code generators *)
let cunit = fun () -> ()
let update a f = let b = f (liftGet a) in fun () -> a () := b ()
let assign a b = fun () -> (a ()) := (b ())
let apply  f x = fun () -> (f ()) (x ())
let updateM a f = ret (update a f)
let assignM a b = ret (assign a b)
let applyM  f x = ret (apply f x)

(* This type is needed for the output, and is tracked during pivoting. 
   It's hard to find the right place for this lifting. If this
   is moved to domans_*.ml modules, this code should be placed
   into CONTAINER2D.
*)
type perm = RowSwap of (int * int) | ColSwap of (int*int)
let liftRowSwap a b = fun () -> RowSwap (a (), b ())
let liftColSwap a b = fun () -> ColSwap (a (), b ())

(* code transformers *)
module Transformers = struct
    let rec full_unroll lb ub body = 
        if lb>ub then cunit
        else if lb = ub then body lb
        else fun () ->
            begin body lb (); full_unroll (lb+1) ub body () end
end

module Array1Dim = struct
  let init n = fun() -> Array.init (n ()) (fun i -> i)
  let setL a v = fun () -> let (x,y)= v () in
                    let b = a () and t=(a ()).(x) in
                    begin
                        b.(x) <- b.(y);
                        b.(y) <- t;
                        b
                    end
end
