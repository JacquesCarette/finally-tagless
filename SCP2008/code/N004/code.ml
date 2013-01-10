type ('a, 'b) abstract = ('a, 'b) code

open StateCPSMonad
open Prelude

(* This one is very special! *)
let retN (a : ('c,'v)code) : 
 (<classif: 'c; answer: ('c,'w)code; ..>,('c,'v)code) monad 
   = fun s k -> .<let t = .~a in .~(k s .<t>.)>.

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

let seq a b = .< begin .~a ; .~b end >.
let seqL a b = ret (seq a b)

let seqM a b = fun s k -> k s .< begin .~(a s k0) ; .~(b s k0) end >.

let optSeq a = function
    | Some c -> seq a c
    | None   -> a

let optSeqM a = function
    | Some c -> seqM a c
    | None   -> a

(* conditional *)
(* Note the implicit `reset'.  Also note how the condition does not
   involve a `reset' *)
let ifM test th el = fun s k -> 
  k s .< if .~(test) then .~(th s k0) else .~(el s k0) >.

let rshiftM cf = fun s k -> k s (cf s)

let whenM test th  = rshiftM (fun s -> 
  .< if .~(test) then .~(th s k0) >.)

(* we have 2 kinds of loops, going up or down.  We control this via
   a type which is shared between implementations.  Note that the
   #$%#$ monomorphism restriction would force an extra argument on us
   if we did not put the up/down choice after body below.  Note
   that in the DOWN case, low>high is assumed ! *)
(* loops actually bind a value *)
let loopM low high body = function
  | UP -> (fun s k -> 
      k s .< for j = .~low to .~high do .~(body .<j>. s k0) done >. )
  | DOWN -> (fun s k -> 
      k s .< for j = .~low downto .~high do .~(body .<j>. s k0) done >. )

(* while ``loops'' do not naturally bind a value *)
let whileM cond body = fun s k -> 
  k s .< while .~(cond) do .~(body s k0) done >.

(* match for Some/None *)
let matchM x som non = fun s k -> k s .< match .~x with
           | Some i -> .~(som .<i>. s k0)
           | None   -> .~(non s k0) >.

(* generic loop *)
let genrecloop gen rtarg = fun s k -> 
  k s .<let rec loop j = .~(gen .<loop>. .<j>. s k0) in loop .~rtarg>.

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

let lift x = .< x >.

let liftRef x = .< ref .~x >. 
let liftGet x = .< ! .~x >. 

(* Need to use `fun' explictly to avoid monomorphising *)
let unitL = fun s k -> k s .< () >.

(* To be able to deconstruct pairs in monadic code:
   perform (a,b) <-- ret (liftPair pv) *)
let liftPair x = (.< fst .~x >., .< snd .~x >.)
let liftPPair x = (.< fst (fst .~x) >., .< snd (fst .~x) >., .< snd .~x >.)

(* logic code combinators - plain and monadic *)
module Logic = struct
  let notL a        = .< not .~a >.
  let equalL a b    = .< .~a = .~ b >.
  let notequalL a b = .< .~a <> .~ b >.
  let andL a b     = .< .~a && .~b >. 
end

(* operations on code indices *)
module Idx = struct
  let zero = .< 0 >.
  let one = .< 1 >.
  let minusone = .< -1 >.
  let succ a = .< .~a + 1 >.
  let pred a = .< .~a - 1 >.
  let less a b = .< .~a < .~b >.
  let uminus a = .< - .~a >.
  let add a b = .< .~a + .~b >.

  (* need explicit fun to avoid monomorphising *)
  let minusoneL = fun s k -> k s minusone
end

(* Maybe code generator *)
module Maybe = struct
  let just x = .< Some .~x >.
  let none   = .< None >.
end

let applyMaybe g x = match g with
    | Some f -> f x
    | None   -> x

(* monadic tuples *)
module Tuple = struct
  let tup2 a b = .< ( .~a, .~b ) >.
  let tup3 a b c = .< ( .~a, .~b, .~c ) >.
  let tup4 a b c d = .< ( .~a, .~b, .~c, .~d ) >.
end

(* List ops *)
module CList = struct
  let nil = .< [] >.
  let cons a b = .< .~a :: .~b >.
end

(* Some basic code generators *)
let cunit = .< () >.
let update a f = let b = f (liftGet a) in .< .~a := .~b >.
let assign a b = .< .~a := .~b >.
let apply  f x = .< .~f .~x >.
let updateM a f = ret (update a f)
let assignM a b = ret (assign a b)
let applyM  f x = ret (apply f x)

(* This type is needed for the output, and is tracked during pivoting. 
   It's hard to find the right place for this lifting. If this
   is moved to domans_*.ml modules, this code should be placed
   into CONTAINER2D.
*)
type perm = RowSwap of (int * int) | ColSwap of (int*int)
let liftRowSwap a b = .< RowSwap (.~a, .~b) >.
let liftColSwap a b = .< ColSwap (.~a, .~b) >.

(* code transformers *)
module Transformers = struct
    let rec full_unroll lb ub body = 
        if lb>ub then cunit
        else if lb = ub then body lb
        else .< begin .~(body lb); .~(full_unroll (lb+1) ub body) end >.
end

module Array1Dim = struct
  let init n = .< Array.init .~n (fun i -> i) >.
  let setL a v = .< let (x,y)= .~v in
                    let b = .~a and t=(.~a).(x) in
                    begin
                        b.(x) <- b.(y);
                        b.(y) <- t;
                        b
                    end >.
end
