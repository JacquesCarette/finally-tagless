open Prelude

(*module type T = functor(R: Abstractrep.T) -> sig *)
module type T = sig
(* Abstract Representation of ``code'' of ``code'' *)
type (+'a,'b) abstract (* = ('a, 'b) R.abstract *)

val retN : ('a, 'b) abstract -> ('s -> ('s -> ('a, 'b) abstract -> ('a, 'w)
abstract ) -> ('a, 'w) abstract)

val seq : ('a, 'b) abstract -> ('a, 'c) abstract -> ('a, 'c) abstract
val seqL :
  ('a, 'b) abstract -> ('a, 'c) abstract -> 'd -> ('d -> ('a, 'c) abstract -> 'e) -> 'e
val seqM :
  ('a -> ('b -> 'c -> 'c) -> ('d, 'e) abstract) ->
  ('a -> ('f -> 'g -> 'g) -> ('d, 'h) abstract) ->
  'a -> ('a -> ('d, 'h) abstract -> 'i) -> 'i

val optSeq : ('a, 'b) abstract -> ('a, 'b) abstract option -> ('a, 'b) abstract

val optSeqM : 
  ('a -> ('a -> ('d,'e) abstract -> ('d,'e) abstract) -> ('d, 'e) abstract) ->
  ('a -> ('a -> 'g -> 'g) -> ('d, 'e) abstract) option ->
  'a -> ('a -> ('d, 'e) abstract -> ('d,'e) abstract) -> ('d,'e) abstract

val ifM :
  ('a, bool) abstract ->
  ('b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
  ('b -> ('f -> 'g -> 'g) -> ('a, 'e) abstract) ->
  'b -> ('b -> ('a, 'e) abstract -> 'h) -> 'h
val rshiftM : ('a -> 'b) -> 'a -> ('a -> 'b -> 'c) -> 'c
val whenM :
  ('a, bool) abstract ->
  ('b -> ('c -> 'd -> 'd) -> ('a, unit) abstract) ->
  'b -> ('b -> ('a, unit) abstract -> 'e) -> 'e
val loopM :
  ('a, int) abstract ->
  ('a, int) abstract ->
  (('a, int) abstract -> 'b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
  dir ->
  'b -> ('b -> ('a, unit) abstract -> 'f) -> 'f
val whileM :
  ('a, bool) abstract ->
  ('b -> ('c -> 'd -> 'd) -> ('a, 'e) abstract) ->
  'b -> ('b -> ('a, unit) abstract -> 'f) -> 'f
val matchM :
  ('a, 'b option) abstract ->
  (('a, 'b) abstract -> 'c -> ('d -> 'e -> 'e) -> ('a, 'f) abstract) ->
  ('c -> ('g -> 'h -> 'h) -> ('a, 'f) abstract) ->
  'c -> ('c -> ('a, 'f) abstract -> 'i) -> 'i
val genrecloop :
  (('a, 'b -> 'c) abstract ->
   ('a, 'b) abstract -> 'd -> ('e -> 'f -> 'f) -> ('a, 'c) abstract) ->
  ('a, 'b) abstract -> 'd -> ('d -> ('a, 'c) abstract -> 'g) -> 'g
val lift : 'a -> ('b, 'a) abstract
val liftRef : ('a, 'b) abstract -> ('a, 'b ref) abstract
val liftGet : ('a, 'b ref) abstract -> ('a, 'b) abstract
val unitL : 'a -> ('a -> ('b, unit) abstract -> 'c) -> 'c
val liftPair : ('a, 'b * 'c) abstract -> ('a, 'b) abstract * ('a, 'c) abstract
val liftPPair :
  ('a, ('b * 'c) * 'd) abstract -> ('a, 'b) abstract * ('a, 'c) abstract * ('a,
  'd) abstract
module Logic :
  sig
    val notL : ('a, bool) abstract -> ('a, bool) abstract
    val equalL : ('a, 'b) abstract -> ('a, 'b) abstract -> ('a, bool) abstract
    val notequalL : ('a, 'b) abstract -> ('a, 'b) abstract -> ('a, bool)
    abstract
    val andL : ('a, bool) abstract -> ('a, bool) abstract -> ('a, bool) abstract
  end
module Idx :
  sig
    val zero : ('a, int) abstract
    val one : ('a, int) abstract
    val minusone : ('a, int) abstract
    val succ : ('a, int) abstract -> ('a, int) abstract
    val pred : ('a, int) abstract -> ('a, int) abstract
    val less : ('a, 'b) abstract -> ('a, 'b) abstract -> ('a, bool) abstract
    val uminus : ('a, int) abstract -> ('a, int) abstract
    val add : ('a, int) abstract -> ('a, int) abstract -> ('a, int) abstract
    val minusoneL : 'a -> ('a -> ('b, int) abstract -> 'c) -> 'c
  end
module Maybe :
  sig
    val just : ('a, 'b) abstract -> ('a, 'b option) abstract
    val none : ('a, 'b option) abstract
  end
val applyMaybe : ('a -> 'a) option -> 'a -> 'a
module Tuple :
  sig
    val tup2 : ('a, 'b) abstract -> ('a, 'c) abstract -> ('a, 'b * 'c) abstract
    val tup3 :
      ('a, 'b) abstract ->
      ('a, 'c) abstract -> ('a, 'd) abstract -> ('a, 'b * 'c * 'd) abstract
    val tup4 :
      ('a, 'b) abstract ->
      ('a, 'c) abstract ->
      ('a, 'd) abstract -> ('a, 'e) abstract -> ('a, 'b * 'c * 'd * 'e) abstract
  end
module CList :
  sig
    val nil : ('a, 'b list) abstract
    val cons : ('a, 'b) abstract -> ('a, 'b list) abstract -> ('a, 'b list)
    abstract
  end
val cunit : ('a, unit) abstract
val update :
  ('a, 'b ref) abstract -> (('a, 'b) abstract -> ('a, 'b) abstract) -> ('a,
  unit) abstract
val assign : ('a, 'b ref) abstract -> ('a, 'b) abstract -> ('a, unit) abstract
val apply : ('a, 'b -> 'c) abstract -> ('a, 'b) abstract -> ('a, 'c) abstract
val updateM :
  ('a, 'b ref) abstract ->
  (('a, 'b) abstract -> ('a, 'b) abstract) ->
  'c -> ('c -> ('a, unit) abstract -> 'd) -> 'd
val assignM :
  ('a, 'b ref) abstract ->
  ('a, 'b) abstract -> 'c -> ('c -> ('a, unit) abstract -> 'd) -> 'd
val applyM :
  ('a, 'b -> 'c) abstract ->
  ('a, 'b) abstract -> 'd -> ('d -> ('a, 'c) abstract -> 'e) -> 'e
module Transformers :
  sig
    val full_unroll :
      int -> int -> (int -> ('a, unit) abstract) -> ('a, unit) abstract
  end

module Array1Dim : sig
  val init : ('a,int) abstract -> ('a, int array) abstract
  val setL : ('a,int array) abstract -> ('a,int*int) abstract -> ('a,int array) abstract
end

(* This type is needed for the output, and is tracked during pivoting. *)
type perm = RowSwap of (int * int) | ColSwap of (int*int)
val liftRowSwap : 
    ('a, int) abstract -> ('a, int) abstract -> ('a, perm) abstract
val liftColSwap : 
    ('a, int) abstract -> ('a, int) abstract -> ('a, perm) abstract
end
