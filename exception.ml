(* name:          exception.ml
 * synopsis:      exception monad
 * author:        Lydia E. van Dijk
 * last revision: Thu Nov 13 09:51:57 UTC 2008
 * ocaml version: 3.11
 *
 * Copyright (C) 2006-2008  J. Carette, L. E. van Dijk, O. Kiselyov
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


(** The values [('left, 'right) t] of generated from this module
    represent (mutually exclusive) alternatives.  Following the
    Haskell folklore we put the correct or "right" result of a
    computation into [Right] components and incorrect, or
    exceptional values into [Left] components. *)
module Either =
struct
  (** Disjoint alternatives *)
  type ('left, 'right) t = Left of 'left | Right of 'right


  (** [either f g x]

      Apply [f] to [x] for [Left x] and [g] to [x] for [Right x]. *)
  let either f g = function
      Left x -> f x
    | Right y -> g y
end


type ('left, 'right) t = ('left, 'right) Either.t


let bind an_exception_monad a_function =
  match an_exception_monad with
      Either.Right value -> a_function value
    | Either.Left _ as error -> error


let return a_value = Either.Right a_value


let throw an_error = Either.Left an_error


let catch an_exception_monad an_error_handler =
  (function
       Either.Right _ as value -> value
     | Either.Left error -> an_error_handler error)
    an_exception_monad


let run a_failure_function a_success_function =
  Either.either a_failure_function a_success_function
