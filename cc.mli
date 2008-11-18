(* name:          cc.mli
 * synopsis:      Interface to the delimited continuation monad
 *                shift/reset with multiple prompts
 * authors:       Oleg Kiselyov, Chung-chieh Shan, and Amr Sabry
 * last revision: Thu Nov 13 10:08:03 UTC 2008
 * ocaml version: 3.11
 *
 * Copyright (C) 2006-2008  O. Kiselyov, C.-c. Shan, A. Sabry
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


(** Delimited continuations: multi-prompt shift/reset with the
    polymorphic answertype and variously typed prompts. *)


(** The type of monadic values. *)
type 'a m

(** The type of prompts. *)
type 'a prompt


(** {4 Fundamental Functions} *)

(** [return a_value] *)
val return: 'a -> 'a m

(** [bind cc_monad f] *)
val bind: 'a m -> ('a -> 'b m) -> 'b m


(** {4 Running the monad} *)
val run: 'a m -> 'a


(** {4 Specific monad morphisms} *)

(** [new_prompt ()]

    Create a new prompt that corresponds to the value of the type
    ['a] *)
val new_prompt: unit ->  ('a prompt) m

(** [pushP prompt m]

    Push the [prompt] and execute the computation [m] *)
val pushP: 'a prompt -> 'a m -> 'a m

(** [shiftP prompt f]

    Capture the delimited continuation up to the dynamically closest
    occurrence of [prompt], remove that continuation and execute [f
    c], where [c] is the reified captured continuation enclosed in its
    own [prompt]. *)
val shiftP: 'a prompt -> (('b m -> 'a m) -> 'a m) -> 'b m
