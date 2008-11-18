(* name:          exception.mli
 * synopsis:      interface of exception monad
 * author:        Lydia E. van Dijk
 * last revision: Thu Nov 13 09:52:17 UTC 2008
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


(** Exception is a purely functional replacement of
    OCaml's built-in exceptions.

    To indicate a normal value use {!Exception.return}.  For
    exceptional conditions use {!Exception.throw}.  Function
    {!Exception.catch} splices in an exception handler into the
    thread of control.  Execute an exception monad with
    {!Exception.run}. *)


(** Type of an exception monad.  ['left] is the exception's type and
    ['right] is the normal value's type. *)
type ('left, 'right) t


(** [bind a_monad a_function]

    Apply [a_function] to [a_monad] producing another monad.
    [a_function] takes a normal value as argument and returns a
    monad. *)
val bind: ('left, 'right) t -> ('right -> ('left, 'new_right) t) -> ('left, 'new_right) t


(** [return a_normal_value]

    Answer [a_normal_value]. *)
val return: 'right -> ('left, 'right) t


(** [throw an_exception]

    Answer [an_exception], or in other words, throw [an_exception]. *)
val throw: 'left -> ('left, 'right) t


(** [catch a_monad an_exception_handler]

    Catch exceptions from [a_monad] and feed them into
    [an_exception_handler].  [an_exception_handler] takes an
    exceptional value as argument and returns a monad. *)
val catch: ('left, 'right) t -> ('left -> ('new_left, 'right) t) -> ('new_left, 'right) t


(** [run a_failure_function a_success_function a_monad]

    Run [a_monad].  If [a_monad] does not {!Exception.throw} an
    exception, pass the result of evaluating the monad to
    [a_success_function].  Otherwise, if the [a_monad] throws, pass
    the exception to [a_failure_function]. *)
val run: ('left -> 'a) -> ('right -> 'a) -> ('left, 'right) t -> 'a
