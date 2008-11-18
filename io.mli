(* name:          io.mli
 * synopsis:      Interface of rudimentary IO-Monad
 * author:        Lydia E. van Dijk
 * last revision: Thu Nov 13 09:52:54 UTC 2008
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


(** Rudimentary I/O-monad *)


(** {3 IO-Monad} *)

(** {4 Types} *)


(** Possible errors during an IO-operation.  They are named after
    their imperative counterparts. *)
type error =
    EndOfFile                           (** Tried to read beyond end of file *)
  | IntOfString                         (** Could not convert [string] to [int] *)
  | SysError of string                  (** Operating system signaled an error.
                                            The [string] takes the precise reason. *)


(** The imperative, "outside" world the {e functional} IO-monad deals with. *)
type world


(** This type takes care of the control-flow similarly to an exception
    monad.  The ['left] type is for errors, the ['right] type for correct
    values. *)
type ('left, 'right) either


(** An IO-monad of type ['a] takes the current world and produces
    either a correct value (of type ['a] or an [error] and a new world. *)
type 'a t = world -> (error, 'a) either * world


(** {4 Run-Time Support} *)

(** [__conjure_up ()]

    Conjure up a new "world".

    This function should only be used once per program.  It is best
    place in some pre-main initialization code like, for example,
    {[
        let () =
          let world = Io.__conjure_up () in
            ignore ((Io.catch (main ()) (fun _error -> ...)) world)
    ]}
    where we call [main] with the initial world. *)
val __conjure_up: unit -> world


(** {4 Fundamental Functions} *)

(** [bind an_iomonad a_function]

    Apply [a_function] to [an_iomonad] producing another IO-monad.
    [a_function] takes an ['a] value as argument and returns a ['b]
    IO-monad. *)
val bind: 'a t -> ('a -> 'b t) -> 'b t


(** [return a_value]

    List [a_value] into the IO-monad. *)
val return: 'a -> 'a t


(** [throw an_error]

    Throw [an_error] inside the IO-monad. *)
val throw: error -> 'a t


(** [catch an_iomonad a_handler_function]

    Catch IO-exceptions from [an_iomonad] and feed them into
    [a_handler_function], which takes an [error] value as argument and
    returns an ['a] IO-monad. *)
val catch: 'a t -> (error -> 'a t) -> 'a t


(** {4 Output Functions}

    All of these functions have exactly the same names as their imperative
    counterparts.  Moreover, they take the same arguments. *)

(** [print_char a_character] *)
val print_char: char -> unit t


(** [print_string a_string] *)
val print_string: string -> unit t


(** [print_int an_integer] *)
val print_int: int -> unit t


(** [print_float a_float] *)
val print_float: float -> unit t


(** [print_endline a_string] *)
val print_endline: string -> unit t


(** [print_newline ()] *)
val print_newline: unit -> unit t



(** [prerr_char a_character] *)
val prerr_char: char -> unit t


(** [prerr_string a_string] *)
val prerr_string: string -> unit t


(** [prerr_int an_integer] *)
val prerr_int: int -> unit t


(** [prerr_float a_float] *)
val prerr_float: float -> unit t


(** [prerr_endline a_string] *)
val prerr_endline: string -> unit t


(** [prerr_newline ()] *)
val prerr_newline: unit -> unit t


(** {4 Input Functions} *)

(** [read_line ()] *)
val read_line: unit -> string t


(** [read_int ()] *)
val read_int: unit -> int t


(** [read_float ()] *)
val read_float: unit -> float t


(** {4 General Output Functions} *)

(** [open_out a_filename] *)
val open_out: string -> out_channel t


(** [output_char a_channel a_char] *)
val output_char: out_channel -> char -> unit t


(** [output_string a_channel a_string] *)
val output_string: out_channel -> string -> unit t


(** [close_out a_channel] *)
val close_out: out_channel -> unit t
