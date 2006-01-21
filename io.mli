(* name:          io.mli
 * synopsis:      Interface of rudimentary IO-Monad
 * author:        Lydia E. Van Dijk
 * last revision: Sat Jan 21 18:34:13 UTC 2006
 * ocaml version: 3.09.0
 *
 * Copyright (C) 2006  J. Carette, L. E. van Dijk, O. Kiselyov
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


(** {3: IO-Monad} *)

type error =
    EndOfFile
  | IntOfString
  | SysError of string


type world


type 'a t = world -> 'a * world


val __conjure_up: unit -> world

val bind: 'a t -> ('a -> 'b t) -> 'b t
val return: 'a -> 'a t


(** {4: Output Functions} *)

val print_char: char -> unit t
val print_string: string -> unit t
val print_int: int -> unit t
val print_float: float -> unit t
val print_endline: string -> unit t
val print_newline: unit -> unit t

val prerr_char: char -> unit t
val prerr_string: string -> unit t
val prerr_int: int -> unit t
val prerr_float: float -> unit t
val prerr_endline: string -> unit t
val prerr_newline: unit -> unit t


(** {4: Input Functions} *)

val read_line: unit -> (error, string t) Exception.t
val read_int: unit -> (error, int t) Exception.t
val read_float: unit -> (error, float t) Exception.t


(** {4: General Output Functions} *)

val open_out: string -> (error, out_channel t) Exception.t
val output_char: out_channel -> char -> unit t
val output_string: out_channel -> string -> unit t
val close_out: out_channel -> (error, unit t) Exception.t
