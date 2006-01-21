(* name:          io.ml
 * synopsis:      Rudimentary IO-Monad
 * author:        Lydia E. Van Dijk
 * last revision: Sat Jan 21 18:38:54 UTC 2006
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


module World =
struct
  type t = unit

  let make () = ()
end


(* IO-Monad *)

type error =
    EndOfFile
  | IntOfString
  | SysError of string


type world = World.t


type 'a t = world -> 'a * world


let __conjure_up () =
  World.make ()


let bind an_iomonad a_function =
  fun w ->
    let result, w' =
      an_iomonad w
    in
      a_function result w'


let return x =
  fun w -> x, w


(* Output Functions *)

let print_char a_character =
  fun w -> Pervasives.print_char a_character, w


let print_string a_string =
  fun w -> Pervasives.print_string a_string, w


let print_int an_integer =
  fun w -> Pervasives.print_int an_integer, w


let print_float a_float =
  fun w -> Pervasives.print_float a_float, w


let print_endline a_string =
  fun w -> Pervasives.print_endline a_string, w


let print_newline () =
  fun w -> Pervasives.print_newline (), w


let prerr_char a_character =
  fun w -> Pervasives.prerr_char a_character, w


let prerr_string a_string =
  fun w -> Pervasives.prerr_string a_string, w


let prerr_int an_integer =
  fun w -> Pervasives.prerr_int an_integer, w


let prerr_float a_float =
  fun w -> Pervasives.prerr_float a_float, w


let prerr_endline a_string =
  fun w -> Pervasives.prerr_endline a_string, w


let prerr_newline () =
  fun w -> Pervasives.prerr_newline (), w


(* Input Functions *)

let read_line () =
  try Exception.return (fun w -> Pervasives.read_line (), w)
  with End_of_file -> Exception.throw EndOfFile


let read_int () =
  try Exception.return (fun w -> Pervasives.read_int (), w)
  with
      End_of_file -> Exception.throw EndOfFile
    | Failure "int_of_string" -> Exception.throw IntOfString


let read_float () =
  try Exception.return (fun w -> Pervasives.read_float (), w)
  with End_of_file -> Exception.throw EndOfFile


(* General Output Functions *)

let open_out a_filename =
  try Exception.return (fun w -> Pervasives.open_out a_filename, w)
  with Sys_error s -> Exception.throw (SysError s)


let output_char a_channel a_char =
  fun w -> Pervasives.output_char a_channel a_char, w


let output_string a_channel a_string =
  fun w -> Pervasives.output_string a_channel a_string, w


let close_out a_channel =
  try Exception.return (fun w -> Pervasives.close_out a_channel, w)
  with Sys_error s -> Exception.throw (SysError s)
