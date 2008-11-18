(* name:          io.ml
 * synopsis:      Rudimentary IO-Monad
 * author:        Lydia E. van Dijk
 * last revision: Thu Nov 13 09:52:36 UTC 2008
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


type ('left, 'right) either = Left of 'left | Right of 'right


type 'a t = world -> (error, 'a) either * world


let __conjure_up () =
  World.make ()


let bind an_iomonad a_function =
  fun w ->
    match an_iomonad w with
        (Left _e, _w) as error -> error
      | Right result, w' -> a_function result w'


let return a_value =
  fun w -> Right a_value, w


let throw an_error =
  fun w -> Left an_error, w


let catch an_iomonad a_handler_function =
  fun w ->
    match an_iomonad w with
        Left error, w' -> a_handler_function error w'
      | (Right _r, _w) as result -> result


(* Output Functions *)

let print_char a_character =
  return (Pervasives.print_char a_character)


let print_string a_string =
  return (Pervasives.print_string a_string)


let print_int an_integer =
  return (Pervasives.print_int an_integer)


let print_float a_float =
  return (Pervasives.print_float a_float)


let print_endline a_string =
  return (Pervasives.print_endline a_string)


let print_newline () =
  return (Pervasives.print_newline ())


let prerr_char a_character =
  return (Pervasives.prerr_char a_character)


let prerr_string a_string =
  return (Pervasives.prerr_string a_string)


let prerr_int an_integer =
  return (Pervasives.prerr_int an_integer)


let prerr_float a_float =
  return (Pervasives.prerr_float a_float)


let prerr_endline a_string =
  return (Pervasives.prerr_endline a_string)


let prerr_newline () =
  return (Pervasives.prerr_newline ())


(* Input Functions *)

let read_line () =
  try return (Pervasives.read_line ())
  with End_of_file -> throw EndOfFile


let read_int () =
  try return (Pervasives.read_int ())
  with
      End_of_file -> throw EndOfFile
    | Failure "int_of_string" -> throw IntOfString


let read_float () =
  try return (Pervasives.read_float ())
  with End_of_file -> throw EndOfFile


(* General Output Functions *)

let open_out a_filename =
  try return (Pervasives.open_out a_filename)
  with Sys_error s -> throw (SysError s)


let output_char a_channel a_char =
  return (Pervasives.output_char a_channel a_char)


let output_string a_channel a_string =
  return (Pervasives.output_string a_channel a_string)


let close_out a_channel =
  try return (Pervasives.close_out a_channel)
  with Sys_error s -> throw (SysError s)
