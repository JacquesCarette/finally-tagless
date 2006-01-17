(* name:          monadic-io.ml
 * synopsis:      Do input/output in a monadic way
 * author:        Lydia E. Van Dijk
 * last revision: Tue Jan 17 08:34:35 UTC 2006
 * ocaml version: 3.09.0 *)


let bind = Io.bind


let print_upcased a_string =
  let rec up i =
    if i >= String.length a_string then Io.return ()
    else
      perform
        Io.print_char (Char.uppercase a_string.[i]);
        up (succ i)
  in
    up 0


let rec process_line a_channel =
  perform
    Io.print_string "> ";
    Exception.run
      (fun _e -> Io.prerr_endline "*** exit via signal ***")
      (fun io ->
         perform
           s <-- io;
           if s = "" then Io.prerr_endline "*** exit via line-feed ***"
           else
             perform
               Io.output_string a_channel s;
               Io.output_char a_channel '\n';
               print_upcased s;
               Io.print_newline ();
               process_line a_channel)
      (Io.read_line ())


let to_uppercase () =
  Exception.run
    (function Io.SysError s -> Io.prerr_endline ("o/s error: \"" ^ s ^ "\"")
       | _ -> Io.prerr_endline "unknown error")
    (fun io ->
       perform
         ch <-- io;
         process_line ch;
         Exception.run
           (fun _e -> Io.return ())   (* ignore errors of [close_out] *)
           (fun io -> io)
           (Io.close_out ch))
    (Io.open_out "transcript.log")


(*
let to_uppercase' () =
  perform with module Exception in
    io <-- Io.open_out "transcript.log";
    Exception.return
      (perform
         ch <-- io;
         process_line ch);
*)


let main () =
  perform
    Io.print_endline "Monadic I/O";
    to_uppercase ();
    Io.print_endline "done."


let () =
  let world = Io.__conjure_up () in
    ignore (main () world)
