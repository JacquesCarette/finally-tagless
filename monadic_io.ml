(* name:          monadic_io.ml
 * synopsis:      Do input/output in a monadic way
 * author:        Lydia E. van Dijk
 * last revision: Wed Oct 29 10:04:40 UTC 2008
 * ocaml version: 3.12.0 *)


let bind = Io.bind
let (>>=) = bind
let (>!=) = Io.catch


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
  Io.catch
    (perform
       Io.print_string "> ";
       s <-- Io.read_line ();
       if s = "" then Io.prerr_endline "*** exit via line-feed ***"
       else
         perform
           Io.output_string a_channel s;
           Io.output_char a_channel '\n';
           print_upcased s;
           Io.print_newline ();
           process_line a_channel)
    (fun _e -> Io.prerr_endline "*** exit via signal ***")


let to_uppercase () =
  perform
    let filename = "transcript.log" in
      ch <-- Io.open_out filename >!=
               (fun e ->
                  perform
                    Io.prerr_endline ("*** failed to open file \"" ^ filename ^ "\": " ^
                                        match e with
                                            Io.SysError s -> "system error \"" ^ s ^ "\""
                                          | _e -> "unknown error");
                    Io.throw e);
      process_line ch;
      Io.close_out ch >!= (fun _e ->
                             Io.prerr_endline ("*** failed to close file \"" ^
                                                 filename ^
                                                 "\""))


let main () =
  perform
    Io.print_endline "Monadic I/O";
    Io.print_endline "Translate lines of text to uppercase.";
    Io.print_endline "An empty line or SIGQUIT terminate.";
    to_uppercase ();
    Io.print_endline "done."


let () =
  let string_of_exception = function
      Io.EndOfFile -> "end of file"
    | Io.IntOfString -> "integer of string"
    | Io.SysError s -> "operating system error: \"" ^ s ^ "\""
  and world = Io.__conjure_up () in
    ignore
      begin
        (Io.catch
           (main ())
           (fun e -> Io.prerr_endline ("*** uncaught I/O error: " ^
                                         string_of_exception e)))
          world
      end
