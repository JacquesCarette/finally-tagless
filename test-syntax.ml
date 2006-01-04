(* name:          test-syntax.ml
 * synopsis:      test the syntax extension "pa_monad"
 * author:        Lydia E. Van Dijk
 * last revision: Sun Dec 18 08:17:37 UTC 2005
 * ocaml version: 3.09.0 *)


(* module *)
module IdentityMonad:
sig
  val bind: int -> (int -> int) -> int
  val ret: int -> int
  val failwith: string -> int
end
  =
struct
  let bind a f = f a
  let ret a = a
  let failwith s = print_endline ("failed with " ^ s); 0
end

module NestedIdentityMonad = struct module M = IdentityMonad end


(* standalone top-level bindings *)

let bind a f = f a
let ret a = a
let fail s = print_endline ("failed with " ^ s); 0


(* tests *)

let _ = perform ret 1                       (* - : int = 1 *)
let _ = perform (print_endline "1"); 1      (* - : int = 1 *)
let _ = perform x <-- 1; x                  (* - : int = 1 *)
let _ = perform let x = 1 in x              (* - : int = 1 *)

let _ = perform ret '1'                     (* - : char = '1' *)
let _ = perform (print_endline "1"); '1'    (* - : char = '1' *)
let _ = perform let x = '1' in x <-- x; x                (* - : char = '1' *)
let _ = perform let x = '1' in x            (* - : char = '1' *)

let _ = perform ret "1"                     (* - : string = "1" *)
let _ = perform (print_endline "1"); "1"    (* - : string = "1" *)
let _ = perform x <-- "1"; x                 (* - : string = "1" *)
let _ = perform let x = "1" in x            (* - : string = "1" *)


let _ = perform () <-- (); 1                 (* - : int = 1 *)
let _ = perform (((x))) <-- 1; x             (* - : int = 1 *)
let _ = perform _ <-- 99; 1                  (* - : int = 1 *)
let _ = perform (x, y, z) <-- 1, 2, 3; x + y + z (* - : int = 6 *)
(* This one does not work:
  And should it? The `as' pattern is not simple!
let _ =
    perform
      ((x, y, z) as tuple) <-- 1, 2, 3;
      x + y + z (* - : int = 6 *)
*)
let _ = perform (x: int) <-- 1; x           (* - : int = 1 *)
let _ = perform let module M = IdentityMonad in 
                let rec fact n = if n < 1 then 1 else n * (fact (n-1)) in
		M.ret (fact 5)              (* - : int = 120 *)


module Rec = struct type t = {x: int; y: int; z: int} end
let _ =
  perform
    {Rec.x = x'; y = y'; z = z'} <-- {Rec.x = 1; y = 2; z = 3};
    x' + y' + z' (* - : int = 6 *)


(* "with" form *)

let _ = perform with bind in x <-- 1; x (* - : int = 1 *)
let _ = perform with IdentityMonad.bind in x <-- 1; x (* - : int = 1 *)
let _ = perform with fun a f -> f a in x <-- 1; x (* - : int = 1 *)


(* "with module" form *)

let _ = perform with module IdentityMonad in x <-- 1; x (* - : int = 1 *)
let _ = perform with module NestedIdentityMonad.M in x <-- 1; x (* - : int = 1 *)


(* Refutable Patterns *)

let _ = perform 1 <-- 1; 1                  (* - : int = 1 *)
let _ = perform (((1))) <-- 1; 1            (* - : int = 1 *)

let _ = perform (x, 2, z) <-- 1, 2, 3; x + z (* - : int = 4 *)

let _ =
  perform
    {Rec.x = x'; y = 2; z = z'} <-- {Rec.x = 1; y = 2; z = 3};
    x' + z'                             (* - : int = 4 *)

(* This one does not work:
 *     type single = Foo
 *     let _ = perform Foo <-- Foo; 1 *)
type multiple = Bar | Baz
let _ = perform Bar <-- Bar; 1


(* Refutable Patterns and "with"-"and" form *)

let _ = perform with bind and fail in 1 <-- 1; 1 (* - : int = 1 *)
let _ = perform with module IdentityMonad in 1 <-- 1; 1 (* - : int = 1 *)


(* Recursive Binding *)

(* Does not work yet:
 * (See also: http://www.cse.ogi.edu/PacSoft/projects/rmb/repMin.html)
 *  let some_ones =
 *    perform
 *      xs <-- Some (1 :: xs);
 *      return xs
 *)
