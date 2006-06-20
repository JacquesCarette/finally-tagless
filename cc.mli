(* name:          cc.mli
 * synopsis:      Interface to the delimited continuation monad
 *                shift/reset with multiple prompts
 * ocaml version: 3.09.0
 * This is joint work with Chung-chieh Shan and Amr Sabry.
 *)

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
    create a new prompt that corresponds to the value of the type ['a] *)
val new_prompt: unit ->  ('a prompt) m

(** [pushP prompt m] 
    Push the [prompt] and execute the computation [m] *)
val pushP: 'a prompt -> 'a m -> 'a m

(** [shiftP prompt f]
    Capture the delimited continuation up to the dynamically closest 
    occurrence of [prompt], remove that continuation and execute
    [f c], where [c] is the reified captured continuation enclosed in
    its own [prompt]. *)
val shiftP: 'a prompt -> (('b m -> 'a m) -> 'a m) -> 'b m


