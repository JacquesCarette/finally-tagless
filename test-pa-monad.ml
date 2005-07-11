(* Test non-determinism monad in the simplest possible implementation. *)

module Nondet:
sig
  type 'a stream =
      Nil
    | Cons of 'a * (unit -> 'a stream)
    | InC of (unit -> 'a stream)

  val ret: 'a -> unit -> 'a stream
  val mplus: (unit -> 'a stream) -> (unit -> 'a stream) -> unit -> 'a stream
  val bind: (unit -> 'a stream) -> ('a -> unit -> 'b stream) -> unit -> 'b stream
  val guard: bool -> unit -> unit stream
end
  =
struct
  type 'a stream =
      Nil
    | Cons of 'a * (unit -> 'a stream)
    | InC of (unit -> 'a stream)

  let mfail = fun () -> Nil

  let ret a = fun () -> Cons (a, mfail)

  (* actually, interleave: a fair disjunction with breadth-first search *)
  let rec mplus a b =
    fun () ->
      match a () with
          Nil -> InC b
        | InC a ->
            begin
              match b () with
		  Nil -> InC a
		| InC b -> InC (mplus a b)
		| Cons (b1, b2) -> Cons (b1, mplus a b2)
            end
        | Cons (a1, a2) -> Cons (a1, mplus b a2)

  (* a fair conjunction *)
  let rec bind m f =
    fun () ->
      match m () with
          Nil -> mfail ()
        | InC a -> InC (bind a f)
        | Cons (a, b) -> mplus (f a) (bind b f) ()

  let guard a_condition =
    if a_condition then ret () else mfail
end


let rec run n m =
  if n = 0 then []
  else
    match m () with
        Nondet.Nil -> []
      | Nondet.InC a -> run n a
      | Nondet.Cons (a, b) -> a :: run (pred n) b


let pythagorean_triples a_count =
  let rec number () =
    Nondet.InC
      begin
        Nondet.mplus
          (Nondet.ret 0)
          (mdo with Nondet.bind in
             n <-- number;
             Nondet.ret (succ n))
      end in
  let test =
    mdo with Nondet.bind in
      i <-- number;
      Nondet.guard (i > 0);
      j <-- number;
      Nondet.guard (j > 0);
      k <-- number;
      Nondet.guard (k > 0);
      (* Just to illustrate the `let' form within mdo *)
      let predicate n = n * n = j * j + k * k in;
        Nondet.guard (predicate i);
        Nondet.ret (i, j, k)
  in
    run a_count test


(* Find first n Pythagorean triples *)
let (_: unit) =
  List.iter
    (fun (i, j, k) -> Printf.printf "%2i  %2i  %2i\n" i j k)
    (pythagorean_triples 10)
