(* name:          exception.ml
   synopsis:      exception monad
   author:        Lydia E. van Dijk
   last revision: Thu Jan  5 15:47:03 UTC 2006
   ocaml version: 3.09.0 *)


(** An [ExceptionMonad] is the purely functional replacement of
    OCaml's built-in exceptions.

    To indicate a normal value use {!ExceptionMonad.return}.  For
    exceptional conditions use {!ExceptionMonad.throw}.  Function
    {!ExceptionMonad.catch} splices in an exception handler into the
    thread of control.  Execute an exception monad with
    {!ExceptionMonad.run}. *)
module ExceptionMonad:
sig
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

      Run [a_monad].  If [a_monad] does not {!ExceptionMonad.throw} an
      exception, pass the result of evaluating the monad to
      [a_success_function].  Otherwise, if the [a_monad] throws, pass
      the exception to [a_failure_function]. *)
  val run: ('left -> 'a) -> ('right -> 'a) -> ('left, 'right) t -> 'a
end
  =
struct
  (** The values [('left, 'right) t] of generated from this module
      represent (mutually exclusive) alternatives.  Following the
      Haskell folklore we put the correct or "right" result of a
      computation into [Right] components and incorrect, or
      exceptional values into [Left] components. *)
  module Either =
  struct
    (** Disjoint alternatives *)
    type ('left, 'right) t = Left of 'left | Right of 'right


    (** [either f g x]

        Apply [f] to [x] for [Left x] and [g] to [x] for [Right x]. *)
    let either f g = function
        Left x -> f x
      | Right y -> g y
  end


  type ('left, 'right) t = ('left, 'right) Either.t


  let bind an_exception_monad a_function =
    (function
         Either.Right value -> a_function value
       | Either.Left _ as error -> error)
      an_exception_monad


  let return a_value = Either.Right a_value


  let throw an_error = Either.Left an_error


  let catch an_exception_monad an_error_handler =
    (function
         Either.Right _ as value -> value
       | Either.Left error -> an_error_handler error)
      an_exception_monad


  let run a_failure_function a_success_function =
    Either.either a_failure_function a_success_function
end


(**********************************************************************)

module type LIMIT =
sig
  val min: int64
  val max: int64
end


module ArithmeticFn (L: LIMIT) =
struct
  type 'a extended =
      Normal of 'a
    | PosInf
    | NegInf
    | NotANumber

  let string_of_extended =
    function
        Normal x -> Int64.to_string x
      | PosInf -> "+Infinity"
      | NegInf -> "-Infinity"
      | NotANumber -> "Not a Number"

  type reason =
      NegOverflow
    | PosOverflow
    | DivByZero

  let string_of_reason =
    function
        NegOverflow -> "negative overflow"
      | PosOverflow -> "positive overflow"
      | DivByZero -> "division by zero"

  type failure = {
    value: int64 extended;
    func: string;
    cause: reason
  }

  let op name f x y =
    perform with ExceptionMonad.bind in
      x' <-- x;
      y' <-- y;
      let z' = f (Int64.of_int x') (Int64.of_int y') in
        if z' < L.min then
          ExceptionMonad.throw {value = Normal z'; func = name; cause = NegOverflow}
        else if z' > L.max then
          ExceptionMonad.throw {value = Normal z'; func = name; cause = PosOverflow}
        else ExceptionMonad.return (Int64.to_int z')

  let add x y = op "add" Int64.add x y
  let sub x y = op "sub" Int64.sub x y
  let mul x y = op "mul" Int64.mul x y

  let div x y =
    let name = "div" in
      perform with ExceptionMonad.bind in
        x' <-- x;
        y' <-- y;
        if y' = 0 then
          ExceptionMonad.throw
            {value = if x' > 0 then PosInf else if x' < 0 then NegInf else NotANumber;
             func = name;
             cause = DivByZero}
        else op name Int64.div x y
end


(**********************************************************************)

let min_number = (-128)
and max_number = 127


module Arithmetic =
  ArithmeticFn (struct
                  let min = Int64.of_int min_number
                  let max = Int64.of_int max_number
                end)


let result =
  let zero = ExceptionMonad.return 0
  and one = ExceptionMonad.return 1
  and min = ExceptionMonad.return min_number
  and max = ExceptionMonad.return max_number
  and (+^) = Arithmetic.add
  and (/^) = Arithmetic.div
  in
    ExceptionMonad.catch
      (one +^ one +^ max /^ zero)
      (fun {Arithmetic.value = _v; func = _f; cause = c} as e ->
         match c with
             Arithmetic.NegOverflow -> max
           | Arithmetic.PosOverflow -> min
           | Arithmetic.DivByZero -> ExceptionMonad.throw e)


let test_exception_monad _ =
  Utest.expect_pass
    "exception monad"
    (fun () ->
       ExceptionMonad.run
         (fun {Arithmetic.value = v; func = f; cause = c} ->
            Printf.printf
              "error: %s in function \"%s\"\nvalue: %s\n"
              (Arithmetic.string_of_reason c)
              f
              (Arithmetic.string_of_extended v);
            false)
         (fun v -> v = 127)
         (ExceptionMonad.catch
            result
            (fun {Arithmetic.value = v; func = _f; cause = _c} as e ->
               match v with
                   Arithmetic.Normal n -> ExceptionMonad.return (Int64.to_int n)
                 | Arithmetic.NegInf -> ExceptionMonad.return min_number
                 | Arithmetic.PosInf -> ExceptionMonad.return max_number
                 | Arithmetic.NotANumber -> ExceptionMonad.throw e)))


(**********************************************************************)

let (_: unit) =
  let results = Utest.run_tests ~verbose:true [test_exception_monad]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 || results.Utest.unresolved <> 0 then 1 else 0)
