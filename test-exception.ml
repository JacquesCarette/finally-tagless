(* name:          test-exception.ml
   synopsis:      test exception monad
   author:        Lydia E. van Dijk
   last revision: Fri Feb 10 14:27:11 UTC 2006
   ocaml version: 3.09.0 *)


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
    perform with Exception.bind in
      x' <-- x;
      y' <-- y;
      let z' = f (Int64.of_int x') (Int64.of_int y') in
        if z' < L.min then
          Exception.throw {value = Normal z'; func = name; cause = NegOverflow}
        else if z' > L.max then
          Exception.throw {value = Normal z'; func = name; cause = PosOverflow}
        else Exception.return (Int64.to_int z')

  let add x y = op "add" Int64.add x y
  let sub x y = op "sub" Int64.sub x y
  let mul x y = op "mul" Int64.mul x y

  let div x y =
    let name = "div" in
      perform with Exception.bind in
        x' <-- x;
        y' <-- y;
        if y' = 0 then
          Exception.throw
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
  let zero = Exception.return 0
  and one = Exception.return 1
  and min = Exception.return min_number
  and max = Exception.return max_number
  and (+^) = Arithmetic.add
  and (/^) = Arithmetic.div
  in
    Exception.catch
      (one +^ one +^ max /^ zero)
      (fun {Arithmetic.value = _v; func = _f; cause = c} as e ->
         match c with
             Arithmetic.NegOverflow -> max
           | Arithmetic.PosOverflow -> min
           | Arithmetic.DivByZero -> Exception.throw e)


let test_exception_monad _ =
  Utest.expect_pass
    "exception monad"
    (fun () ->
       Exception.run
         (fun {Arithmetic.value = v; func = f; cause = c} ->
            Printf.printf
              "error: %s in function \"%s\"\nvalue: %s\n"
              (Arithmetic.string_of_reason c)
              f
              (Arithmetic.string_of_extended v);
            false)
         (fun v -> v = 127)
         (Exception.catch
            result
            (fun {Arithmetic.value = v; func = _f; cause = _c} as e ->
               match v with
                   Arithmetic.Normal n -> Exception.return (Int64.to_int n)
                 | Arithmetic.NegInf -> Exception.return min_number
                 | Arithmetic.PosInf -> Exception.return max_number
                 | Arithmetic.NotANumber -> Exception.throw e)))


(**********************************************************************)

let (_: unit) =
  let results = Utest.run_tests Utest.PrintFailedTests [test_exception_monad]
  in
    Pervasives.exit
      (if results.Utest.failed <> 0 || results.Utest.unresolved <> 0 then 1 else 0)
