module type Symantics = sig
    type e
    type t
    val john: e
    val mary: e
    val everyone: (e -> t) -> t
    val someone: (e -> t) -> t
    val saw: e -> e -> t
end

module Pheno = struct
    type e = string
    type t = string
    let john = "john"
    let mary = "mary"
    let everyone f = f "everyone"
    let someone f = f "someone"
    let saw obj subj = subj ^ " saw " ^ obj
end

module Truth = struct
    type e = John | Mary
    type t = bool
    let john = John
    let mary = Mary
    let everyone f = f John && f Mary
    let someone f = f John || f Mary
    let saw obj subj = match subj, obj with
        John, Mary | Mary, John -> true
        | _ -> false
end

module TEST(S: Symantics) = struct
    open S
    let test1 = saw mary john
    let test2 = someone (fun x -> everyone (fun y -> saw y x))
    let test3 = everyone (fun y -> someone (fun x -> saw y x))
end

module TP = TEST(Pheno)
module TT = TEST(Truth)

let test1TP = TP.test1
let test2TP = TP.test2
let test3TP = TP.test3
let test1TT = TT.test1
let test2TT = TT.test2
let test3TT = TT.test3
