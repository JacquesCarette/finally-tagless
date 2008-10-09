module type Symantics1 = sig
  type ('c,'dv) repr
  type 'c dint
  type 'c dbool
  type ('c,'da,'db) darr
  val int  : int  -> ('c, 'c dint) repr
  val bool : bool -> ('c, 'c dbool) repr
  val add  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dint) repr
  val mul  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dint) repr
  val leq  : ('c, 'c dint) repr -> ('c, 'c dint) repr -> ('c, 'c dbool) repr
  val if_  : ('c, 'c dbool) repr ->
             (unit -> 'x) -> (unit -> 'x) -> (('c, 'da) repr as 'x)

  val lam : (('c, 'da) repr -> ('c, 'db) repr) -> ('c, ('c,'da,'db) darr) repr
  val app : ('c, ('c,'da,'db) darr) repr -> ('c, 'da) repr -> ('c, 'db) repr
  val fix : ('x -> 'x) -> (('c, ('c,'da,'db) darr) repr as 'x)
end;;

module EX(S: Symantics1) = struct
 open S
 let testpowfix () = lam (fun x ->
                      fix (fun self -> lam (fun n ->
                        if_ (leq n (int 0)) (fun () -> int 1)
                            (fun () -> mul x (app self (add n (int (-1))))))))
 let testpowfix7 () = lam (fun x -> app (app (testpowfix ()) x) (int 7))
 let testpowfix0 () = app (testpowfix ()) (int 0)
 let diverg () = app (lam (fun x -> int 1)) (app (fix (fun f -> f)) (int 2))
end;;

module R = struct
  type ('c,'dv) repr = 'dv
  type 'c dint = int
  type 'c dbool = bool
  type ('c,'da,'db) darr = 'da -> 'db
  let int (x:int) = x
  let bool (b:bool) = b
  let add e1 e2 = e1 + e2
  let mul e1 e2 = e1 * e2
  let leq x y = x <= y
  let if_ eb et ee = if eb then (et ()) else (ee ())

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f = let rec self n = f self n in self
end;;

module EXR = EX(R);;

module C = struct
  type ('c,'dv) repr = ('c,'dv) code
  type 'c dint = int
  type 'c dbool = bool
  type ('c,'da,'db) darr = 'da -> 'db
  let int (x:int) = .<x>.
  let bool (b:bool) = .<b>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let mul e1 e2 = .<.~e1 * .~e2>.
  let leq x y = .< .~x <= .~y >.
  let if_ eb et ee = .<if .~eb then .~(et () ) else .~(ee () )>.

  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<let rec self n = .~(f .<self>.) n in self>.
end;;

module EXC = EX(C);;

module CPST(S: Symantics1)(W: sig type 'c dw end) = struct
  open W
  type ('c,'dv) repr = ('c, ('c, ('c, 'dv, 'c dw) S.darr, 'c dw) S.darr) S.repr
  type 'c dint = 'c S.dint
  type 'c dbool = 'c S.dbool
  type ('c,'da,'db) darr = ('c, 'da, ('c, ('c, 'db, 'c dw) S.darr, 'c dw) S.darr) S.darr
  let int i = S.lam (fun k -> S.app k (S.int i))
  let bool b = S.lam (fun k -> S.app k (S.bool b))
  let add e1 e2 = S.lam (fun k ->
    S.app e1 (S.lam (fun v1 ->
    S.app e2 (S.lam (fun v2 -> S.app k (S.add v1 v2))))))
  let mul e1 e2 = S.lam (fun k ->
    S.app e1 (S.lam (fun v1 ->
    S.app e2 (S.lam (fun v2 -> S.app k (S.mul v1 v2))))))
  let leq e1 e2 = S.lam (fun k ->
    S.app e1 (S.lam (fun v1 ->
    S.app e2 (S.lam (fun v2 -> S.app k (S.leq v1 v2))))))
  let if_ ec et ef = S.lam (fun k ->
    S.app ec (S.lam (fun vc ->
    S.if_ vc (fun () -> S.app (et ()) k) (fun () -> S.app (ef ()) k))))
  let lam f = S.lam (fun k -> S.app k (S.lam (fun x ->
    f (S.lam (fun k -> S.app k x)))))
  let app e1 e2 = S.lam (fun k -> 
    S.app e1 (S.lam (fun f ->
    S.app e2 (S.lam (fun v -> S.app (S.app f v) k)))))
  let fix = S.fix
end;;

module RCPST = CPST(R)(struct type 'c dw = 'c R.dint end);;
module EXRCPST = EX(RCPST);;
let 128 = RCPST.app (EXRCPST.testpowfix7 ()) (RCPST.int 2) (fun x -> x);;
let 129 = .! .<1 + .~(CCPST.app (EXCCPST.testpowfix7 ()) (CCPST.int 2)) (fun x -> x)>.;;
module RCPSTCPST = CPST(RCPST)(struct type 'c dw = 'c R.dbool end);;

module CCPST = CPST(C)(struct type 'c dw = 'c C.dint end);;
module EXCCPST = EX(CCPST);;

(* Danvy and Filinski, "Representing Control", Figure 7 *)
module CN(S: Symantics1)(W: sig type 'c dw end) = struct
  open W
  type ('c,'dv) repr =
       (('c, 'dv) S.repr -> ('c, 'c dw) S.repr) -> ('c, 'c dw) S.repr
  type 'c dint = 'c S.dint
  type 'c dbool = 'c S.dbool
  type ('c, 'dv) reprrepr = ('c, ('c, 'dv, 'c dw) S.darr, 'c dw) S.darr
  type ('c,'da,'db) darr = ('c, ('c, 'da) reprrepr, ('c, 'db) reprrepr) S.darr
  let int x k = k (S.int x)
  let bool b k = k (S.bool b)
  let add e1 e2 k = e1 (fun v1 -> e2 (fun v2 -> k (S.add v1 v2)))
  let mul e1 e2 k = e1 (fun v1 -> e2 (fun v2 -> k (S.mul v1 v2)))
  let leq e1 e2 k = e1 (fun v1 -> e2 (fun v2 -> k (S.leq v1 v2)))
  let if_ eb et ee k = eb (fun vb ->
      S.if_ vb (fun () -> et () k) (fun () -> ee () k))

  let reify x = S.lam (fun k -> x (S.app k))
  let reflect x = fun k -> S.app x (S.lam k)
  let lam f k = k (S.lam (fun x -> reify (f (reflect x))))
  let app e1 e2 k = e1 (fun m -> reflect (S.app m (reify e2)) k)
  let fix f = reflect (S.fix (fun self -> reify (f (reflect self))))
end;;

module RCN = CN(R)(struct type 'c dw = 'c R.dint end);;
module EXRCN = EX(RCN);;
let 128 = RCN.app (EXRCN.testpowfix7 ()) (RCN.int 2) (fun x -> x);;
let 1 = EXRCN.diverg () (fun x -> x);;

module CCN = CN(C)(struct type 'c dw = 'c C.dint end);;
module EXCCN = EX(CCN);;
let 129 = .! .< 1 + .~(CCN.app (EXCCN.testpowfix7 ()) (CCN.int 2) (fun x -> x)) >.;;
let 2 = .! .< 1 + .~(EXCCN.diverg () (fun x -> x)) >.;;

