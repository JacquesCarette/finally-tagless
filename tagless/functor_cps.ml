(* Add this code just for pure completeness.  Maybe it should be folded
   in to incope.ml ?  *)
module type Symantics = sig
  type ('c,'sv,'dv) repr

  val int : int  -> ('c,int,int) repr
  val bool: bool -> ('c,bool,bool) repr

  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x)
            -> ('c,'x,'da -> 'db) repr
  val app : ('c,'x,'da -> 'db) repr
            -> (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x)
  val fix : ('x -> 'x) -> (('c, ('c,'sa,'da) repr -> ('c,'sb,'db) repr,
                                'da -> 'db) repr as 'x)

  val add : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val mul : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val leq : ('c,int,int) repr -> ('c,int,int) repr -> ('c,bool,bool) repr
  val if_ : ('c,bool,bool) repr
            -> (unit -> 'x) -> (unit -> 'x) -> (('c,'sa,'da) repr as 'x)
end

module RCN(W : sig type w end)= struct
  type ('c,'sv,'dv) repr = ('sv -> W.w) -> W.w
  let int (x:int) = fun k -> k x
  let bool (x:bool) = fun k -> k x
  let add e1 e2 = fun k ->
      e1 (fun v1 -> e2 (fun v2 -> k (v1 + v2)))
  let mul e1 e2 = fun k ->
      e1 (fun v1 -> e2 (fun v2 -> k (v1 * v2)))
  let leq e1 e2 = fun k ->
      e1 (fun v1 -> e2 (fun v2 -> k (v1 <= v2)))
  let if_ eb et ee = fun k ->
      eb (fun vb -> if vb then (et ()) k else (ee ()) k)
  let lam f = fun k -> k f
  let app e1 e2 = fun k -> e1 (fun f -> (f e2) k)
  let fix f = let rec fx f n = app (f (lam (fx f))) n in lam (fx f)
  let run x = x (fun v -> v)
end;;

module X = struct type w=int end;;
module C : Symantics = RCN(X);;
