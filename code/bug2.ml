(* Base monad type, to be used throughout *)
type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let force = Lazy.force
let ret a = fun s k -> k s a
let retN a = fun s k -> lazy (let t = force a in force (k s (lazy t)))
let bind a f = fun s k -> a s (fun s' b -> f b s' k)
let k0 s v = v  (* Initial continuation -- for `reset' and `run' *)
let runM m = m [] k0 (* running our monad *)
let liftGet x = lazy (! (force x) )
let liftRef x = lazy (ref (force x))

let l1 f x = bind x (fun t -> f t)

let seqM a b = fun s k -> k s (lazy (begin force (a s k0) ; force(b s k0) end))

(* while ``loops'' do not naturally bind a value *)
let retWhileM cond body = fun s k -> 
    k s (lazy (while (force (cond s k0)) do force (body s k0) done))

(* operations on indices *)
module Idx = struct
  let zero = lazy 0
  let succ a = lazy ((force a)+1)
  let less a b = lazy (force a < force b)
end

(* code generators *)
module Code = struct
  let update a f = let b = f (liftGet a) in ret (lazy ((force a) := (force b)))
end

let dogen a = 
  bind (retN (liftRef Idx.zero)) (fun c ->
  bind (retN (lazy (Array.length a))) (fun m -> 
    (retWhileM (ret (Idx.less (liftGet c) m))
       (bind (retN (liftGet c)) (fun cc ->
       Printf.printf "%i %i\n" (force cc) !(force c);
       (Code.update c Idx.succ)))))) ;;

let gen a = runM (dogen a) ;;

(gen (Array.make 1 1.)) ;;
