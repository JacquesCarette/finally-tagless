(* report as sent to caml-list *)
(* Base monad type, to be used throughout *)
type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let ret a = fun s k -> k s a
let retN a = fun s k -> let t = a in k s t
let bind a f = fun s k -> a s (fun s' b -> f b s' k)
let k0 s v = v  (* Initial continuation -- for `reset' and `run' *)
let runM m = m [] k0 (* running our monad *)
let liftGet x = ! x
let liftRef x = ref x

let l1 f x = bind x (fun t -> f t)

let seqM a b = fun s k -> k s (begin a s k0 ; b s k0 end)

(* while ``loops'' do not naturally bind a value *)
let retWhileM cond body = fun s k -> 
    k s (while cond s k0 do body s k0 done)

(* monadic logic combinators *)
module LogicCode = struct
  let and_ a b = ret (a && b)
end

(* operations on indices *)
module Idx = struct
  let zero = 0
  let succ a = a+1
  let less a b = a<b
end

(* code generators *)
module Code = struct
  let update a f = let b = f (liftGet a) in ret (a := b) 
end

let dogen a = 
  bind (retN (liftRef Idx.zero)) (fun c ->
  bind (retN (Array.length a)) (fun m -> 
    (retWhileM (ret (Idx.less (liftGet c) m))
       (bind (retN (liftGet c)) (fun cc ->
       Printf.printf "%i %i\n" cc !c;
       Code.update c Idx.succ ))))) ;;

let gen a = runM (dogen a) ;;

gen (Array.make 1 1.) ;;
