(* Base monad type, to be used throughout *)
(* $Id$ *)

type ('p,'v) monad = 's -> ('s -> 'v -> 'w) -> 'w
    constraint 'p = <state : 's; answer : 'w; ..>

let ret (a :'v) : ('p,'v) monad = fun s k -> k s a
let bind a f = fun s k -> a s (fun s' b -> f b s' k)

let k0 _ v = v  (* Initial continuation -- for `reset' and `run' *)

(* running our monad; need to pass the init state *)
let runM m = fun s0 -> m s0 k0 

(* Monad lifting functions *)
let l1 f = fun x     -> perform t <-- x; f t
let l2 f = fun x y   -> perform tx <-- x; ty <-- y; f tx ty
let l3 f = fun x y z -> perform tx <-- x; ty <-- y; tz <-- z; f tx ty tz

(* The monad has 2 parts: the continuation and the state.  For the
   state part, we only use 2 morphisms *)
let fetch s k = k s s  and  store v s k = k (v::s) ()

