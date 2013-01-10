(* Base monad type, to be used throughout *)
(* $Id$ *)

type ('p,'v) monad = 's -> ('s -> 'v -> 'w) -> 'w
    constraint 'p = <state : 's; answer : 'w; ..>

let ret (a :'v) : ('p,'v) monad = fun s k -> k s a
let (let!) (m : ('p,'v) monad) (f : 'v -> ('p,'u) monad) : ('p,'u) monad
  = fun s k -> m s (fun s' b -> f b s' k)

let k0 _ v = v  (* Initial continuation -- for `reset' and `run' *)

(* running our monad; need to pass the init state *)
let runM m = fun s0 -> m s0 k0 

(* The monad has 2 parts: the continuation and the state.  For the
   state part, we only use 2 morphisms *)
let fetch s k = k s s  and  store v _ k = k v ()

