(* Base monad type, to be used throughout *)
type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let ret a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k)
let k0 s v = v  (* Initial continuation -- for `reset' and `run' *)
let runM m = m [] k0 (* running our monad *)

(* Monad lifting functions *)
let l1 f = fun x -> perform t <-- x; f t
let l2 f = fun x y -> perform tx <-- x; ty <-- y; f tx ty
let l3 f = fun x y z -> perform tx <-- x; ty <-- y; tz <-- z; f tx ty tz
