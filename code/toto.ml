type ('v,'s,'w) monad = 's -> ('s -> 'v -> 'w) -> 'w

let retS a = fun s k -> k s a
let retN a = fun s k -> .<let t = .~a in .~(k s .<t>.)>.
let bind a f = fun s k -> a s (fun s' b -> f b s' k)

let codegen v cf = fun s k -> cf (k s v)

let gen = 
    let dogen = mdo {
         x <-- retS .< 5 >. ;
         y <-- codegen () (fun z -> .<.~x + .~z>. ) ;
         t <-- retS .< 6 >. ;
         retS t}
    in .< .~(dogen () (fun s k -> k)) >.
