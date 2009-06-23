type contour = int list
type 'a repr = contour -> ('a -> unit) -> unit
type ('a,'b) darr = (('a -> unit) -> unit) -> 'b repr

let printd (d : contour) : unit =
    List.iter (fun l -> print_int l; print_char ' ') d;
    print_newline ()

let memo' : 'b repr -> (contour -> 'b -> unit) * 'b repr =
    let add (ys,ks) y =
        if not (Hashtbl.mem ys y) then
        (Hashtbl.add ys y (); List.iter (fun k -> k y) !ks)
    in
    fun f ->
    let find = let table = Hashtbl.create 17 in
               fun x ->
               try Hashtbl.find table x with Not_found ->
               let entry = (Hashtbl.create 17, ref []) in
               Hashtbl.add table x entry;
               f x (add entry);
               entry
    in
    (fun x -> add (find x)),
    (fun x k -> print_string "contour "; printd x;
        match find x with ys,ks ->
        ks := k :: !ks;
        Hashtbl.iter (fun y () -> k y) ys)

let memo (f : 'b repr) : 'b repr = snd (memo' f)

let var () : (contour -> 'b -> unit) * 'b repr = memo' (fun d k -> ())

let gensym : unit -> int =
    let fresh = ref 0 in
    fun () -> incr fresh; !fresh

let unit () : unit repr =
    fun d k -> k ()

let int n : int repr =
    fun d k -> k n

let plus e1 e2 =
    memo (fun d k -> e1 d (fun n1 -> e2 d (fun n2 -> k (n1 + n2))))

let app (f : ('a,'b) darr repr) (e : 'a repr) : 'b repr =
    let l = gensym () in
    memo (fun d k -> f d (fun g -> g (e d) (l::d) k))

let lam (f : ('a repr -> 'b repr)) : ('a,'b) darr repr =
    let (add_arg, arg) = var () in
    let body = f arg in
    let g arg d k = arg (add_arg d); body d k in
    fun d k -> k g

let fix (f : 'b repr -> 'b repr) : 'b repr =
    let (add_self, self) = var () in
    let body = f self in
    fun d k -> body d (add_self d); body d k

let pirepr label e = e [] (fun n -> print_string label; print_char ' '; print_int n; print_newline ()); e
