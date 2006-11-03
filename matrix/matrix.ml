type var = V of int
type var_stream = VarStream of int
let init_var_stream = VarStream 0
let next_var (VarStream i) = (V i, VarStream (i + 1))

type term = Known of int | Unknown of var
type subst = Subst of var_stream * (var -> term)
let empty_subst = Subst (init_var_stream, fun v -> Unknown v)
let extend_subst (v: var) (t: term) (Subst (vs, f))
    = Subst (vs, fun v' -> let t' = f v' in if t' == Unknown v then t else t')
let apply_subst (Subst (vs, f)) = function Unknown v -> f v | t -> t
let fresh () = fun k (Subst(vs, f) ) ->
    let (v, vs') = next_var vs in 
    let s' = Subst(vs',f) in
    k v s'

type 'e dimen_repr = Dimen of ('e, int) code * term
let dimen (Dimen (c,a)) = c

type ('e, 'a, 'b) matrix_repr
    = Matrix of ('e, ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t) code * 
        'e dimen_repr * 'e dimen_repr
(* both of these should verify that row/col are within bounds! *)
let get (Matrix (arr, Dimen (rows,_), Dimen (cols,_))) row col
    = .< (.~arr).{.~row, .~col} >.
let set (Matrix (arr, Dimen (rows,_), Dimen (cols,_))) row col y =
    .< (.~arr).{.~row, .~col} <- .~y >.
let rows (Matrix (_, r, c)) = r
let cols (Matrix (_, r, c)) = c
let mat  (Matrix (a, _, _)) = a

type (+'v, 'w) monad = ('v -> subst -> 'w) -> subst -> 'w

let ret c = fun k s -> k c s
let bind m n = fun k s -> m (fun x s' -> n x k s') s
let retN c = fun k s -> .<let a = .~c in .~(k .<a>. s) >.
let seq s1 s2 = .< begin .~s1 ; .~s2 end >.
let seqL s1 s2 = ret (seq s1 s2)

let liftmat m = .< .~(mat m) >.
let liftmatL m = ret (liftmat m)

let check (Dimen (c1,a1)) (Dimen (c2,a2)) = fun k s ->
    match apply_subst s a1, apply_subst s a2 with
    | Unknown v1, Unknown v2 when v1 == v2 -> k () s
    | Unknown v, t | t, Unknown v ->
            .<( assert (.~c1 == .~c2);
                .~(k () (extend_subst v t s)) )>.
    | Known k1, Known k2 -> assert (k1 == k2); k () s

open Bigarray
let arr_make rows cols = .< Array2.create int c_layout .~rows .~cols >.

let make rows cols = perform
    a <-- retN (arr_make (dimen rows) (dimen cols));
    ret (Matrix (a,rows,cols))

let iter_array (Matrix (a, Dimen(rows,_), Dimen(cols,_))) f =
    .< for i = 0 to .~rows - 1 do
           for j = 0 to .~cols - 1 do
               (.~a).{ i, j} <- .~(f .<i>. .<j>. );
           done
       done >.

(* this should be broken down further *)
let sum_up lfrom lend f = fun i j ->
    .< let sum = ref 0 in
       for l = .~lfrom to .~lend - 1 do
        sum := !sum + .~(f .<l>. i j)
       done; !sum >.

let read_dim () = fun k s -> .< let dim = read_int () in .~(k .<dim>. s) >.

let read () = perform
    rowv <-- fresh ();
    colv <-- fresh ();
    rows <-- read_dim ();
    cols <-- read_dim ();
    matrix <-- make (Dimen (rows, Unknown rowv)) (Dimen (cols, Unknown colv));
    ret matrix

let mult m1 m2 = perform
    _ <-- check (cols m1) (rows m2);
    m <-- make (rows m1) (cols m2);
    c <-- ret (iter_array m 
        (sum_up .<0>. (dimen (cols m1)) 
            (fun l i j -> .< .~(get m1 i l) * .~(get m2 l j) >. )));
    mat <-- liftmatL m;
    ret (c, mat)

let eval m = m () (fun c s -> c) empty_subst

let test () = perform
    m1 <-- read ();
    m2 <-- read ();
    (loop,mat) <-- mult m1 m2;
    code <-- seqL loop mat;
    ret code
