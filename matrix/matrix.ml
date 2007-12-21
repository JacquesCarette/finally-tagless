(*
 The Matrix compiler

 The doal is to write a compiler from a DSL to OCaml code (to be more
 precise, MetaOCaml code values). The DSL is that of Linear Algebra. 
 The terms have the following (HOAS) syntax:

   <var> |
   lit_int <integer-literal> |
   lit_float <float-literal: IEEE double> |
   <term> $ <term> |  (* sequencing *)
   let_ <term> (fun x -> <term>) | (* creates vars *)
   mat_unit <int-lit> <int-lit> |
   mat_read "file-name" |
   mat_copy <term> |
   mat_addto <term> <term> |   (* a += b *)
   mat_scale <term> <term> |   (* a *= b, b is a scalar *)
   mat_mul   <term> <term> |   (* a * b, return new matrix *)
   mat_get   <term> <term> <term> | (* mat_get a i j *)
   mat_set   <term> <term> <term> <term> | (* mat_set a i j new_val *)
   if_ <term> <term> <term> |
   ltei <term> <term> | (* comparison of ints and floats *)
   ltef <term> <term> |
   addi <term> <term> | (* integer addition *)
   whilepos <term>      | (* while's body should return an int *)

  Comment: should we rather take the SPL language as our goal? I mean
  the language of SPIRAL.

 The language is first-order (at present) and imperative. The above syntax
 is clearly monadic. In the future, we can use my `syntax-subst' trick (aka,
 FunM, requires the use of camlp4) to make the code look natural, but desugar
 it into the above syntax.

 Our goal is to _statically_ guarantee that all operations on matrices
 respect the dimensions and sizes. There is no attempt to index an element
 that is not in a matrix. Matrix addition, multiplication and other operations
 must be properly well-dimensioned.

 If the dimensions are statically known (that is, known at the compilation
 time -- e.g., mat_unit), we can check them as we generate the code.
 Alas, mat_read can read the matrix, along with its dimensions, from a file.
 So, the dimensions do not become known until the run-time. Our compiler
 has to insert some checks. Our goal is to insert the checks right after
 the matrix has been read. The rest of the code must be free from any
 index bounds checks. So, the goal is to eliminate matrix bound/dim checks.

*)

(* Representation of object values in the meta-language, MetaOcaml.
  The object value of type 'b is represented as ('a,'b) code annotated
  with an arbitrary number of attributes. The attributes may be refined
  as the compilation progresses. Therefore, we associate with each code
  value a unique identifier, vid. The attributes for that vid are stored
  in the monadic state.
*)
type vid = int				(* vid of 0 is reserved *)
type ('a,'b) repr = R of ('a,'b) code * vid
;;

(* Our monad: continuation plus state *)
let ret c = fun k s -> k c s
let bind m n = fun k s -> m (fun x s' -> n x k s') s
let retN c = fun k s -> .<let a = .~c in .~(k .<a>. s) >.
let seq s1 s2 = .< begin .~s1 ; .~s2 end >.
let seqL s1 s2 = ret (seq s1 s2)
;;

(* The state is the polymorphic extensible record, collecting attributes
   of various vid *)
type state = {curr_vid : vid;
	      dim1 : (vid * int) list;
	    };;
let init_state = {curr_vid = 1;
		  dim1 = []
		};;
type ('v, 'w) monad = ('v -> state -> 'w) -> state -> 'w
type ('a, 'v,'w) cmonad 
      = (('a,'v) repr -> state -> ('a,'w) code) -> state -> ('a,'w) code
;;

(* I made the run return the accumulated state, so we can print and see it *)
let run m = 
  let sr = ref (0,init_state) in
  let v = m (fun (R (c,v)) s -> let () = sr := (v,s) in c) init_state
  in (!sr,v)
;;


let new_vid s = 
  let v = s.curr_vid in
  (v, {s with curr_vid = succ v});;

let upd_state f k s = k () (f s);;

let st_new lst v vl = 
  let found = try ignore(List.assoc v lst); true with Not_found -> false in
  if found 
  then failwith (Printf.sprintf "st_new: vid %d is already present" v)
  else (v,vl)::lst;;


let with_newvid cd k s = let (v,s') = new_vid s in k (R (cd,v)) s';;
let lit_int x : (('a,int) repr,'w) monad = with_newvid .<x>.;;

let ($) (e1:('v1,'w) monad) (e2:('v2,'w) monad) : ('v2,'w) monad
    = bind e1 (fun _ -> e2);;

let let_ e f = 
  bind e (function R (c,v) ->
    bind (retN c) (fun c' -> f (R (c',v))));;

(* matrix representation and its operations *)
(* A matrix with n rows and m columns is represented as a
   one-dimensional array of n*m elements, arranged in a row-by-row fashion.
   The following primitives are considered to be primitives of the
   _object_ language. In the generated code, they will appear as CSP.
   No error checking is done here.
*)

type mat = M of int * float array;; (* int is the number of cols *)

let o_zero n m = M (m, Array.make (n*m) 0.0);;
let o_clone = function M (m, ar) -> M(m, Array.copy ar);;
let o_unit n m = 
  let len = n * m in
  let ar = Array.make len 0.0 in
  let rec loop i = 
    if i >= len then M(m,ar) 
    else let () = ar.(i) <- 1.0 in loop (i+m+1)
  in loop 0
;;
let o_addto (M (m,ar1)) (M (_,ar2)) = 
  for i=0 to Array.length ar1 do
    ar1.(i) <- ar1.(i) +. ar2.(i)
  done;;
let o_mul (M (m1,ar1)) (M (m2,ar2)) =
  let len2 = Array.length ar2 in
  let len = Array.length ar1 / m1 * m2 in
  let ar = Array.make len 0.0 in
  let rec loop_inner p1 p2 acc =
    if p2 >= len2 then acc else
    loop_inner (succ p1) (p2+m2) (acc +. ar1.(p1) *. ar2.(p2)) in
  let rec loop_row p p1 c =
    if p >= len then M (m2,ar) else
    let () = ar.(p) <- loop_inner p1 c 0.0 in
    let c' = succ c in
    if c' >= m2 
    then loop_row (succ p) (p1+m1) 0
    else loop_row (succ p) p1 c'
  in loop_row 0 0 0
;;

(* Compilation of matrix expressions *)

let mat_unit n m = 
  bind (with_newvid .<o_unit n m>.) (function R (_,v) as r ->
    upd_state (fun s ->
      {s with dim1 = st_new s.dim1 v n}) $ ret r)
;;

(* We assume that cloning of a matrix preserves all the static properties
   we care about: dimensions. Thus, we preserve vid.
*)
let mat_clone e = bind e (function R (c,v) ->
  ret (R (.<o_clone .~c>.,v)));;

let test1 () =
  let_ (mat_unit 5 5) (fun m1 ->
  let_ (mat_clone (ret m1)) (fun m2 ->
    ret m2
));;

let test1r = run (test1 ());;


(*



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


let test () = perform
    m1 <-- read ();
    m2 <-- read ();
    (loop,mat) <-- mult m1 m2;
    code <-- seqL loop mat;
    ret code
*)
