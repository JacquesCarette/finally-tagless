type var = V of int;;
type var_stream = VarStream of int;;
let init_var_stream = VarStream 0;;
let next_var (VarStream i) = (V i, VarStream (i + 1));;

type term = Known of int | Unknown of var;;
type subst = Subst of var_stream * (var -> term);;
let empty_subst = Subst (init_var_stream, fun v -> Unknown v);;
let extend_subst (v: var) (t: term) (Subst (vs, f))
    = Subst (vs, fun v' -> let t' = f v' in if t' == Unknown v then t else t');;
let apply_subst (Subst (vs, f)) = function Unknown v -> f v | t -> t;;
let fresh (Subst (vs, f)) = let (v, vs') = next_var vs in (v, Subst (vs', f));;

type 'e dimen_repr = Dimen of ('e, int) code * term;;
let dimen (Dimen (c,a)) = c;;

type ('e, 'a) matrix_repr
    = Matrix of ('e, 'a array) code * 'e dimen_repr * 'e dimen_repr;;
let get (Matrix (arr, Dimen (rows,_), Dimen (cols,_))) row col
    = .< Array.get .~arr (.~row + .~rows * .~col) >.;;
let set (Matrix (arr, Dimen (rows,_), Dimen (cols,_))) row col
    = .< Array.set .~arr (.~row + .~rows * .~col) >.;;
let rows (Matrix (_, r, c)) = r;;
let cols (Matrix (_, r, c)) = c;;

let check (Dimen (c1,a1)) (Dimen (c2,a2)) k s
    = match apply_subst s a1, apply_subst s a2 with
    | Unknown v1, Unknown v2 when v1 == v2 -> k () s
    | Unknown v, t | t, Unknown v ->
            .<( assert (.~c1 == .~c2);
                .~(k () (extend_subst v t s)) )>.
    | Known k1, Known k2 -> assert (k1 == k2); k () s;;

let make rows cols k s
    = .< let arr = Array.make .~(dimen rows) .~(dimen cols)
         in .~(k (Matrix (.<arr>., rows, cols)) s) >.;;

let read k s
    = let (rowv, s) = fresh s in
      let (colv, s) = fresh s in
      .< let rows = read_int () in
         let cols = read_int () in
         .~( make (Dimen (.<rows>., Unknown rowv))
                  (Dimen (.<cols>., Unknown colv))
                  (fun matrix s -> .< (
                      for col = 0 to cols - 1 do
                          for row = 0 to rows - 1 do
                              .~(set matrix .<row>. .<col>.) (read_int ())
                          done
                      done;
                      .~(k matrix s) ) >.)
                  s ) >.;;

let mult m1 m2 k
    = check (cols m1) (rows m2) (fun () s ->
        make (rows m1) (cols m2) (fun m s -> .< (
            for i = 0 to .~(dimen (rows m1)) - 1 do
                for k = 0 to .~(dimen (cols m2)) - 1 do
                    let rec loop j sum
                        = if j < 0 then .~(set m .<i>. .<k>.) sum
                          else loop (j - 1)
                                    (sum + .~(get m1 .<i>. .<j>.)
                                         * .~(get m2 .<j>. .<k>.))
                    in loop (.~(dimen (cols m1)) - 1) 0
                done
            done;
            .~(k m s) ) >.) s);;

let ret c k = k c;;
let bind m n k = m (fun x -> n x k);;
let eval m = m () (fun c s -> c) empty_subst;;

let test () = perform
    m1 <-- read;
    m2 <-- read;
    m <-- mult m1 m2;
    ret (get m .<0>. .<0>.);;

(* read (fun m1 ->
    read (fun m2 ->
        mult m1 m2 (fun m s ->
            get m .<0>. .<0>.)))
empty_subst;;
*)

