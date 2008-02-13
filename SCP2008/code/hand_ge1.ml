(* unstaged Gaussian Elimination, in-place on Arrays

hand-written code that corresponds to GenIV5:
integer matrix, determinant and rank, flat vector representation, full
pivoting, returning the U factor, determinant and rank. *)

(*
 The algorithm follows the pseudo-code at
 http://en.wikipedia.org/wiki/Gaussian_elimination
 With the following changes:
  we use 'r' for the row index (rather than 'i') and 'c' for the
  column index (rather than 'j').
 Indices start with 0 rather than 1 (following the C convention).
 To refer to a matrix element a[i,j], we write a.(i*m+j), where
 m is the number of columns. This corresponds to the C-like arrangement
 of the elements of the matrix.
 When searching for pivot, to avoid extra dereferencing of A[maxr,c], 
 we keep not only the location of the pivot (maxr in pseudo-code),
 but also its value.
 We do full pivoting rather than the partial pivoting. 
 As in the pseudo-code, our algorithm is in-place. To avoid
 clobbering the input matrix, we make a copy of it.
 We also accumulate the sign and the magnitude of the determinant.
 Our algorithm is fraction-free, following the outline in XXX book.
*)

(* Matrix layed out row after row, in a C fashion *)
type 'a container2dfromvector = {arr:('a array); n:int; m:int}
;;

let swap a i j =			(* Swap two elements of a vector *)
    let t = a.(i) in
      begin
        a.(i) <- a.(j);
        a.(j) <- t;
      end;;


(* Swap the row r with the row i in the non-yet examined portion of 
   the matrix, rectangular block (r,c)-(n,m).
   We do not touch the elements to the left of the column c because
   they are all zeros. 
   Because we know the layout of the matrix, we can avoid 2D index
   computations.
*)
let swap_rows a (n,m) (r,c) i =
  let row_r = r*m in			(* Beginning of row r *)
  let row_i = i*m in 			(* Beginning of row i *)
  for k = c to m-1 do
      swap a (row_r + k) (row_i + k)
  done;;

(* In a (n,m) matrix, swap the column c with the column j.
   Because we know the layout of the matrix, we can avoid 2D index
   computations.
*)
let swap_cols a (n,m) c j =
  let end_vector = n * m in
  let rec loop col_c col_j =
    if col_j < end_vector then
      begin
        swap a col_c col_j;
        loop (col_c + m) (col_j + m)
      end
  in loop c j
;;


(* Full pivoting *)
(* Search the non-yet examined portion of the matrix, rectangular *)
(* block (r,c)-(n,m), for the element with the max abs value. *)
(* Remember the value of that element and its location. *)
let find_pivot a (n,m) r c =
  let pivot = ref None in		(* ((i,j), pivot_val) option *)
  begin
  for i = r to n-1 do
    for j = c to m-1 do
      let cur = a.(i*m+j) in
      if not (cur == 0) then
          match !pivot with
          | Some (_,oldpivot) ->
              if abs oldpivot > abs cur then
                 pivot := Some ((i,j), cur)
          | None -> pivot := Some ((i,j),cur)
  done; done;
  !pivot
  end;;

let ge = fun a_orig ->
    let r = ref 0 in			(* current row index, 0-based *)
    let c = ref 0 in			(* current column index, 0-based *)
    let a = Array.copy (a_orig.arr) in	(* to avoid clobbering A, save it *)
    let m = a_orig.m in			(* the number of columns *)
    let n = a_orig.n in			(* the number of rows *)
    let det_sign = ref 1 in		(* Accumulate sign and magnitude *)
    let det_magn = ref 1 in		(*   of the determinant *)
    while !c < m && !r < n do
        (* Look for a pivot *)
        let pivot = find_pivot a (n,m) !r !c in
        (* if we found a pivot, swap the current column with the pivot column,
           and swap the current row with the pivot row. After the swap,
           a[r,c] element of the matrix is the pivot.
           Swapping two rows or two columns changes the sign of the det
        *)
        let piv_val = (match  pivot with
        | Some ((piv_r, piv_c),piv_val) ->
            if piv_c <> !c then
               begin
                 swap_cols a (n,m) !c piv_c;
                 det_sign := - !det_sign (* flip the sign of the det *)
               end;
            if piv_r <> !r then
               begin
                 swap_rows a (n,m) (!r,!c) piv_r;
                 det_sign := - !det_sign (* flip the sign of the det *)
               end;
            Some piv_val
        | None -> None) in
        (* now do the row-reduction over the (r,c)-(n,m) block *)
        (match piv_val with
        | Some a_rc -> begin
            for ii = !r+1 to n-1 do
                let cur = a.(ii*m + !c) in
                if not (cur == 0)  then
                  begin
                    for j = !c+1 to m-1 do
                      (* fraction-free elimination *)
                      a.(ii*m+j) <- (a.(ii*m+j) * a_rc - a.(!r*m+j) * cur) 
                                    / ! det_magn
                    done;
                    a.(ii*m+ !c) <- 0
                  end;
            done;
            det_magn := a_rc;
            r := !r + 1			(* advance the rank only if pivot > 0*)
          end
        | None -> det_sign := 0);
        c := !c + 1
    done;
    (* Final result *)
    ({arr=a; n=n; m=m},			(* The matrix now has the U factor*)
    (if      !det_sign = 0 then   0     (* compute the signed det *)
     else if !det_sign = 1 then   !det_magn 
     else                        (- !det_magn)),
    !r) 				(* Rank *)
    ;;

(* tests *)
let iv0 = {arr=Array.make 1 1; n=1; m=1}
let iv1 = {arr=Array.of_list [ 1; 2; 3; 4; 13; 5; (-1); 3; 0]; n=3; m=3}
let iv2 = {arr=Array.of_list [ 1; 2; 3; 0; 4; 13; 5; 0; (-1); 3; 0; 0]; 
           n=3; m=4}
let iv3 = {arr=Array.of_list [ 1; 2; 3; 4; 13; 5; (-1); 3; 0; 0; 0; 0]; 
           n=4; m=3}
let iv4 = {arr=Array.of_list [ 0; 2; 3; 0; 13; 5; 0; 3; 0]; n=3; m=3}
let iv5 = [iv0; iv1; iv2; iv3; iv4]
;;

assert (ge iv0 =
        ({arr = [|1|]; n = 1; m = 1}, 1, 1)
       );;

assert (ge iv1 =
        ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3)
       );;

assert (ge iv2 =
    ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3)
       );;

assert (ge iv3 =
     ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50; 0; 0; 0|]; n = 4; m = 3}, 50, 3)
       );;

assert (ge iv4 =
     ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2)
       );;


