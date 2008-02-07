(* unstaged Gaussian Elimination, in-place on Arrays

hand-written code that corresponds to GenIV5:
integer matrix, determinant and rank, flat vector representation, full
pivoting, returning the U factor, determinant and rank. *)

(* Matrix layed out row after row, in a C fashion *)
type 'a container2dfromvector = {arr:('a array); n:int; m:int}

let swap b i j =
    let t = b.(i) in
      begin
        b.(i) <- b.(j);
        b.(j) <- t;
      end;;

let ge = fun a ->
    let r = ref 0 in
    let c = ref 0 in
    let b = Array.copy (a.arr) in
    let m = a.m in
    let n = a.n in
    let det_sign = ref 1 in
    let det_magn = ref 1 in
    while !c < m && !r < n do
        (* find pivot - remember location and value *)
        let pivot = ref None in
        for j = !r to n-1 do
          for i = !c to m-1 do
            let cur = b.(j*m+j) in
            if not ( cur == 0) then
                match !pivot with
                | Some oldpivot ->
                    if abs (snd oldpivot) > abs cur then
                        pivot := Some ((j,i), cur);
                | None -> pivot := Some ((j,i),cur);
        done;
        (* if we found a pivot, switch rows, pass value on *)
        let piv_val = 
        (match !pivot with
        | Some pivot ->
            if pivot <> !r then
              begin
                for j = !c to m-1 do
                    swap b (pivot*m+j) (!r*m+j);
                    det_sign := - !det_sign;
                done;
                Some (snd pivot)
              end;
        | None -> None) in
        (* now do the row-reduction *)
        (match piv_val with
        | Some value -> begin
            for ii = !r+1 to n-1 do
                let cur = b.(ii*m+ !c) in
                if not (cur == 0)  then
                  begin
                    let t = cur / b.(!r*m+ !c) in
                    for j = !c+1 to m-1 do
                        b.(ii*m+j) <- ((b.(ii*m+j)*value) - (t * b.(!r*m+j)))
                                       / ! det_magn;
                    done;
                    b.(ii*m+ !c) <- 0;
                  end;
            done;
            det_magn := value;
            r := !r + 1;
          end
        | None -> det_sign := 0);
        c := !c + 1
    done ;
    (b, 
    (if     !det_sign = 0 then   0 
    else if !det_sign = 1 then   !det_magn 
    else                       (- !det_magn)),
    !r) 
    ;;
