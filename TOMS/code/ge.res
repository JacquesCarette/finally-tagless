        Objective Caml version 3.09.1

# # #   val instantiate :
  (('a, 'b) code -> 'c list -> ('d -> 'e -> 'e) -> ('a, 'f) code) ->
  ('a, 'b -> 'f) code = <fun>
#   val resFA1 : ('a, Ge.GenFA1.Input.inp -> Ge.GenFA1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_16 =
     begin
      for j_13 = t_8 to (t_7 - 1) do
       let t_14 = (t_5.(j_13)).(t_9) in
       if (t_14 <> 0.) then
        (match (! t_10) with
         | Some (i_15) ->
            if ((abs_float (snd i_15)) < (abs_float t_14)) then
             (t_10 := (Some (j_13, t_14)))
            else ()
         | None -> (t_10 := (Some (j_13, t_14))))
       else ()
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((fst i_11) <> t_8) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst i_11);
           t_5.(fst i_11) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_16 with
     | Some (i_17) ->
        begin
         for j_18 = (t_8 + 1) to (t_7 - 1) do
          let t_19 = (t_5.(j_18)).(t_9) in
          if (t_19 <> 0.) then begin
           for j_20 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_18)).(j_20) <-
             ((t_5.(j_18)).(j_20) -. ((t_19 /. i_17) *. (t_5.(t_8)).(j_20)))
           done;
           (t_5.(j_18)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resFA2 : ('a, Ge.GenFA2.Input.inp -> Ge.GenFA2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0.) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs_float (snd i_17)) < (abs_float t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0.) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((t_5.(j_20)).(j_22) -. ((t_21 /. i_19) *. (t_5.(t_10)).(j_22)))
           done;
           (t_5.(j_20)).(t_11) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_19))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)))>.
# val resFA3 : ('a, Ge.GenFA3.Input.inp -> Ge.GenFA3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_16 =
     begin
      for j_13 = t_8 to (t_7 - 1) do
       let t_14 = (t_5.(j_13)).(t_9) in
       if (t_14 <> 0.) then
        (match (! t_10) with
         | Some (i_15) ->
            if ((abs_float (snd i_15)) < (abs_float t_14)) then
             (t_10 := (Some (j_13, t_14)))
            else ()
         | None -> (t_10 := (Some (j_13, t_14))))
       else ()
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((fst i_11) <> t_8) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst i_11);
           t_5.(fst i_11) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_16 with
     | Some (i_17) ->
        begin
         for j_18 = (t_8 + 1) to (t_7 - 1) do
          let t_19 = (t_5.(j_18)).(t_9) in
          if (t_19 <> 0.) then begin
           for j_20 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_18)).(j_20) <-
             ((t_5.(j_18)).(j_20) -. ((t_19 /. i_17) *. (t_5.(t_8)).(j_20)))
           done;
           (t_5.(j_18)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
# val resFA4 : ('a, Ge.GenFA4.Input.inp -> Ge.GenFA4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0.) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs_float (snd i_17)) < (abs_float t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0.) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((t_5.(j_20)).(j_22) -. ((t_21 /. i_19) *. (t_5.(t_10)).(j_22)))
           done;
           (t_5.(j_20)).(t_11) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_19))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2))>.
# val resFV1 : ('a, Ge.GenFV1.Input.inp -> Ge.GenFV1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (! t_2) in
    let t_8 = (! t_3) in
    let t_9 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_7 to (t_6 - 1) do
       let t_18 = (t_4.arr).((j_17 * t_4.m) + t_8) in
       if (t_18 <> 0.) then
        (match (! t_9) with
         | Some (i_19) ->
            if ((abs_float (snd i_19)) < (abs_float t_18)) then
             (t_9 := (Some (j_17, t_18)))
            else ()
         | None -> (t_9 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_9) with
       | Some (i_10) ->
          if ((fst i_10) <> t_7) then
           let a_11 = t_4.arr
           and m_12 = t_4.m in
           let i1_13 = (t_7 * m_12)
           and i2_14 = ((fst i_10) * m_12) in
           for i_15 = 0 to (m_12 - 1) do
            let t_16 = a_11.(i1_13 + i_15) in
            a_11.(i1_13 + i_15) <- a_11.(i2_14 + i_15);
            a_11.(i2_14 + i_15) <- t_16
           done
          else ();
          (Some (snd i_10))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_7 + 1) to (t_6 - 1) do
          let t_23 = (t_4.arr).((j_22 * t_4.m) + t_8) in
          if (t_23 <> 0.) then begin
           for j_24 = (t_8 + 1) to (t_5 - 1) do
            (t_4.arr).((j_22 * t_4.m) + j_24) <-
             ((t_4.arr).((j_22 * t_4.m) + j_24) -.
               ((t_23 /. i_21) *. (t_4.arr).((t_7 * t_4.m) + j_24)))
           done;
           (t_4.arr).((j_22 * t_4.m) + t_8) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
# val resFV2 : ('a, Ge.GenFV2.Input.inp -> Ge.GenFV2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0.) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs_float (snd i_21)) < (abs_float t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0.) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((t_4.arr).((j_24 * t_4.m) + j_26) -.
               ((t_25 /. i_23) *. (t_4.arr).((t_9 * t_4.m) + j_26)))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_23))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)))>.
# val resFV3 : ('a, Ge.GenFV3.Input.inp -> Ge.GenFV3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (! t_2) in
    let t_8 = (! t_3) in
    let t_9 = (ref (None)) in
    let t_20 =
     begin
      for j_17 = t_7 to (t_6 - 1) do
       let t_18 = (t_4.arr).((j_17 * t_4.m) + t_8) in
       if (t_18 <> 0.) then
        (match (! t_9) with
         | Some (i_19) ->
            if ((abs_float (snd i_19)) < (abs_float t_18)) then
             (t_9 := (Some (j_17, t_18)))
            else ()
         | None -> (t_9 := (Some (j_17, t_18))))
       else ()
      done;
      (match (! t_9) with
       | Some (i_10) ->
          if ((fst i_10) <> t_7) then
           let a_11 = t_4.arr
           and m_12 = t_4.m in
           let i1_13 = (t_7 * m_12)
           and i2_14 = ((fst i_10) * m_12) in
           for i_15 = 0 to (m_12 - 1) do
            let t_16 = a_11.(i1_13 + i_15) in
            a_11.(i1_13 + i_15) <- a_11.(i2_14 + i_15);
            a_11.(i2_14 + i_15) <- t_16
           done
          else ();
          (Some (snd i_10))
       | None -> (None))
     end in
    (match t_20 with
     | Some (i_21) ->
        begin
         for j_22 = (t_7 + 1) to (t_6 - 1) do
          let t_23 = (t_4.arr).((j_22 * t_4.m) + t_8) in
          if (t_23 <> 0.) then begin
           for j_24 = (t_8 + 1) to (t_5 - 1) do
            (t_4.arr).((j_22 * t_4.m) + j_24) <-
             ((t_4.arr).((j_22 * t_4.m) + j_24) -.
               ((t_23 /. i_21) *. (t_4.arr).((t_7 * t_4.m) + j_24)))
           done;
           (t_4.arr).((j_22 * t_4.m) + t_8) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
# val resFV4 : ('a, Ge.GenFV4.Input.inp -> Ge.GenFV4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0.) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs_float (snd i_21)) < (abs_float t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0.) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((t_4.arr).((j_24 * t_4.m) + j_26) -.
               ((t_25 /. i_23) *. (t_4.arr).((t_9 * t_4.m) + j_26)))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_23))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
# val resFV5 : ('a, Ge.GenFV5.Input.inp -> Ge.GenFV5.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_30 =
     begin
      for j_26 = t_9 to (t_6 - 1) do
       for j_27 = t_10 to (t_5 - 1) do
        let t_28 = (t_4.arr).((j_26 * t_4.m) + j_27) in
        if (t_28 <> 0.) then
         (match (! t_11) with
          | Some (i_29) ->
             if ((abs_float (snd i_29)) < (abs_float t_28)) then
              (t_11 := (Some ((j_26, j_27), t_28)))
             else ()
          | None -> (t_11 := (Some ((j_26, j_27), t_28))))
        else ()
       done
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((snd (fst i_12)) <> t_10) then begin
           let a_19 = t_4.arr
           and nm_20 = (t_4.n * t_4.m)
           and m_21 = t_4.m in
           let rec loop_22 =
            fun i1_23 ->
             fun i2_24 ->
              if (i2_24 < nm_20) then
               let t_25 = a_19.(i1_23) in
               a_19.(i1_23) <- a_19.(i2_24);
               a_19.(i2_24) <- t_25;
               (loop_22 (i1_23 + m_21) (i2_24 + m_21))
              else () in
           (loop_22 t_10 (snd (fst i_12)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_12)) <> t_10) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((snd (fst i_12)) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_30 with
     | Some (i_31) ->
        begin
         for j_32 = (t_9 + 1) to (t_6 - 1) do
          let t_33 = (t_4.arr).((j_32 * t_4.m) + t_10) in
          if (t_33 <> 0.) then begin
           for j_34 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_32 * t_4.m) + j_34) <-
             ((t_4.arr).((j_32 * t_4.m) + j_34) -.
               ((t_33 /. i_31) *. (t_4.arr).((t_9 * t_4.m) + j_34)))
           done;
           (t_4.arr).((j_32 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_31))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
# val resIA1 : ('a, Ge.GenIA1.Input.inp -> Ge.GenIA1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resIA2 : ('a, Ge.GenIA2.Input.inp -> Ge.GenIA2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0
    else if ((! t_9) = 1) then (! t_8)
    else (~- (! t_8)))>.
# val resIA3 : ('a, Ge.GenIA3.Input.inp -> Ge.GenIA3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
# val resIA4 : ('a, Ge.GenIA4.Input.inp -> Ge.GenIA4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_18 =
     begin
      for j_15 = t_10 to (t_7 - 1) do
       let t_16 = (t_5.(j_15)).(t_11) in
       if (t_16 <> 0) then
        (match (! t_12) with
         | Some (i_17) ->
            if ((abs (snd i_17)) > (abs t_16)) then
             (t_12 := (Some (j_15, t_16)))
            else ()
         | None -> (t_12 := (Some (j_15, t_16))))
       else ()
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          let t_21 = (t_5.(j_20)).(t_11) in
          if (t_21 <> 0) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_22) <-
             ((((t_5.(j_20)).(j_22) * i_19) - ((t_5.(t_10)).(j_22) * t_21)) /
               (! t_8))
           done;
           (t_5.(j_20)).(t_11) <- 0
          end else ()
         done;
         (t_8 := i_19)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0
    else if ((! t_9) = 1) then (! t_8)
    else (~- (! t_8)), (! t_2))>.
# val resIV1 : ('a, Ge.GenIV1.Input.inp -> Ge.GenIV1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
# val resIV2 : ('a, Ge.GenIV2.Input.inp -> Ge.GenIV2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)))>.
# val resIV3 : ('a, Ge.GenIV3.Input.inp -> Ge.GenIV3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
# val resIV4 : ('a, Ge.GenIV4.Input.inp -> Ge.GenIV4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_22 =
     begin
      for j_19 = t_9 to (t_6 - 1) do
       let t_20 = (t_4.arr).((j_19 * t_4.m) + t_10) in
       if (t_20 <> 0) then
        (match (! t_11) with
         | Some (i_21) ->
            if ((abs (snd i_21)) > (abs t_20)) then
             (t_11 := (Some (j_19, t_20)))
            else ()
         | None -> (t_11 := (Some (j_19, t_20))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((fst i_12) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_22 with
     | Some (i_23) ->
        begin
         for j_24 = (t_9 + 1) to (t_6 - 1) do
          let t_25 = (t_4.arr).((j_24 * t_4.m) + t_10) in
          if (t_25 <> 0) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_24 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_24 * t_4.m) + j_26) * i_23) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) * t_25)) / (! t_7))
           done;
           (t_4.arr).((j_24 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_23)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
# val resIV5 : ('a, Ge.GenIV5.Input.inp -> Ge.GenIV5.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_3) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_30 =
     begin
      for j_26 = t_9 to (t_6 - 1) do
       for j_27 = t_10 to (t_5 - 1) do
        let t_28 = (t_4.arr).((j_26 * t_4.m) + j_27) in
        if (t_28 <> 0) then
         (match (! t_11) with
          | Some (i_29) ->
             if ((abs (snd i_29)) > (abs t_28)) then
              (t_11 := (Some ((j_26, j_27), t_28)))
             else ()
          | None -> (t_11 := (Some ((j_26, j_27), t_28))))
        else ()
       done
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((snd (fst i_12)) <> t_10) then begin
           let a_19 = t_4.arr
           and nm_20 = (t_4.n * t_4.m)
           and m_21 = t_4.m in
           let rec loop_22 =
            fun i1_23 ->
             fun i2_24 ->
              if (i2_24 < nm_20) then
               let t_25 = a_19.(i1_23) in
               a_19.(i1_23) <- a_19.(i2_24);
               a_19.(i2_24) <- t_25;
               (loop_22 (i1_23 + m_21) (i2_24 + m_21))
              else () in
           (loop_22 t_10 (snd (fst i_12)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_12)) <> t_10) then begin
           let a_13 = t_4.arr
           and m_14 = t_4.m in
           let i1_15 = (t_9 * m_14)
           and i2_16 = ((snd (fst i_12)) * m_14) in
           for i_17 = 0 to (m_14 - 1) do
            let t_18 = a_13.(i1_15 + i_17) in
            a_13.(i1_15 + i_17) <- a_13.(i2_16 + i_17);
            a_13.(i2_16 + i_17) <- t_18
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_30 with
     | Some (i_31) ->
        begin
         for j_32 = (t_9 + 1) to (t_6 - 1) do
          let t_33 = (t_4.arr).((j_32 * t_4.m) + t_10) in
          if (t_33 <> 0) then begin
           for j_34 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_32 * t_4.m) + j_34) <-
             ((((t_4.arr).((j_32 * t_4.m) + j_34) * i_31) -
                ((t_4.arr).((t_9 * t_4.m) + j_34) * t_33)) / (! t_7))
           done;
           (t_4.arr).((j_32 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_31)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
# val resFA11 : ('a, Ge.GenFA11.Input.inp -> Ge.GenFA11.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_19 =
     begin
      for j_15 = t_8 to (t_7 - 1) do
       for j_16 = t_9 to (t_6 - 1) do
        let t_17 = (t_5.(j_15)).(j_16) in
        if (t_17 <> 0.) then
         (match (! t_10) with
          | Some (i_18) ->
             if ((abs_float (snd i_18)) < (abs_float t_17)) then
              (t_10 := (Some ((j_15, j_16), t_17)))
             else ()
          | None -> (t_10 := (Some ((j_15, j_16), t_17))))
        else ()
       done
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((snd (fst i_11)) <> t_9) then
           for r_13 = 0 to ((Array.length t_5) - 1) do
            let t_14 = (t_5.(r_13)).(t_9) in
            (t_5.(r_13)).(t_9) <- (t_5.(r_13)).(snd (fst i_11));
            (t_5.(r_13)).(snd (fst i_11)) <- t_14
           done
          else ();
          if ((fst (fst i_11)) <> t_9) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(snd (fst i_11));
           t_5.(snd (fst i_11)) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_8 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_9) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_8)).(j_23)))
           done;
           (t_5.(j_21)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resFA12 : ('a, Ge.GenFA12.Input.inp -> Ge.GenFA12.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_21 =
     begin
      for j_17 = t_10 to (t_7 - 1) do
       for j_18 = t_11 to (t_6 - 1) do
        let t_19 = (t_5.(j_17)).(j_18) in
        if (t_19 <> 0.) then
         (match (! t_12) with
          | Some (i_20) ->
             if ((abs_float (snd i_20)) < (abs_float t_19)) then
              (t_12 := (Some ((j_17, j_18), t_19)))
             else ()
          | None -> (t_12 := (Some ((j_17, j_18), t_19))))
        else ()
       done
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((snd (fst i_13)) <> t_11) then begin
           for r_15 = 0 to ((Array.length t_5) - 1) do
            let t_16 = (t_5.(r_15)).(t_11) in
            (t_5.(r_15)).(t_11) <- (t_5.(r_15)).(snd (fst i_13));
            (t_5.(r_15)).(snd (fst i_13)) <- t_16
           done;
           (t_9 := (~- (! t_9)))
          end else ();
          if ((fst (fst i_13)) <> t_11) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(snd (fst i_13));
           t_5.(snd (fst i_13)) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_10 + 1) to (t_7 - 1) do
          let t_24 = (t_5.(j_23)).(t_11) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             ((t_5.(j_23)).(j_25) -. ((t_24 /. i_22) *. (t_5.(t_10)).(j_25)))
           done;
           (t_5.(j_23)).(t_11) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)))>.
# val resFA13 : ('a, Ge.GenFA13.Input.inp -> Ge.GenFA13.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (! t_2) in
    let t_9 = (! t_3) in
    let t_10 = (ref (None)) in
    let t_19 =
     begin
      for j_15 = t_8 to (t_7 - 1) do
       for j_16 = t_9 to (t_6 - 1) do
        let t_17 = (t_5.(j_15)).(j_16) in
        if (t_17 <> 0.) then
         (match (! t_10) with
          | Some (i_18) ->
             if ((abs_float (snd i_18)) < (abs_float t_17)) then
              (t_10 := (Some ((j_15, j_16), t_17)))
             else ()
          | None -> (t_10 := (Some ((j_15, j_16), t_17))))
        else ()
       done
      done;
      (match (! t_10) with
       | Some (i_11) ->
          if ((snd (fst i_11)) <> t_9) then
           for r_13 = 0 to ((Array.length t_5) - 1) do
            let t_14 = (t_5.(r_13)).(t_9) in
            (t_5.(r_13)).(t_9) <- (t_5.(r_13)).(snd (fst i_11));
            (t_5.(r_13)).(snd (fst i_11)) <- t_14
           done
          else ();
          if ((fst (fst i_11)) <> t_9) then
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(snd (fst i_11));
           t_5.(snd (fst i_11)) <- t_12
          else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_8 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_9) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_8)).(j_23)))
           done;
           (t_5.(j_21)).(t_9) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
# val resFA14 : ('a, Ge.GenFA14.Input.inp -> Ge.GenFA14.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_21 =
     begin
      for j_17 = t_10 to (t_7 - 1) do
       for j_18 = t_11 to (t_6 - 1) do
        let t_19 = (t_5.(j_17)).(j_18) in
        if (t_19 <> 0.) then
         (match (! t_12) with
          | Some (i_20) ->
             if ((abs_float (snd i_20)) < (abs_float t_19)) then
              (t_12 := (Some ((j_17, j_18), t_19)))
             else ()
          | None -> (t_12 := (Some ((j_17, j_18), t_19))))
        else ()
       done
      done;
      (match (! t_12) with
       | Some (i_13) ->
          if ((snd (fst i_13)) <> t_11) then begin
           for r_15 = 0 to ((Array.length t_5) - 1) do
            let t_16 = (t_5.(r_15)).(t_11) in
            (t_5.(r_15)).(t_11) <- (t_5.(r_15)).(snd (fst i_13));
            (t_5.(r_15)).(snd (fst i_13)) <- t_16
           done;
           (t_9 := (~- (! t_9)))
          end else ();
          if ((fst (fst i_13)) <> t_11) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(snd (fst i_13));
           t_5.(snd (fst i_13)) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_10 + 1) to (t_7 - 1) do
          let t_24 = (t_5.(j_23)).(t_11) in
          if (t_24 <> 0.) then begin
           for j_25 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_25) <-
             ((t_5.(j_23)).(j_25) -. ((t_24 /. i_22) *. (t_5.(t_10)).(j_25)))
           done;
           (t_5.(j_23)).(t_11) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_22))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2))>.
# val resFA24 : ('a, Ge.GenFA24.Input.inp -> Ge.GenFA24.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1) in
   let t_10 = (ref ([])) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_19 =
     begin
      for j_16 = t_11 to (t_7 - 1) do
       let t_17 = (t_5.(j_16)).(t_12) in
       if (t_17 <> 0.) then
        (match (! t_13) with
         | Some (i_18) ->
            if ((abs_float (snd i_18)) < (abs_float t_17)) then
             (t_13 := (Some (j_16, t_17)))
            else ()
         | None -> (t_13 := (Some (j_16, t_17))))
       else ()
      done;
      (match (! t_13) with
       | Some (i_14) ->
          if ((fst i_14) <> t_11) then begin
           begin
            let t_15 = t_5.(t_11) in
            t_5.(t_11) <- t_5.(fst i_14);
            t_5.(fst i_14) <- t_15;
            (t_9 := (~- (! t_9)))
           end;
           (t_10 := ((RowSwap ((fst i_14), t_11)) :: (! t_10)))
          end else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_11 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_12) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_12 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_11)).(j_23)))
           done;
           (t_5.(j_21)).(t_12) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then 0.
    else if ((! t_9) = 1) then (! t_8)
    else (~-. (! t_8)), (! t_2), (! t_10))>.
# val resRA1 : ('a, Ge.GenRA1.Input.inp -> Ge.GenRA1.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_19 =
     begin
      let t_15 = (t_5.(t_10)).(t_11) in
      if (t_15 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_12 := (Some (t_10, t_15)))
      else
       let rec loop_16 =
        fun j_17 ->
         if (j_17 < t_7) then
          let t_18 = (t_5.(j_17)).(t_11) in
          if (t_18 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_16 (j_17 + 1))
          else (t_12 := (Some (j_17, t_18)))
         else () in
       (loop_16 (t_10 + 1));
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_10 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_11) in
          if (t_22 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_23 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_23)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_22 i_20) (t_5.(t_10)).(j_23)))
           done;
           (t_5.(j_21)).(t_11) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resRA2 : ('a, Ge.GenRA2.Input.inp -> Ge.GenRA2.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_19 =
     begin
      let t_15 = (t_5.(t_10)).(t_11) in
      if (t_15 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_12 := (Some (t_10, t_15)))
      else
       let rec loop_16 =
        fun j_17 ->
         if (j_17 < t_7) then
          let t_18 = (t_5.(j_17)).(t_11) in
          if (t_18 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_16 (j_17 + 1))
          else (t_12 := (Some (j_17, t_18)))
         else () in
       (loop_16 (t_10 + 1));
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_10 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_11) in
          if (t_22 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_23 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_23)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_22 i_20) (t_5.(t_10)).(j_23)))
           done;
           (t_5.(j_21)).(t_11) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_9) = 1) then (! t_8)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_8)))>.
# val resRA3 : ('a, Ge.GenRA3.Input.inp -> Ge.GenRA3.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_19 =
     begin
      let t_15 = (t_5.(t_10)).(t_11) in
      if (t_15 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_12 := (Some (t_10, t_15)))
      else
       let rec loop_16 =
        fun j_17 ->
         if (j_17 < t_7) then
          let t_18 = (t_5.(j_17)).(t_11) in
          if (t_18 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_16 (j_17 + 1))
          else (t_12 := (Some (j_17, t_18)))
         else () in
       (loop_16 (t_10 + 1));
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_10 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_11) in
          if (t_22 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_23 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_23)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_22 i_20) (t_5.(t_10)).(j_23)))
           done;
           (t_5.(j_21)).(t_11) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
# val resRA4 : ('a, Ge.GenRA4.Input.inp -> Ge.GenRA4.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_9 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (! t_2) in
    let t_11 = (! t_3) in
    let t_12 = (ref (None)) in
    let t_19 =
     begin
      let t_15 = (t_5.(t_10)).(t_11) in
      if (t_15 <> (* cross-stage persistent value (as id: zero) *)) then
       (t_12 := (Some (t_10, t_15)))
      else
       let rec loop_16 =
        fun j_17 ->
         if (j_17 < t_7) then
          let t_18 = (t_5.(j_17)).(t_11) in
          if (t_18 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_16 (j_17 + 1))
          else (t_12 := (Some (j_17, t_18)))
         else () in
       (loop_16 (t_10 + 1));
      (match (! t_12) with
       | Some (i_13) ->
          if ((fst i_13) <> t_10) then begin
           let t_14 = t_5.(t_10) in
           t_5.(t_10) <- t_5.(fst i_13);
           t_5.(fst i_13) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_10 + 1) to (t_7 - 1) do
          let t_22 = (t_5.(j_21)).(t_11) in
          if (t_22 <> (* cross-stage persistent value (as id: zero) *)) then begin
           for j_23 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_23)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   t_22 i_20) (t_5.(t_10)).(j_23)))
           done;
           (t_5.(j_21)).(t_11) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_8 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_8) i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_9) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_9) = 1) then (! t_8)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_8)),
    (! t_2))>.
# val resFA5 : ('a, Ge.GenFA5.Input.inp -> Ge.GenFA5.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_8 - 1) do
       let t_15 = (t_5.(j_14)).(t_10) in
       if (t_15 <> 0.) then
        (match (! t_11) with
         | Some (i_16) ->
            if ((abs_float (snd i_16)) < (abs_float t_15)) then
             (t_11 := (Some (j_14, t_15)))
            else ()
         | None -> (t_11 := (Some (j_14, t_15))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13
          else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_8 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then begin
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done;
           (t_5.(j_19)).(t_10) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resFA6 : ('a, Ge.GenFA6.Input.inp -> Ge.GenFA6.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   let t_9 = (ref 1.) in
   let t_10 = (ref 1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_11 = (! t_2) in
    let t_12 = (! t_3) in
    let t_13 = (ref (None)) in
    let t_19 =
     begin
      for j_16 = t_11 to (t_8 - 1) do
       let t_17 = (t_5.(j_16)).(t_12) in
       if (t_17 <> 0.) then
        (match (! t_13) with
         | Some (i_18) ->
            if ((abs_float (snd i_18)) < (abs_float t_17)) then
             (t_13 := (Some (j_16, t_17)))
            else ()
         | None -> (t_13 := (Some (j_16, t_17))))
       else ()
      done;
      (match (! t_13) with
       | Some (i_14) ->
          if ((fst i_14) <> t_11) then begin
           let t_15 = t_5.(t_11) in
           t_5.(t_11) <- t_5.(fst i_14);
           t_5.(fst i_14) <- t_15;
           (t_10 := (~- (! t_10)))
          end else ();
          (Some (snd i_14))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_11 + 1) to (t_8 - 1) do
          let t_22 = (t_5.(j_21)).(t_12) in
          if (t_22 <> 0.) then begin
           for j_23 = (t_12 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_23) <-
             ((t_5.(j_21)).(j_23) -. ((t_22 /. i_20) *. (t_5.(t_11)).(j_23)))
           done;
           (t_5.(j_21)).(t_12) <- 0.
          end else ()
         done;
         (t_9 := ((! t_9) *. i_20))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_10 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5,
    if ((! t_10) = 0) then 0.
    else if ((! t_10) = 1) then (! t_9)
    else (~-. (! t_9)))>.
# val resFA7 : ('a, Ge.GenFA7.Input.inp -> Ge.GenFA7.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_8 - 1) do
       let t_15 = (t_5.(j_14)).(t_10) in
       if (t_15 <> 0.) then
        (match (! t_11) with
         | Some (i_16) ->
            if ((abs_float (snd i_16)) < (abs_float t_15)) then
             (t_11 := (Some (j_14, t_15)))
            else ()
         | None -> (t_11 := (Some (j_14, t_15))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13
          else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_8 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then begin
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done;
           (t_5.(j_19)).(t_10) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
# val resFA8 : ('a, Ge.GenFA8.Input.inp -> Ge.GenFA8.Output.res) code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy (fst a_1))) in
   let t_6 = (Array.length (fst a_1).(0)) in
   let t_7 = (snd a_1) in
   let t_8 = (Array.length (fst a_1)) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_9 = (! t_2) in
    let t_10 = (! t_3) in
    let t_11 = (ref (None)) in
    let t_17 =
     begin
      for j_14 = t_9 to (t_8 - 1) do
       let t_15 = (t_5.(j_14)).(t_10) in
       if (t_15 <> 0.) then
        (match (! t_11) with
         | Some (i_16) ->
            if ((abs_float (snd i_16)) < (abs_float t_15)) then
             (t_11 := (Some (j_14, t_15)))
            else ()
         | None -> (t_11 := (Some (j_14, t_15))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then
           let t_13 = t_5.(t_9) in
           t_5.(t_9) <- t_5.(fst i_12);
           t_5.(fst i_12) <- t_13
          else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_17 with
     | Some (i_18) ->
        begin
         for j_19 = (t_9 + 1) to (t_8 - 1) do
          let t_20 = (t_5.(j_19)).(t_10) in
          if (t_20 <> 0.) then begin
           for j_21 = (t_10 + 1) to (t_6 - 1) do
            (t_5.(j_19)).(j_21) <-
             ((t_5.(j_19)).(j_21) -. ((t_20 /. i_18) *. (t_5.(t_9)).(j_21)))
           done;
           (t_5.(j_19)).(t_10) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (), (! t_2))>.
#   val rFA1 : Ge.GenFA1.Input.inp -> Ge.GenFA1.Output.res = <fun>
# val rFA2 : Ge.GenFA2.Input.inp -> Ge.GenFA2.Output.res = <fun>
# val rFA3 : Ge.GenFA3.Input.inp -> Ge.GenFA3.Output.res = <fun>
# val rFA4 : Ge.GenFA4.Input.inp -> Ge.GenFA4.Output.res = <fun>
# val rFV1 : Ge.GenFV1.Input.inp -> Ge.GenFV1.Output.res = <fun>
# val rFV2 : Ge.GenFV2.Input.inp -> Ge.GenFV2.Output.res = <fun>
# val rFV3 : Ge.GenFV3.Input.inp -> Ge.GenFV3.Output.res = <fun>
# val rFV4 : Ge.GenFV4.Input.inp -> Ge.GenFV4.Output.res = <fun>
# val rFV5 : Ge.GenFV5.Input.inp -> Ge.GenFV5.Output.res = <fun>
# val rIA1 : Ge.GenIA1.Input.inp -> Ge.GenIA1.Output.res = <fun>
# val rIA2 : Ge.GenIA2.Input.inp -> Ge.GenIA2.Output.res = <fun>
# val rIA3 : Ge.GenIA3.Input.inp -> Ge.GenIA3.Output.res = <fun>
# val rIA4 : Ge.GenIA4.Input.inp -> Ge.GenIA4.Output.res = <fun>
# val rIV1 : Ge.GenIV1.Input.inp -> Ge.GenIV1.Output.res = <fun>
# val rIV2 : Ge.GenIV2.Input.inp -> Ge.GenIV2.Output.res = <fun>
# val rIV3 : Ge.GenIV3.Input.inp -> Ge.GenIV3.Output.res = <fun>
# val rIV4 : Ge.GenIV4.Input.inp -> Ge.GenIV4.Output.res = <fun>
# val rIV5 : Ge.GenIV5.Input.inp -> Ge.GenIV5.Output.res = <fun>
# val rFA11 : Ge.GenFA11.Input.inp -> Ge.GenFA11.Output.res = <fun>
# val rFA12 : Ge.GenFA12.Input.inp -> Ge.GenFA12.Output.res = <fun>
# val rFA13 : Ge.GenFA13.Input.inp -> Ge.GenFA13.Output.res = <fun>
# val rFA14 : Ge.GenFA14.Input.inp -> Ge.GenFA14.Output.res = <fun>
# val rFA24 : Ge.GenFA24.Input.inp -> Ge.GenFA24.Output.res = <fun>
# val rRA1 : Ge.GenRA1.Input.inp -> Ge.GenRA1.Output.res = <fun>
# val rRA2 : Ge.GenRA2.Input.inp -> Ge.GenRA2.Output.res = <fun>
# val rRA3 : Ge.GenRA3.Input.inp -> Ge.GenRA3.Output.res = <fun>
# val rRA4 : Ge.GenRA4.Input.inp -> Ge.GenRA4.Output.res = <fun>
# val rFA5 : Ge.GenFA5.Input.inp -> Ge.GenFA5.Output.res = <fun>
# val rFA6 : Ge.GenFA6.Input.inp -> Ge.GenFA6.Output.res = <fun>
# val rFA7 : Ge.GenFA7.Input.inp -> Ge.GenFA7.Output.res = <fun>
# val rFA8 : Ge.GenFA8.Input.inp -> Ge.GenFA8.Output.res = <fun>
#                                                   val ia0 : Ge.GAC_I.Dom.v array array = [|[|1|]|]
val ia1 : Ge.GAC_I.Dom.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|]
val ia2 : Ge.GAC_I.Dom.v array array =
  [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|]
val ia3 : Ge.GAC_I.Dom.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|]
val ia4 : Ge.GAC_I.Dom.v array array =
  [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]
val ia5 : Ge.GAC_I.Dom.v array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|];
   [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|];
   [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]]
val resI11 : Ge.GenIA1.Output.res list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|]]
# val resI12 : Ge.GenIA2.Output.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0)]
# val resI13 : Ge.GenIA3.Output.res list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 2)]
# val resI14 : Ge.GenIA4.Output.res list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; -9|]; [|0; 0; 0|]|], 0, 2)]
#                 val iv0 : Ge.GVC_I.Dom.v Infra.container2dfromvector =
  {arr = [|1|]; n = 1; m = 1}
val iv1 : Ge.GVC_I.Dom.v Infra.container2dfromvector =
  {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3}
val iv2 : Ge.GVC_I.Dom.v Infra.container2dfromvector =
  {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4}
val iv4 : Ge.GVC_I.Dom.v Infra.container2dfromvector =
  {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}
val iv5 : Ge.GVC_I.Dom.v Infra.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}]
val resI21 : Ge.GenIV1.Output.res list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}]
# val resI22 : Ge.GenIV2.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0)]
# val resI23 : Ge.GenIV3.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 2)]
# val resI24 : Ge.GenIV4.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; -9; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
# val resI25 : Ge.GenIV5.Output.res list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 3; 2; 0; 3; 5; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 3; 2; 0; 0; 3; 5; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|2; 3; 0; 0; -9; 0; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
#   val fa0 : float array array = [|[|1.|]|]
#         val fa1 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|]
#         val fa2 : float array array =
  [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|]
#           val fa3 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|]
#           val fa4 : float array array =
  [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]
#                                                       * *     val fa5 : Ge.GAC_F.Dom.v array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]]
val fa6 : Ge.GAC_F.Dom.v array array = [|[|1.; 1.|]|]
val fa7 : float array array =
  [|[|1.; 2.; 3.; 1.; 0.; 0.|]; [|4.; 13.; 5.; 0.; 1.; 0.|];
    [|-1.; 3.; 0.; 0.; 0.; 1.|]|]
val resF1 : Ge.GenFA1.Output.res list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
#                                                                                                 val a2v : 'a array array -> 'a Infra.container2dfromvector = <fun>
val xxx : Ge.GAC_F.Dom.v Infra.container2dfromvector list =
  [{arr = [|1.|]; n = 1; m = 1};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.|]; n = 3; m = 3};
   {arr = [|1.; 2.; 3.; 0.; 4.; 13.; 5.; 0.; -1.; 3.; 0.; 0.|]; n = 3; m = 4};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.; 0.; 0.; 0.|]; n = 4; m = 3};
   {arr = [|0.; 2.; 3.; 0.; 10.; 5.; 0.; 3.; 0.|]; n = 3; m = 3}]
val resFV5 : Ge.GenFV5.Output.res list =
  [({arr = [|1.|]; n = 1; m = 1}, 1., 1);
   ({arr =
      [|13.; 5.; 4.; 0.; 2.23076923076923084; 0.384615384615384581; 0.; 0.;
        -1.72413793103448287|];
     n = 3; m = 3},
    50., 3);
   ({arr =
      [|13.; 5.; 4.; 0.; 0.; 2.23076923076923084; 0.384615384615384581; 0.;
        0.; 0.; -1.72413793103448287; 0.|];
     n = 3; m = 4},
    50., 3);
   ({arr =
      [|13.; 5.; 4.; 0.; 2.23076923076923084; 0.384615384615384581; 0.; 0.;
        -1.72413793103448287; 0.; 0.; 0.|];
     n = 4; m = 3},
    50., 3);
   ({arr = [|10.; 5.; 0.; 0.; 2.; 0.; 0.; 0.; 0.|]; n = 3; m = 3}, 0., 2)]
val resF11 : Ge.GenFA11.Output.res list =
  [[|[|1.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]|];
   [|[|13.; 5.; 4.; 0.|];
     [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
     [|0.; 0.; -1.72413793103448287; 0.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|];
   [|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|]]
# val resF12 : Ge.GenFA12.Output.res list =
  [([|[|1.|]|], 1.);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]|],
    50.);
   ([|[|13.; 5.; 4.; 0.|];
      [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
      [|0.; 0.; -1.72413793103448287; 0.|]|],
    50.);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|],
    50.);
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 0.)]
# val resF13 : Ge.GenFA13.Output.res list =
  [([|[|1.|]|], 1);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]|],
    3);
   ([|[|13.; 5.; 4.; 0.|];
      [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
      [|0.; 0.; -1.72413793103448287; 0.|]|],
    3);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|],
    3);
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 2)]
# val resF14 : Ge.GenFA14.Output.res list =
  [([|[|1.|]|], 1., 1);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]|],
    50., 3);
   ([|[|13.; 5.; 4.; 0.|];
      [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
      [|0.; 0.; -1.72413793103448287; 0.|]|],
    50., 3);
   ([|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
      [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|],
    50., 3);
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 0., 2)]
# val resF24 : Ge.GenFA24.Output.res list =
  [([|[|1.|]|], 1., 1, []);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3,
    [RowSwap (2, 1); RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3, [RowSwap (2, 1); RowSwap (1, 0)]);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3, [RowSwap (2, 1); RowSwap (1, 0)]);
   ([|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|], 0., 2,
    [RowSwap (1, 0)])]
#   * * * * * * * * *     val ra0 : Num.num array array = [|[|Num.Int 1|]|]
#           val ra1 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
    [|Num.Int 4; Num.Int 13; Num.Int 5|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0|]|]
#           val ra2 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
    [|Num.Int 4; Num.Int 13; Num.Int 5; Num.Int 0|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0; Num.Int 0|]|]
#             val ra3 : Num.num array array =
  [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
    [|Num.Int 4; Num.Int 13; Num.Int 5|];
    [|Num.Int (-1); Num.Int 3; Num.Int 0|];
    [|Num.Int 0; Num.Int 0; Num.Int 0|]|]
#           val ra4 : Num.num array array =
  [|[|Num.Int 0; Num.Int 2; Num.Int 3|];
    [|Num.Int 0; Num.Int 13; Num.Int 5|];
    [|Num.Int 0; Num.Int 3; Num.Int 0|]|]
# val ra5 : Num.num array array list =
  [[|[|Num.Int 1|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 4; Num.Int 13; Num.Int 5|];
     [|Num.Int (-1); Num.Int 3; Num.Int 0|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
     [|Num.Int 4; Num.Int 13; Num.Int 5; Num.Int 0|];
     [|Num.Int (-1); Num.Int 3; Num.Int 0; Num.Int 0|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 4; Num.Int 13; Num.Int 5|];
     [|Num.Int (-1); Num.Int 3; Num.Int 0|];
     [|Num.Int 0; Num.Int 0; Num.Int 0|]|];
   [|[|Num.Int 0; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 13; Num.Int 5|];
     [|Num.Int 0; Num.Int 3; Num.Int 0|]|]]
#   val resR11 : Ge.GenRA1.Output.res list =
  [[|[|Num.Int 1|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
     [|Num.Int 0; Num.Int 0; Num.Int 10|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
     [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
     [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|];
   [|[|Num.Int 1; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
     [|Num.Int 0; Num.Int 0; Num.Int 10|];
     [|Num.Int 0; Num.Int 0; Num.Int 0|]|];
   [|[|Num.Int 0; Num.Int 2; Num.Int 3|];
     [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
     [|Num.Int 0; Num.Int 0; Num.Int 0|]|]]
# val resR12 : Ge.GenRA2.Output.res list =
  [([|[|Num.Int 1|]|], Num.Int 1);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|]|],
    Num.Int 50);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
      [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|],
    Num.Int 50);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 50);
   ([|[|Num.Int 0; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 0)]
# val resR13 : Ge.GenRA3.Output.res list =
  [([|[|Num.Int 1|]|], 1);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|]|],
    3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
      [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|],
    3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    3);
   ([|[|Num.Int 0; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    2)]
# val resR14 : Ge.GenRA4.Output.res list =
  [([|[|Num.Int 1|]|], Num.Int 1, 1);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|]|],
    Num.Int 50, 3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3; Num.Int 0|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7); Num.Int 0|];
      [|Num.Int 0; Num.Int 0; Num.Int 10; Num.Int 0|]|],
    Num.Int 50, 3);
   ([|[|Num.Int 1; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 5; Num.Int (-7)|];
      [|Num.Int 0; Num.Int 0; Num.Int 10|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 50, 3);
   ([|[|Num.Int 0; Num.Int 2; Num.Int 3|];
      [|Num.Int 0; Num.Int 0; Num.Ratio <abstr>|];
      [|Num.Int 0; Num.Int 0; Num.Int 0|]|],
    Num.Int 0, 2)]
# 
