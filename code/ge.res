        Objective Caml version 3.08.0

# # #   val instantiate :
  (('a, 'b) code -> 'c list -> ('d -> 'e -> 'e) -> ('a, 'f) code) ->
  ('a, 'b -> 'f) code = <fun>
# val resFA1 :
  ('a,
   Ge.GenFA1.Ctr.contr ->
   Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.NoDet(Infra.FloatDomain)).res)
  code =
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
       if (not (t_14 = 0.)) then
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
          if ((fst i_11) <> t_8) then begin
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst i_11);
           t_5.(fst i_11) <- t_12;
           ()
          end else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_16 with
     | Some (i_17) ->
        begin
         for j_18 = (t_8 + 1) to (t_7 - 1) do
          if (not ((t_5.(j_18)).(t_9) = 0.)) then begin
           for j_19 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_18)).(j_19) <-
             ((t_5.(j_18)).(j_19) -.
               (((t_5.(j_18)).(t_9) /. (t_5.(t_8)).(t_9)) *.
                 (t_5.(t_8)).(j_19)))
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
# val resFA2 :
  ('a,
   Ge.GenFA2.Ctr.contr ->
   Ge.OutDet(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet).res)
  code =
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
       if (not (t_16 = 0.)) then
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
          if (not ((t_5.(j_20)).(t_11) = 0.)) then begin
           for j_21 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_21) <-
             ((t_5.(j_20)).(j_21) -.
               (((t_5.(j_20)).(t_11) /. (t_5.(t_10)).(t_11)) *.
                 (t_5.(t_10)).(j_21)))
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
# val resFA3 :
  ('a,
   Ge.GenFA3.Ctr.contr ->
   Ge.OutRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.Rank).res)
  code =
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
       if (not (t_14 = 0.)) then
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
          if ((fst i_11) <> t_8) then begin
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst i_11);
           t_5.(fst i_11) <- t_12;
           ()
          end else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_16 with
     | Some (i_17) ->
        begin
         for j_18 = (t_8 + 1) to (t_7 - 1) do
          if (not ((t_5.(j_18)).(t_9) = 0.)) then begin
           for j_19 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_18)).(j_19) <-
             ((t_5.(j_18)).(j_19) -.
               (((t_5.(j_18)).(t_9) /. (t_5.(t_8)).(t_9)) *.
                 (t_5.(t_8)).(j_19)))
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
# val resFA4 :
  ('a,
   Ge.GenFA4.Ctr.contr ->
   Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet)(Ge.Rank).res)
  code =
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
       if (not (t_16 = 0.)) then
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
          if (not ((t_5.(j_20)).(t_11) = 0.)) then begin
           for j_21 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_21) <-
             ((t_5.(j_20)).(j_21) -.
               (((t_5.(j_20)).(t_11) /. (t_5.(t_10)).(t_11)) *.
                 (t_5.(t_10)).(j_21)))
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
# val resFV1 :
  ('a,
   Ge.GenFV1.Ctr.contr ->
   Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet).res)
  code =
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
    let t_23 =
     begin
      for j_20 = t_9 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_10) in
       if (not (t_21 = 0.)) then
        (match (! t_11) with
         | Some (i_22) ->
            if ((abs_float (snd i_22)) < (abs_float t_21)) then
             (t_11 := (Some (j_20, t_21)))
            else ()
         | None -> (t_11 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst i_12) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_25 * t_4.m) + t_10) = 0.)) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_26) <-
             ((t_4.arr).((j_25 * t_4.m) + j_26) -.
               (((t_4.arr).((j_25 * t_4.m) + t_10) /.
                  (t_4.arr).((t_9 * t_4.m) + t_10)) *.
                 (t_4.arr).((t_9 * t_4.m) + j_26)))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_24))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
# val resFV2 :
  ('a,
   Ge.GenFV2.Ctr.contr ->
   Ge.OutDet(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet).res)
  code =
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
    let t_23 =
     begin
      for j_20 = t_9 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_10) in
       if (not (t_21 = 0.)) then
        (match (! t_11) with
         | Some (i_22) ->
            if ((abs_float (snd i_22)) < (abs_float t_21)) then
             (t_11 := (Some (j_20, t_21)))
            else ()
         | None -> (t_11 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst i_12) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_25 * t_4.m) + t_10) = 0.)) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_26) <-
             ((t_4.arr).((j_25 * t_4.m) + j_26) -.
               (((t_4.arr).((j_25 * t_4.m) + t_10) /.
                  (t_4.arr).((t_9 * t_4.m) + t_10)) *.
                 (t_4.arr).((t_9 * t_4.m) + j_26)))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_24))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)))>.
# val resFV3 :
  ('a,
   Ge.GenFV3.Ctr.contr ->
   Ge.OutRank(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.Rank).res)
  code =
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
    let t_21 =
     begin
      for j_18 = t_7 to (t_6 - 1) do
       let t_19 = (t_4.arr).((j_18 * t_4.m) + t_8) in
       if (not (t_19 = 0.)) then
        (match (! t_9) with
         | Some (i_20) ->
            if ((abs_float (snd i_20)) < (abs_float t_19)) then
             (t_9 := (Some (j_18, t_19)))
            else ()
         | None -> (t_9 := (Some (j_18, t_19))))
       else ()
      done;
      (match (! t_9) with
       | Some (i_10) ->
          if ((fst i_10) <> t_7) then begin
           let a_11 = t_4.arr
           and n_12 = t_4.n
           and m_13 = t_4.m in
           let i1_14 = (t_7 * m_13)
           and i2_15 = ((fst i_10) * m_13) in
           for i_16 = 0 to (m_13 - 1) do
            let t_17 = a_11.(i1_14 + i_16) in
            a_11.(i1_14 + i_16) <- a_11.(i2_15 + i_16);
            a_11.(i2_15 + i_16) <- t_17
           done;
           ()
          end else ();
          (Some (snd i_10))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_7 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_23 * t_4.m) + t_8) = 0.)) then begin
           for j_24 = (t_8 + 1) to (t_5 - 1) do
            (t_4.arr).((j_23 * t_4.m) + j_24) <-
             ((t_4.arr).((j_23 * t_4.m) + j_24) -.
               (((t_4.arr).((j_23 * t_4.m) + t_8) /.
                  (t_4.arr).((t_7 * t_4.m) + t_8)) *.
                 (t_4.arr).((t_7 * t_4.m) + j_24)))
           done;
           (t_4.arr).((j_23 * t_4.m) + t_8) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
# val resFV4 :
  ('a,
   Ge.GenFV4.Ctr.contr ->
   Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet)(Ge.Rank).res)
  code =
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
    let t_23 =
     begin
      for j_20 = t_9 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_10) in
       if (not (t_21 = 0.)) then
        (match (! t_11) with
         | Some (i_22) ->
            if ((abs_float (snd i_22)) < (abs_float t_21)) then
             (t_11 := (Some (j_20, t_21)))
            else ()
         | None -> (t_11 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst i_12) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_25 * t_4.m) + t_10) = 0.)) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_26) <-
             ((t_4.arr).((j_25 * t_4.m) + j_26) -.
               (((t_4.arr).((j_25 * t_4.m) + t_10) /.
                  (t_4.arr).((t_9 * t_4.m) + t_10)) *.
                 (t_4.arr).((t_9 * t_4.m) + j_26)))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_24))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
# val resFV5 :
  ('a,
   Ge.GenFV5.Ctr.contr ->
   Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet)(Ge.Rank).res)
  code =
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
    let t_31 =
     begin
      for j_27 = t_9 to (t_6 - 1) do
       for j_28 = t_10 to (t_5 - 1) do
        let t_29 = (t_4.arr).((j_27 * t_4.m) + j_28) in
        if (not (t_29 = 0.)) then
         (match (! t_11) with
          | Some (i_30) ->
             if ((abs_float (snd i_30)) < (abs_float t_29)) then
              (t_11 := (Some ((j_27, j_28), t_29)))
             else ()
          | None -> (t_11 := (Some ((j_27, j_28), t_29))))
        else ()
       done
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((snd (fst i_12)) <> t_10) then begin
           let a_20 = t_4.arr
           and nm_21 = (t_4.n * t_4.m)
           and m_22 = t_4.m in
           let rec loop_23 =
            fun i1_24 ->
             fun i2_25 ->
              if (i2_25 < nm_21) then
               let t_26 = a_20.(i1_24) in
               a_20.(i1_24) <- a_20.(i2_25);
               a_20.(i2_25) <- t_26;
               (loop_23 (i1_24 + m_22) (i2_25 + m_22))
              else () in
           (loop_23 t_10 (snd (fst i_12)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_12)) <> t_10) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst (fst i_12)) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_31 with
     | Some (i_32) ->
        begin
         for j_33 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_33 * t_4.m) + t_10) = 0.)) then begin
           for j_34 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_33 * t_4.m) + j_34) <-
             ((t_4.arr).((j_33 * t_4.m) + j_34) -.
               (((t_4.arr).((j_33 * t_4.m) + t_10) /.
                  (t_4.arr).((t_9 * t_4.m) + t_10)) *.
                 (t_4.arr).((t_9 * t_4.m) + j_34)))
           done;
           (t_4.arr).((j_33 * t_4.m) + t_10) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. i_32))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0.
    else if ((! t_8) = 1) then (! t_7)
    else (~-. (! t_7)), (! t_2))>.
# val resIA1 :
  ('a,
   Ge.GenIA1.Ctr.contr ->
   Ge.OutJustMatrix(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet).res)
  code =
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
       if (not (t_16 = 0)) then
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
          if (not ((t_5.(j_20)).(t_11) = 0)) then begin
           for j_21 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_21) <-
             ((((t_5.(j_20)).(j_21) * (t_5.(t_10)).(t_11)) -
                ((t_5.(t_10)).(j_21) * (t_5.(j_20)).(t_10))) / (! t_8))
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
# val resIA2 :
  ('a,
   Ge.GenIA2.Ctr.contr ->
   Ge.OutDet(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet).res)
  code =
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
       if (not (t_16 = 0)) then
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
          if (not ((t_5.(j_20)).(t_11) = 0)) then begin
           for j_21 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_21) <-
             ((((t_5.(j_20)).(j_21) * (t_5.(t_10)).(t_11)) -
                ((t_5.(t_10)).(j_21) * (t_5.(j_20)).(t_10))) / (! t_8))
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
# val resIA3 :
  ('a,
   Ge.GenIA3.Ctr.contr ->
   Ge.OutRank(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.Rank).res)
  code =
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
       if (not (t_16 = 0)) then
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
           ()
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_18 with
     | Some (i_19) ->
        begin
         for j_20 = (t_10 + 1) to (t_7 - 1) do
          if (not ((t_5.(j_20)).(t_11) = 0)) then begin
           for j_21 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_21) <-
             ((((t_5.(j_20)).(j_21) * (t_5.(t_10)).(t_11)) -
                ((t_5.(t_10)).(j_21) * (t_5.(j_20)).(t_10))) / (! t_8))
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
# val resIA4 :
  ('a,
   Ge.GenIA4.Ctr.contr ->
   Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet)(Ge.Rank).res)
  code =
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
       if (not (t_16 = 0)) then
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
          if (not ((t_5.(j_20)).(t_11) = 0)) then begin
           for j_21 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_20)).(j_21) <-
             ((((t_5.(j_20)).(j_21) * (t_5.(t_10)).(t_11)) -
                ((t_5.(t_10)).(j_21) * (t_5.(j_20)).(t_10))) / (! t_8))
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
# val resIV1 :
  ('a,
   Ge.GenIV1.Ctr.contr ->
   Ge.OutJustMatrix(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.NoDet(Infra.IntegerDomain)).res)
  code =
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
    let t_23 =
     begin
      for j_20 = t_9 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_10) in
       if (not (t_21 = 0)) then
        (match (! t_11) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_11 := (Some (j_20, t_21)))
            else ()
         | None -> (t_11 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst i_12) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           ()
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_25 * t_4.m) + t_10) = 0)) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_26) *
                 (t_4.arr).((t_9 * t_4.m) + t_10)) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) *
                  (t_4.arr).((j_25 * t_4.m) + t_9))) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
# val resIV2 :
  ('a,
   Ge.GenIV2.Ctr.contr ->
   Ge.OutDet(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet).res)
  code =
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
    let t_23 =
     begin
      for j_20 = t_9 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_10) in
       if (not (t_21 = 0)) then
        (match (! t_11) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_11 := (Some (j_20, t_21)))
            else ()
         | None -> (t_11 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst i_12) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_25 * t_4.m) + t_10) = 0)) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_26) *
                 (t_4.arr).((t_9 * t_4.m) + t_10)) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) *
                  (t_4.arr).((j_25 * t_4.m) + t_9))) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)))>.
# val resIV3 :
  ('a,
   Ge.GenIV3.Ctr.contr ->
   Ge.OutRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.Rank).res)
  code =
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
    let t_23 =
     begin
      for j_20 = t_9 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_10) in
       if (not (t_21 = 0)) then
        (match (! t_11) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_11 := (Some (j_20, t_21)))
            else ()
         | None -> (t_11 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst i_12) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           ()
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_25 * t_4.m) + t_10) = 0)) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_26) *
                 (t_4.arr).((t_9 * t_4.m) + t_10)) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) *
                  (t_4.arr).((j_25 * t_4.m) + t_9))) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
# val resIV4 :
  ('a,
   Ge.GenIV4.Ctr.contr ->
   Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet)(Ge.Rank).res)
  code =
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
    let t_23 =
     begin
      for j_20 = t_9 to (t_6 - 1) do
       let t_21 = (t_4.arr).((j_20 * t_4.m) + t_10) in
       if (not (t_21 = 0)) then
        (match (! t_11) with
         | Some (i_22) ->
            if ((abs (snd i_22)) > (abs t_21)) then
             (t_11 := (Some (j_20, t_21)))
            else ()
         | None -> (t_11 := (Some (j_20, t_21))))
       else ()
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((fst i_12) <> t_9) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst i_12) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_23 with
     | Some (i_24) ->
        begin
         for j_25 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_25 * t_4.m) + t_10) = 0)) then begin
           for j_26 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_25 * t_4.m) + j_26) <-
             ((((t_4.arr).((j_25 * t_4.m) + j_26) *
                 (t_4.arr).((t_9 * t_4.m) + t_10)) -
                ((t_4.arr).((t_9 * t_4.m) + j_26) *
                  (t_4.arr).((j_25 * t_4.m) + t_9))) / (! t_7))
           done;
           (t_4.arr).((j_25 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_24)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
# val resIV5 :
  ('a,
   Ge.GenIV5.Ctr.contr ->
   Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet)(Ge.Rank).res)
  code =
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
    let t_31 =
     begin
      for j_27 = t_9 to (t_6 - 1) do
       for j_28 = t_10 to (t_5 - 1) do
        let t_29 = (t_4.arr).((j_27 * t_4.m) + j_28) in
        if (not (t_29 = 0)) then
         (match (! t_11) with
          | Some (i_30) ->
             if ((abs (snd i_30)) > (abs t_29)) then
              (t_11 := (Some ((j_27, j_28), t_29)))
             else ()
          | None -> (t_11 := (Some ((j_27, j_28), t_29))))
        else ()
       done
      done;
      (match (! t_11) with
       | Some (i_12) ->
          if ((snd (fst i_12)) <> t_10) then begin
           let a_20 = t_4.arr
           and nm_21 = (t_4.n * t_4.m)
           and m_22 = t_4.m in
           let rec loop_23 =
            fun i1_24 ->
             fun i2_25 ->
              if (i2_25 < nm_21) then
               let t_26 = a_20.(i1_24) in
               a_20.(i1_24) <- a_20.(i2_25);
               a_20.(i2_25) <- t_26;
               (loop_23 (i1_24 + m_22) (i2_25 + m_22))
              else () in
           (loop_23 t_10 (snd (fst i_12)));
           (t_8 := (~- (! t_8)))
          end else ();
          if ((fst (fst i_12)) <> t_10) then begin
           let a_13 = t_4.arr
           and n_14 = t_4.n
           and m_15 = t_4.m in
           let i1_16 = (t_9 * m_15)
           and i2_17 = ((fst (fst i_12)) * m_15) in
           for i_18 = 0 to (m_15 - 1) do
            let t_19 = a_13.(i1_16 + i_18) in
            a_13.(i1_16 + i_18) <- a_13.(i2_17 + i_18);
            a_13.(i2_17 + i_18) <- t_19
           done;
           (t_8 := (~- (! t_8)))
          end else ();
          (Some (snd i_12))
       | None -> (None))
     end in
    (match t_31 with
     | Some (i_32) ->
        begin
         for j_33 = (t_9 + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_33 * t_4.m) + t_10) = 0)) then begin
           for j_34 = (t_10 + 1) to (t_5 - 1) do
            (t_4.arr).((j_33 * t_4.m) + j_34) <-
             ((((t_4.arr).((j_33 * t_4.m) + j_34) *
                 (t_4.arr).((t_9 * t_4.m) + t_10)) -
                ((t_4.arr).((t_9 * t_4.m) + j_34) *
                  (t_4.arr).((j_33 * t_4.m) + t_9))) / (! t_7))
           done;
           (t_4.arr).((j_33 * t_4.m) + t_10) <- 0
          end else ()
         done;
         (t_7 := i_32)
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4,
    if ((! t_8) = 0) then 0
    else if ((! t_8) = 1) then (! t_7)
    else (~- (! t_7)), (! t_2))>.
# val resFA11 :
  ('a,
   Ge.GenFA11.Ctr.contr ->
   Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.NoDet(Infra.FloatDomain)).res)
  code =
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
        if (not (t_17 = 0.)) then
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
          if ((snd (fst i_11)) <> t_9) then begin
           for r_13 = 0 to ((Array.length t_5) - 1) do
            let t_14 = (t_5.(r_13)).(t_9) in
            (t_5.(r_13)).(t_9) <- (t_5.(r_13)).(snd (fst i_11));
            (t_5.(r_13)).(snd (fst i_11)) <- t_14
           done;
           ()
          end else ();
          if ((fst (fst i_11)) <> t_9) then begin
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst (fst i_11));
           t_5.(fst (fst i_11)) <- t_12;
           ()
          end else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_8 + 1) to (t_7 - 1) do
          if (not ((t_5.(j_21)).(t_9) = 0.)) then begin
           for j_22 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_22) <-
             ((t_5.(j_21)).(j_22) -.
               (((t_5.(j_21)).(t_9) /. (t_5.(t_8)).(t_9)) *.
                 (t_5.(t_8)).(j_22)))
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
# val resFA12 :
  ('a,
   Ge.GenFA12.Ctr.contr ->
   Ge.OutDet(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet).res)
  code =
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
        if (not (t_19 = 0.)) then
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
           t_5.(t_10) <- t_5.(fst (fst i_13));
           t_5.(fst (fst i_13)) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_10 + 1) to (t_7 - 1) do
          if (not ((t_5.(j_23)).(t_11) = 0.)) then begin
           for j_24 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_24) <-
             ((t_5.(j_23)).(j_24) -.
               (((t_5.(j_23)).(t_11) /. (t_5.(t_10)).(t_11)) *.
                 (t_5.(t_10)).(j_24)))
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
# val resFA13 :
  ('a,
   Ge.GenFA13.Ctr.contr ->
   Ge.OutRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.Rank).res)
  code =
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
        if (not (t_17 = 0.)) then
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
          if ((snd (fst i_11)) <> t_9) then begin
           for r_13 = 0 to ((Array.length t_5) - 1) do
            let t_14 = (t_5.(r_13)).(t_9) in
            (t_5.(r_13)).(t_9) <- (t_5.(r_13)).(snd (fst i_11));
            (t_5.(r_13)).(snd (fst i_11)) <- t_14
           done;
           ()
          end else ();
          if ((fst (fst i_11)) <> t_9) then begin
           let t_12 = t_5.(t_8) in
           t_5.(t_8) <- t_5.(fst (fst i_11));
           t_5.(fst (fst i_11)) <- t_12;
           ()
          end else ();
          (Some (snd i_11))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_8 + 1) to (t_7 - 1) do
          if (not ((t_5.(j_21)).(t_9) = 0.)) then begin
           for j_22 = (t_9 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_22) <-
             ((t_5.(j_21)).(j_22) -.
               (((t_5.(j_21)).(t_9) /. (t_5.(t_8)).(t_9)) *.
                 (t_5.(t_8)).(j_22)))
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
# val resFA14 :
  ('a,
   Ge.GenFA14.Ctr.contr ->
   Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet)(Ge.Rank).res)
  code =
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
        if (not (t_19 = 0.)) then
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
           t_5.(t_10) <- t_5.(fst (fst i_13));
           t_5.(fst (fst i_13)) <- t_14;
           (t_9 := (~- (! t_9)))
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_21 with
     | Some (i_22) ->
        begin
         for j_23 = (t_10 + 1) to (t_7 - 1) do
          if (not ((t_5.(j_23)).(t_11) = 0.)) then begin
           for j_24 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_23)).(j_24) <-
             ((t_5.(j_23)).(j_24) -.
               (((t_5.(j_23)).(t_11) /. (t_5.(t_10)).(t_11)) *.
                 (t_5.(t_10)).(j_24)))
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
# val resRA1 :
  ('a,
   Ge.GenRA1.Ctr.contr ->
   Ge.OutJustMatrix(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet).res)
  code =
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
      if (not (t_15 = (* cross-stage persistent value (as id: zero) *))) then
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
          if (not
               ((t_5.(j_21)).(t_11) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_22) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_22)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_5.(j_21)).(t_11) (t_5.(t_10)).(t_11))
                 (t_5.(t_10)).(j_22)))
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
# val resRA2 :
  ('a,
   Ge.GenRA2.Ctr.contr ->
   Ge.OutDet(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet).res)
  code =
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
      if (not (t_15 = (* cross-stage persistent value (as id: zero) *))) then
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
          if (not
               ((t_5.(j_21)).(t_11) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_22) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_22)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_5.(j_21)).(t_11) (t_5.(t_10)).(t_11))
                 (t_5.(t_10)).(j_22)))
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
# val resRA3 :
  ('a,
   Ge.GenRA3.Ctr.contr ->
   Ge.OutRank(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.Rank).res)
  code =
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
      if (not (t_15 = (* cross-stage persistent value (as id: zero) *))) then
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
           ()
          end else ();
          (Some (snd i_13))
       | None -> (None))
     end in
    (match t_19 with
     | Some (i_20) ->
        begin
         for j_21 = (t_10 + 1) to (t_7 - 1) do
          if (not
               ((t_5.(j_21)).(t_11) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_22) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_22)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_5.(j_21)).(t_11) (t_5.(t_10)).(t_11))
                 (t_5.(t_10)).(j_22)))
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
# val resRA4 :
  ('a,
   Ge.GenRA4.Ctr.contr ->
   Ge.OutDetRank(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet)(Ge.Rank).res)
  code =
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
      if (not (t_15 = (* cross-stage persistent value (as id: zero) *))) then
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
          if (not
               ((t_5.(j_21)).(t_11) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_22 = (t_11 + 1) to (t_6 - 1) do
            (t_5.(j_21)).(j_22) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_5.(j_21)).(j_22)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_5.(j_21)).(t_11) (t_5.(t_10)).(t_11))
                 (t_5.(t_10)).(j_22)))
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
#   val rFA1 :
  Ge.GenFA1.Ctr.contr ->
  Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.NoDet(Infra.FloatDomain)).res =
  <fun>
# val rFA2 :
  Ge.GenFA2.Ctr.contr ->
  Ge.OutDet(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet).res =
  <fun>
# val rFA3 :
  Ge.GenFA3.Ctr.contr ->
  Ge.OutRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.Rank).res =
  <fun>
# val rFA4 :
  Ge.GenFA4.Ctr.contr ->
  Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet)(Ge.Rank).res =
  <fun>
# val rFV1 :
  Ge.GenFV1.Ctr.contr ->
  Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet).res =
  <fun>
# val rFV2 :
  Ge.GenFV2.Ctr.contr ->
  Ge.OutDet(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet).res =
  <fun>
# val rFV3 :
  Ge.GenFV3.Ctr.contr ->
  Ge.OutRank(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.Rank).res =
  <fun>
# val rFV4 :
  Ge.GenFV4.Ctr.contr ->
  Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet)(Ge.Rank).res =
  <fun>
# val rFV5 :
  Ge.GenFV5.Ctr.contr ->
  Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet)(Ge.Rank).res =
  <fun>
# val rIA1 :
  Ge.GenIA1.Ctr.contr ->
  Ge.OutJustMatrix(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet).res =
  <fun>
# val rIA2 :
  Ge.GenIA2.Ctr.contr ->
  Ge.OutDet(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet).res =
  <fun>
# val rIA3 :
  Ge.GenIA3.Ctr.contr ->
  Ge.OutRank(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.Rank).res =
  <fun>
# val rIA4 :
  Ge.GenIA4.Ctr.contr ->
  Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet)(Ge.Rank).res =
  <fun>
# val rIV1 :
  Ge.GenIV1.Ctr.contr ->
  Ge.OutJustMatrix(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.NoDet(Infra.IntegerDomain)).res =
  <fun>
# val rIV2 :
  Ge.GenIV2.Ctr.contr ->
  Ge.OutDet(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet).res =
  <fun>
# val rIV3 :
  Ge.GenIV3.Ctr.contr ->
  Ge.OutRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.Rank).res =
  <fun>
# val rIV4 :
  Ge.GenIV4.Ctr.contr ->
  Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet)(Ge.Rank).res =
  <fun>
# val rIV5 :
  Ge.GenIV5.Ctr.contr ->
  Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet)(Ge.Rank).res =
  <fun>
# val rFA11 :
  Ge.GenFA11.Ctr.contr ->
  Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.NoDet(Infra.FloatDomain)).res =
  <fun>
# val rFA12 :
  Ge.GenFA12.Ctr.contr ->
  Ge.OutDet(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet).res =
  <fun>
# val rFA13 :
  Ge.GenFA13.Ctr.contr ->
  Ge.OutRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.Rank).res =
  <fun>
# val rFA14 :
  Ge.GenFA14.Ctr.contr ->
  Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet)(Ge.Rank).res =
  <fun>
# val rRA1 :
  Ge.GenRA1.Ctr.contr ->
  Ge.OutJustMatrix(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet).res =
  <fun>
# val rRA2 :
  Ge.GenRA2.Ctr.contr ->
  Ge.OutDet(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet).res =
  <fun>
# val rRA3 :
  Ge.GenRA3.Ctr.contr ->
  Ge.OutRank(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.Rank).res =
  <fun>
# val rRA4 :
  Ge.GenRA4.Ctr.contr ->
  Ge.OutDetRank(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet)(Ge.Rank).res =
  <fun>
#                                                   val ia0 : Infra.IntegerDomain.v array array = [|[|1|]|]
val ia1 : Infra.IntegerDomain.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|]
val ia2 : Infra.IntegerDomain.v array array =
  [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|]
val ia3 : Infra.IntegerDomain.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|]
val ia4 : Infra.IntegerDomain.v array array =
  [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]
val ia5 : Infra.IntegerDomain.v array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|];
   [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|];
   [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]]
val resI11 :
  Ge.OutJustMatrix(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet).res
  list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|]]
# val resI12 :
  Ge.OutDet(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet).res
  list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 0)]
# val resI13 :
  Ge.OutRank(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.Rank).res
  list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 2)]
# val resI14 :
  Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericArrayContainer)(Ge.IDet)(Ge.Rank).res
  list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 0, 2)]
#                 val iv0 : Infra.IntegerDomain.v Infra.container2dfromvector =
  {arr = [|1|]; n = 1; m = 1}
val iv1 : Infra.IntegerDomain.v Infra.container2dfromvector =
  {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3}
val iv2 : Infra.IntegerDomain.v Infra.container2dfromvector =
  {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4}
val iv4 : Infra.IntegerDomain.v Infra.container2dfromvector =
  {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}
val iv5 : Infra.IntegerDomain.v Infra.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}]
val resI21 :
  Ge.OutJustMatrix(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.NoDet(Infra.IntegerDomain)).res
  list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}]
# val resI22 :
  Ge.OutDet(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet).res
  list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}, 0)]
# val resI23 :
  Ge.OutRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.Rank).res
  list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}, 2)]
# val resI24 :
  Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet)(Ge.Rank).res
  list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
# val resI25 :
  Ge.OutDetRank(Infra.IntegerDomain)(Infra.GenericVectorContainer)(Ge.IDet)(Ge.Rank).res
  list =
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
#                                     val fa5 : Infra.FloatDomain.v array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]]
val resF1 :
  Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.NoDet(Infra.FloatDomain)).res
  list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
#                                                                                                 val a2v : 'a array array -> 'a Infra.container2dfromvector = <fun>
val xxx : Infra.FloatDomain.v Infra.container2dfromvector list =
  [{arr = [|1.|]; n = 1; m = 1};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.|]; n = 3; m = 3};
   {arr = [|1.; 2.; 3.; 0.; 4.; 13.; 5.; 0.; -1.; 3.; 0.; 0.|]; n = 3; m = 4};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.; 0.; 0.; 0.|]; n = 4; m = 3};
   {arr = [|0.; 2.; 3.; 0.; 10.; 5.; 0.; 3.; 0.|]; n = 3; m = 3}]
val resFV5 :
  Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericVectorContainer)(Ge.FDet)(Ge.Rank).res
  list =
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
val resF11 :
  Ge.OutJustMatrix(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.NoDet(Infra.FloatDomain)).res
  list =
  [[|[|1.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]|];
   [|[|13.; 5.; 4.; 0.|];
     [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
     [|0.; 0.; -1.72413793103448287; 0.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|];
   [|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|]]
# val resF12 :
  Ge.OutDet(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet).res list =
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
# val resF13 :
  Ge.OutRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.Rank).res
  list =
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
# val resF14 :
  Ge.OutDetRank(Infra.FloatDomain)(Infra.GenericArrayContainer)(Ge.FDet)(Ge.Rank).res
  list =
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
#   val ra0 : Num.num array array = [|[|Num.Int 1|]|]
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
#   val resR11 :
  Ge.OutJustMatrix(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet).res
  list =
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
# val resR12 :
  Ge.OutDet(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet).res
  list =
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
# val resR13 :
  Ge.OutRank(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.Rank).res
  list =
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
# val resR14 :
  Ge.OutDetRank(Infra.RationalDomain)(Infra.GenericArrayContainer)(Ge.RDet)(Ge.Rank).res
  list =
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
