        MetaOCaml version 3.08.0 alpha 015

# #   val resFA1 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutJustMatrix(Funct4.FArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (ref (-1)) in
    let t_10 =
     begin
      for j_9 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_9)).(! t_3) = 0.)) then
        if (((! t_8) == (-1)) ||
             ((abs_float (t_5.(j_9)).(! t_3)) <
               (abs_float (t_5.(! t_8)).(! t_3)))) then
         (t_8 := j_9)
        else ()
       else ()
      done;
      if ((! t_8) == (-1)) then (None) else (Some (! t_8))
     end in
    (match t_10 with
     | Some (i_11) ->
        if (i_11 <> (! t_2)) then
         let t_12 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_11);
         t_5.(i_11) <- t_12
        else ();
        begin
         for j_13 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_13)).(! t_3) = 0.)) then begin
           for j_14 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_13)).(j_14) <-
             ((((t_5.(j_13)).(! t_3) /. (t_5.(! t_2)).(! t_3)) *.
                (t_5.(! t_2)).(j_14)) -. (t_5.(j_13)).(j_14))
           done;
           (t_5.(j_13)).(! t_3) <- 0.
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
   Funct4.FArrayContainer.contr ->
   Funct4.OutDet(Funct4.FArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1.) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (ref (-1)) in
    let t_12 =
     begin
      for j_11 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_11)).(! t_3) = 0.)) then
        if (((! t_10) == (-1)) ||
             ((abs_float (t_5.(j_11)).(! t_3)) <
               (abs_float (t_5.(! t_10)).(! t_3)))) then
         (t_10 := j_11)
        else ()
       else ()
      done;
      if ((! t_10) == (-1)) then (None) else (Some (! t_10))
     end in
    (match t_12 with
     | Some (i_13) ->
        if (i_13 <> (! t_2)) then
         let t_14 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_13);
         t_5.(i_13) <- t_14
        else ();
        begin
         for j_15 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_15)).(! t_3) = 0.)) then begin
           for j_16 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_15)).(j_16) <-
             ((((t_5.(j_15)).(! t_3) /. (t_5.(! t_2)).(! t_3)) *.
                (t_5.(! t_2)).(j_16)) -. (t_5.(j_15)).(j_16))
           done;
           (t_5.(j_15)).(! t_3) <- 0.
          end else ()
         done;
         (t_8 := ((! t_8) *. (t_5.(! t_2)).(! t_3)))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0.));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, ((! t_9) *. (! t_8)))>.
# val resFA3 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutRank(Funct4.FArrayContainer)(Funct4.Rank)(Funct4.FloatDomain).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (ref (-1)) in
    let t_10 =
     begin
      for j_9 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_9)).(! t_3) = 0.)) then
        if (((! t_8) == (-1)) ||
             ((abs_float (t_5.(j_9)).(! t_3)) <
               (abs_float (t_5.(! t_8)).(! t_3)))) then
         (t_8 := j_9)
        else ()
       else ()
      done;
      if ((! t_8) == (-1)) then (None) else (Some (! t_8))
     end in
    (match t_10 with
     | Some (i_11) ->
        if (i_11 <> (! t_2)) then
         let t_12 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_11);
         t_5.(i_11) <- t_12
        else ();
        begin
         for j_13 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_13)).(! t_3) = 0.)) then begin
           for j_14 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_13)).(j_14) <-
             ((((t_5.(j_13)).(! t_3) /. (t_5.(! t_2)).(! t_3)) *.
                (t_5.(! t_2)).(j_14)) -. (t_5.(j_13)).(j_14))
           done;
           (t_5.(j_13)).(! t_3) <- 0.
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
   Funct4.FArrayContainer.contr ->
   Funct4.OutDetRank(Funct4.FArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1.) in
   let t_9 = (ref 1.) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (ref (-1)) in
    let t_12 =
     begin
      for j_11 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_11)).(! t_3) = 0.)) then
        if (((! t_10) == (-1)) ||
             ((abs_float (t_5.(j_11)).(! t_3)) <
               (abs_float (t_5.(! t_10)).(! t_3)))) then
         (t_10 := j_11)
        else ()
       else ()
      done;
      if ((! t_10) == (-1)) then (None) else (Some (! t_10))
     end in
    (match t_12 with
     | Some (i_13) ->
        if (i_13 <> (! t_2)) then
         let t_14 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_13);
         t_5.(i_13) <- t_14
        else ();
        begin
         for j_15 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_15)).(! t_3) = 0.)) then begin
           for j_16 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_15)).(j_16) <-
             ((((t_5.(j_15)).(j_16) *. (t_5.(! t_2)).(! t_3)) -.
                ((t_5.(! t_2)).(j_16) *. (t_5.(j_15)).(! t_3))) /.
               (t_5.(! t_2)).(j_16))
           done;
           (t_5.(j_15)).(! t_3) <- 0.
          end else ()
         done;
         (t_8 := (t_5.(! t_2)).(! t_3))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0.));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, ((! t_9) *. (! t_8)), (! t_2))>.
# val resFV1 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutJustMatrix(Funct4.FVectorContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (ref (-1)) in
    let t_9 =
     begin
      for j_8 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_8 * t_4.n) + (! t_3)) = 0.)) then
        if (((! t_7) == (-1)) ||
             ((abs_float (t_4.arr).((j_8 * t_4.n) + (! t_3))) <
               (abs_float (t_4.arr).(((! t_7) * t_4.n) + (! t_3))))) then
         (t_7 := j_8)
        else ()
       else ()
      done;
      if ((! t_7) == (-1)) then (None) else (Some (! t_7))
     end in
    (match t_9 with
     | Some (i_10) ->
        if (i_10 <> (! t_2)) then
         let a_11 = t_4.arr
         and n_12 = t_4.n
         and m_13 = t_4.m in
         let i1_14 = ((! t_2) * n_12)
         and i2_15 = (i_10 * n_12) in
         for i_16 = 0 to (m_13 - 1) do
          let t_17 = a_11.(i1_14 + i_16) in
          a_11.(i2_15 + i_16) <- a_11.(i1_14 + i_16);
          a_11.(i1_14 + i_16) <- t_17
         done
        else ();
        begin
         for j_18 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_18 * t_4.n) + (! t_3)) = 0.)) then begin
           for j_19 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_18 * t_4.n) + j_19) <-
             ((((t_4.arr).((j_18 * t_4.n) + (! t_3)) /.
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) *.
                (t_4.arr).(((! t_2) * t_4.n) + j_19)) -.
               (t_4.arr).((j_18 * t_4.n) + j_19))
           done;
           (t_4.arr).((j_18 * t_4.n) + (! t_3)) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
# val resFV2 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutDet(Funct4.FVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1.) in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (ref (-1)) in
    let t_11 =
     begin
      for j_10 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_10 * t_4.n) + (! t_3)) = 0.)) then
        if (((! t_9) == (-1)) ||
             ((abs_float (t_4.arr).((j_10 * t_4.n) + (! t_3))) <
               (abs_float (t_4.arr).(((! t_9) * t_4.n) + (! t_3))))) then
         (t_9 := j_10)
        else ()
       else ()
      done;
      if ((! t_9) == (-1)) then (None) else (Some (! t_9))
     end in
    (match t_11 with
     | Some (i_12) ->
        if (i_12 <> (! t_2)) then
         let a_13 = t_4.arr
         and n_14 = t_4.n
         and m_15 = t_4.m in
         let i1_16 = ((! t_2) * n_14)
         and i2_17 = (i_12 * n_14) in
         for i_18 = 0 to (m_15 - 1) do
          let t_19 = a_13.(i1_16 + i_18) in
          a_13.(i2_17 + i_18) <- a_13.(i1_16 + i_18);
          a_13.(i1_16 + i_18) <- t_19
         done
        else ();
        begin
         for j_20 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_20 * t_4.n) + (! t_3)) = 0.)) then begin
           for j_21 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_20 * t_4.n) + j_21) <-
             ((((t_4.arr).((j_20 * t_4.n) + (! t_3)) /.
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) *.
                (t_4.arr).(((! t_2) * t_4.n) + j_21)) -.
               (t_4.arr).((j_20 * t_4.n) + j_21))
           done;
           (t_4.arr).((j_20 * t_4.n) + (! t_3)) <- 0.
          end else ()
         done;
         (t_7 := ((! t_7) *. (t_4.arr).(((! t_2) * t_4.n) + (! t_3))))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0.));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, ((! t_8) *. (! t_7)))>.
# val resFV3 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutRank(Funct4.FVectorContainer)(Funct4.Rank)(Funct4.FloatDomain).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (ref (-1)) in
    let t_9 =
     begin
      for j_8 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_8 * t_4.n) + (! t_3)) = 0.)) then
        if (((! t_7) == (-1)) ||
             ((abs_float (t_4.arr).((j_8 * t_4.n) + (! t_3))) <
               (abs_float (t_4.arr).(((! t_7) * t_4.n) + (! t_3))))) then
         (t_7 := j_8)
        else ()
       else ()
      done;
      if ((! t_7) == (-1)) then (None) else (Some (! t_7))
     end in
    (match t_9 with
     | Some (i_10) ->
        if (i_10 <> (! t_2)) then
         let a_11 = t_4.arr
         and n_12 = t_4.n
         and m_13 = t_4.m in
         let i1_14 = ((! t_2) * n_12)
         and i2_15 = (i_10 * n_12) in
         for i_16 = 0 to (m_13 - 1) do
          let t_17 = a_11.(i1_14 + i_16) in
          a_11.(i2_15 + i_16) <- a_11.(i1_14 + i_16);
          a_11.(i1_14 + i_16) <- t_17
         done
        else ();
        begin
         for j_18 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_18 * t_4.n) + (! t_3)) = 0.)) then begin
           for j_19 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_18 * t_4.n) + j_19) <-
             ((((t_4.arr).((j_18 * t_4.n) + (! t_3)) /.
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) *.
                (t_4.arr).(((! t_2) * t_4.n) + j_19)) -.
               (t_4.arr).((j_18 * t_4.n) + j_19))
           done;
           (t_4.arr).((j_18 * t_4.n) + (! t_3)) <- 0.
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
   Funct4.FVectorContainer.contr ->
   Funct4.OutDetRank(Funct4.FVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1.) in
   let t_8 = (ref 1.) in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (ref (-1)) in
    let t_11 =
     begin
      for j_10 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_10 * t_4.n) + (! t_3)) = 0.)) then
        if (((! t_9) == (-1)) ||
             ((abs_float (t_4.arr).((j_10 * t_4.n) + (! t_3))) <
               (abs_float (t_4.arr).(((! t_9) * t_4.n) + (! t_3))))) then
         (t_9 := j_10)
        else ()
       else ()
      done;
      if ((! t_9) == (-1)) then (None) else (Some (! t_9))
     end in
    (match t_11 with
     | Some (i_12) ->
        if (i_12 <> (! t_2)) then
         let a_13 = t_4.arr
         and n_14 = t_4.n
         and m_15 = t_4.m in
         let i1_16 = ((! t_2) * n_14)
         and i2_17 = (i_12 * n_14) in
         for i_18 = 0 to (m_15 - 1) do
          let t_19 = a_13.(i1_16 + i_18) in
          a_13.(i2_17 + i_18) <- a_13.(i1_16 + i_18);
          a_13.(i1_16 + i_18) <- t_19
         done
        else ();
        begin
         for j_20 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_20 * t_4.n) + (! t_3)) = 0.)) then begin
           for j_21 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_20 * t_4.n) + j_21) <-
             ((((t_4.arr).((j_20 * t_4.n) + j_21) *.
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) -.
                ((t_4.arr).(((! t_2) * t_4.n) + j_21) *.
                  (t_4.arr).((j_20 * t_4.n) + (! t_3)))) /.
               (t_4.arr).(((! t_2) * t_4.n) + j_21))
           done;
           (t_4.arr).((j_20 * t_4.n) + (! t_3)) <- 0.
          end else ()
         done;
         (t_7 := (t_4.arr).(((! t_2) * t_4.n) + (! t_3)))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0.));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, ((! t_8) *. (! t_7)), (! t_2))>.
# val resIA1 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutJustMatrix(Funct4.IArrayContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (ref (-1)) in
    let t_10 =
     begin
      for j_9 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_9)).(! t_3) = 0)) then
        if (((! t_8) == (-1)) ||
             ((abs (t_5.(j_9)).(! t_3)) < (abs (t_5.(! t_8)).(! t_3)))) then
         (t_8 := j_9)
        else ()
       else ()
      done;
      if ((! t_8) == (-1)) then (None) else (Some (! t_8))
     end in
    (match t_10 with
     | Some (i_11) ->
        if (i_11 <> (! t_2)) then
         let t_12 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_11);
         t_5.(i_11) <- t_12
        else ();
        begin
         for j_13 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_13)).(! t_3) = 0)) then begin
           for j_14 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_13)).(j_14) <-
             ((((t_5.(j_13)).(! t_3) / (t_5.(! t_2)).(! t_3)) *
                (t_5.(! t_2)).(j_14)) - (t_5.(j_13)).(j_14))
           done;
           (t_5.(j_13)).(! t_3) <- 0
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resIA2 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutDet(Funct4.IArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (ref (-1)) in
    let t_12 =
     begin
      for j_11 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_11)).(! t_3) = 0)) then
        if (((! t_10) == (-1)) ||
             ((abs (t_5.(j_11)).(! t_3)) < (abs (t_5.(! t_10)).(! t_3)))) then
         (t_10 := j_11)
        else ()
       else ()
      done;
      if ((! t_10) == (-1)) then (None) else (Some (! t_10))
     end in
    (match t_12 with
     | Some (i_13) ->
        if (i_13 <> (! t_2)) then
         let t_14 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_13);
         t_5.(i_13) <- t_14
        else ();
        begin
         for j_15 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_15)).(! t_3) = 0)) then begin
           for j_16 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_15)).(j_16) <-
             ((((t_5.(j_15)).(! t_3) / (t_5.(! t_2)).(! t_3)) *
                (t_5.(! t_2)).(j_16)) - (t_5.(j_15)).(j_16))
           done;
           (t_5.(j_15)).(! t_3) <- 0
          end else ()
         done;
         (t_8 := ((! t_8) * (t_5.(! t_2)).(! t_3)))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, ((! t_9) * (! t_8)))>.
# val resIA3 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutRank(Funct4.IArrayContainer)(Funct4.Rank)(Funct4.IntegerDomain).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (ref (-1)) in
    let t_10 =
     begin
      for j_9 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_9)).(! t_3) = 0)) then
        if (((! t_8) == (-1)) ||
             ((abs (t_5.(j_9)).(! t_3)) < (abs (t_5.(! t_8)).(! t_3)))) then
         (t_8 := j_9)
        else ()
       else ()
      done;
      if ((! t_8) == (-1)) then (None) else (Some (! t_8))
     end in
    (match t_10 with
     | Some (i_11) ->
        if (i_11 <> (! t_2)) then
         let t_12 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_11);
         t_5.(i_11) <- t_12
        else ();
        begin
         for j_13 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_13)).(! t_3) = 0)) then begin
           for j_14 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_13)).(j_14) <-
             ((((t_5.(j_13)).(! t_3) / (t_5.(! t_2)).(! t_3)) *
                (t_5.(! t_2)).(j_14)) - (t_5.(j_13)).(j_14))
           done;
           (t_5.(j_13)).(! t_3) <- 0
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
# val resIA4 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutDetRank(Funct4.IArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   let t_8 = (ref 1) in
   let t_9 = (ref 1) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_10 = (ref (-1)) in
    let t_12 =
     begin
      for j_11 = (! t_2) to (t_7 - 1) do
       if (not ((t_5.(j_11)).(! t_3) = 0)) then
        if (((! t_10) == (-1)) ||
             ((abs (t_5.(j_11)).(! t_3)) < (abs (t_5.(! t_10)).(! t_3)))) then
         (t_10 := j_11)
        else ()
       else ()
      done;
      if ((! t_10) == (-1)) then (None) else (Some (! t_10))
     end in
    (match t_12 with
     | Some (i_13) ->
        if (i_13 <> (! t_2)) then
         let t_14 = t_5.(! t_2) in
         t_5.(! t_2) <- t_5.(i_13);
         t_5.(i_13) <- t_14
        else ();
        begin
         for j_15 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_15)).(! t_3) = 0)) then begin
           for j_16 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_15)).(j_16) <-
             ((((t_5.(j_15)).(j_16) * (t_5.(! t_2)).(! t_3)) -
                ((t_5.(! t_2)).(j_16) * (t_5.(j_15)).(! t_3))) /
               (t_5.(! t_2)).(j_16))
           done;
           (t_5.(j_15)).(! t_3) <- 0
          end else ()
         done;
         (t_8 := (t_5.(! t_2)).(! t_3))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, ((! t_9) * (! t_8)), (! t_2))>.
# val resIV1 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutJustMatrix(Funct4.IVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (ref (-1)) in
    let t_9 =
     begin
      for j_8 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_8 * t_4.n) + (! t_3)) = 0)) then
        if (((! t_7) == (-1)) ||
             ((abs (t_4.arr).((j_8 * t_4.n) + (! t_3))) <
               (abs (t_4.arr).(((! t_7) * t_4.n) + (! t_3))))) then
         (t_7 := j_8)
        else ()
       else ()
      done;
      if ((! t_7) == (-1)) then (None) else (Some (! t_7))
     end in
    (match t_9 with
     | Some (i_10) ->
        if (i_10 <> (! t_2)) then
         let a_11 = t_4.arr
         and n_12 = t_4.n
         and m_13 = t_4.m in
         let i1_14 = ((! t_2) * n_12)
         and i2_15 = (i_10 * n_12) in
         for i_16 = 0 to (m_13 - 1) do
          let t_17 = a_11.(i1_14 + i_16) in
          a_11.(i2_15 + i_16) <- a_11.(i1_14 + i_16);
          a_11.(i1_14 + i_16) <- t_17
         done
        else ();
        begin
         for j_18 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_18 * t_4.n) + (! t_3)) = 0)) then begin
           for j_19 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_18 * t_4.n) + j_19) <-
             ((((t_4.arr).((j_18 * t_4.n) + (! t_3)) /
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) *
                (t_4.arr).(((! t_2) * t_4.n) + j_19)) -
               (t_4.arr).((j_18 * t_4.n) + j_19))
           done;
           (t_4.arr).((j_18 * t_4.n) + (! t_3)) <- 0
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_4>.
# val resIV2 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutDet(Funct4.IVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (ref (-1)) in
    let t_11 =
     begin
      for j_10 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_10 * t_4.n) + (! t_3)) = 0)) then
        if (((! t_9) == (-1)) ||
             ((abs (t_4.arr).((j_10 * t_4.n) + (! t_3))) <
               (abs (t_4.arr).(((! t_9) * t_4.n) + (! t_3))))) then
         (t_9 := j_10)
        else ()
       else ()
      done;
      if ((! t_9) == (-1)) then (None) else (Some (! t_9))
     end in
    (match t_11 with
     | Some (i_12) ->
        if (i_12 <> (! t_2)) then
         let a_13 = t_4.arr
         and n_14 = t_4.n
         and m_15 = t_4.m in
         let i1_16 = ((! t_2) * n_14)
         and i2_17 = (i_12 * n_14) in
         for i_18 = 0 to (m_15 - 1) do
          let t_19 = a_13.(i1_16 + i_18) in
          a_13.(i2_17 + i_18) <- a_13.(i1_16 + i_18);
          a_13.(i1_16 + i_18) <- t_19
         done
        else ();
        begin
         for j_20 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_20 * t_4.n) + (! t_3)) = 0)) then begin
           for j_21 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_20 * t_4.n) + j_21) <-
             ((((t_4.arr).((j_20 * t_4.n) + (! t_3)) /
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) *
                (t_4.arr).(((! t_2) * t_4.n) + j_21)) -
               (t_4.arr).((j_20 * t_4.n) + j_21))
           done;
           (t_4.arr).((j_20 * t_4.n) + (! t_3)) <- 0
          end else ()
         done;
         (t_7 := ((! t_7) * (t_4.arr).(((! t_2) * t_4.n) + (! t_3))))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, ((! t_8) * (! t_7)))>.
# val resIV3 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutRank(Funct4.IVectorContainer)(Funct4.Rank)(Funct4.IntegerDomain).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_7 = (ref (-1)) in
    let t_9 =
     begin
      for j_8 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_8 * t_4.n) + (! t_3)) = 0)) then
        if (((! t_7) == (-1)) ||
             ((abs (t_4.arr).((j_8 * t_4.n) + (! t_3))) <
               (abs (t_4.arr).(((! t_7) * t_4.n) + (! t_3))))) then
         (t_7 := j_8)
        else ()
       else ()
      done;
      if ((! t_7) == (-1)) then (None) else (Some (! t_7))
     end in
    (match t_9 with
     | Some (i_10) ->
        if (i_10 <> (! t_2)) then
         let a_11 = t_4.arr
         and n_12 = t_4.n
         and m_13 = t_4.m in
         let i1_14 = ((! t_2) * n_12)
         and i2_15 = (i_10 * n_12) in
         for i_16 = 0 to (m_13 - 1) do
          let t_17 = a_11.(i1_14 + i_16) in
          a_11.(i2_15 + i_16) <- a_11.(i1_14 + i_16);
          a_11.(i1_14 + i_16) <- t_17
         done
        else ();
        begin
         for j_18 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_18 * t_4.n) + (! t_3)) = 0)) then begin
           for j_19 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_18 * t_4.n) + j_19) <-
             ((((t_4.arr).((j_18 * t_4.n) + (! t_3)) /
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) *
                (t_4.arr).(((! t_2) * t_4.n) + j_19)) -
               (t_4.arr).((j_18 * t_4.n) + j_19))
           done;
           (t_4.arr).((j_18 * t_4.n) + (! t_3)) <- 0
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
# val resIV4 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutDetRank(Funct4.IVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = (ref 0) in
   let t_4 = {arr = (Array.copy a_1.arr)} (a_1) in
   let t_5 = a_1.m in
   let t_6 = a_1.n in
   let t_7 = (ref 1) in
   let t_8 = (ref 1) in
   while (((! t_2) < t_5) && ((! t_2) < t_6)) do
    let t_9 = (ref (-1)) in
    let t_11 =
     begin
      for j_10 = (! t_2) to (t_6 - 1) do
       if (not ((t_4.arr).((j_10 * t_4.n) + (! t_3)) = 0)) then
        if (((! t_9) == (-1)) ||
             ((abs (t_4.arr).((j_10 * t_4.n) + (! t_3))) <
               (abs (t_4.arr).(((! t_9) * t_4.n) + (! t_3))))) then
         (t_9 := j_10)
        else ()
       else ()
      done;
      if ((! t_9) == (-1)) then (None) else (Some (! t_9))
     end in
    (match t_11 with
     | Some (i_12) ->
        if (i_12 <> (! t_2)) then
         let a_13 = t_4.arr
         and n_14 = t_4.n
         and m_15 = t_4.m in
         let i1_16 = ((! t_2) * n_14)
         and i2_17 = (i_12 * n_14) in
         for i_18 = 0 to (m_15 - 1) do
          let t_19 = a_13.(i1_16 + i_18) in
          a_13.(i2_17 + i_18) <- a_13.(i1_16 + i_18);
          a_13.(i1_16 + i_18) <- t_19
         done
        else ();
        begin
         for j_20 = ((! t_2) + 1) to (t_6 - 1) do
          if (not ((t_4.arr).((j_20 * t_4.n) + (! t_3)) = 0)) then begin
           for j_21 = ((! t_3) + 1) to (t_5 - 1) do
            (t_4.arr).((j_20 * t_4.n) + j_21) <-
             ((((t_4.arr).((j_20 * t_4.n) + j_21) *
                 (t_4.arr).(((! t_2) * t_4.n) + (! t_3))) -
                ((t_4.arr).(((! t_2) * t_4.n) + j_21) *
                  (t_4.arr).((j_20 * t_4.n) + (! t_3)))) /
               (t_4.arr).(((! t_2) * t_4.n) + j_21))
           done;
           (t_4.arr).((j_20 * t_4.n) + (! t_3)) <- 0
          end else ()
         done;
         (t_7 := (t_4.arr).(((! t_2) * t_4.n) + (! t_3)))
        end;
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, ((! t_8) * (! t_7)), (! t_2))>.
#   val rFA1 :
  Funct4.FArrayContainer.contr ->
  Funct4.OutJustMatrix(Funct4.FArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res =
  <fun>
# val rFA2 :
  Funct4.FArrayContainer.contr ->
  Funct4.OutDet(Funct4.FArrayContainer)(Funct4.FDet).res = <fun>
# val rFA3 :
  Funct4.FArrayContainer.contr ->
  Funct4.OutRank(Funct4.FArrayContainer)(Funct4.Rank)(Funct4.FloatDomain).res =
  <fun>
# val rFA4 :
  Funct4.FArrayContainer.contr ->
  Funct4.OutDetRank(Funct4.FArrayContainer)(Funct4.FDet)(Funct4.Rank).res =
  <fun>
# val rFV1 :
  Funct4.FVectorContainer.contr ->
  Funct4.OutJustMatrix(Funct4.FVectorContainer)(Funct4.NoDet(Funct4.FloatDomain)).res =
  <fun>
# val rFV2 :
  Funct4.FVectorContainer.contr ->
  Funct4.OutDet(Funct4.FVectorContainer)(Funct4.FDet).res = <fun>
# val rFV3 :
  Funct4.FVectorContainer.contr ->
  Funct4.OutRank(Funct4.FVectorContainer)(Funct4.Rank)(Funct4.FloatDomain).res =
  <fun>
# val rFV4 :
  Funct4.FVectorContainer.contr ->
  Funct4.OutDetRank(Funct4.FVectorContainer)(Funct4.FDet)(Funct4.Rank).res =
  <fun>
# val rIA1 :
  Funct4.IArrayContainer.contr ->
  Funct4.OutJustMatrix(Funct4.IArrayContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res =
  <fun>
# val rIA2 :
  Funct4.IArrayContainer.contr ->
  Funct4.OutDet(Funct4.IArrayContainer)(Funct4.IDet).res = <fun>
# val rIA3 :
  Funct4.IArrayContainer.contr ->
  Funct4.OutRank(Funct4.IArrayContainer)(Funct4.Rank)(Funct4.IntegerDomain).res =
  <fun>
# val rIA4 :
  Funct4.IArrayContainer.contr ->
  Funct4.OutDetRank(Funct4.IArrayContainer)(Funct4.IDet)(Funct4.Rank).res =
  <fun>
# val rIV1 :
  Funct4.IVectorContainer.contr ->
  Funct4.OutJustMatrix(Funct4.IVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res =
  <fun>
# val rIV2 :
  Funct4.IVectorContainer.contr ->
  Funct4.OutDet(Funct4.IVectorContainer)(Funct4.IDet).res = <fun>
# val rIV3 :
  Funct4.IVectorContainer.contr ->
  Funct4.OutRank(Funct4.IVectorContainer)(Funct4.Rank)(Funct4.IntegerDomain).res =
  <fun>
# val rIV4 :
  Funct4.IVectorContainer.contr ->
  Funct4.OutDetRank(Funct4.IVectorContainer)(Funct4.IDet)(Funct4.Rank).res =
  <fun>
# 
