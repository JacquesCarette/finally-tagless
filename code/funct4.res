        MetaOCaml version 3.08.0 alpha 015

# #   val res1 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutJustMatrix(Funct4.FArrayContainer).res)
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
         for j_12 = (! t_3) to (t_6 - 1) do
          let t_13 = (t_5.(i_11)).(j_12) in
          (t_5.(i_11)).(j_12) <- (t_5.(! t_2)).(j_12);
          (t_5.(! t_2)).(j_12) <- t_13
         done
        else ();
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val res2 :
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
         for j_14 = (! t_3) to (t_6 - 1) do
          let t_15 = (t_5.(i_13)).(j_14) in
          (t_5.(i_13)).(j_14) <- (t_5.(! t_2)).(j_14);
          (t_5.(! t_2)).(j_14) <- t_15
         done
        else ();
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0.));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_8))>.
# val res3 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutRank(Funct4.IArrayContainer)(Funct4.Rank).res)
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
         for j_12 = (! t_3) to (t_6 - 1) do
          let t_13 = (t_5.(i_11)).(j_12) in
          (t_5.(i_11)).(j_12) <- (t_5.(! t_2)).(j_12);
          (t_5.(! t_2)).(j_12) <- t_13
         done
        else ();
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_2))>.
# val res4 :
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
         for j_14 = (! t_3) to (t_6 - 1) do
          let t_15 = (t_5.(i_13)).(j_14) in
          (t_5.(i_13)).(j_14) <- (t_5.(! t_2)).(j_14);
          (t_5.(! t_2)).(j_14) <- t_15
         done
        else ();
        (t_2 := ((! t_2) + 1))
     | None -> (t_9 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_5, (! t_8), (! t_2))>.
# val res5 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutRank(Funct4.IVectorContainer)(Funct4.Rank).res)
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
         for j_11 = (! t_3) to (t_5 - 1) do
          let t_12 = (t_4.arr).((i_10 * t_4.n) + j_11) in
          (t_4.arr).((i_10 * t_4.n) + j_11) <-
           (t_4.arr).(((! t_2) * t_4.n) + j_11);
          (t_4.arr).(((! t_2) * t_4.n) + j_11) <- t_12
         done
        else ();
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_2))>.
# val res6 :
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
         for j_13 = (! t_3) to (t_5 - 1) do
          let t_14 = (t_4.arr).((i_12 * t_4.n) + j_13) in
          (t_4.arr).((i_12 * t_4.n) + j_13) <-
           (t_4.arr).(((! t_2) * t_4.n) + j_13);
          (t_4.arr).(((! t_2) * t_4.n) + j_13) <- t_14
         done
        else ();
        (t_2 := ((! t_2) + 1))
     | None -> (t_8 := 0));
    (t_3 := ((! t_3) + 1))
   done;
   (t_4, (! t_7), (! t_2))>.
#   val r1 :
  Funct4.FArrayContainer.contr ->
  Funct4.OutJustMatrix(Funct4.FArrayContainer).res = <fun>
# val r2 :
  Funct4.FArrayContainer.contr ->
  Funct4.OutDet(Funct4.FArrayContainer)(Funct4.FDet).res = <fun>
# val r3 :
  Funct4.IArrayContainer.contr ->
  Funct4.OutRank(Funct4.IArrayContainer)(Funct4.Rank).res = <fun>
# val r4 :
  Funct4.IArrayContainer.contr ->
  Funct4.OutDetRank(Funct4.IArrayContainer)(Funct4.IDet)(Funct4.Rank).res =
  <fun>
# val r5 :
  Funct4.IVectorContainer.contr ->
  Funct4.OutRank(Funct4.IVectorContainer)(Funct4.Rank).res = <fun>
# val r6 :
  Funct4.IVectorContainer.contr ->
  Funct4.OutDetRank(Funct4.IVectorContainer)(Funct4.IDet)(Funct4.Rank).res =
  <fun>
# 
