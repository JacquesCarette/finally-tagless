        MetaOCaml version 3.08.0 alpha 015

# #   val res1 : ('a, Funct4.FArrayContainer.contr -> Funct4.NoFDetOutput.res) code =
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
    (match t_10 with | Some (i_11) -> () | None -> ());
    (t_2 := ((! t_2) + 1))
   done;
   t_5>.
# val res2 : ('a, Funct4.FArrayContainer.contr -> Funct4.FDetOutput.res) code =
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
    (match t_12 with | Some (i_13) -> () | None -> ());
    (t_2 := ((! t_2) + 1))
   done;
   (t_5, (! t_8))>.
# val res2 : ('a, Funct4.IArrayContainer.contr -> Funct4.NoIDetOutput.res) code =
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
    (match t_10 with | Some (i_11) -> () | None -> ());
    (t_2 := ((! t_2) + 1))
   done;
   t_5>.
# val res2 : ('a, Funct4.IArrayContainer.contr -> Funct4.IDetOutput.res) code =
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
    (match t_12 with | Some (i_13) -> () | None -> ());
    (t_2 := ((! t_2) + 1))
   done;
   (t_5, (! t_8))>.
#   val r1 : Funct4.FArrayContainer.contr -> Funct4.NoFDetOutput.res = <fun>
# val r2 : Funct4.FArrayContainer.contr -> Funct4.NoFDetOutput.res = <fun>
# val r3 : Funct4.FArrayContainer.contr -> Funct4.NoFDetOutput.res = <fun>
# val r4 : Funct4.FArrayContainer.contr -> Funct4.NoFDetOutput.res = <fun>
# 
