        MetaOCaml version 3.08.0 alpha 015

# #   - : ('a, Funct4.ArrayContainer.contr -> Funct4.NoDetOutput.res) code =
.<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_2) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (ref (-1)) in
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
    end;
    (t_2 := ((! t_2) + 1))
   done;
   t_5>.
# 
