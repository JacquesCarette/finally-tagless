        Objective Caml version 3.08.0

# #   val resFA1 :
  ('a,
   Funct4.GenFA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_1 ->
   let t_2 = (ref 0) in
   let t_3 = (ref 0) in
   let t_5 = (Array.map (fun x_4 -> (Array.copy x_4)) (Array.copy a_1)) in
   let t_6 = (Array.length a_1.(0)) in
   let t_7 = (Array.length a_1) in
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
    let t_8 = (ref (None)) in
    let t_14 =
     begin
      for j_11 = (! t_2) to (t_7 - 1) do
       let t_12 = (t_5.(j_11)).(! t_3) in
       if (not (t_12 = 0.)) then
        (match (! t_8) with
         | Some (i_13) ->
            if ((abs_float (snd i_13)) < (abs_float t_12)) then
             (t_8 := (Some (j_11, t_12)))
            else ()
         | None -> (t_8 := (Some (j_11, t_12))))
       else ()
      done;
      (match (! t_8) with
       | Some (i_9) ->
          if ((fst i_9) <> (! t_2)) then begin
           let t_10 = t_5.(! t_2) in
           t_5.(! t_2) <- t_5.(fst i_9);
           t_5.(fst i_9) <- t_10;
           ()
          end else ();
          (Some (snd i_9))
       | None -> (None))
     end in
    (match t_14 with
     | Some (i_15) ->
        begin
         for j_16 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_16)).(! t_3) = 0.)) then begin
           for j_17 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_16)).(j_17) <-
             ((t_5.(j_16)).(j_17) -.
               (((t_5.(j_16)).(! t_3) /. (t_5.(! t_2)).(! t_3)) *.
                 (t_5.(! t_2)).(j_17)))
           done;
           (t_5.(j_16)).(! t_3) <- 0.
          end else ()
         done;
         ()
        end;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resFA11 :
  ('a,
   Funct4.GenFA11.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_18 ->
   let t_19 = (ref 0) in
   let t_20 = (ref 0) in
   let t_22 = (Array.map (fun x_21 -> (Array.copy x_21)) (Array.copy a_18)) in
   let t_23 = (Array.length a_18.(0)) in
   let t_24 = (Array.length a_18) in
   while (((! t_20) < t_23) && ((! t_19) < t_24)) do
    let t_25 = (ref (None)) in
    let t_32 =
     begin
      for j_28 = (! t_19) to (t_24 - 1) do
       for j_29 = (! t_20) to (t_23 - 1) do
        let t_30 = (t_22.(j_28)).(j_29) in
        if (not (t_30 = 0.)) then
         (match (! t_25) with
          | Some (i_31) ->
             if ((abs_float (snd i_31)) < (abs_float t_30)) then
              (t_25 := (Some ((j_28, j_29), t_30)))
             else ()
          | None -> (t_25 := (Some ((j_28, j_29), t_30))))
        else ()
       done
      done;
      (match (! t_25) with
       | Some (i_26) ->
          if ((snd (fst i_26)) <> (! t_20)) then begin
           (failwith "swap_cols_stmt not yet implemeted"); ()
          end else ();
          if ((snd (fst i_26)) <> (! t_20)) then begin
           let t_27 = t_22.(! t_19) in
           t_22.(! t_19) <- t_22.(fst (fst i_26));
           t_22.(fst (fst i_26)) <- t_27;
           ()
          end else ();
          (Some (snd i_26))
       | None -> (None))
     end in
    (match t_32 with
     | Some (i_33) ->
        begin
         for j_34 = ((! t_19) + 1) to (t_24 - 1) do
          if (not ((t_22.(j_34)).(! t_20) = 0.)) then begin
           for j_35 = ((! t_20) + 1) to (t_23 - 1) do
            (t_22.(j_34)).(j_35) <-
             ((t_22.(j_34)).(j_35) -.
               (((t_22.(j_34)).(! t_20) /. (t_22.(! t_19)).(! t_20)) *.
                 (t_22.(! t_19)).(j_35)))
           done;
           (t_22.(j_34)).(! t_20) <- 0.
          end else ()
         done;
         ()
        end;
        (t_19 := ((! t_19) + 1))
     | None -> ());
    (t_20 := ((! t_20) + 1))
   done;
   t_22>.
# val resFA2 :
  ('a,
   Funct4.GenFA2.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_36 ->
   let t_37 = (ref 0) in
   let t_38 = (ref 0) in
   let t_40 = (Array.map (fun x_39 -> (Array.copy x_39)) (Array.copy a_36)) in
   let t_41 = (Array.length a_36.(0)) in
   let t_42 = (Array.length a_36) in
   let t_43 = (ref 1.) in
   let t_44 = (ref 1) in
   while (((! t_38) < t_41) && ((! t_37) < t_42)) do
    let t_45 = (ref (None)) in
    let t_51 =
     begin
      for j_48 = (! t_37) to (t_42 - 1) do
       let t_49 = (t_40.(j_48)).(! t_38) in
       if (not (t_49 = 0.)) then
        (match (! t_45) with
         | Some (i_50) ->
            if ((abs_float (snd i_50)) < (abs_float t_49)) then
             (t_45 := (Some (j_48, t_49)))
            else ()
         | None -> (t_45 := (Some (j_48, t_49))))
       else ()
      done;
      (match (! t_45) with
       | Some (i_46) ->
          if ((fst i_46) <> (! t_37)) then begin
           let t_47 = t_40.(! t_37) in
           t_40.(! t_37) <- t_40.(fst i_46);
           t_40.(fst i_46) <- t_47;
           (t_44 := (~- (! t_44)))
          end else ();
          (Some (snd i_46))
       | None -> (None))
     end in
    (match t_51 with
     | Some (i_52) ->
        begin
         for j_53 = ((! t_37) + 1) to (t_42 - 1) do
          if (not ((t_40.(j_53)).(! t_38) = 0.)) then begin
           for j_54 = ((! t_38) + 1) to (t_41 - 1) do
            (t_40.(j_53)).(j_54) <-
             ((t_40.(j_53)).(j_54) -.
               (((t_40.(j_53)).(! t_38) /. (t_40.(! t_37)).(! t_38)) *.
                 (t_40.(! t_37)).(j_54)))
           done;
           (t_40.(j_53)).(! t_38) <- 0.
          end else ()
         done;
         (t_43 := ((! t_43) *. (t_40.(! t_37)).(! t_38)))
        end;
        (t_37 := ((! t_37) + 1))
     | None -> (t_44 := 0));
    (t_38 := ((! t_38) + 1))
   done;
   (t_40,
    if ((! t_44) = 0) then 0.
    else if ((! t_44) = 1) then (! t_43)
    else (0. -. (! t_43)))>.
# val resFA3 :
  ('a,
   Funct4.GenFA3.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_55 ->
   let t_56 = (ref 0) in
   let t_57 = (ref 0) in
   let t_59 = (Array.map (fun x_58 -> (Array.copy x_58)) (Array.copy a_55)) in
   let t_60 = (Array.length a_55.(0)) in
   let t_61 = (Array.length a_55) in
   while (((! t_57) < t_60) && ((! t_56) < t_61)) do
    let t_62 = (ref (None)) in
    let t_68 =
     begin
      for j_65 = (! t_56) to (t_61 - 1) do
       let t_66 = (t_59.(j_65)).(! t_57) in
       if (not (t_66 = 0.)) then
        (match (! t_62) with
         | Some (i_67) ->
            if ((abs_float (snd i_67)) < (abs_float t_66)) then
             (t_62 := (Some (j_65, t_66)))
            else ()
         | None -> (t_62 := (Some (j_65, t_66))))
       else ()
      done;
      (match (! t_62) with
       | Some (i_63) ->
          if ((fst i_63) <> (! t_56)) then begin
           let t_64 = t_59.(! t_56) in
           t_59.(! t_56) <- t_59.(fst i_63);
           t_59.(fst i_63) <- t_64;
           ()
          end else ();
          (Some (snd i_63))
       | None -> (None))
     end in
    (match t_68 with
     | Some (i_69) ->
        begin
         for j_70 = ((! t_56) + 1) to (t_61 - 1) do
          if (not ((t_59.(j_70)).(! t_57) = 0.)) then begin
           for j_71 = ((! t_57) + 1) to (t_60 - 1) do
            (t_59.(j_70)).(j_71) <-
             ((t_59.(j_70)).(j_71) -.
               (((t_59.(j_70)).(! t_57) /. (t_59.(! t_56)).(! t_57)) *.
                 (t_59.(! t_56)).(j_71)))
           done;
           (t_59.(j_70)).(! t_57) <- 0.
          end else ()
         done;
         ()
        end;
        (t_56 := ((! t_56) + 1))
     | None -> ());
    (t_57 := ((! t_57) + 1))
   done;
   (t_59, (! t_56))>.
# val resFA4 :
  ('a,
   Funct4.GenFA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_72 ->
   let t_73 = (ref 0) in
   let t_74 = (ref 0) in
   let t_76 = (Array.map (fun x_75 -> (Array.copy x_75)) (Array.copy a_72)) in
   let t_77 = (Array.length a_72.(0)) in
   let t_78 = (Array.length a_72) in
   let t_79 = (ref 1.) in
   let t_80 = (ref 1) in
   while (((! t_74) < t_77) && ((! t_73) < t_78)) do
    let t_81 = (ref (None)) in
    let t_87 =
     begin
      for j_84 = (! t_73) to (t_78 - 1) do
       let t_85 = (t_76.(j_84)).(! t_74) in
       if (not (t_85 = 0.)) then
        (match (! t_81) with
         | Some (i_86) ->
            if ((abs_float (snd i_86)) < (abs_float t_85)) then
             (t_81 := (Some (j_84, t_85)))
            else ()
         | None -> (t_81 := (Some (j_84, t_85))))
       else ()
      done;
      (match (! t_81) with
       | Some (i_82) ->
          if ((fst i_82) <> (! t_73)) then begin
           let t_83 = t_76.(! t_73) in
           t_76.(! t_73) <- t_76.(fst i_82);
           t_76.(fst i_82) <- t_83;
           (t_80 := (~- (! t_80)))
          end else ();
          (Some (snd i_82))
       | None -> (None))
     end in
    (match t_87 with
     | Some (i_88) ->
        begin
         for j_89 = ((! t_73) + 1) to (t_78 - 1) do
          if (not ((t_76.(j_89)).(! t_74) = 0.)) then begin
           for j_90 = ((! t_74) + 1) to (t_77 - 1) do
            (t_76.(j_89)).(j_90) <-
             ((t_76.(j_89)).(j_90) -.
               (((t_76.(j_89)).(! t_74) /. (t_76.(! t_73)).(! t_74)) *.
                 (t_76.(! t_73)).(j_90)))
           done;
           (t_76.(j_89)).(! t_74) <- 0.
          end else ()
         done;
         (t_79 := ((! t_79) *. (t_76.(! t_73)).(! t_74)))
        end;
        (t_73 := ((! t_73) + 1))
     | None -> (t_80 := 0));
    (t_74 := ((! t_74) + 1))
   done;
   (t_76,
    if ((! t_80) = 0) then 0.
    else if ((! t_80) = 1) then (! t_79)
    else (0. -. (! t_79)), (! t_73))>.
# val resFV1 :
  ('a,
   Funct4.GenFV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_91 ->
   let t_92 = (ref 0) in
   let t_93 = (ref 0) in
   let t_94 = {arr = (Array.copy a_91.arr)} (a_91) in
   let t_95 = a_91.m in
   let t_96 = a_91.n in
   let t_97 = (ref 1.) in
   let t_98 = (ref 1) in
   while (((! t_93) < t_95) && ((! t_92) < t_96)) do
    let t_99 = (ref (None)) in
    let t_111 =
     begin
      for j_108 = (! t_92) to (t_96 - 1) do
       let t_109 = (t_94.arr).((j_108 * t_94.n) + (! t_93)) in
       if (not (t_109 = 0.)) then
        (match (! t_99) with
         | Some (i_110) ->
            if ((abs_float (snd i_110)) < (abs_float t_109)) then
             (t_99 := (Some (j_108, t_109)))
            else ()
         | None -> (t_99 := (Some (j_108, t_109))))
       else ()
      done;
      (match (! t_99) with
       | Some (i_100) ->
          if ((fst i_100) <> (! t_92)) then begin
           let a_101 = t_94.arr
           and n_102 = t_94.n
           and m_103 = t_94.m in
           let i1_104 = ((! t_92) * n_102)
           and i2_105 = ((fst i_100) * n_102) in
           for i_106 = 0 to (m_103 - 1) do
            let t_107 = a_101.(i1_104 + i_106) in
            a_101.(i2_105 + i_106) <- a_101.(i1_104 + i_106);
            a_101.(i1_104 + i_106) <- t_107
           done;
           (t_98 := (~- (! t_98)))
          end else ();
          (Some (snd i_100))
       | None -> (None))
     end in
    (match t_111 with
     | Some (i_112) ->
        begin
         for j_113 = ((! t_92) + 1) to (t_96 - 1) do
          if (not ((t_94.arr).((j_113 * t_94.n) + (! t_93)) = 0.)) then begin
           for j_114 = ((! t_93) + 1) to (t_95 - 1) do
            (t_94.arr).((j_113 * t_94.n) + j_114) <-
             ((t_94.arr).((j_113 * t_94.n) + j_114) -.
               (((t_94.arr).((j_113 * t_94.n) + (! t_93)) /.
                  (t_94.arr).(((! t_92) * t_94.n) + (! t_93))) *.
                 (t_94.arr).(((! t_92) * t_94.n) + j_114)))
           done;
           (t_94.arr).((j_113 * t_94.n) + (! t_93)) <- 0.
          end else ()
         done;
         (t_97 := ((! t_97) *. (t_94.arr).(((! t_92) * t_94.n) + (! t_93))))
        end;
        (t_92 := ((! t_92) + 1))
     | None -> (t_98 := 0));
    (t_93 := ((! t_93) + 1))
   done;
   t_94>.
# val resFV2 :
  ('a,
   Funct4.GenFV2.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_115 ->
   let t_116 = (ref 0) in
   let t_117 = (ref 0) in
   let t_118 = {arr = (Array.copy a_115.arr)} (a_115) in
   let t_119 = a_115.m in
   let t_120 = a_115.n in
   let t_121 = (ref 1.) in
   let t_122 = (ref 1) in
   while (((! t_117) < t_119) && ((! t_116) < t_120)) do
    let t_123 = (ref (None)) in
    let t_135 =
     begin
      for j_132 = (! t_116) to (t_120 - 1) do
       let t_133 = (t_118.arr).((j_132 * t_118.n) + (! t_117)) in
       if (not (t_133 = 0.)) then
        (match (! t_123) with
         | Some (i_134) ->
            if ((abs_float (snd i_134)) < (abs_float t_133)) then
             (t_123 := (Some (j_132, t_133)))
            else ()
         | None -> (t_123 := (Some (j_132, t_133))))
       else ()
      done;
      (match (! t_123) with
       | Some (i_124) ->
          if ((fst i_124) <> (! t_116)) then begin
           let a_125 = t_118.arr
           and n_126 = t_118.n
           and m_127 = t_118.m in
           let i1_128 = ((! t_116) * n_126)
           and i2_129 = ((fst i_124) * n_126) in
           for i_130 = 0 to (m_127 - 1) do
            let t_131 = a_125.(i1_128 + i_130) in
            a_125.(i2_129 + i_130) <- a_125.(i1_128 + i_130);
            a_125.(i1_128 + i_130) <- t_131
           done;
           (t_122 := (~- (! t_122)))
          end else ();
          (Some (snd i_124))
       | None -> (None))
     end in
    (match t_135 with
     | Some (i_136) ->
        begin
         for j_137 = ((! t_116) + 1) to (t_120 - 1) do
          if (not ((t_118.arr).((j_137 * t_118.n) + (! t_117)) = 0.)) then begin
           for j_138 = ((! t_117) + 1) to (t_119 - 1) do
            (t_118.arr).((j_137 * t_118.n) + j_138) <-
             ((t_118.arr).((j_137 * t_118.n) + j_138) -.
               (((t_118.arr).((j_137 * t_118.n) + (! t_117)) /.
                  (t_118.arr).(((! t_116) * t_118.n) + (! t_117))) *.
                 (t_118.arr).(((! t_116) * t_118.n) + j_138)))
           done;
           (t_118.arr).((j_137 * t_118.n) + (! t_117)) <- 0.
          end else ()
         done;
         (t_121 :=
           ((! t_121) *. (t_118.arr).(((! t_116) * t_118.n) + (! t_117))))
        end;
        (t_116 := ((! t_116) + 1))
     | None -> (t_122 := 0));
    (t_117 := ((! t_117) + 1))
   done;
   (t_118,
    if ((! t_122) = 0) then 0.
    else if ((! t_122) = 1) then (! t_121)
    else (0. -. (! t_121)))>.
# val resFV3 :
  ('a,
   Funct4.GenFV3.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_139 ->
   let t_140 = (ref 0) in
   let t_141 = (ref 0) in
   let t_142 = {arr = (Array.copy a_139.arr)} (a_139) in
   let t_143 = a_139.m in
   let t_144 = a_139.n in
   while (((! t_141) < t_143) && ((! t_140) < t_144)) do
    let t_145 = (ref (None)) in
    let t_157 =
     begin
      for j_154 = (! t_140) to (t_144 - 1) do
       let t_155 = (t_142.arr).((j_154 * t_142.n) + (! t_141)) in
       if (not (t_155 = 0.)) then
        (match (! t_145) with
         | Some (i_156) ->
            if ((abs_float (snd i_156)) < (abs_float t_155)) then
             (t_145 := (Some (j_154, t_155)))
            else ()
         | None -> (t_145 := (Some (j_154, t_155))))
       else ()
      done;
      (match (! t_145) with
       | Some (i_146) ->
          if ((fst i_146) <> (! t_140)) then begin
           let a_147 = t_142.arr
           and n_148 = t_142.n
           and m_149 = t_142.m in
           let i1_150 = ((! t_140) * n_148)
           and i2_151 = ((fst i_146) * n_148) in
           for i_152 = 0 to (m_149 - 1) do
            let t_153 = a_147.(i1_150 + i_152) in
            a_147.(i2_151 + i_152) <- a_147.(i1_150 + i_152);
            a_147.(i1_150 + i_152) <- t_153
           done;
           ()
          end else ();
          (Some (snd i_146))
       | None -> (None))
     end in
    (match t_157 with
     | Some (i_158) ->
        begin
         for j_159 = ((! t_140) + 1) to (t_144 - 1) do
          if (not ((t_142.arr).((j_159 * t_142.n) + (! t_141)) = 0.)) then begin
           for j_160 = ((! t_141) + 1) to (t_143 - 1) do
            (t_142.arr).((j_159 * t_142.n) + j_160) <-
             ((t_142.arr).((j_159 * t_142.n) + j_160) -.
               (((t_142.arr).((j_159 * t_142.n) + (! t_141)) /.
                  (t_142.arr).(((! t_140) * t_142.n) + (! t_141))) *.
                 (t_142.arr).(((! t_140) * t_142.n) + j_160)))
           done;
           (t_142.arr).((j_159 * t_142.n) + (! t_141)) <- 0.
          end else ()
         done;
         ()
        end;
        (t_140 := ((! t_140) + 1))
     | None -> ());
    (t_141 := ((! t_141) + 1))
   done;
   (t_142, (! t_140))>.
# val resFV4 :
  ('a,
   Funct4.GenFV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_161 ->
   let t_162 = (ref 0) in
   let t_163 = (ref 0) in
   let t_164 = {arr = (Array.copy a_161.arr)} (a_161) in
   let t_165 = a_161.m in
   let t_166 = a_161.n in
   let t_167 = (ref 1.) in
   let t_168 = (ref 1) in
   while (((! t_163) < t_165) && ((! t_162) < t_166)) do
    let t_169 = (ref (None)) in
    let t_181 =
     begin
      for j_178 = (! t_162) to (t_166 - 1) do
       let t_179 = (t_164.arr).((j_178 * t_164.n) + (! t_163)) in
       if (not (t_179 = 0.)) then
        (match (! t_169) with
         | Some (i_180) ->
            if ((abs_float (snd i_180)) < (abs_float t_179)) then
             (t_169 := (Some (j_178, t_179)))
            else ()
         | None -> (t_169 := (Some (j_178, t_179))))
       else ()
      done;
      (match (! t_169) with
       | Some (i_170) ->
          if ((fst i_170) <> (! t_162)) then begin
           let a_171 = t_164.arr
           and n_172 = t_164.n
           and m_173 = t_164.m in
           let i1_174 = ((! t_162) * n_172)
           and i2_175 = ((fst i_170) * n_172) in
           for i_176 = 0 to (m_173 - 1) do
            let t_177 = a_171.(i1_174 + i_176) in
            a_171.(i2_175 + i_176) <- a_171.(i1_174 + i_176);
            a_171.(i1_174 + i_176) <- t_177
           done;
           (t_168 := (~- (! t_168)))
          end else ();
          (Some (snd i_170))
       | None -> (None))
     end in
    (match t_181 with
     | Some (i_182) ->
        begin
         for j_183 = ((! t_162) + 1) to (t_166 - 1) do
          if (not ((t_164.arr).((j_183 * t_164.n) + (! t_163)) = 0.)) then begin
           for j_184 = ((! t_163) + 1) to (t_165 - 1) do
            (t_164.arr).((j_183 * t_164.n) + j_184) <-
             ((t_164.arr).((j_183 * t_164.n) + j_184) -.
               (((t_164.arr).((j_183 * t_164.n) + (! t_163)) /.
                  (t_164.arr).(((! t_162) * t_164.n) + (! t_163))) *.
                 (t_164.arr).(((! t_162) * t_164.n) + j_184)))
           done;
           (t_164.arr).((j_183 * t_164.n) + (! t_163)) <- 0.
          end else ()
         done;
         (t_167 :=
           ((! t_167) *. (t_164.arr).(((! t_162) * t_164.n) + (! t_163))))
        end;
        (t_162 := ((! t_162) + 1))
     | None -> (t_168 := 0));
    (t_163 := ((! t_163) + 1))
   done;
   (t_164,
    if ((! t_168) = 0) then 0.
    else if ((! t_168) = 1) then (! t_167)
    else (0. -. (! t_167)), (! t_162))>.
# val resIA1 :
  ('a,
   Funct4.GenIA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_185 ->
   let t_186 = (ref 0) in
   let t_187 = (ref 0) in
   let t_189 =
    (Array.map (fun x_188 -> (Array.copy x_188)) (Array.copy a_185)) in
   let t_190 = (Array.length a_185.(0)) in
   let t_191 = (Array.length a_185) in
   let t_192 = (ref 1) in
   let t_193 = (ref 1) in
   while (((! t_187) < t_190) && ((! t_186) < t_191)) do
    let t_194 = (ref (None)) in
    let t_200 =
     begin
      for j_197 = (! t_186) to (t_191 - 1) do
       let t_198 = (t_189.(j_197)).(! t_187) in
       if (not (t_198 = 0)) then
        (match (! t_194) with
         | Some (i_199) ->
            if ((abs (snd i_199)) > (abs t_198)) then
             (t_194 := (Some (j_197, t_198)))
            else ()
         | None -> (t_194 := (Some (j_197, t_198))))
       else ()
      done;
      (match (! t_194) with
       | Some (i_195) ->
          if ((fst i_195) <> (! t_186)) then begin
           let t_196 = t_189.(! t_186) in
           t_189.(! t_186) <- t_189.(fst i_195);
           t_189.(fst i_195) <- t_196;
           (t_193 := (~- (! t_193)))
          end else ();
          (Some (snd i_195))
       | None -> (None))
     end in
    (match t_200 with
     | Some (i_201) ->
        begin
         for j_202 = ((! t_186) + 1) to (t_191 - 1) do
          if (not ((t_189.(j_202)).(! t_187) = 0)) then begin
           for j_203 = ((! t_187) + 1) to (t_190 - 1) do
            (t_189.(j_202)).(j_203) <-
             ((((t_189.(j_202)).(j_203) * (t_189.(! t_186)).(! t_187)) -
                ((t_189.(! t_186)).(j_203) * (t_189.(j_202)).(! t_186))) /
               (! t_192))
           done;
           (t_189.(j_202)).(! t_187) <- 0
          end else ()
         done;
         (t_192 := (t_189.(! t_186)).(! t_187))
        end;
        (t_186 := ((! t_186) + 1))
     | None -> (t_193 := 0));
    (t_187 := ((! t_187) + 1))
   done;
   t_189>.
# val resIA2 :
  ('a,
   Funct4.GenIA2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_204 ->
   let t_205 = (ref 0) in
   let t_206 = (ref 0) in
   let t_208 =
    (Array.map (fun x_207 -> (Array.copy x_207)) (Array.copy a_204)) in
   let t_209 = (Array.length a_204.(0)) in
   let t_210 = (Array.length a_204) in
   let t_211 = (ref 1) in
   let t_212 = (ref 1) in
   while (((! t_206) < t_209) && ((! t_205) < t_210)) do
    let t_213 = (ref (None)) in
    let t_219 =
     begin
      for j_216 = (! t_205) to (t_210 - 1) do
       let t_217 = (t_208.(j_216)).(! t_206) in
       if (not (t_217 = 0)) then
        (match (! t_213) with
         | Some (i_218) ->
            if ((abs (snd i_218)) > (abs t_217)) then
             (t_213 := (Some (j_216, t_217)))
            else ()
         | None -> (t_213 := (Some (j_216, t_217))))
       else ()
      done;
      (match (! t_213) with
       | Some (i_214) ->
          if ((fst i_214) <> (! t_205)) then begin
           let t_215 = t_208.(! t_205) in
           t_208.(! t_205) <- t_208.(fst i_214);
           t_208.(fst i_214) <- t_215;
           (t_212 := (~- (! t_212)))
          end else ();
          (Some (snd i_214))
       | None -> (None))
     end in
    (match t_219 with
     | Some (i_220) ->
        begin
         for j_221 = ((! t_205) + 1) to (t_210 - 1) do
          if (not ((t_208.(j_221)).(! t_206) = 0)) then begin
           for j_222 = ((! t_206) + 1) to (t_209 - 1) do
            (t_208.(j_221)).(j_222) <-
             ((((t_208.(j_221)).(j_222) * (t_208.(! t_205)).(! t_206)) -
                ((t_208.(! t_205)).(j_222) * (t_208.(j_221)).(! t_205))) /
               (! t_211))
           done;
           (t_208.(j_221)).(! t_206) <- 0
          end else ()
         done;
         (t_211 := (t_208.(! t_205)).(! t_206))
        end;
        (t_205 := ((! t_205) + 1))
     | None -> (t_212 := 0));
    (t_206 := ((! t_206) + 1))
   done;
   (t_208,
    if ((! t_212) = 0) then 0
    else if ((! t_212) = 1) then (! t_211)
    else (0 - (! t_211)))>.
# val resIA3 :
  ('a,
   Funct4.GenIA3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_223 ->
   let t_224 = (ref 0) in
   let t_225 = (ref 0) in
   let t_227 =
    (Array.map (fun x_226 -> (Array.copy x_226)) (Array.copy a_223)) in
   let t_228 = (Array.length a_223.(0)) in
   let t_229 = (Array.length a_223) in
   let t_230 = (ref 1) in
   let t_231 = (ref 1) in
   while (((! t_225) < t_228) && ((! t_224) < t_229)) do
    let t_232 = (ref (None)) in
    let t_238 =
     begin
      for j_235 = (! t_224) to (t_229 - 1) do
       let t_236 = (t_227.(j_235)).(! t_225) in
       if (not (t_236 = 0)) then
        (match (! t_232) with
         | Some (i_237) ->
            if ((abs (snd i_237)) > (abs t_236)) then
             (t_232 := (Some (j_235, t_236)))
            else ()
         | None -> (t_232 := (Some (j_235, t_236))))
       else ()
      done;
      (match (! t_232) with
       | Some (i_233) ->
          if ((fst i_233) <> (! t_224)) then begin
           let t_234 = t_227.(! t_224) in
           t_227.(! t_224) <- t_227.(fst i_233);
           t_227.(fst i_233) <- t_234;
           ()
          end else ();
          (Some (snd i_233))
       | None -> (None))
     end in
    (match t_238 with
     | Some (i_239) ->
        begin
         for j_240 = ((! t_224) + 1) to (t_229 - 1) do
          if (not ((t_227.(j_240)).(! t_225) = 0)) then begin
           for j_241 = ((! t_225) + 1) to (t_228 - 1) do
            (t_227.(j_240)).(j_241) <-
             ((((t_227.(j_240)).(j_241) * (t_227.(! t_224)).(! t_225)) -
                ((t_227.(! t_224)).(j_241) * (t_227.(j_240)).(! t_224))) /
               (! t_230))
           done;
           (t_227.(j_240)).(! t_225) <- 0
          end else ()
         done;
         (t_230 := (t_227.(! t_224)).(! t_225))
        end;
        (t_224 := ((! t_224) + 1))
     | None -> (t_231 := 0));
    (t_225 := ((! t_225) + 1))
   done;
   (t_227, (! t_224))>.
# val resIA4 :
  ('a,
   Funct4.GenIA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_242 ->
   let t_243 = (ref 0) in
   let t_244 = (ref 0) in
   let t_246 =
    (Array.map (fun x_245 -> (Array.copy x_245)) (Array.copy a_242)) in
   let t_247 = (Array.length a_242.(0)) in
   let t_248 = (Array.length a_242) in
   let t_249 = (ref 1) in
   let t_250 = (ref 1) in
   while (((! t_244) < t_247) && ((! t_243) < t_248)) do
    let t_251 = (ref (None)) in
    let t_257 =
     begin
      for j_254 = (! t_243) to (t_248 - 1) do
       let t_255 = (t_246.(j_254)).(! t_244) in
       if (not (t_255 = 0)) then
        (match (! t_251) with
         | Some (i_256) ->
            if ((abs (snd i_256)) > (abs t_255)) then
             (t_251 := (Some (j_254, t_255)))
            else ()
         | None -> (t_251 := (Some (j_254, t_255))))
       else ()
      done;
      (match (! t_251) with
       | Some (i_252) ->
          if ((fst i_252) <> (! t_243)) then begin
           let t_253 = t_246.(! t_243) in
           t_246.(! t_243) <- t_246.(fst i_252);
           t_246.(fst i_252) <- t_253;
           (t_250 := (~- (! t_250)))
          end else ();
          (Some (snd i_252))
       | None -> (None))
     end in
    (match t_257 with
     | Some (i_258) ->
        begin
         for j_259 = ((! t_243) + 1) to (t_248 - 1) do
          if (not ((t_246.(j_259)).(! t_244) = 0)) then begin
           for j_260 = ((! t_244) + 1) to (t_247 - 1) do
            (t_246.(j_259)).(j_260) <-
             ((((t_246.(j_259)).(j_260) * (t_246.(! t_243)).(! t_244)) -
                ((t_246.(! t_243)).(j_260) * (t_246.(j_259)).(! t_243))) /
               (! t_249))
           done;
           (t_246.(j_259)).(! t_244) <- 0
          end else ()
         done;
         (t_249 := (t_246.(! t_243)).(! t_244))
        end;
        (t_243 := ((! t_243) + 1))
     | None -> (t_250 := 0));
    (t_244 := ((! t_244) + 1))
   done;
   (t_246,
    if ((! t_250) = 0) then 0
    else if ((! t_250) = 1) then (! t_249)
    else (0 - (! t_249)), (! t_243))>.
# val resIV1 :
  ('a,
   Funct4.GenIV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_261 ->
   let t_262 = (ref 0) in
   let t_263 = (ref 0) in
   let t_264 = {arr = (Array.copy a_261.arr)} (a_261) in
   let t_265 = a_261.m in
   let t_266 = a_261.n in
   let t_267 = (ref 1) in
   let t_268 = (ref 1) in
   while (((! t_263) < t_265) && ((! t_262) < t_266)) do
    let t_269 = (ref (None)) in
    let t_281 =
     begin
      for j_278 = (! t_262) to (t_266 - 1) do
       let t_279 = (t_264.arr).((j_278 * t_264.n) + (! t_263)) in
       if (not (t_279 = 0)) then
        (match (! t_269) with
         | Some (i_280) ->
            if ((abs (snd i_280)) > (abs t_279)) then
             (t_269 := (Some (j_278, t_279)))
            else ()
         | None -> (t_269 := (Some (j_278, t_279))))
       else ()
      done;
      (match (! t_269) with
       | Some (i_270) ->
          if ((fst i_270) <> (! t_262)) then begin
           let a_271 = t_264.arr
           and n_272 = t_264.n
           and m_273 = t_264.m in
           let i1_274 = ((! t_262) * n_272)
           and i2_275 = ((fst i_270) * n_272) in
           for i_276 = 0 to (m_273 - 1) do
            let t_277 = a_271.(i1_274 + i_276) in
            a_271.(i2_275 + i_276) <- a_271.(i1_274 + i_276);
            a_271.(i1_274 + i_276) <- t_277
           done;
           ()
          end else ();
          (Some (snd i_270))
       | None -> (None))
     end in
    (match t_281 with
     | Some (i_282) ->
        begin
         for j_283 = ((! t_262) + 1) to (t_266 - 1) do
          if (not ((t_264.arr).((j_283 * t_264.n) + (! t_263)) = 0)) then begin
           for j_284 = ((! t_263) + 1) to (t_265 - 1) do
            (t_264.arr).((j_283 * t_264.n) + j_284) <-
             ((((t_264.arr).((j_283 * t_264.n) + j_284) *
                 (t_264.arr).(((! t_262) * t_264.n) + (! t_263))) -
                ((t_264.arr).(((! t_262) * t_264.n) + j_284) *
                  (t_264.arr).((j_283 * t_264.n) + (! t_262)))) / (! t_267))
           done;
           (t_264.arr).((j_283 * t_264.n) + (! t_263)) <- 0
          end else ()
         done;
         (t_267 := (t_264.arr).(((! t_262) * t_264.n) + (! t_263)))
        end;
        (t_262 := ((! t_262) + 1))
     | None -> (t_268 := 0));
    (t_263 := ((! t_263) + 1))
   done;
   t_264>.
# val resIV2 :
  ('a,
   Funct4.GenIV2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_285 ->
   let t_286 = (ref 0) in
   let t_287 = (ref 0) in
   let t_288 = {arr = (Array.copy a_285.arr)} (a_285) in
   let t_289 = a_285.m in
   let t_290 = a_285.n in
   let t_291 = (ref 1) in
   let t_292 = (ref 1) in
   while (((! t_287) < t_289) && ((! t_286) < t_290)) do
    let t_293 = (ref (None)) in
    let t_305 =
     begin
      for j_302 = (! t_286) to (t_290 - 1) do
       let t_303 = (t_288.arr).((j_302 * t_288.n) + (! t_287)) in
       if (not (t_303 = 0)) then
        (match (! t_293) with
         | Some (i_304) ->
            if ((abs (snd i_304)) > (abs t_303)) then
             (t_293 := (Some (j_302, t_303)))
            else ()
         | None -> (t_293 := (Some (j_302, t_303))))
       else ()
      done;
      (match (! t_293) with
       | Some (i_294) ->
          if ((fst i_294) <> (! t_286)) then begin
           let a_295 = t_288.arr
           and n_296 = t_288.n
           and m_297 = t_288.m in
           let i1_298 = ((! t_286) * n_296)
           and i2_299 = ((fst i_294) * n_296) in
           for i_300 = 0 to (m_297 - 1) do
            let t_301 = a_295.(i1_298 + i_300) in
            a_295.(i2_299 + i_300) <- a_295.(i1_298 + i_300);
            a_295.(i1_298 + i_300) <- t_301
           done;
           (t_292 := (~- (! t_292)))
          end else ();
          (Some (snd i_294))
       | None -> (None))
     end in
    (match t_305 with
     | Some (i_306) ->
        begin
         for j_307 = ((! t_286) + 1) to (t_290 - 1) do
          if (not ((t_288.arr).((j_307 * t_288.n) + (! t_287)) = 0)) then begin
           for j_308 = ((! t_287) + 1) to (t_289 - 1) do
            (t_288.arr).((j_307 * t_288.n) + j_308) <-
             ((((t_288.arr).((j_307 * t_288.n) + j_308) *
                 (t_288.arr).(((! t_286) * t_288.n) + (! t_287))) -
                ((t_288.arr).(((! t_286) * t_288.n) + j_308) *
                  (t_288.arr).((j_307 * t_288.n) + (! t_286)))) / (! t_291))
           done;
           (t_288.arr).((j_307 * t_288.n) + (! t_287)) <- 0
          end else ()
         done;
         (t_291 := (t_288.arr).(((! t_286) * t_288.n) + (! t_287)))
        end;
        (t_286 := ((! t_286) + 1))
     | None -> (t_292 := 0));
    (t_287 := ((! t_287) + 1))
   done;
   (t_288,
    if ((! t_292) = 0) then 0
    else if ((! t_292) = 1) then (! t_291)
    else (0 - (! t_291)))>.
# val resIV3 :
  ('a,
   Funct4.GenIV3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_309 ->
   let t_310 = (ref 0) in
   let t_311 = (ref 0) in
   let t_312 = {arr = (Array.copy a_309.arr)} (a_309) in
   let t_313 = a_309.m in
   let t_314 = a_309.n in
   let t_315 = (ref 1) in
   let t_316 = (ref 1) in
   while (((! t_311) < t_313) && ((! t_310) < t_314)) do
    let t_317 = (ref (None)) in
    let t_329 =
     begin
      for j_326 = (! t_310) to (t_314 - 1) do
       let t_327 = (t_312.arr).((j_326 * t_312.n) + (! t_311)) in
       if (not (t_327 = 0)) then
        (match (! t_317) with
         | Some (i_328) ->
            if ((abs (snd i_328)) > (abs t_327)) then
             (t_317 := (Some (j_326, t_327)))
            else ()
         | None -> (t_317 := (Some (j_326, t_327))))
       else ()
      done;
      (match (! t_317) with
       | Some (i_318) ->
          if ((fst i_318) <> (! t_310)) then begin
           let a_319 = t_312.arr
           and n_320 = t_312.n
           and m_321 = t_312.m in
           let i1_322 = ((! t_310) * n_320)
           and i2_323 = ((fst i_318) * n_320) in
           for i_324 = 0 to (m_321 - 1) do
            let t_325 = a_319.(i1_322 + i_324) in
            a_319.(i2_323 + i_324) <- a_319.(i1_322 + i_324);
            a_319.(i1_322 + i_324) <- t_325
           done;
           ()
          end else ();
          (Some (snd i_318))
       | None -> (None))
     end in
    (match t_329 with
     | Some (i_330) ->
        begin
         for j_331 = ((! t_310) + 1) to (t_314 - 1) do
          if (not ((t_312.arr).((j_331 * t_312.n) + (! t_311)) = 0)) then begin
           for j_332 = ((! t_311) + 1) to (t_313 - 1) do
            (t_312.arr).((j_331 * t_312.n) + j_332) <-
             ((((t_312.arr).((j_331 * t_312.n) + j_332) *
                 (t_312.arr).(((! t_310) * t_312.n) + (! t_311))) -
                ((t_312.arr).(((! t_310) * t_312.n) + j_332) *
                  (t_312.arr).((j_331 * t_312.n) + (! t_310)))) / (! t_315))
           done;
           (t_312.arr).((j_331 * t_312.n) + (! t_311)) <- 0
          end else ()
         done;
         (t_315 := (t_312.arr).(((! t_310) * t_312.n) + (! t_311)))
        end;
        (t_310 := ((! t_310) + 1))
     | None -> (t_316 := 0));
    (t_311 := ((! t_311) + 1))
   done;
   (t_312, (! t_310))>.
# val resIV4 :
  ('a,
   Funct4.GenIV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_333 ->
   let t_334 = (ref 0) in
   let t_335 = (ref 0) in
   let t_336 = {arr = (Array.copy a_333.arr)} (a_333) in
   let t_337 = a_333.m in
   let t_338 = a_333.n in
   let t_339 = (ref 1) in
   let t_340 = (ref 1) in
   while (((! t_335) < t_337) && ((! t_334) < t_338)) do
    let t_341 = (ref (None)) in
    let t_353 =
     begin
      for j_350 = (! t_334) to (t_338 - 1) do
       let t_351 = (t_336.arr).((j_350 * t_336.n) + (! t_335)) in
       if (not (t_351 = 0)) then
        (match (! t_341) with
         | Some (i_352) ->
            if ((abs (snd i_352)) > (abs t_351)) then
             (t_341 := (Some (j_350, t_351)))
            else ()
         | None -> (t_341 := (Some (j_350, t_351))))
       else ()
      done;
      (match (! t_341) with
       | Some (i_342) ->
          if ((fst i_342) <> (! t_334)) then begin
           let a_343 = t_336.arr
           and n_344 = t_336.n
           and m_345 = t_336.m in
           let i1_346 = ((! t_334) * n_344)
           and i2_347 = ((fst i_342) * n_344) in
           for i_348 = 0 to (m_345 - 1) do
            let t_349 = a_343.(i1_346 + i_348) in
            a_343.(i2_347 + i_348) <- a_343.(i1_346 + i_348);
            a_343.(i1_346 + i_348) <- t_349
           done;
           (t_340 := (~- (! t_340)))
          end else ();
          (Some (snd i_342))
       | None -> (None))
     end in
    (match t_353 with
     | Some (i_354) ->
        begin
         for j_355 = ((! t_334) + 1) to (t_338 - 1) do
          if (not ((t_336.arr).((j_355 * t_336.n) + (! t_335)) = 0)) then begin
           for j_356 = ((! t_335) + 1) to (t_337 - 1) do
            (t_336.arr).((j_355 * t_336.n) + j_356) <-
             ((((t_336.arr).((j_355 * t_336.n) + j_356) *
                 (t_336.arr).(((! t_334) * t_336.n) + (! t_335))) -
                ((t_336.arr).(((! t_334) * t_336.n) + j_356) *
                  (t_336.arr).((j_355 * t_336.n) + (! t_334)))) / (! t_339))
           done;
           (t_336.arr).((j_355 * t_336.n) + (! t_335)) <- 0
          end else ()
         done;
         (t_339 := (t_336.arr).(((! t_334) * t_336.n) + (! t_335)))
        end;
        (t_334 := ((! t_334) + 1))
     | None -> (t_340 := 0));
    (t_335 := ((! t_335) + 1))
   done;
   (t_336,
    if ((! t_340) = 0) then 0
    else if ((! t_340) = 1) then (! t_339)
    else (0 - (! t_339)), (! t_334))>.
#   val rFA1 :
  Funct4.GenFA1.Ctr.contr ->
  Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res =
  <fun>
# val rFA2 :
  Funct4.GenFA2.Ctr.contr ->
  Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res =
  <fun>
# val rFA3 :
  Funct4.GenFA3.Ctr.contr ->
  Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res =
  <fun>
# val rFA4 :
  Funct4.GenFA4.Ctr.contr ->
  Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res =
  <fun>
# val rFV1 :
  Funct4.GenFV1.Ctr.contr ->
  Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res =
  <fun>
# val rFV2 :
  Funct4.GenFV2.Ctr.contr ->
  Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res =
  <fun>
# val rFV3 :
  Funct4.GenFV3.Ctr.contr ->
  Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res =
  <fun>
# val rFV4 :
  Funct4.GenFV4.Ctr.contr ->
  Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res =
  <fun>
# val rIA1 :
  Funct4.GenIA1.Ctr.contr ->
  Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res =
  <fun>
# val rIA2 :
  Funct4.GenIA2.Ctr.contr ->
  Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res =
  <fun>
# val rIA3 :
  Funct4.GenIA3.Ctr.contr ->
  Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res =
  <fun>
# val rIA4 :
  Funct4.GenIA4.Ctr.contr ->
  Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res =
  <fun>
# val rIV1 :
  Funct4.GenIV1.Ctr.contr ->
  Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res =
  <fun>
# val rIV2 :
  Funct4.GenIV2.Ctr.contr ->
  Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet).res =
  <fun>
# val rIV3 :
  Funct4.GenIV3.Ctr.contr ->
  Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res =
  <fun>
# val rIV4 :
  Funct4.GenIV4.Ctr.contr ->
  Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res =
  <fun>
#                                                   val ia0 : Funct4.IntegerDomain.v array array = [|[|1|]|]
val ia1 : Funct4.IntegerDomain.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|]
val ia2 : Funct4.IntegerDomain.v array array =
  [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|]
val ia3 : Funct4.IntegerDomain.v array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|]
val ia4 : Funct4.IntegerDomain.v array array =
  [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]
val ia5 : Funct4.IntegerDomain.v array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|];
   [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|];
   [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]]
val resI1 :
  Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res
  list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|]]
# val resI2 :
  Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res
  list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 0)]
# val resI3 :
  Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res
  list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 2)]
# val resI4 :
  Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res
  list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 0, 2)]
#   val fa0 : float array array = [|[|1.|]|]
#         val fa1 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|]
#         val fa2 : float array array =
  [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|]
#           val fa3 : float array array =
  [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|]
#           val fa4 : float array array =
  [|[|0.; 2.; 3.|]; [|0.; 13.; 5.|]; [|0.; 3.; 0.|]|]
#   val fa5 : Funct4.FloatDomain.v array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 13.; 5.|]; [|0.; 3.; 0.|]|]]
val resF1 :
  Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res
  list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|]]
# val resF2 :
  Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res
  list =
  [([|[|1.|]|], 1.);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50.);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50.);
   ([|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|], 0.)]
# val resF3 :
  Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res
  list =
  [([|[|1.|]|], 1);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 3);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 3);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    3);
   ([|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|], 2)]
# val resF4 :
  Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res
  list =
  [([|[|1.|]|], 1., 1);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 50., 3);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 50.,
    3);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    50., 3);
   ([|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|], 0.,
    2)]
# 
