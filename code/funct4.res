        Objective Caml version 3.08.0

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
           (Some (1, (snd i_9)))
          end else (Some (0, (snd i_9)))
       | None -> (None))
     end in
    (match t_14 with
     | Some (i_15) ->
        begin
         for j_17 = ((! t_2) + 1) to (t_7 - 1) do
          if (not ((t_5.(j_17)).(! t_3) = 0.)) then begin
           for j_18 = ((! t_3) + 1) to (t_6 - 1) do
            (t_5.(j_17)).(j_18) <-
             ((t_5.(j_17)).(j_18) -.
               (((t_5.(j_17)).(! t_3) /. (t_5.(! t_2)).(! t_3)) *.
                 (t_5.(! t_2)).(j_18)))
           done;
           (t_5.(j_17)).(! t_3) <- 0.
          end else ()
         done;
         ()
        end;
        for j_16 = 0 to (fst i_15) do () done;
        (t_2 := ((! t_2) + 1))
     | None -> ());
    (t_3 := ((! t_3) + 1))
   done;
   t_5>.
# val resFA11 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutJustMatrix(Funct4.FArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_19 ->
   let t_20 = (ref 0) in
   let t_21 = (ref 0) in
   let t_23 = (Array.map (fun x_22 -> (Array.copy x_22)) (Array.copy a_19)) in
   let t_24 = (Array.length a_19.(0)) in
   let t_25 = (Array.length a_19) in
   while (((! t_21) < t_24) && ((! t_20) < t_25)) do
    let t_26 = (ref (None)) in
    let t_34 =
     begin
      for j_30 = (! t_20) to (t_25 - 1) do
       for j_31 = (! t_21) to (t_24 - 1) do
        let t_32 = (t_23.(j_30)).(j_31) in
        if (not (t_32 = 0.)) then
         (match (! t_26) with
          | Some (i_33) ->
             if ((abs_float (snd i_33)) < (abs_float t_32)) then
              (t_26 := (Some ((j_30, j_31), t_32)))
             else ()
          | None -> (t_26 := (Some ((j_30, j_31), t_32))))
        else ()
       done
      done;
      (match (! t_26) with
       | Some (i_27) ->
          let t_28 = (ref 0) in
          if ((snd (fst i_27)) <> (! t_21)) then begin
           (failwith "swap_cols_stmt not yet implemeted");
           (t_28 := ((! t_28) + 1))
          end else ();
          if ((snd (fst i_27)) <> (! t_21)) then begin
           let t_29 = t_23.(! t_20) in
           t_23.(! t_20) <- t_23.(fst (fst i_27));
           t_23.(fst (fst i_27)) <- t_29;
           (t_28 := ((! t_28) + 1))
          end else ();
          (Some ((! t_28), (snd i_27)))
       | None -> (None))
     end in
    (match t_34 with
     | Some (i_35) ->
        begin
         for j_37 = ((! t_20) + 1) to (t_25 - 1) do
          if (not ((t_23.(j_37)).(! t_21) = 0.)) then begin
           for j_38 = ((! t_21) + 1) to (t_24 - 1) do
            (t_23.(j_37)).(j_38) <-
             ((t_23.(j_37)).(j_38) -.
               (((t_23.(j_37)).(! t_21) /. (t_23.(! t_20)).(! t_21)) *.
                 (t_23.(! t_20)).(j_38)))
           done;
           (t_23.(j_37)).(! t_21) <- 0.
          end else ()
         done;
         ()
        end;
        for j_36 = 0 to (fst i_35) do () done;
        (t_20 := ((! t_20) + 1))
     | None -> ());
    (t_21 := ((! t_21) + 1))
   done;
   t_23>.
# val resFA2 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutDet(Funct4.FArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_39 ->
   let t_40 = (ref 0) in
   let t_41 = (ref 0) in
   let t_43 = (Array.map (fun x_42 -> (Array.copy x_42)) (Array.copy a_39)) in
   let t_44 = (Array.length a_39.(0)) in
   let t_45 = (Array.length a_39) in
   let t_46 = (ref 1.) in
   let t_47 = (ref 1.) in
   while (((! t_41) < t_44) && ((! t_40) < t_45)) do
    let t_48 = (ref (None)) in
    let t_54 =
     begin
      for j_51 = (! t_40) to (t_45 - 1) do
       let t_52 = (t_43.(j_51)).(! t_41) in
       if (not (t_52 = 0.)) then
        (match (! t_48) with
         | Some (i_53) ->
            if ((abs_float (snd i_53)) < (abs_float t_52)) then
             (t_48 := (Some (j_51, t_52)))
            else ()
         | None -> (t_48 := (Some (j_51, t_52))))
       else ()
      done;
      (match (! t_48) with
       | Some (i_49) ->
          if ((fst i_49) <> (! t_40)) then begin
           let t_50 = t_43.(! t_40) in
           t_43.(! t_40) <- t_43.(fst i_49);
           t_43.(fst i_49) <- t_50;
           (Some (1, (snd i_49)))
          end else (Some (0, (snd i_49)))
       | None -> (None))
     end in
    (match t_54 with
     | Some (i_55) ->
        begin
         for j_57 = ((! t_40) + 1) to (t_45 - 1) do
          if (not ((t_43.(j_57)).(! t_41) = 0.)) then begin
           for j_58 = ((! t_41) + 1) to (t_44 - 1) do
            (t_43.(j_57)).(j_58) <-
             ((t_43.(j_57)).(j_58) -.
               (((t_43.(j_57)).(! t_41) /. (t_43.(! t_40)).(! t_41)) *.
                 (t_43.(! t_40)).(j_58)))
           done;
           (t_43.(j_57)).(! t_41) <- 0.
          end else ()
         done;
         (t_46 := ((! t_46) *. (t_43.(! t_40)).(! t_41)))
        end;
        for j_56 = 0 to (fst i_55) do (t_47 := ((! t_47) *. (-1.))) done;
        (t_40 := ((! t_40) + 1))
     | None -> (t_47 := 0.));
    (t_41 := ((! t_41) + 1))
   done;
   (t_43, ((! t_47) *. (! t_46)))>.
# val resFA3 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutRank(Funct4.FArrayContainer)(Funct4.Rank)(Funct4.FloatDomain).res)
  code =
  .<fun a_59 ->
   let t_60 = (ref 0) in
   let t_61 = (ref 0) in
   let t_63 = (Array.map (fun x_62 -> (Array.copy x_62)) (Array.copy a_59)) in
   let t_64 = (Array.length a_59.(0)) in
   let t_65 = (Array.length a_59) in
   while (((! t_61) < t_64) && ((! t_60) < t_65)) do
    let t_66 = (ref (None)) in
    let t_72 =
     begin
      for j_69 = (! t_60) to (t_65 - 1) do
       let t_70 = (t_63.(j_69)).(! t_61) in
       if (not (t_70 = 0.)) then
        (match (! t_66) with
         | Some (i_71) ->
            if ((abs_float (snd i_71)) < (abs_float t_70)) then
             (t_66 := (Some (j_69, t_70)))
            else ()
         | None -> (t_66 := (Some (j_69, t_70))))
       else ()
      done;
      (match (! t_66) with
       | Some (i_67) ->
          if ((fst i_67) <> (! t_60)) then begin
           let t_68 = t_63.(! t_60) in
           t_63.(! t_60) <- t_63.(fst i_67);
           t_63.(fst i_67) <- t_68;
           (Some (1, (snd i_67)))
          end else (Some (0, (snd i_67)))
       | None -> (None))
     end in
    (match t_72 with
     | Some (i_73) ->
        begin
         for j_75 = ((! t_60) + 1) to (t_65 - 1) do
          if (not ((t_63.(j_75)).(! t_61) = 0.)) then begin
           for j_76 = ((! t_61) + 1) to (t_64 - 1) do
            (t_63.(j_75)).(j_76) <-
             ((t_63.(j_75)).(j_76) -.
               (((t_63.(j_75)).(! t_61) /. (t_63.(! t_60)).(! t_61)) *.
                 (t_63.(! t_60)).(j_76)))
           done;
           (t_63.(j_75)).(! t_61) <- 0.
          end else ()
         done;
         ()
        end;
        for j_74 = 0 to (fst i_73) do () done;
        (t_60 := ((! t_60) + 1))
     | None -> ());
    (t_61 := ((! t_61) + 1))
   done;
   (t_63, (! t_60))>.
# val resFA4 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutDetRank(Funct4.FArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_77 ->
   let t_78 = (ref 0) in
   let t_79 = (ref 0) in
   let t_81 = (Array.map (fun x_80 -> (Array.copy x_80)) (Array.copy a_77)) in
   let t_82 = (Array.length a_77.(0)) in
   let t_83 = (Array.length a_77) in
   let t_84 = (ref 1.) in
   let t_85 = (ref 1.) in
   while (((! t_79) < t_82) && ((! t_78) < t_83)) do
    let t_86 = (ref (None)) in
    let t_92 =
     begin
      for j_89 = (! t_78) to (t_83 - 1) do
       let t_90 = (t_81.(j_89)).(! t_79) in
       if (not (t_90 = 0.)) then
        (match (! t_86) with
         | Some (i_91) ->
            if ((abs_float (snd i_91)) < (abs_float t_90)) then
             (t_86 := (Some (j_89, t_90)))
            else ()
         | None -> (t_86 := (Some (j_89, t_90))))
       else ()
      done;
      (match (! t_86) with
       | Some (i_87) ->
          if ((fst i_87) <> (! t_78)) then begin
           let t_88 = t_81.(! t_78) in
           t_81.(! t_78) <- t_81.(fst i_87);
           t_81.(fst i_87) <- t_88;
           (Some (1, (snd i_87)))
          end else (Some (0, (snd i_87)))
       | None -> (None))
     end in
    (match t_92 with
     | Some (i_93) ->
        begin
         for j_95 = ((! t_78) + 1) to (t_83 - 1) do
          if (not ((t_81.(j_95)).(! t_79) = 0.)) then begin
           for j_96 = ((! t_79) + 1) to (t_82 - 1) do
            (t_81.(j_95)).(j_96) <-
             ((t_81.(j_95)).(j_96) -.
               (((t_81.(j_95)).(! t_79) /. (t_81.(! t_78)).(! t_79)) *.
                 (t_81.(! t_78)).(j_96)))
           done;
           (t_81.(j_95)).(! t_79) <- 0.
          end else ()
         done;
         (t_84 := ((! t_84) *. (t_81.(! t_78)).(! t_79)))
        end;
        for j_94 = 0 to (fst i_93) do (t_85 := ((! t_85) *. (-1.))) done;
        (t_78 := ((! t_78) + 1))
     | None -> (t_85 := 0.));
    (t_79 := ((! t_79) + 1))
   done;
   (t_81, ((! t_85) *. (! t_84)), (! t_78))>.
# val resFV1 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutJustMatrix(Funct4.FVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_97 ->
   let t_98 = (ref 0) in
   let t_99 = (ref 0) in
   let t_100 = {arr = (Array.copy a_97.arr)} (a_97) in
   let t_101 = a_97.m in
   let t_102 = a_97.n in
   let t_103 = (ref 1.) in
   let t_104 = (ref 1.) in
   while (((! t_99) < t_101) && ((! t_98) < t_102)) do
    let t_105 = (ref (None)) in
    let t_117 =
     begin
      for j_114 = (! t_98) to (t_102 - 1) do
       let t_115 = (t_100.arr).((j_114 * t_100.n) + (! t_99)) in
       if (not (t_115 = 0.)) then
        (match (! t_105) with
         | Some (i_116) ->
            if ((abs_float (snd i_116)) < (abs_float t_115)) then
             (t_105 := (Some (j_114, t_115)))
            else ()
         | None -> (t_105 := (Some (j_114, t_115))))
       else ()
      done;
      (match (! t_105) with
       | Some (i_106) ->
          if ((fst i_106) <> (! t_98)) then begin
           let a_107 = t_100.arr
           and n_108 = t_100.n
           and m_109 = t_100.m in
           let i1_110 = ((! t_98) * n_108)
           and i2_111 = ((fst i_106) * n_108) in
           for i_112 = 0 to (m_109 - 1) do
            let t_113 = a_107.(i1_110 + i_112) in
            a_107.(i2_111 + i_112) <- a_107.(i1_110 + i_112);
            a_107.(i1_110 + i_112) <- t_113
           done;
           (Some (1, (snd i_106)))
          end else (Some (0, (snd i_106)))
       | None -> (None))
     end in
    (match t_117 with
     | Some (i_118) ->
        begin
         for j_120 = ((! t_98) + 1) to (t_102 - 1) do
          if (not ((t_100.arr).((j_120 * t_100.n) + (! t_99)) = 0.)) then begin
           for j_121 = ((! t_99) + 1) to (t_101 - 1) do
            (t_100.arr).((j_120 * t_100.n) + j_121) <-
             ((t_100.arr).((j_120 * t_100.n) + j_121) -.
               (((t_100.arr).((j_120 * t_100.n) + (! t_99)) /.
                  (t_100.arr).(((! t_98) * t_100.n) + (! t_99))) *.
                 (t_100.arr).(((! t_98) * t_100.n) + j_121)))
           done;
           (t_100.arr).((j_120 * t_100.n) + (! t_99)) <- 0.
          end else ()
         done;
         (t_103 :=
           ((! t_103) *. (t_100.arr).(((! t_98) * t_100.n) + (! t_99))))
        end;
        for j_119 = 0 to (fst i_118) do (t_104 := ((! t_104) *. (-1.))) done;
        (t_98 := ((! t_98) + 1))
     | None -> (t_104 := 0.));
    (t_99 := ((! t_99) + 1))
   done;
   t_100>.
# val resFV2 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutDet(Funct4.FVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_122 ->
   let t_123 = (ref 0) in
   let t_124 = (ref 0) in
   let t_125 = {arr = (Array.copy a_122.arr)} (a_122) in
   let t_126 = a_122.m in
   let t_127 = a_122.n in
   let t_128 = (ref 1.) in
   let t_129 = (ref 1.) in
   while (((! t_124) < t_126) && ((! t_123) < t_127)) do
    let t_130 = (ref (None)) in
    let t_142 =
     begin
      for j_139 = (! t_123) to (t_127 - 1) do
       let t_140 = (t_125.arr).((j_139 * t_125.n) + (! t_124)) in
       if (not (t_140 = 0.)) then
        (match (! t_130) with
         | Some (i_141) ->
            if ((abs_float (snd i_141)) < (abs_float t_140)) then
             (t_130 := (Some (j_139, t_140)))
            else ()
         | None -> (t_130 := (Some (j_139, t_140))))
       else ()
      done;
      (match (! t_130) with
       | Some (i_131) ->
          if ((fst i_131) <> (! t_123)) then begin
           let a_132 = t_125.arr
           and n_133 = t_125.n
           and m_134 = t_125.m in
           let i1_135 = ((! t_123) * n_133)
           and i2_136 = ((fst i_131) * n_133) in
           for i_137 = 0 to (m_134 - 1) do
            let t_138 = a_132.(i1_135 + i_137) in
            a_132.(i2_136 + i_137) <- a_132.(i1_135 + i_137);
            a_132.(i1_135 + i_137) <- t_138
           done;
           (Some (1, (snd i_131)))
          end else (Some (0, (snd i_131)))
       | None -> (None))
     end in
    (match t_142 with
     | Some (i_143) ->
        begin
         for j_145 = ((! t_123) + 1) to (t_127 - 1) do
          if (not ((t_125.arr).((j_145 * t_125.n) + (! t_124)) = 0.)) then begin
           for j_146 = ((! t_124) + 1) to (t_126 - 1) do
            (t_125.arr).((j_145 * t_125.n) + j_146) <-
             ((t_125.arr).((j_145 * t_125.n) + j_146) -.
               (((t_125.arr).((j_145 * t_125.n) + (! t_124)) /.
                  (t_125.arr).(((! t_123) * t_125.n) + (! t_124))) *.
                 (t_125.arr).(((! t_123) * t_125.n) + j_146)))
           done;
           (t_125.arr).((j_145 * t_125.n) + (! t_124)) <- 0.
          end else ()
         done;
         (t_128 :=
           ((! t_128) *. (t_125.arr).(((! t_123) * t_125.n) + (! t_124))))
        end;
        for j_144 = 0 to (fst i_143) do (t_129 := ((! t_129) *. (-1.))) done;
        (t_123 := ((! t_123) + 1))
     | None -> (t_129 := 0.));
    (t_124 := ((! t_124) + 1))
   done;
   (t_125, ((! t_129) *. (! t_128)))>.
# val resFV3 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutRank(Funct4.FVectorContainer)(Funct4.Rank)(Funct4.FloatDomain).res)
  code =
  .<fun a_147 ->
   let t_148 = (ref 0) in
   let t_149 = (ref 0) in
   let t_150 = {arr = (Array.copy a_147.arr)} (a_147) in
   let t_151 = a_147.m in
   let t_152 = a_147.n in
   while (((! t_149) < t_151) && ((! t_148) < t_152)) do
    let t_153 = (ref (None)) in
    let t_165 =
     begin
      for j_162 = (! t_148) to (t_152 - 1) do
       let t_163 = (t_150.arr).((j_162 * t_150.n) + (! t_149)) in
       if (not (t_163 = 0.)) then
        (match (! t_153) with
         | Some (i_164) ->
            if ((abs_float (snd i_164)) < (abs_float t_163)) then
             (t_153 := (Some (j_162, t_163)))
            else ()
         | None -> (t_153 := (Some (j_162, t_163))))
       else ()
      done;
      (match (! t_153) with
       | Some (i_154) ->
          if ((fst i_154) <> (! t_148)) then begin
           let a_155 = t_150.arr
           and n_156 = t_150.n
           and m_157 = t_150.m in
           let i1_158 = ((! t_148) * n_156)
           and i2_159 = ((fst i_154) * n_156) in
           for i_160 = 0 to (m_157 - 1) do
            let t_161 = a_155.(i1_158 + i_160) in
            a_155.(i2_159 + i_160) <- a_155.(i1_158 + i_160);
            a_155.(i1_158 + i_160) <- t_161
           done;
           (Some (1, (snd i_154)))
          end else (Some (0, (snd i_154)))
       | None -> (None))
     end in
    (match t_165 with
     | Some (i_166) ->
        begin
         for j_168 = ((! t_148) + 1) to (t_152 - 1) do
          if (not ((t_150.arr).((j_168 * t_150.n) + (! t_149)) = 0.)) then begin
           for j_169 = ((! t_149) + 1) to (t_151 - 1) do
            (t_150.arr).((j_168 * t_150.n) + j_169) <-
             ((t_150.arr).((j_168 * t_150.n) + j_169) -.
               (((t_150.arr).((j_168 * t_150.n) + (! t_149)) /.
                  (t_150.arr).(((! t_148) * t_150.n) + (! t_149))) *.
                 (t_150.arr).(((! t_148) * t_150.n) + j_169)))
           done;
           (t_150.arr).((j_168 * t_150.n) + (! t_149)) <- 0.
          end else ()
         done;
         ()
        end;
        for j_167 = 0 to (fst i_166) do () done;
        (t_148 := ((! t_148) + 1))
     | None -> ());
    (t_149 := ((! t_149) + 1))
   done;
   (t_150, (! t_148))>.
# val resFV4 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutDetRank(Funct4.FVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_170 ->
   let t_171 = (ref 0) in
   let t_172 = (ref 0) in
   let t_173 = {arr = (Array.copy a_170.arr)} (a_170) in
   let t_174 = a_170.m in
   let t_175 = a_170.n in
   let t_176 = (ref 1.) in
   let t_177 = (ref 1.) in
   while (((! t_172) < t_174) && ((! t_171) < t_175)) do
    let t_178 = (ref (None)) in
    let t_190 =
     begin
      for j_187 = (! t_171) to (t_175 - 1) do
       let t_188 = (t_173.arr).((j_187 * t_173.n) + (! t_172)) in
       if (not (t_188 = 0.)) then
        (match (! t_178) with
         | Some (i_189) ->
            if ((abs_float (snd i_189)) < (abs_float t_188)) then
             (t_178 := (Some (j_187, t_188)))
            else ()
         | None -> (t_178 := (Some (j_187, t_188))))
       else ()
      done;
      (match (! t_178) with
       | Some (i_179) ->
          if ((fst i_179) <> (! t_171)) then begin
           let a_180 = t_173.arr
           and n_181 = t_173.n
           and m_182 = t_173.m in
           let i1_183 = ((! t_171) * n_181)
           and i2_184 = ((fst i_179) * n_181) in
           for i_185 = 0 to (m_182 - 1) do
            let t_186 = a_180.(i1_183 + i_185) in
            a_180.(i2_184 + i_185) <- a_180.(i1_183 + i_185);
            a_180.(i1_183 + i_185) <- t_186
           done;
           (Some (1, (snd i_179)))
          end else (Some (0, (snd i_179)))
       | None -> (None))
     end in
    (match t_190 with
     | Some (i_191) ->
        begin
         for j_193 = ((! t_171) + 1) to (t_175 - 1) do
          if (not ((t_173.arr).((j_193 * t_173.n) + (! t_172)) = 0.)) then begin
           for j_194 = ((! t_172) + 1) to (t_174 - 1) do
            (t_173.arr).((j_193 * t_173.n) + j_194) <-
             ((t_173.arr).((j_193 * t_173.n) + j_194) -.
               (((t_173.arr).((j_193 * t_173.n) + (! t_172)) /.
                  (t_173.arr).(((! t_171) * t_173.n) + (! t_172))) *.
                 (t_173.arr).(((! t_171) * t_173.n) + j_194)))
           done;
           (t_173.arr).((j_193 * t_173.n) + (! t_172)) <- 0.
          end else ()
         done;
         (t_176 :=
           ((! t_176) *. (t_173.arr).(((! t_171) * t_173.n) + (! t_172))))
        end;
        for j_192 = 0 to (fst i_191) do (t_177 := ((! t_177) *. (-1.))) done;
        (t_171 := ((! t_171) + 1))
     | None -> (t_177 := 0.));
    (t_172 := ((! t_172) + 1))
   done;
   (t_173, ((! t_177) *. (! t_176)), (! t_171))>.
# val resIA1 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutJustMatrix(Funct4.IArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_195 ->
   let t_196 = (ref 0) in
   let t_197 = (ref 0) in
   let t_199 =
    (Array.map (fun x_198 -> (Array.copy x_198)) (Array.copy a_195)) in
   let t_200 = (Array.length a_195.(0)) in
   let t_201 = (Array.length a_195) in
   let t_202 = (ref 1) in
   let t_203 = (ref 1) in
   while (((! t_197) < t_200) && ((! t_196) < t_201)) do
    let t_204 = (ref (None)) in
    let t_210 =
     begin
      for j_207 = (! t_196) to (t_201 - 1) do
       let t_208 = (t_199.(j_207)).(! t_197) in
       if (not (t_208 = 0)) then
        (match (! t_204) with
         | Some (i_209) ->
            if ((abs (snd i_209)) > (abs t_208)) then
             (t_204 := (Some (j_207, t_208)))
            else ()
         | None -> (t_204 := (Some (j_207, t_208))))
       else ()
      done;
      (match (! t_204) with
       | Some (i_205) ->
          if ((fst i_205) <> (! t_196)) then begin
           let t_206 = t_199.(! t_196) in
           t_199.(! t_196) <- t_199.(fst i_205);
           t_199.(fst i_205) <- t_206;
           (Some (1, (snd i_205)))
          end else (Some (0, (snd i_205)))
       | None -> (None))
     end in
    (match t_210 with
     | Some (i_211) ->
        begin
         for j_213 = ((! t_196) + 1) to (t_201 - 1) do
          if (not ((t_199.(j_213)).(! t_197) = 0)) then begin
           for j_214 = ((! t_197) + 1) to (t_200 - 1) do
            (t_199.(j_213)).(j_214) <-
             ((((t_199.(j_213)).(j_214) * (t_199.(! t_196)).(! t_197)) -
                ((t_199.(! t_196)).(j_214) * (t_199.(j_213)).(! t_196))) /
               (! t_202))
           done;
           (t_199.(j_213)).(! t_197) <- 0
          end else ()
         done;
         (t_202 := (t_199.(! t_196)).(! t_197))
        end;
        for j_212 = 0 to (fst i_211) do (t_203 := ((! t_203) * (-1))) done;
        (t_196 := ((! t_196) + 1))
     | None -> (t_203 := 0));
    (t_197 := ((! t_197) + 1))
   done;
   t_199>.
# val resIA2 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutDet(Funct4.IArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_215 ->
   let t_216 = (ref 0) in
   let t_217 = (ref 0) in
   let t_219 =
    (Array.map (fun x_218 -> (Array.copy x_218)) (Array.copy a_215)) in
   let t_220 = (Array.length a_215.(0)) in
   let t_221 = (Array.length a_215) in
   let t_222 = (ref 1) in
   let t_223 = (ref 1) in
   while (((! t_217) < t_220) && ((! t_216) < t_221)) do
    let t_224 = (ref (None)) in
    let t_230 =
     begin
      for j_227 = (! t_216) to (t_221 - 1) do
       let t_228 = (t_219.(j_227)).(! t_217) in
       if (not (t_228 = 0)) then
        (match (! t_224) with
         | Some (i_229) ->
            if ((abs (snd i_229)) > (abs t_228)) then
             (t_224 := (Some (j_227, t_228)))
            else ()
         | None -> (t_224 := (Some (j_227, t_228))))
       else ()
      done;
      (match (! t_224) with
       | Some (i_225) ->
          if ((fst i_225) <> (! t_216)) then begin
           let t_226 = t_219.(! t_216) in
           t_219.(! t_216) <- t_219.(fst i_225);
           t_219.(fst i_225) <- t_226;
           (Some (1, (snd i_225)))
          end else (Some (0, (snd i_225)))
       | None -> (None))
     end in
    (match t_230 with
     | Some (i_231) ->
        begin
         for j_233 = ((! t_216) + 1) to (t_221 - 1) do
          if (not ((t_219.(j_233)).(! t_217) = 0)) then begin
           for j_234 = ((! t_217) + 1) to (t_220 - 1) do
            (t_219.(j_233)).(j_234) <-
             ((((t_219.(j_233)).(j_234) * (t_219.(! t_216)).(! t_217)) -
                ((t_219.(! t_216)).(j_234) * (t_219.(j_233)).(! t_216))) /
               (! t_222))
           done;
           (t_219.(j_233)).(! t_217) <- 0
          end else ()
         done;
         (t_222 := (t_219.(! t_216)).(! t_217))
        end;
        for j_232 = 0 to (fst i_231) do (t_223 := ((! t_223) * (-1))) done;
        (t_216 := ((! t_216) + 1))
     | None -> (t_223 := 0));
    (t_217 := ((! t_217) + 1))
   done;
   (t_219, ((! t_223) * (! t_222)))>.
# val resIA3 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutRank(Funct4.IArrayContainer)(Funct4.Rank)(Funct4.IntegerDomain).res)
  code =
  .<fun a_235 ->
   let t_236 = (ref 0) in
   let t_237 = (ref 0) in
   let t_239 =
    (Array.map (fun x_238 -> (Array.copy x_238)) (Array.copy a_235)) in
   let t_240 = (Array.length a_235.(0)) in
   let t_241 = (Array.length a_235) in
   let t_242 = (ref 1) in
   let t_243 = (ref 1) in
   while (((! t_237) < t_240) && ((! t_236) < t_241)) do
    let t_244 = (ref (None)) in
    let t_250 =
     begin
      for j_247 = (! t_236) to (t_241 - 1) do
       let t_248 = (t_239.(j_247)).(! t_237) in
       if (not (t_248 = 0)) then
        (match (! t_244) with
         | Some (i_249) ->
            if ((abs (snd i_249)) > (abs t_248)) then
             (t_244 := (Some (j_247, t_248)))
            else ()
         | None -> (t_244 := (Some (j_247, t_248))))
       else ()
      done;
      (match (! t_244) with
       | Some (i_245) ->
          if ((fst i_245) <> (! t_236)) then begin
           let t_246 = t_239.(! t_236) in
           t_239.(! t_236) <- t_239.(fst i_245);
           t_239.(fst i_245) <- t_246;
           (Some (1, (snd i_245)))
          end else (Some (0, (snd i_245)))
       | None -> (None))
     end in
    (match t_250 with
     | Some (i_251) ->
        begin
         for j_253 = ((! t_236) + 1) to (t_241 - 1) do
          if (not ((t_239.(j_253)).(! t_237) = 0)) then begin
           for j_254 = ((! t_237) + 1) to (t_240 - 1) do
            (t_239.(j_253)).(j_254) <-
             ((((t_239.(j_253)).(j_254) * (t_239.(! t_236)).(! t_237)) -
                ((t_239.(! t_236)).(j_254) * (t_239.(j_253)).(! t_236))) /
               (! t_242))
           done;
           (t_239.(j_253)).(! t_237) <- 0
          end else ()
         done;
         (t_242 := (t_239.(! t_236)).(! t_237))
        end;
        for j_252 = 0 to (fst i_251) do (t_243 := ((! t_243) * (-1))) done;
        (t_236 := ((! t_236) + 1))
     | None -> (t_243 := 0));
    (t_237 := ((! t_237) + 1))
   done;
   (t_239, (! t_236))>.
# val resIA4 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutDetRank(Funct4.IArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_255 ->
   let t_256 = (ref 0) in
   let t_257 = (ref 0) in
   let t_259 =
    (Array.map (fun x_258 -> (Array.copy x_258)) (Array.copy a_255)) in
   let t_260 = (Array.length a_255.(0)) in
   let t_261 = (Array.length a_255) in
   let t_262 = (ref 1) in
   let t_263 = (ref 1) in
   while (((! t_257) < t_260) && ((! t_256) < t_261)) do
    let t_264 = (ref (None)) in
    let t_270 =
     begin
      for j_267 = (! t_256) to (t_261 - 1) do
       let t_268 = (t_259.(j_267)).(! t_257) in
       if (not (t_268 = 0)) then
        (match (! t_264) with
         | Some (i_269) ->
            if ((abs (snd i_269)) > (abs t_268)) then
             (t_264 := (Some (j_267, t_268)))
            else ()
         | None -> (t_264 := (Some (j_267, t_268))))
       else ()
      done;
      (match (! t_264) with
       | Some (i_265) ->
          if ((fst i_265) <> (! t_256)) then begin
           let t_266 = t_259.(! t_256) in
           t_259.(! t_256) <- t_259.(fst i_265);
           t_259.(fst i_265) <- t_266;
           (Some (1, (snd i_265)))
          end else (Some (0, (snd i_265)))
       | None -> (None))
     end in
    (match t_270 with
     | Some (i_271) ->
        begin
         for j_273 = ((! t_256) + 1) to (t_261 - 1) do
          if (not ((t_259.(j_273)).(! t_257) = 0)) then begin
           for j_274 = ((! t_257) + 1) to (t_260 - 1) do
            (t_259.(j_273)).(j_274) <-
             ((((t_259.(j_273)).(j_274) * (t_259.(! t_256)).(! t_257)) -
                ((t_259.(! t_256)).(j_274) * (t_259.(j_273)).(! t_256))) /
               (! t_262))
           done;
           (t_259.(j_273)).(! t_257) <- 0
          end else ()
         done;
         (t_262 := (t_259.(! t_256)).(! t_257))
        end;
        for j_272 = 0 to (fst i_271) do (t_263 := ((! t_263) * (-1))) done;
        (t_256 := ((! t_256) + 1))
     | None -> (t_263 := 0));
    (t_257 := ((! t_257) + 1))
   done;
   (t_259, ((! t_263) * (! t_262)), (! t_256))>.
# val resIV1 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutJustMatrix(Funct4.IVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_275 ->
   let t_276 = (ref 0) in
   let t_277 = (ref 0) in
   let t_278 = {arr = (Array.copy a_275.arr)} (a_275) in
   let t_279 = a_275.m in
   let t_280 = a_275.n in
   let t_281 = (ref 1) in
   let t_282 = (ref 1) in
   while (((! t_277) < t_279) && ((! t_276) < t_280)) do
    let t_283 = (ref (None)) in
    let t_295 =
     begin
      for j_292 = (! t_276) to (t_280 - 1) do
       let t_293 = (t_278.arr).((j_292 * t_278.n) + (! t_277)) in
       if (not (t_293 = 0)) then
        (match (! t_283) with
         | Some (i_294) ->
            if ((abs (snd i_294)) > (abs t_293)) then
             (t_283 := (Some (j_292, t_293)))
            else ()
         | None -> (t_283 := (Some (j_292, t_293))))
       else ()
      done;
      (match (! t_283) with
       | Some (i_284) ->
          if ((fst i_284) <> (! t_276)) then begin
           let a_285 = t_278.arr
           and n_286 = t_278.n
           and m_287 = t_278.m in
           let i1_288 = ((! t_276) * n_286)
           and i2_289 = ((fst i_284) * n_286) in
           for i_290 = 0 to (m_287 - 1) do
            let t_291 = a_285.(i1_288 + i_290) in
            a_285.(i2_289 + i_290) <- a_285.(i1_288 + i_290);
            a_285.(i1_288 + i_290) <- t_291
           done;
           (Some (1, (snd i_284)))
          end else (Some (0, (snd i_284)))
       | None -> (None))
     end in
    (match t_295 with
     | Some (i_296) ->
        begin
         for j_298 = ((! t_276) + 1) to (t_280 - 1) do
          if (not ((t_278.arr).((j_298 * t_278.n) + (! t_277)) = 0)) then begin
           for j_299 = ((! t_277) + 1) to (t_279 - 1) do
            (t_278.arr).((j_298 * t_278.n) + j_299) <-
             ((((t_278.arr).((j_298 * t_278.n) + j_299) *
                 (t_278.arr).(((! t_276) * t_278.n) + (! t_277))) -
                ((t_278.arr).(((! t_276) * t_278.n) + j_299) *
                  (t_278.arr).((j_298 * t_278.n) + (! t_276)))) / (! t_281))
           done;
           (t_278.arr).((j_298 * t_278.n) + (! t_277)) <- 0
          end else ()
         done;
         (t_281 := (t_278.arr).(((! t_276) * t_278.n) + (! t_277)))
        end;
        for j_297 = 0 to (fst i_296) do (t_282 := ((! t_282) * (-1))) done;
        (t_276 := ((! t_276) + 1))
     | None -> (t_282 := 0));
    (t_277 := ((! t_277) + 1))
   done;
   t_278>.
# val resIV2 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutDet(Funct4.IVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_300 ->
   let t_301 = (ref 0) in
   let t_302 = (ref 0) in
   let t_303 = {arr = (Array.copy a_300.arr)} (a_300) in
   let t_304 = a_300.m in
   let t_305 = a_300.n in
   let t_306 = (ref 1) in
   let t_307 = (ref 1) in
   while (((! t_302) < t_304) && ((! t_301) < t_305)) do
    let t_308 = (ref (None)) in
    let t_320 =
     begin
      for j_317 = (! t_301) to (t_305 - 1) do
       let t_318 = (t_303.arr).((j_317 * t_303.n) + (! t_302)) in
       if (not (t_318 = 0)) then
        (match (! t_308) with
         | Some (i_319) ->
            if ((abs (snd i_319)) > (abs t_318)) then
             (t_308 := (Some (j_317, t_318)))
            else ()
         | None -> (t_308 := (Some (j_317, t_318))))
       else ()
      done;
      (match (! t_308) with
       | Some (i_309) ->
          if ((fst i_309) <> (! t_301)) then begin
           let a_310 = t_303.arr
           and n_311 = t_303.n
           and m_312 = t_303.m in
           let i1_313 = ((! t_301) * n_311)
           and i2_314 = ((fst i_309) * n_311) in
           for i_315 = 0 to (m_312 - 1) do
            let t_316 = a_310.(i1_313 + i_315) in
            a_310.(i2_314 + i_315) <- a_310.(i1_313 + i_315);
            a_310.(i1_313 + i_315) <- t_316
           done;
           (Some (1, (snd i_309)))
          end else (Some (0, (snd i_309)))
       | None -> (None))
     end in
    (match t_320 with
     | Some (i_321) ->
        begin
         for j_323 = ((! t_301) + 1) to (t_305 - 1) do
          if (not ((t_303.arr).((j_323 * t_303.n) + (! t_302)) = 0)) then begin
           for j_324 = ((! t_302) + 1) to (t_304 - 1) do
            (t_303.arr).((j_323 * t_303.n) + j_324) <-
             ((((t_303.arr).((j_323 * t_303.n) + j_324) *
                 (t_303.arr).(((! t_301) * t_303.n) + (! t_302))) -
                ((t_303.arr).(((! t_301) * t_303.n) + j_324) *
                  (t_303.arr).((j_323 * t_303.n) + (! t_301)))) / (! t_306))
           done;
           (t_303.arr).((j_323 * t_303.n) + (! t_302)) <- 0
          end else ()
         done;
         (t_306 := (t_303.arr).(((! t_301) * t_303.n) + (! t_302)))
        end;
        for j_322 = 0 to (fst i_321) do (t_307 := ((! t_307) * (-1))) done;
        (t_301 := ((! t_301) + 1))
     | None -> (t_307 := 0));
    (t_302 := ((! t_302) + 1))
   done;
   (t_303, ((! t_307) * (! t_306)))>.
# val resIV3 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutRank(Funct4.IVectorContainer)(Funct4.Rank)(Funct4.IntegerDomain).res)
  code =
  .<fun a_325 ->
   let t_326 = (ref 0) in
   let t_327 = (ref 0) in
   let t_328 = {arr = (Array.copy a_325.arr)} (a_325) in
   let t_329 = a_325.m in
   let t_330 = a_325.n in
   let t_331 = (ref 1) in
   let t_332 = (ref 1) in
   while (((! t_327) < t_329) && ((! t_326) < t_330)) do
    let t_333 = (ref (None)) in
    let t_345 =
     begin
      for j_342 = (! t_326) to (t_330 - 1) do
       let t_343 = (t_328.arr).((j_342 * t_328.n) + (! t_327)) in
       if (not (t_343 = 0)) then
        (match (! t_333) with
         | Some (i_344) ->
            if ((abs (snd i_344)) > (abs t_343)) then
             (t_333 := (Some (j_342, t_343)))
            else ()
         | None -> (t_333 := (Some (j_342, t_343))))
       else ()
      done;
      (match (! t_333) with
       | Some (i_334) ->
          if ((fst i_334) <> (! t_326)) then begin
           let a_335 = t_328.arr
           and n_336 = t_328.n
           and m_337 = t_328.m in
           let i1_338 = ((! t_326) * n_336)
           and i2_339 = ((fst i_334) * n_336) in
           for i_340 = 0 to (m_337 - 1) do
            let t_341 = a_335.(i1_338 + i_340) in
            a_335.(i2_339 + i_340) <- a_335.(i1_338 + i_340);
            a_335.(i1_338 + i_340) <- t_341
           done;
           (Some (1, (snd i_334)))
          end else (Some (0, (snd i_334)))
       | None -> (None))
     end in
    (match t_345 with
     | Some (i_346) ->
        begin
         for j_348 = ((! t_326) + 1) to (t_330 - 1) do
          if (not ((t_328.arr).((j_348 * t_328.n) + (! t_327)) = 0)) then begin
           for j_349 = ((! t_327) + 1) to (t_329 - 1) do
            (t_328.arr).((j_348 * t_328.n) + j_349) <-
             ((((t_328.arr).((j_348 * t_328.n) + j_349) *
                 (t_328.arr).(((! t_326) * t_328.n) + (! t_327))) -
                ((t_328.arr).(((! t_326) * t_328.n) + j_349) *
                  (t_328.arr).((j_348 * t_328.n) + (! t_326)))) / (! t_331))
           done;
           (t_328.arr).((j_348 * t_328.n) + (! t_327)) <- 0
          end else ()
         done;
         (t_331 := (t_328.arr).(((! t_326) * t_328.n) + (! t_327)))
        end;
        for j_347 = 0 to (fst i_346) do (t_332 := ((! t_332) * (-1))) done;
        (t_326 := ((! t_326) + 1))
     | None -> (t_332 := 0));
    (t_327 := ((! t_327) + 1))
   done;
   (t_328, (! t_326))>.
# val resIV4 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutDetRank(Funct4.IVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_350 ->
   let t_351 = (ref 0) in
   let t_352 = (ref 0) in
   let t_353 = {arr = (Array.copy a_350.arr)} (a_350) in
   let t_354 = a_350.m in
   let t_355 = a_350.n in
   let t_356 = (ref 1) in
   let t_357 = (ref 1) in
   while (((! t_352) < t_354) && ((! t_351) < t_355)) do
    let t_358 = (ref (None)) in
    let t_370 =
     begin
      for j_367 = (! t_351) to (t_355 - 1) do
       let t_368 = (t_353.arr).((j_367 * t_353.n) + (! t_352)) in
       if (not (t_368 = 0)) then
        (match (! t_358) with
         | Some (i_369) ->
            if ((abs (snd i_369)) > (abs t_368)) then
             (t_358 := (Some (j_367, t_368)))
            else ()
         | None -> (t_358 := (Some (j_367, t_368))))
       else ()
      done;
      (match (! t_358) with
       | Some (i_359) ->
          if ((fst i_359) <> (! t_351)) then begin
           let a_360 = t_353.arr
           and n_361 = t_353.n
           and m_362 = t_353.m in
           let i1_363 = ((! t_351) * n_361)
           and i2_364 = ((fst i_359) * n_361) in
           for i_365 = 0 to (m_362 - 1) do
            let t_366 = a_360.(i1_363 + i_365) in
            a_360.(i2_364 + i_365) <- a_360.(i1_363 + i_365);
            a_360.(i1_363 + i_365) <- t_366
           done;
           (Some (1, (snd i_359)))
          end else (Some (0, (snd i_359)))
       | None -> (None))
     end in
    (match t_370 with
     | Some (i_371) ->
        begin
         for j_373 = ((! t_351) + 1) to (t_355 - 1) do
          if (not ((t_353.arr).((j_373 * t_353.n) + (! t_352)) = 0)) then begin
           for j_374 = ((! t_352) + 1) to (t_354 - 1) do
            (t_353.arr).((j_373 * t_353.n) + j_374) <-
             ((((t_353.arr).((j_373 * t_353.n) + j_374) *
                 (t_353.arr).(((! t_351) * t_353.n) + (! t_352))) -
                ((t_353.arr).(((! t_351) * t_353.n) + j_374) *
                  (t_353.arr).((j_373 * t_353.n) + (! t_351)))) / (! t_356))
           done;
           (t_353.arr).((j_373 * t_353.n) + (! t_352)) <- 0
          end else ()
         done;
         (t_356 := (t_353.arr).(((! t_351) * t_353.n) + (! t_352)))
        end;
        for j_372 = 0 to (fst i_371) do (t_357 := ((! t_357) * (-1))) done;
        (t_351 := ((! t_351) + 1))
     | None -> (t_357 := 0));
    (t_352 := ((! t_352) + 1))
   done;
   (t_353, ((! t_357) * (! t_356)), (! t_351))>.
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
  Funct4.OutJustMatrix(Funct4.FVectorContainer)(Funct4.FDet).res = <fun>
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
  Funct4.OutJustMatrix(Funct4.IArrayContainer)(Funct4.IDet).res = <fun>
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
#                                                   val ia0 : Funct4.IArrayContainer.obj array array = [|[|1|]|]
val ia1 : Funct4.IArrayContainer.obj array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|]
val ia2 : Funct4.IArrayContainer.obj array array =
  [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|]
val ia3 : Funct4.IArrayContainer.obj array array =
  [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|]
val ia4 : Funct4.IArrayContainer.obj array array =
  [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]
val ia5 : Funct4.IArrayContainer.obj array array list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]|];
   [|[|1; 2; 3; 0|]; [|4; 13; 5; 0|]; [|-1; 3; 0; 0|]|];
   [|[|1; 2; 3|]; [|4; 13; 5|]; [|-1; 3; 0|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 13; 5|]; [|0; 3; 0|]|]]
val resI1 :
  Funct4.OutJustMatrix(Funct4.IArrayContainer)(Funct4.IDet).res list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|]]
# val resI2 : Funct4.OutDet(Funct4.IArrayContainer)(Funct4.IDet).res list =
  [([|[|1|]|], -1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], -50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], -50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], -50);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 0)]
# val resI3 :
  Funct4.OutRank(Funct4.IArrayContainer)(Funct4.Rank)(Funct4.IntegerDomain).res
  list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 2)]
# val resI4 :
  Funct4.OutDetRank(Funct4.IArrayContainer)(Funct4.IDet)(Funct4.Rank).res
  list =
  [([|[|1|]|], -1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], -50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], -50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], -50, 3);
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
#   val fa5 : Funct4.FArrayContainer.obj array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 13.; 5.|]; [|0.; 3.; 0.|]|]]
val resF1 :
  Funct4.OutJustMatrix(Funct4.FArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res
  list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|]]
# val resF2 : Funct4.OutDet(Funct4.FArrayContainer)(Funct4.FDet).res list =
  [([|[|1.|]|], -1.);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], -50.);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|],
    -50.);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    -50.);
   ([|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|],
    -0.)]
# val resF3 :
  Funct4.OutRank(Funct4.FArrayContainer)(Funct4.Rank)(Funct4.FloatDomain).res
  list =
  [([|[|1.|]|], 1);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], 3);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|], 3);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    3);
   ([|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|], 2)]
# val resF4 :
  Funct4.OutDetRank(Funct4.FArrayContainer)(Funct4.FDet)(Funct4.Rank).res
  list =
  [([|[|1.|]|], -1., 1);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|], -50., 3);
   ([|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|],
    -50., 3);
   ([|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|],
    -50., 3);
   ([|[|0.; 13.; 5.|]; [|0.; 0.; 2.23076923076923084|]; [|0.; 0.; 0.|]|],
    -0., 2)]
# 
