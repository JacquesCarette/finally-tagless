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
   Funct4.GenFA2.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_20 ->
   let t_21 = (ref 0) in
   let t_22 = (ref 0) in
   let t_24 = (Array.map (fun x_23 -> (Array.copy x_23)) (Array.copy a_20)) in
   let t_25 = (Array.length a_20.(0)) in
   let t_26 = (Array.length a_20) in
   let t_27 = (ref 1.) in
   let t_28 = (ref 1) in
   while (((! t_22) < t_25) && ((! t_21) < t_26)) do
    let t_29 = (! t_21) in
    let t_30 = (! t_22) in
    let t_31 = (ref (None)) in
    let t_37 =
     begin
      for j_34 = t_29 to (t_26 - 1) do
       let t_35 = (t_24.(j_34)).(t_30) in
       if (not (t_35 = 0.)) then
        (match (! t_31) with
         | Some (i_36) ->
            if ((abs_float (snd i_36)) < (abs_float t_35)) then
             (t_31 := (Some (j_34, t_35)))
            else ()
         | None -> (t_31 := (Some (j_34, t_35))))
       else ()
      done;
      (match (! t_31) with
       | Some (i_32) ->
          if ((fst i_32) <> t_29) then begin
           let t_33 = t_24.(t_29) in
           t_24.(t_29) <- t_24.(fst i_32);
           t_24.(fst i_32) <- t_33;
           (t_28 := (~- (! t_28)))
          end else ();
          (Some (snd i_32))
       | None -> (None))
     end in
    (match t_37 with
     | Some (i_38) ->
        begin
         for j_39 = (t_29 + 1) to (t_26 - 1) do
          if (not ((t_24.(j_39)).(t_30) = 0.)) then begin
           for j_40 = (t_30 + 1) to (t_25 - 1) do
            (t_24.(j_39)).(j_40) <-
             ((t_24.(j_39)).(j_40) -.
               (((t_24.(j_39)).(t_30) /. (t_24.(t_29)).(t_30)) *.
                 (t_24.(t_29)).(j_40)))
           done;
           (t_24.(j_39)).(t_30) <- 0.
          end else ()
         done;
         (t_27 := ((! t_27) *. i_38))
        end;
        (t_21 := ((! t_21) + 1))
     | None -> (t_28 := 0));
    (t_22 := ((! t_22) + 1))
   done;
   (t_24,
    if ((! t_28) = 0) then 0.
    else if ((! t_28) = 1) then (! t_27)
    else (~-. (! t_27)))>.
# val resFA3 :
  ('a,
   Funct4.GenFA3.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_41 ->
   let t_42 = (ref 0) in
   let t_43 = (ref 0) in
   let t_45 = (Array.map (fun x_44 -> (Array.copy x_44)) (Array.copy a_41)) in
   let t_46 = (Array.length a_41.(0)) in
   let t_47 = (Array.length a_41) in
   while (((! t_43) < t_46) && ((! t_42) < t_47)) do
    let t_48 = (! t_42) in
    let t_49 = (! t_43) in
    let t_50 = (ref (None)) in
    let t_56 =
     begin
      for j_53 = t_48 to (t_47 - 1) do
       let t_54 = (t_45.(j_53)).(t_49) in
       if (not (t_54 = 0.)) then
        (match (! t_50) with
         | Some (i_55) ->
            if ((abs_float (snd i_55)) < (abs_float t_54)) then
             (t_50 := (Some (j_53, t_54)))
            else ()
         | None -> (t_50 := (Some (j_53, t_54))))
       else ()
      done;
      (match (! t_50) with
       | Some (i_51) ->
          if ((fst i_51) <> t_48) then begin
           let t_52 = t_45.(t_48) in
           t_45.(t_48) <- t_45.(fst i_51);
           t_45.(fst i_51) <- t_52;
           ()
          end else ();
          (Some (snd i_51))
       | None -> (None))
     end in
    (match t_56 with
     | Some (i_57) ->
        begin
         for j_58 = (t_48 + 1) to (t_47 - 1) do
          if (not ((t_45.(j_58)).(t_49) = 0.)) then begin
           for j_59 = (t_49 + 1) to (t_46 - 1) do
            (t_45.(j_58)).(j_59) <-
             ((t_45.(j_58)).(j_59) -.
               (((t_45.(j_58)).(t_49) /. (t_45.(t_48)).(t_49)) *.
                 (t_45.(t_48)).(j_59)))
           done;
           (t_45.(j_58)).(t_49) <- 0.
          end else ()
         done;
         ()
        end;
        (t_42 := ((! t_42) + 1))
     | None -> ());
    (t_43 := ((! t_43) + 1))
   done;
   (t_45, (! t_42))>.
# val resFA4 :
  ('a,
   Funct4.GenFA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_60 ->
   let t_61 = (ref 0) in
   let t_62 = (ref 0) in
   let t_64 = (Array.map (fun x_63 -> (Array.copy x_63)) (Array.copy a_60)) in
   let t_65 = (Array.length a_60.(0)) in
   let t_66 = (Array.length a_60) in
   let t_67 = (ref 1.) in
   let t_68 = (ref 1) in
   while (((! t_62) < t_65) && ((! t_61) < t_66)) do
    let t_69 = (! t_61) in
    let t_70 = (! t_62) in
    let t_71 = (ref (None)) in
    let t_77 =
     begin
      for j_74 = t_69 to (t_66 - 1) do
       let t_75 = (t_64.(j_74)).(t_70) in
       if (not (t_75 = 0.)) then
        (match (! t_71) with
         | Some (i_76) ->
            if ((abs_float (snd i_76)) < (abs_float t_75)) then
             (t_71 := (Some (j_74, t_75)))
            else ()
         | None -> (t_71 := (Some (j_74, t_75))))
       else ()
      done;
      (match (! t_71) with
       | Some (i_72) ->
          if ((fst i_72) <> t_69) then begin
           let t_73 = t_64.(t_69) in
           t_64.(t_69) <- t_64.(fst i_72);
           t_64.(fst i_72) <- t_73;
           (t_68 := (~- (! t_68)))
          end else ();
          (Some (snd i_72))
       | None -> (None))
     end in
    (match t_77 with
     | Some (i_78) ->
        begin
         for j_79 = (t_69 + 1) to (t_66 - 1) do
          if (not ((t_64.(j_79)).(t_70) = 0.)) then begin
           for j_80 = (t_70 + 1) to (t_65 - 1) do
            (t_64.(j_79)).(j_80) <-
             ((t_64.(j_79)).(j_80) -.
               (((t_64.(j_79)).(t_70) /. (t_64.(t_69)).(t_70)) *.
                 (t_64.(t_69)).(j_80)))
           done;
           (t_64.(j_79)).(t_70) <- 0.
          end else ()
         done;
         (t_67 := ((! t_67) *. i_78))
        end;
        (t_61 := ((! t_61) + 1))
     | None -> (t_68 := 0));
    (t_62 := ((! t_62) + 1))
   done;
   (t_64,
    if ((! t_68) = 0) then 0.
    else if ((! t_68) = 1) then (! t_67)
    else (~-. (! t_67)), (! t_61))>.
# val resFV1 :
  ('a,
   Funct4.GenFV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_81 ->
   let t_82 = (ref 0) in
   let t_83 = (ref 0) in
   let t_84 = {arr = (Array.copy a_81.arr)} (a_81) in
   let t_85 = a_81.m in
   let t_86 = a_81.n in
   let t_87 = (ref 1.) in
   let t_88 = (ref 1) in
   while (((! t_83) < t_85) && ((! t_82) < t_86)) do
    let t_89 = (! t_82) in
    let t_90 = (! t_83) in
    let t_91 = (ref (None)) in
    let t_103 =
     begin
      for j_100 = t_89 to (t_86 - 1) do
       let t_101 = (t_84.arr).((j_100 * t_84.n) + t_90) in
       if (not (t_101 = 0.)) then
        (match (! t_91) with
         | Some (i_102) ->
            if ((abs_float (snd i_102)) < (abs_float t_101)) then
             (t_91 := (Some (j_100, t_101)))
            else ()
         | None -> (t_91 := (Some (j_100, t_101))))
       else ()
      done;
      (match (! t_91) with
       | Some (i_92) ->
          if ((fst i_92) <> t_89) then begin
           let a_93 = t_84.arr
           and n_94 = t_84.n
           and m_95 = t_84.m in
           let i1_96 = (t_89 * n_94)
           and i2_97 = ((fst i_92) * n_94) in
           for i_98 = 0 to (m_95 - 1) do
            let t_99 = a_93.(i1_96 + i_98) in
            a_93.(i2_97 + i_98) <- a_93.(i1_96 + i_98);
            a_93.(i1_96 + i_98) <- t_99
           done;
           (t_88 := (~- (! t_88)))
          end else ();
          (Some (snd i_92))
       | None -> (None))
     end in
    (match t_103 with
     | Some (i_104) ->
        begin
         for j_105 = (t_89 + 1) to (t_86 - 1) do
          if (not ((t_84.arr).((j_105 * t_84.n) + t_90) = 0.)) then begin
           for j_106 = (t_90 + 1) to (t_85 - 1) do
            (t_84.arr).((j_105 * t_84.n) + j_106) <-
             ((t_84.arr).((j_105 * t_84.n) + j_106) -.
               (((t_84.arr).((j_105 * t_84.n) + t_90) /.
                  (t_84.arr).((t_89 * t_84.n) + t_90)) *.
                 (t_84.arr).((t_89 * t_84.n) + j_106)))
           done;
           (t_84.arr).((j_105 * t_84.n) + t_90) <- 0.
          end else ()
         done;
         (t_87 := ((! t_87) *. i_104))
        end;
        (t_82 := ((! t_82) + 1))
     | None -> (t_88 := 0));
    (t_83 := ((! t_83) + 1))
   done;
   t_84>.
# val resFV2 :
  ('a,
   Funct4.GenFV2.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_107 ->
   let t_108 = (ref 0) in
   let t_109 = (ref 0) in
   let t_110 = {arr = (Array.copy a_107.arr)} (a_107) in
   let t_111 = a_107.m in
   let t_112 = a_107.n in
   let t_113 = (ref 1.) in
   let t_114 = (ref 1) in
   while (((! t_109) < t_111) && ((! t_108) < t_112)) do
    let t_115 = (! t_108) in
    let t_116 = (! t_109) in
    let t_117 = (ref (None)) in
    let t_129 =
     begin
      for j_126 = t_115 to (t_112 - 1) do
       let t_127 = (t_110.arr).((j_126 * t_110.n) + t_116) in
       if (not (t_127 = 0.)) then
        (match (! t_117) with
         | Some (i_128) ->
            if ((abs_float (snd i_128)) < (abs_float t_127)) then
             (t_117 := (Some (j_126, t_127)))
            else ()
         | None -> (t_117 := (Some (j_126, t_127))))
       else ()
      done;
      (match (! t_117) with
       | Some (i_118) ->
          if ((fst i_118) <> t_115) then begin
           let a_119 = t_110.arr
           and n_120 = t_110.n
           and m_121 = t_110.m in
           let i1_122 = (t_115 * n_120)
           and i2_123 = ((fst i_118) * n_120) in
           for i_124 = 0 to (m_121 - 1) do
            let t_125 = a_119.(i1_122 + i_124) in
            a_119.(i2_123 + i_124) <- a_119.(i1_122 + i_124);
            a_119.(i1_122 + i_124) <- t_125
           done;
           (t_114 := (~- (! t_114)))
          end else ();
          (Some (snd i_118))
       | None -> (None))
     end in
    (match t_129 with
     | Some (i_130) ->
        begin
         for j_131 = (t_115 + 1) to (t_112 - 1) do
          if (not ((t_110.arr).((j_131 * t_110.n) + t_116) = 0.)) then begin
           for j_132 = (t_116 + 1) to (t_111 - 1) do
            (t_110.arr).((j_131 * t_110.n) + j_132) <-
             ((t_110.arr).((j_131 * t_110.n) + j_132) -.
               (((t_110.arr).((j_131 * t_110.n) + t_116) /.
                  (t_110.arr).((t_115 * t_110.n) + t_116)) *.
                 (t_110.arr).((t_115 * t_110.n) + j_132)))
           done;
           (t_110.arr).((j_131 * t_110.n) + t_116) <- 0.
          end else ()
         done;
         (t_113 := ((! t_113) *. i_130))
        end;
        (t_108 := ((! t_108) + 1))
     | None -> (t_114 := 0));
    (t_109 := ((! t_109) + 1))
   done;
   (t_110,
    if ((! t_114) = 0) then 0.
    else if ((! t_114) = 1) then (! t_113)
    else (~-. (! t_113)))>.
# val resFV3 :
  ('a,
   Funct4.GenFV3.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_133 ->
   let t_134 = (ref 0) in
   let t_135 = (ref 0) in
   let t_136 = {arr = (Array.copy a_133.arr)} (a_133) in
   let t_137 = a_133.m in
   let t_138 = a_133.n in
   while (((! t_135) < t_137) && ((! t_134) < t_138)) do
    let t_139 = (! t_134) in
    let t_140 = (! t_135) in
    let t_141 = (ref (None)) in
    let t_153 =
     begin
      for j_150 = t_139 to (t_138 - 1) do
       let t_151 = (t_136.arr).((j_150 * t_136.n) + t_140) in
       if (not (t_151 = 0.)) then
        (match (! t_141) with
         | Some (i_152) ->
            if ((abs_float (snd i_152)) < (abs_float t_151)) then
             (t_141 := (Some (j_150, t_151)))
            else ()
         | None -> (t_141 := (Some (j_150, t_151))))
       else ()
      done;
      (match (! t_141) with
       | Some (i_142) ->
          if ((fst i_142) <> t_139) then begin
           let a_143 = t_136.arr
           and n_144 = t_136.n
           and m_145 = t_136.m in
           let i1_146 = (t_139 * n_144)
           and i2_147 = ((fst i_142) * n_144) in
           for i_148 = 0 to (m_145 - 1) do
            let t_149 = a_143.(i1_146 + i_148) in
            a_143.(i2_147 + i_148) <- a_143.(i1_146 + i_148);
            a_143.(i1_146 + i_148) <- t_149
           done;
           ()
          end else ();
          (Some (snd i_142))
       | None -> (None))
     end in
    (match t_153 with
     | Some (i_154) ->
        begin
         for j_155 = (t_139 + 1) to (t_138 - 1) do
          if (not ((t_136.arr).((j_155 * t_136.n) + t_140) = 0.)) then begin
           for j_156 = (t_140 + 1) to (t_137 - 1) do
            (t_136.arr).((j_155 * t_136.n) + j_156) <-
             ((t_136.arr).((j_155 * t_136.n) + j_156) -.
               (((t_136.arr).((j_155 * t_136.n) + t_140) /.
                  (t_136.arr).((t_139 * t_136.n) + t_140)) *.
                 (t_136.arr).((t_139 * t_136.n) + j_156)))
           done;
           (t_136.arr).((j_155 * t_136.n) + t_140) <- 0.
          end else ()
         done;
         ()
        end;
        (t_134 := ((! t_134) + 1))
     | None -> ());
    (t_135 := ((! t_135) + 1))
   done;
   (t_136, (! t_134))>.
# val resFV4 :
  ('a,
   Funct4.GenFV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_157 ->
   let t_158 = (ref 0) in
   let t_159 = (ref 0) in
   let t_160 = {arr = (Array.copy a_157.arr)} (a_157) in
   let t_161 = a_157.m in
   let t_162 = a_157.n in
   let t_163 = (ref 1.) in
   let t_164 = (ref 1) in
   while (((! t_159) < t_161) && ((! t_158) < t_162)) do
    let t_165 = (! t_158) in
    let t_166 = (! t_159) in
    let t_167 = (ref (None)) in
    let t_179 =
     begin
      for j_176 = t_165 to (t_162 - 1) do
       let t_177 = (t_160.arr).((j_176 * t_160.n) + t_166) in
       if (not (t_177 = 0.)) then
        (match (! t_167) with
         | Some (i_178) ->
            if ((abs_float (snd i_178)) < (abs_float t_177)) then
             (t_167 := (Some (j_176, t_177)))
            else ()
         | None -> (t_167 := (Some (j_176, t_177))))
       else ()
      done;
      (match (! t_167) with
       | Some (i_168) ->
          if ((fst i_168) <> t_165) then begin
           let a_169 = t_160.arr
           and n_170 = t_160.n
           and m_171 = t_160.m in
           let i1_172 = (t_165 * n_170)
           and i2_173 = ((fst i_168) * n_170) in
           for i_174 = 0 to (m_171 - 1) do
            let t_175 = a_169.(i1_172 + i_174) in
            a_169.(i2_173 + i_174) <- a_169.(i1_172 + i_174);
            a_169.(i1_172 + i_174) <- t_175
           done;
           (t_164 := (~- (! t_164)))
          end else ();
          (Some (snd i_168))
       | None -> (None))
     end in
    (match t_179 with
     | Some (i_180) ->
        begin
         for j_181 = (t_165 + 1) to (t_162 - 1) do
          if (not ((t_160.arr).((j_181 * t_160.n) + t_166) = 0.)) then begin
           for j_182 = (t_166 + 1) to (t_161 - 1) do
            (t_160.arr).((j_181 * t_160.n) + j_182) <-
             ((t_160.arr).((j_181 * t_160.n) + j_182) -.
               (((t_160.arr).((j_181 * t_160.n) + t_166) /.
                  (t_160.arr).((t_165 * t_160.n) + t_166)) *.
                 (t_160.arr).((t_165 * t_160.n) + j_182)))
           done;
           (t_160.arr).((j_181 * t_160.n) + t_166) <- 0.
          end else ()
         done;
         (t_163 := ((! t_163) *. i_180))
        end;
        (t_158 := ((! t_158) + 1))
     | None -> (t_164 := 0));
    (t_159 := ((! t_159) + 1))
   done;
   (t_160,
    if ((! t_164) = 0) then 0.
    else if ((! t_164) = 1) then (! t_163)
    else (~-. (! t_163)), (! t_158))>.
# val resIA1 :
  ('a,
   Funct4.GenIA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_183 ->
   let t_184 = (ref 0) in
   let t_185 = (ref 0) in
   let t_187 =
    (Array.map (fun x_186 -> (Array.copy x_186)) (Array.copy a_183)) in
   let t_188 = (Array.length a_183.(0)) in
   let t_189 = (Array.length a_183) in
   let t_190 = (ref 1) in
   let t_191 = (ref 1) in
   while (((! t_185) < t_188) && ((! t_184) < t_189)) do
    let t_192 = (! t_184) in
    let t_193 = (! t_185) in
    let t_194 = (ref (None)) in
    let t_200 =
     begin
      for j_197 = t_192 to (t_189 - 1) do
       let t_198 = (t_187.(j_197)).(t_193) in
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
          if ((fst i_195) <> t_192) then begin
           let t_196 = t_187.(t_192) in
           t_187.(t_192) <- t_187.(fst i_195);
           t_187.(fst i_195) <- t_196;
           (t_191 := (~- (! t_191)))
          end else ();
          (Some (snd i_195))
       | None -> (None))
     end in
    (match t_200 with
     | Some (i_201) ->
        begin
         for j_202 = (t_192 + 1) to (t_189 - 1) do
          if (not ((t_187.(j_202)).(t_193) = 0)) then begin
           for j_203 = (t_193 + 1) to (t_188 - 1) do
            (t_187.(j_202)).(j_203) <-
             ((((t_187.(j_202)).(j_203) * (t_187.(t_192)).(t_193)) -
                ((t_187.(t_192)).(j_203) * (t_187.(j_202)).(t_192))) /
               (! t_190))
           done;
           (t_187.(j_202)).(t_193) <- 0
          end else ()
         done;
         (t_190 := i_201)
        end;
        (t_184 := ((! t_184) + 1))
     | None -> (t_191 := 0));
    (t_185 := ((! t_185) + 1))
   done;
   t_187>.
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
    let t_213 = (! t_205) in
    let t_214 = (! t_206) in
    let t_215 = (ref (None)) in
    let t_221 =
     begin
      for j_218 = t_213 to (t_210 - 1) do
       let t_219 = (t_208.(j_218)).(t_214) in
       if (not (t_219 = 0)) then
        (match (! t_215) with
         | Some (i_220) ->
            if ((abs (snd i_220)) > (abs t_219)) then
             (t_215 := (Some (j_218, t_219)))
            else ()
         | None -> (t_215 := (Some (j_218, t_219))))
       else ()
      done;
      (match (! t_215) with
       | Some (i_216) ->
          if ((fst i_216) <> t_213) then begin
           let t_217 = t_208.(t_213) in
           t_208.(t_213) <- t_208.(fst i_216);
           t_208.(fst i_216) <- t_217;
           (t_212 := (~- (! t_212)))
          end else ();
          (Some (snd i_216))
       | None -> (None))
     end in
    (match t_221 with
     | Some (i_222) ->
        begin
         for j_223 = (t_213 + 1) to (t_210 - 1) do
          if (not ((t_208.(j_223)).(t_214) = 0)) then begin
           for j_224 = (t_214 + 1) to (t_209 - 1) do
            (t_208.(j_223)).(j_224) <-
             ((((t_208.(j_223)).(j_224) * (t_208.(t_213)).(t_214)) -
                ((t_208.(t_213)).(j_224) * (t_208.(j_223)).(t_213))) /
               (! t_211))
           done;
           (t_208.(j_223)).(t_214) <- 0
          end else ()
         done;
         (t_211 := i_222)
        end;
        (t_205 := ((! t_205) + 1))
     | None -> (t_212 := 0));
    (t_206 := ((! t_206) + 1))
   done;
   (t_208,
    if ((! t_212) = 0) then 0
    else if ((! t_212) = 1) then (! t_211)
    else (~- (! t_211)))>.
# val resIA3 :
  ('a,
   Funct4.GenIA3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_225 ->
   let t_226 = (ref 0) in
   let t_227 = (ref 0) in
   let t_229 =
    (Array.map (fun x_228 -> (Array.copy x_228)) (Array.copy a_225)) in
   let t_230 = (Array.length a_225.(0)) in
   let t_231 = (Array.length a_225) in
   let t_232 = (ref 1) in
   let t_233 = (ref 1) in
   while (((! t_227) < t_230) && ((! t_226) < t_231)) do
    let t_234 = (! t_226) in
    let t_235 = (! t_227) in
    let t_236 = (ref (None)) in
    let t_242 =
     begin
      for j_239 = t_234 to (t_231 - 1) do
       let t_240 = (t_229.(j_239)).(t_235) in
       if (not (t_240 = 0)) then
        (match (! t_236) with
         | Some (i_241) ->
            if ((abs (snd i_241)) > (abs t_240)) then
             (t_236 := (Some (j_239, t_240)))
            else ()
         | None -> (t_236 := (Some (j_239, t_240))))
       else ()
      done;
      (match (! t_236) with
       | Some (i_237) ->
          if ((fst i_237) <> t_234) then begin
           let t_238 = t_229.(t_234) in
           t_229.(t_234) <- t_229.(fst i_237);
           t_229.(fst i_237) <- t_238;
           ()
          end else ();
          (Some (snd i_237))
       | None -> (None))
     end in
    (match t_242 with
     | Some (i_243) ->
        begin
         for j_244 = (t_234 + 1) to (t_231 - 1) do
          if (not ((t_229.(j_244)).(t_235) = 0)) then begin
           for j_245 = (t_235 + 1) to (t_230 - 1) do
            (t_229.(j_244)).(j_245) <-
             ((((t_229.(j_244)).(j_245) * (t_229.(t_234)).(t_235)) -
                ((t_229.(t_234)).(j_245) * (t_229.(j_244)).(t_234))) /
               (! t_232))
           done;
           (t_229.(j_244)).(t_235) <- 0
          end else ()
         done;
         (t_232 := i_243)
        end;
        (t_226 := ((! t_226) + 1))
     | None -> (t_233 := 0));
    (t_227 := ((! t_227) + 1))
   done;
   (t_229, (! t_226))>.
# val resIA4 :
  ('a,
   Funct4.GenIA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_246 ->
   let t_247 = (ref 0) in
   let t_248 = (ref 0) in
   let t_250 =
    (Array.map (fun x_249 -> (Array.copy x_249)) (Array.copy a_246)) in
   let t_251 = (Array.length a_246.(0)) in
   let t_252 = (Array.length a_246) in
   let t_253 = (ref 1) in
   let t_254 = (ref 1) in
   while (((! t_248) < t_251) && ((! t_247) < t_252)) do
    let t_255 = (! t_247) in
    let t_256 = (! t_248) in
    let t_257 = (ref (None)) in
    let t_263 =
     begin
      for j_260 = t_255 to (t_252 - 1) do
       let t_261 = (t_250.(j_260)).(t_256) in
       if (not (t_261 = 0)) then
        (match (! t_257) with
         | Some (i_262) ->
            if ((abs (snd i_262)) > (abs t_261)) then
             (t_257 := (Some (j_260, t_261)))
            else ()
         | None -> (t_257 := (Some (j_260, t_261))))
       else ()
      done;
      (match (! t_257) with
       | Some (i_258) ->
          if ((fst i_258) <> t_255) then begin
           let t_259 = t_250.(t_255) in
           t_250.(t_255) <- t_250.(fst i_258);
           t_250.(fst i_258) <- t_259;
           (t_254 := (~- (! t_254)))
          end else ();
          (Some (snd i_258))
       | None -> (None))
     end in
    (match t_263 with
     | Some (i_264) ->
        begin
         for j_265 = (t_255 + 1) to (t_252 - 1) do
          if (not ((t_250.(j_265)).(t_256) = 0)) then begin
           for j_266 = (t_256 + 1) to (t_251 - 1) do
            (t_250.(j_265)).(j_266) <-
             ((((t_250.(j_265)).(j_266) * (t_250.(t_255)).(t_256)) -
                ((t_250.(t_255)).(j_266) * (t_250.(j_265)).(t_255))) /
               (! t_253))
           done;
           (t_250.(j_265)).(t_256) <- 0
          end else ()
         done;
         (t_253 := i_264)
        end;
        (t_247 := ((! t_247) + 1))
     | None -> (t_254 := 0));
    (t_248 := ((! t_248) + 1))
   done;
   (t_250,
    if ((! t_254) = 0) then 0
    else if ((! t_254) = 1) then (! t_253)
    else (~- (! t_253)), (! t_247))>.
# val resIV1 :
  ('a,
   Funct4.GenIV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_267 ->
   let t_268 = (ref 0) in
   let t_269 = (ref 0) in
   let t_270 = {arr = (Array.copy a_267.arr)} (a_267) in
   let t_271 = a_267.m in
   let t_272 = a_267.n in
   let t_273 = (ref 1) in
   let t_274 = (ref 1) in
   while (((! t_269) < t_271) && ((! t_268) < t_272)) do
    let t_275 = (! t_268) in
    let t_276 = (! t_269) in
    let t_277 = (ref (None)) in
    let t_289 =
     begin
      for j_286 = t_275 to (t_272 - 1) do
       let t_287 = (t_270.arr).((j_286 * t_270.n) + t_276) in
       if (not (t_287 = 0)) then
        (match (! t_277) with
         | Some (i_288) ->
            if ((abs (snd i_288)) > (abs t_287)) then
             (t_277 := (Some (j_286, t_287)))
            else ()
         | None -> (t_277 := (Some (j_286, t_287))))
       else ()
      done;
      (match (! t_277) with
       | Some (i_278) ->
          if ((fst i_278) <> t_275) then begin
           let a_279 = t_270.arr
           and n_280 = t_270.n
           and m_281 = t_270.m in
           let i1_282 = (t_275 * n_280)
           and i2_283 = ((fst i_278) * n_280) in
           for i_284 = 0 to (m_281 - 1) do
            let t_285 = a_279.(i1_282 + i_284) in
            a_279.(i2_283 + i_284) <- a_279.(i1_282 + i_284);
            a_279.(i1_282 + i_284) <- t_285
           done;
           ()
          end else ();
          (Some (snd i_278))
       | None -> (None))
     end in
    (match t_289 with
     | Some (i_290) ->
        begin
         for j_291 = (t_275 + 1) to (t_272 - 1) do
          if (not ((t_270.arr).((j_291 * t_270.n) + t_276) = 0)) then begin
           for j_292 = (t_276 + 1) to (t_271 - 1) do
            (t_270.arr).((j_291 * t_270.n) + j_292) <-
             ((((t_270.arr).((j_291 * t_270.n) + j_292) *
                 (t_270.arr).((t_275 * t_270.n) + t_276)) -
                ((t_270.arr).((t_275 * t_270.n) + j_292) *
                  (t_270.arr).((j_291 * t_270.n) + t_275))) / (! t_273))
           done;
           (t_270.arr).((j_291 * t_270.n) + t_276) <- 0
          end else ()
         done;
         (t_273 := i_290)
        end;
        (t_268 := ((! t_268) + 1))
     | None -> (t_274 := 0));
    (t_269 := ((! t_269) + 1))
   done;
   t_270>.
# val resIV2 :
  ('a,
   Funct4.GenIV2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_293 ->
   let t_294 = (ref 0) in
   let t_295 = (ref 0) in
   let t_296 = {arr = (Array.copy a_293.arr)} (a_293) in
   let t_297 = a_293.m in
   let t_298 = a_293.n in
   let t_299 = (ref 1) in
   let t_300 = (ref 1) in
   while (((! t_295) < t_297) && ((! t_294) < t_298)) do
    let t_301 = (! t_294) in
    let t_302 = (! t_295) in
    let t_303 = (ref (None)) in
    let t_315 =
     begin
      for j_312 = t_301 to (t_298 - 1) do
       let t_313 = (t_296.arr).((j_312 * t_296.n) + t_302) in
       if (not (t_313 = 0)) then
        (match (! t_303) with
         | Some (i_314) ->
            if ((abs (snd i_314)) > (abs t_313)) then
             (t_303 := (Some (j_312, t_313)))
            else ()
         | None -> (t_303 := (Some (j_312, t_313))))
       else ()
      done;
      (match (! t_303) with
       | Some (i_304) ->
          if ((fst i_304) <> t_301) then begin
           let a_305 = t_296.arr
           and n_306 = t_296.n
           and m_307 = t_296.m in
           let i1_308 = (t_301 * n_306)
           and i2_309 = ((fst i_304) * n_306) in
           for i_310 = 0 to (m_307 - 1) do
            let t_311 = a_305.(i1_308 + i_310) in
            a_305.(i2_309 + i_310) <- a_305.(i1_308 + i_310);
            a_305.(i1_308 + i_310) <- t_311
           done;
           (t_300 := (~- (! t_300)))
          end else ();
          (Some (snd i_304))
       | None -> (None))
     end in
    (match t_315 with
     | Some (i_316) ->
        begin
         for j_317 = (t_301 + 1) to (t_298 - 1) do
          if (not ((t_296.arr).((j_317 * t_296.n) + t_302) = 0)) then begin
           for j_318 = (t_302 + 1) to (t_297 - 1) do
            (t_296.arr).((j_317 * t_296.n) + j_318) <-
             ((((t_296.arr).((j_317 * t_296.n) + j_318) *
                 (t_296.arr).((t_301 * t_296.n) + t_302)) -
                ((t_296.arr).((t_301 * t_296.n) + j_318) *
                  (t_296.arr).((j_317 * t_296.n) + t_301))) / (! t_299))
           done;
           (t_296.arr).((j_317 * t_296.n) + t_302) <- 0
          end else ()
         done;
         (t_299 := i_316)
        end;
        (t_294 := ((! t_294) + 1))
     | None -> (t_300 := 0));
    (t_295 := ((! t_295) + 1))
   done;
   (t_296,
    if ((! t_300) = 0) then 0
    else if ((! t_300) = 1) then (! t_299)
    else (~- (! t_299)))>.
# val resIV3 :
  ('a,
   Funct4.GenIV3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_319 ->
   let t_320 = (ref 0) in
   let t_321 = (ref 0) in
   let t_322 = {arr = (Array.copy a_319.arr)} (a_319) in
   let t_323 = a_319.m in
   let t_324 = a_319.n in
   let t_325 = (ref 1) in
   let t_326 = (ref 1) in
   while (((! t_321) < t_323) && ((! t_320) < t_324)) do
    let t_327 = (! t_320) in
    let t_328 = (! t_321) in
    let t_329 = (ref (None)) in
    let t_341 =
     begin
      for j_338 = t_327 to (t_324 - 1) do
       let t_339 = (t_322.arr).((j_338 * t_322.n) + t_328) in
       if (not (t_339 = 0)) then
        (match (! t_329) with
         | Some (i_340) ->
            if ((abs (snd i_340)) > (abs t_339)) then
             (t_329 := (Some (j_338, t_339)))
            else ()
         | None -> (t_329 := (Some (j_338, t_339))))
       else ()
      done;
      (match (! t_329) with
       | Some (i_330) ->
          if ((fst i_330) <> t_327) then begin
           let a_331 = t_322.arr
           and n_332 = t_322.n
           and m_333 = t_322.m in
           let i1_334 = (t_327 * n_332)
           and i2_335 = ((fst i_330) * n_332) in
           for i_336 = 0 to (m_333 - 1) do
            let t_337 = a_331.(i1_334 + i_336) in
            a_331.(i2_335 + i_336) <- a_331.(i1_334 + i_336);
            a_331.(i1_334 + i_336) <- t_337
           done;
           ()
          end else ();
          (Some (snd i_330))
       | None -> (None))
     end in
    (match t_341 with
     | Some (i_342) ->
        begin
         for j_343 = (t_327 + 1) to (t_324 - 1) do
          if (not ((t_322.arr).((j_343 * t_322.n) + t_328) = 0)) then begin
           for j_344 = (t_328 + 1) to (t_323 - 1) do
            (t_322.arr).((j_343 * t_322.n) + j_344) <-
             ((((t_322.arr).((j_343 * t_322.n) + j_344) *
                 (t_322.arr).((t_327 * t_322.n) + t_328)) -
                ((t_322.arr).((t_327 * t_322.n) + j_344) *
                  (t_322.arr).((j_343 * t_322.n) + t_327))) / (! t_325))
           done;
           (t_322.arr).((j_343 * t_322.n) + t_328) <- 0
          end else ()
         done;
         (t_325 := i_342)
        end;
        (t_320 := ((! t_320) + 1))
     | None -> (t_326 := 0));
    (t_321 := ((! t_321) + 1))
   done;
   (t_322, (! t_320))>.
# val resIV4 :
  ('a,
   Funct4.GenIV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_345 ->
   let t_346 = (ref 0) in
   let t_347 = (ref 0) in
   let t_348 = {arr = (Array.copy a_345.arr)} (a_345) in
   let t_349 = a_345.m in
   let t_350 = a_345.n in
   let t_351 = (ref 1) in
   let t_352 = (ref 1) in
   while (((! t_347) < t_349) && ((! t_346) < t_350)) do
    let t_353 = (! t_346) in
    let t_354 = (! t_347) in
    let t_355 = (ref (None)) in
    let t_367 =
     begin
      for j_364 = t_353 to (t_350 - 1) do
       let t_365 = (t_348.arr).((j_364 * t_348.n) + t_354) in
       if (not (t_365 = 0)) then
        (match (! t_355) with
         | Some (i_366) ->
            if ((abs (snd i_366)) > (abs t_365)) then
             (t_355 := (Some (j_364, t_365)))
            else ()
         | None -> (t_355 := (Some (j_364, t_365))))
       else ()
      done;
      (match (! t_355) with
       | Some (i_356) ->
          if ((fst i_356) <> t_353) then begin
           let a_357 = t_348.arr
           and n_358 = t_348.n
           and m_359 = t_348.m in
           let i1_360 = (t_353 * n_358)
           and i2_361 = ((fst i_356) * n_358) in
           for i_362 = 0 to (m_359 - 1) do
            let t_363 = a_357.(i1_360 + i_362) in
            a_357.(i2_361 + i_362) <- a_357.(i1_360 + i_362);
            a_357.(i1_360 + i_362) <- t_363
           done;
           (t_352 := (~- (! t_352)))
          end else ();
          (Some (snd i_356))
       | None -> (None))
     end in
    (match t_367 with
     | Some (i_368) ->
        begin
         for j_369 = (t_353 + 1) to (t_350 - 1) do
          if (not ((t_348.arr).((j_369 * t_348.n) + t_354) = 0)) then begin
           for j_370 = (t_354 + 1) to (t_349 - 1) do
            (t_348.arr).((j_369 * t_348.n) + j_370) <-
             ((((t_348.arr).((j_369 * t_348.n) + j_370) *
                 (t_348.arr).((t_353 * t_348.n) + t_354)) -
                ((t_348.arr).((t_353 * t_348.n) + j_370) *
                  (t_348.arr).((j_369 * t_348.n) + t_353))) / (! t_351))
           done;
           (t_348.arr).((j_369 * t_348.n) + t_354) <- 0
          end else ()
         done;
         (t_351 := i_368)
        end;
        (t_346 := ((! t_346) + 1))
     | None -> (t_352 := 0));
    (t_347 := ((! t_347) + 1))
   done;
   (t_348,
    if ((! t_352) = 0) then 0
    else if ((! t_352) = 1) then (! t_351)
    else (~- (! t_351)), (! t_346))>.
# val resFA11 :
  ('a,
   Funct4.GenFA11.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_371 ->
   let t_372 = (ref 0) in
   let t_373 = (ref 0) in
   let t_375 =
    (Array.map (fun x_374 -> (Array.copy x_374)) (Array.copy a_371)) in
   let t_376 = (Array.length a_371.(0)) in
   let t_377 = (Array.length a_371) in
   while (((! t_373) < t_376) && ((! t_372) < t_377)) do
    let t_378 = (! t_372) in
    let t_379 = (! t_373) in
    let t_380 = (ref (None)) in
    let t_389 =
     begin
      for j_385 = t_378 to (t_377 - 1) do
       for j_386 = t_379 to (t_376 - 1) do
        let t_387 = (t_375.(j_385)).(j_386) in
        if (not (t_387 = 0.)) then
         (match (! t_380) with
          | Some (i_388) ->
             if ((abs_float (snd i_388)) < (abs_float t_387)) then
              (t_380 := (Some ((j_385, j_386), t_387)))
             else ()
          | None -> (t_380 := (Some ((j_385, j_386), t_387))))
        else ()
       done
      done;
      (match (! t_380) with
       | Some (i_381) ->
          if ((snd (fst i_381)) <> t_379) then begin
           for r_383 = 0 to ((Array.length t_375) - 1) do
            let t_384 = (t_375.(r_383)).(t_379) in
            (t_375.(r_383)).(t_379) <- (t_375.(r_383)).(snd (fst i_381));
            (t_375.(r_383)).(snd (fst i_381)) <- t_384
           done;
           ()
          end else ();
          if ((fst (fst i_381)) <> t_378) then begin
           let t_382 = t_375.(t_378) in
           t_375.(t_378) <- t_375.(fst (fst i_381));
           t_375.(fst (fst i_381)) <- t_382;
           ()
          end else ();
          (Some (snd i_381))
       | None -> (None))
     end in
    (match t_389 with
     | Some (i_390) ->
        begin
         for j_391 = (t_378 + 1) to (t_377 - 1) do
          if (not ((t_375.(j_391)).(t_379) = 0.)) then begin
           for j_392 = (t_379 + 1) to (t_376 - 1) do
            (t_375.(j_391)).(j_392) <-
             ((t_375.(j_391)).(j_392) -.
               (((t_375.(j_391)).(t_379) /. (t_375.(t_378)).(t_379)) *.
                 (t_375.(t_378)).(j_392)))
           done;
           (t_375.(j_391)).(t_379) <- 0.
          end else ()
         done;
         ()
        end;
        (t_372 := ((! t_372) + 1))
     | None -> ());
    (t_373 := ((! t_373) + 1))
   done;
   t_375>.
# val resFA12 :
  ('a,
   Funct4.GenFA12.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_393 ->
   let t_394 = (ref 0) in
   let t_395 = (ref 0) in
   let t_397 =
    (Array.map (fun x_396 -> (Array.copy x_396)) (Array.copy a_393)) in
   let t_398 = (Array.length a_393.(0)) in
   let t_399 = (Array.length a_393) in
   let t_400 = (ref 1.) in
   let t_401 = (ref 1) in
   while (((! t_395) < t_398) && ((! t_394) < t_399)) do
    let t_402 = (! t_394) in
    let t_403 = (! t_395) in
    let t_404 = (ref (None)) in
    let t_413 =
     begin
      for j_409 = t_402 to (t_399 - 1) do
       for j_410 = t_403 to (t_398 - 1) do
        let t_411 = (t_397.(j_409)).(j_410) in
        if (not (t_411 = 0.)) then
         (match (! t_404) with
          | Some (i_412) ->
             if ((abs_float (snd i_412)) < (abs_float t_411)) then
              (t_404 := (Some ((j_409, j_410), t_411)))
             else ()
          | None -> (t_404 := (Some ((j_409, j_410), t_411))))
        else ()
       done
      done;
      (match (! t_404) with
       | Some (i_405) ->
          if ((snd (fst i_405)) <> t_403) then begin
           for r_407 = 0 to ((Array.length t_397) - 1) do
            let t_408 = (t_397.(r_407)).(t_403) in
            (t_397.(r_407)).(t_403) <- (t_397.(r_407)).(snd (fst i_405));
            (t_397.(r_407)).(snd (fst i_405)) <- t_408
           done;
           (t_401 := (~- (! t_401)))
          end else ();
          if ((fst (fst i_405)) <> t_402) then begin
           let t_406 = t_397.(t_402) in
           t_397.(t_402) <- t_397.(fst (fst i_405));
           t_397.(fst (fst i_405)) <- t_406;
           (t_401 := (~- (! t_401)))
          end else ();
          (Some (snd i_405))
       | None -> (None))
     end in
    (match t_413 with
     | Some (i_414) ->
        begin
         for j_415 = (t_402 + 1) to (t_399 - 1) do
          if (not ((t_397.(j_415)).(t_403) = 0.)) then begin
           for j_416 = (t_403 + 1) to (t_398 - 1) do
            (t_397.(j_415)).(j_416) <-
             ((t_397.(j_415)).(j_416) -.
               (((t_397.(j_415)).(t_403) /. (t_397.(t_402)).(t_403)) *.
                 (t_397.(t_402)).(j_416)))
           done;
           (t_397.(j_415)).(t_403) <- 0.
          end else ()
         done;
         (t_400 := ((! t_400) *. i_414))
        end;
        (t_394 := ((! t_394) + 1))
     | None -> (t_401 := 0));
    (t_395 := ((! t_395) + 1))
   done;
   (t_397,
    if ((! t_401) = 0) then 0.
    else if ((! t_401) = 1) then (! t_400)
    else (~-. (! t_400)))>.
# val resFA13 :
  ('a,
   Funct4.GenFA13.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_417 ->
   let t_418 = (ref 0) in
   let t_419 = (ref 0) in
   let t_421 =
    (Array.map (fun x_420 -> (Array.copy x_420)) (Array.copy a_417)) in
   let t_422 = (Array.length a_417.(0)) in
   let t_423 = (Array.length a_417) in
   while (((! t_419) < t_422) && ((! t_418) < t_423)) do
    let t_424 = (! t_418) in
    let t_425 = (! t_419) in
    let t_426 = (ref (None)) in
    let t_435 =
     begin
      for j_431 = t_424 to (t_423 - 1) do
       for j_432 = t_425 to (t_422 - 1) do
        let t_433 = (t_421.(j_431)).(j_432) in
        if (not (t_433 = 0.)) then
         (match (! t_426) with
          | Some (i_434) ->
             if ((abs_float (snd i_434)) < (abs_float t_433)) then
              (t_426 := (Some ((j_431, j_432), t_433)))
             else ()
          | None -> (t_426 := (Some ((j_431, j_432), t_433))))
        else ()
       done
      done;
      (match (! t_426) with
       | Some (i_427) ->
          if ((snd (fst i_427)) <> t_425) then begin
           for r_429 = 0 to ((Array.length t_421) - 1) do
            let t_430 = (t_421.(r_429)).(t_425) in
            (t_421.(r_429)).(t_425) <- (t_421.(r_429)).(snd (fst i_427));
            (t_421.(r_429)).(snd (fst i_427)) <- t_430
           done;
           ()
          end else ();
          if ((fst (fst i_427)) <> t_424) then begin
           let t_428 = t_421.(t_424) in
           t_421.(t_424) <- t_421.(fst (fst i_427));
           t_421.(fst (fst i_427)) <- t_428;
           ()
          end else ();
          (Some (snd i_427))
       | None -> (None))
     end in
    (match t_435 with
     | Some (i_436) ->
        begin
         for j_437 = (t_424 + 1) to (t_423 - 1) do
          if (not ((t_421.(j_437)).(t_425) = 0.)) then begin
           for j_438 = (t_425 + 1) to (t_422 - 1) do
            (t_421.(j_437)).(j_438) <-
             ((t_421.(j_437)).(j_438) -.
               (((t_421.(j_437)).(t_425) /. (t_421.(t_424)).(t_425)) *.
                 (t_421.(t_424)).(j_438)))
           done;
           (t_421.(j_437)).(t_425) <- 0.
          end else ()
         done;
         ()
        end;
        (t_418 := ((! t_418) + 1))
     | None -> ());
    (t_419 := ((! t_419) + 1))
   done;
   (t_421, (! t_418))>.
# val resFA14 :
  ('a,
   Funct4.GenFA14.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_439 ->
   let t_440 = (ref 0) in
   let t_441 = (ref 0) in
   let t_443 =
    (Array.map (fun x_442 -> (Array.copy x_442)) (Array.copy a_439)) in
   let t_444 = (Array.length a_439.(0)) in
   let t_445 = (Array.length a_439) in
   let t_446 = (ref 1.) in
   let t_447 = (ref 1) in
   while (((! t_441) < t_444) && ((! t_440) < t_445)) do
    let t_448 = (! t_440) in
    let t_449 = (! t_441) in
    let t_450 = (ref (None)) in
    let t_459 =
     begin
      for j_455 = t_448 to (t_445 - 1) do
       for j_456 = t_449 to (t_444 - 1) do
        let t_457 = (t_443.(j_455)).(j_456) in
        if (not (t_457 = 0.)) then
         (match (! t_450) with
          | Some (i_458) ->
             if ((abs_float (snd i_458)) < (abs_float t_457)) then
              (t_450 := (Some ((j_455, j_456), t_457)))
             else ()
          | None -> (t_450 := (Some ((j_455, j_456), t_457))))
        else ()
       done
      done;
      (match (! t_450) with
       | Some (i_451) ->
          if ((snd (fst i_451)) <> t_449) then begin
           for r_453 = 0 to ((Array.length t_443) - 1) do
            let t_454 = (t_443.(r_453)).(t_449) in
            (t_443.(r_453)).(t_449) <- (t_443.(r_453)).(snd (fst i_451));
            (t_443.(r_453)).(snd (fst i_451)) <- t_454
           done;
           (t_447 := (~- (! t_447)))
          end else ();
          if ((fst (fst i_451)) <> t_448) then begin
           let t_452 = t_443.(t_448) in
           t_443.(t_448) <- t_443.(fst (fst i_451));
           t_443.(fst (fst i_451)) <- t_452;
           (t_447 := (~- (! t_447)))
          end else ();
          (Some (snd i_451))
       | None -> (None))
     end in
    (match t_459 with
     | Some (i_460) ->
        begin
         for j_461 = (t_448 + 1) to (t_445 - 1) do
          if (not ((t_443.(j_461)).(t_449) = 0.)) then begin
           for j_462 = (t_449 + 1) to (t_444 - 1) do
            (t_443.(j_461)).(j_462) <-
             ((t_443.(j_461)).(j_462) -.
               (((t_443.(j_461)).(t_449) /. (t_443.(t_448)).(t_449)) *.
                 (t_443.(t_448)).(j_462)))
           done;
           (t_443.(j_461)).(t_449) <- 0.
          end else ()
         done;
         (t_446 := ((! t_446) *. i_460))
        end;
        (t_440 := ((! t_440) + 1))
     | None -> (t_447 := 0));
    (t_441 := ((! t_441) + 1))
   done;
   (t_443,
    if ((! t_447) = 0) then 0.
    else if ((! t_447) = 1) then (! t_446)
    else (~-. (! t_446)), (! t_440))>.
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
# val rFA11 :
  Funct4.GenFA11.Ctr.contr ->
  Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res =
  <fun>
# val rFA12 :
  Funct4.GenFA12.Ctr.contr ->
  Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res =
  <fun>
# val rFA13 :
  Funct4.GenFA13.Ctr.contr ->
  Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res =
  <fun>
# val rFA14 :
  Funct4.GenFA14.Ctr.contr ->
  Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res =
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
#   val resF11 :
  Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res
  list =
  [[|[|1.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]|];
   [|[|13.; 5.; 4.; 0.|];
     [|0.; 2.23076923076923084; 0.384615384615384581; 0.|];
     [|0.; 0.; -1.72413793103448287; 0.|]|];
   [|[|13.; 5.; 4.|]; [|0.; 2.23076923076923084; 0.384615384615384581|];
     [|0.; 0.; -1.72413793103448287|]; [|0.; 0.; 0.|]|];
   [|[|13.; 5.; 0.|]; [|0.; 2.23076923076923084; 0.|]; [|0.; 0.; 0.|]|]]
# val resF12 :
  Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res
  list =
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
   ([|[|13.; 5.; 0.|]; [|0.; 2.23076923076923084; 0.|]; [|0.; 0.; 0.|]|], 0.)]
# val resF13 :
  Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res
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
   ([|[|13.; 5.; 0.|]; [|0.; 2.23076923076923084; 0.|]; [|0.; 0.; 0.|]|], 2)]
# val resF14 :
  Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res
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
   ([|[|13.; 5.; 0.|]; [|0.; 2.23076923076923084; 0.|]; [|0.; 0.; 0.|]|], 0.,
    2)]
# 
