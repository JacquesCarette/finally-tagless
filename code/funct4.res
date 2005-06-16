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
       let t_101 = (t_84.arr).((j_100 * t_84.m) + t_90) in
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
           let i1_96 = (t_89 * m_95)
           and i2_97 = ((fst i_92) * m_95) in
           for i_98 = 0 to (m_95 - 1) do
            let t_99 = a_93.(i1_96 + i_98) in
            a_93.(i1_96 + i_98) <- a_93.(i2_97 + i_98);
            a_93.(i2_97 + i_98) <- t_99
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
          if (not ((t_84.arr).((j_105 * t_84.m) + t_90) = 0.)) then begin
           for j_106 = (t_90 + 1) to (t_85 - 1) do
            (t_84.arr).((j_105 * t_84.m) + j_106) <-
             ((t_84.arr).((j_105 * t_84.m) + j_106) -.
               (((t_84.arr).((j_105 * t_84.m) + t_90) /.
                  (t_84.arr).((t_89 * t_84.m) + t_90)) *.
                 (t_84.arr).((t_89 * t_84.m) + j_106)))
           done;
           (t_84.arr).((j_105 * t_84.m) + t_90) <- 0.
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
       let t_127 = (t_110.arr).((j_126 * t_110.m) + t_116) in
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
           let i1_122 = (t_115 * m_121)
           and i2_123 = ((fst i_118) * m_121) in
           for i_124 = 0 to (m_121 - 1) do
            let t_125 = a_119.(i1_122 + i_124) in
            a_119.(i1_122 + i_124) <- a_119.(i2_123 + i_124);
            a_119.(i2_123 + i_124) <- t_125
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
          if (not ((t_110.arr).((j_131 * t_110.m) + t_116) = 0.)) then begin
           for j_132 = (t_116 + 1) to (t_111 - 1) do
            (t_110.arr).((j_131 * t_110.m) + j_132) <-
             ((t_110.arr).((j_131 * t_110.m) + j_132) -.
               (((t_110.arr).((j_131 * t_110.m) + t_116) /.
                  (t_110.arr).((t_115 * t_110.m) + t_116)) *.
                 (t_110.arr).((t_115 * t_110.m) + j_132)))
           done;
           (t_110.arr).((j_131 * t_110.m) + t_116) <- 0.
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
       let t_151 = (t_136.arr).((j_150 * t_136.m) + t_140) in
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
           let i1_146 = (t_139 * m_145)
           and i2_147 = ((fst i_142) * m_145) in
           for i_148 = 0 to (m_145 - 1) do
            let t_149 = a_143.(i1_146 + i_148) in
            a_143.(i1_146 + i_148) <- a_143.(i2_147 + i_148);
            a_143.(i2_147 + i_148) <- t_149
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
          if (not ((t_136.arr).((j_155 * t_136.m) + t_140) = 0.)) then begin
           for j_156 = (t_140 + 1) to (t_137 - 1) do
            (t_136.arr).((j_155 * t_136.m) + j_156) <-
             ((t_136.arr).((j_155 * t_136.m) + j_156) -.
               (((t_136.arr).((j_155 * t_136.m) + t_140) /.
                  (t_136.arr).((t_139 * t_136.m) + t_140)) *.
                 (t_136.arr).((t_139 * t_136.m) + j_156)))
           done;
           (t_136.arr).((j_155 * t_136.m) + t_140) <- 0.
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
       let t_177 = (t_160.arr).((j_176 * t_160.m) + t_166) in
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
           let i1_172 = (t_165 * m_171)
           and i2_173 = ((fst i_168) * m_171) in
           for i_174 = 0 to (m_171 - 1) do
            let t_175 = a_169.(i1_172 + i_174) in
            a_169.(i1_172 + i_174) <- a_169.(i2_173 + i_174);
            a_169.(i2_173 + i_174) <- t_175
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
          if (not ((t_160.arr).((j_181 * t_160.m) + t_166) = 0.)) then begin
           for j_182 = (t_166 + 1) to (t_161 - 1) do
            (t_160.arr).((j_181 * t_160.m) + j_182) <-
             ((t_160.arr).((j_181 * t_160.m) + j_182) -.
               (((t_160.arr).((j_181 * t_160.m) + t_166) /.
                  (t_160.arr).((t_165 * t_160.m) + t_166)) *.
                 (t_160.arr).((t_165 * t_160.m) + j_182)))
           done;
           (t_160.arr).((j_181 * t_160.m) + t_166) <- 0.
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
# val resFV5 :
  ('a,
   Funct4.GenFV5.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_183 ->
   let t_184 = (ref 0) in
   let t_185 = (ref 0) in
   let t_186 = {arr = (Array.copy a_183.arr)} (a_183) in
   let t_187 = a_183.m in
   let t_188 = a_183.n in
   let t_189 = (ref 1.) in
   let t_190 = (ref 1) in
   while (((! t_185) < t_187) && ((! t_184) < t_188)) do
    let t_191 = (! t_184) in
    let t_192 = (! t_185) in
    let t_193 = (ref (None)) in
    let t_213 =
     begin
      for j_209 = t_191 to (t_188 - 1) do
       for j_210 = t_192 to (t_187 - 1) do
        let t_211 = (t_186.arr).((j_209 * t_186.m) + j_210) in
        if (not (t_211 = 0.)) then
         (match (! t_193) with
          | Some (i_212) ->
             if ((abs_float (snd i_212)) < (abs_float t_211)) then
              (t_193 := (Some ((j_209, j_210), t_211)))
             else ()
          | None -> (t_193 := (Some ((j_209, j_210), t_211))))
        else ()
       done
      done;
      (match (! t_193) with
       | Some (i_194) ->
          if ((snd (fst i_194)) <> t_192) then begin
           let a_202 = t_186.arr
           and nm_203 = (t_186.n * t_186.m)
           and m_204 = t_186.m in
           let rec loop_205 =
            fun i1_206 ->
             fun i2_207 ->
              if (i2_207 < nm_203) then
               let t_208 = a_202.(i1_206) in
               a_202.(i1_206) <- a_202.(i2_207);
               a_202.(i2_207) <- t_208;
               (loop_205 (i1_206 + m_204) (i2_207 + m_204))
              else () in
           (loop_205 t_192 (snd (fst i_194)));
           (t_190 := (~- (! t_190)))
          end else ();
          if ((fst (fst i_194)) <> t_191) then begin
           let a_195 = t_186.arr
           and n_196 = t_186.n
           and m_197 = t_186.m in
           let i1_198 = (t_191 * m_197)
           and i2_199 = ((fst (fst i_194)) * m_197) in
           for i_200 = 0 to (m_197 - 1) do
            let t_201 = a_195.(i1_198 + i_200) in
            a_195.(i1_198 + i_200) <- a_195.(i2_199 + i_200);
            a_195.(i2_199 + i_200) <- t_201
           done;
           (t_190 := (~- (! t_190)))
          end else ();
          (Some (snd i_194))
       | None -> (None))
     end in
    (match t_213 with
     | Some (i_214) ->
        begin
         for j_215 = (t_191 + 1) to (t_188 - 1) do
          if (not ((t_186.arr).((j_215 * t_186.m) + t_192) = 0.)) then begin
           for j_216 = (t_192 + 1) to (t_187 - 1) do
            (t_186.arr).((j_215 * t_186.m) + j_216) <-
             ((t_186.arr).((j_215 * t_186.m) + j_216) -.
               (((t_186.arr).((j_215 * t_186.m) + t_192) /.
                  (t_186.arr).((t_191 * t_186.m) + t_192)) *.
                 (t_186.arr).((t_191 * t_186.m) + j_216)))
           done;
           (t_186.arr).((j_215 * t_186.m) + t_192) <- 0.
          end else ()
         done;
         (t_189 := ((! t_189) *. i_214))
        end;
        (t_184 := ((! t_184) + 1))
     | None -> (t_190 := 0));
    (t_185 := ((! t_185) + 1))
   done;
   (t_186,
    if ((! t_190) = 0) then 0.
    else if ((! t_190) = 1) then (! t_189)
    else (~-. (! t_189)), (! t_184))>.
# val resIA1 :
  ('a,
   Funct4.GenIA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_217 ->
   let t_218 = (ref 0) in
   let t_219 = (ref 0) in
   let t_221 =
    (Array.map (fun x_220 -> (Array.copy x_220)) (Array.copy a_217)) in
   let t_222 = (Array.length a_217.(0)) in
   let t_223 = (Array.length a_217) in
   let t_224 = (ref 1) in
   let t_225 = (ref 1) in
   while (((! t_219) < t_222) && ((! t_218) < t_223)) do
    let t_226 = (! t_218) in
    let t_227 = (! t_219) in
    let t_228 = (ref (None)) in
    let t_234 =
     begin
      for j_231 = t_226 to (t_223 - 1) do
       let t_232 = (t_221.(j_231)).(t_227) in
       if (not (t_232 = 0)) then
        (match (! t_228) with
         | Some (i_233) ->
            if ((abs (snd i_233)) > (abs t_232)) then
             (t_228 := (Some (j_231, t_232)))
            else ()
         | None -> (t_228 := (Some (j_231, t_232))))
       else ()
      done;
      (match (! t_228) with
       | Some (i_229) ->
          if ((fst i_229) <> t_226) then begin
           let t_230 = t_221.(t_226) in
           t_221.(t_226) <- t_221.(fst i_229);
           t_221.(fst i_229) <- t_230;
           (t_225 := (~- (! t_225)))
          end else ();
          (Some (snd i_229))
       | None -> (None))
     end in
    (match t_234 with
     | Some (i_235) ->
        begin
         for j_236 = (t_226 + 1) to (t_223 - 1) do
          if (not ((t_221.(j_236)).(t_227) = 0)) then begin
           for j_237 = (t_227 + 1) to (t_222 - 1) do
            (t_221.(j_236)).(j_237) <-
             ((((t_221.(j_236)).(j_237) * (t_221.(t_226)).(t_227)) -
                ((t_221.(t_226)).(j_237) * (t_221.(j_236)).(t_226))) /
               (! t_224))
           done;
           (t_221.(j_236)).(t_227) <- 0
          end else ()
         done;
         (t_224 := i_235)
        end;
        (t_218 := ((! t_218) + 1))
     | None -> (t_225 := 0));
    (t_219 := ((! t_219) + 1))
   done;
   t_221>.
# val resIA2 :
  ('a,
   Funct4.GenIA2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_238 ->
   let t_239 = (ref 0) in
   let t_240 = (ref 0) in
   let t_242 =
    (Array.map (fun x_241 -> (Array.copy x_241)) (Array.copy a_238)) in
   let t_243 = (Array.length a_238.(0)) in
   let t_244 = (Array.length a_238) in
   let t_245 = (ref 1) in
   let t_246 = (ref 1) in
   while (((! t_240) < t_243) && ((! t_239) < t_244)) do
    let t_247 = (! t_239) in
    let t_248 = (! t_240) in
    let t_249 = (ref (None)) in
    let t_255 =
     begin
      for j_252 = t_247 to (t_244 - 1) do
       let t_253 = (t_242.(j_252)).(t_248) in
       if (not (t_253 = 0)) then
        (match (! t_249) with
         | Some (i_254) ->
            if ((abs (snd i_254)) > (abs t_253)) then
             (t_249 := (Some (j_252, t_253)))
            else ()
         | None -> (t_249 := (Some (j_252, t_253))))
       else ()
      done;
      (match (! t_249) with
       | Some (i_250) ->
          if ((fst i_250) <> t_247) then begin
           let t_251 = t_242.(t_247) in
           t_242.(t_247) <- t_242.(fst i_250);
           t_242.(fst i_250) <- t_251;
           (t_246 := (~- (! t_246)))
          end else ();
          (Some (snd i_250))
       | None -> (None))
     end in
    (match t_255 with
     | Some (i_256) ->
        begin
         for j_257 = (t_247 + 1) to (t_244 - 1) do
          if (not ((t_242.(j_257)).(t_248) = 0)) then begin
           for j_258 = (t_248 + 1) to (t_243 - 1) do
            (t_242.(j_257)).(j_258) <-
             ((((t_242.(j_257)).(j_258) * (t_242.(t_247)).(t_248)) -
                ((t_242.(t_247)).(j_258) * (t_242.(j_257)).(t_247))) /
               (! t_245))
           done;
           (t_242.(j_257)).(t_248) <- 0
          end else ()
         done;
         (t_245 := i_256)
        end;
        (t_239 := ((! t_239) + 1))
     | None -> (t_246 := 0));
    (t_240 := ((! t_240) + 1))
   done;
   (t_242,
    if ((! t_246) = 0) then 0
    else if ((! t_246) = 1) then (! t_245)
    else (~- (! t_245)))>.
# val resIA3 :
  ('a,
   Funct4.GenIA3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_259 ->
   let t_260 = (ref 0) in
   let t_261 = (ref 0) in
   let t_263 =
    (Array.map (fun x_262 -> (Array.copy x_262)) (Array.copy a_259)) in
   let t_264 = (Array.length a_259.(0)) in
   let t_265 = (Array.length a_259) in
   let t_266 = (ref 1) in
   let t_267 = (ref 1) in
   while (((! t_261) < t_264) && ((! t_260) < t_265)) do
    let t_268 = (! t_260) in
    let t_269 = (! t_261) in
    let t_270 = (ref (None)) in
    let t_276 =
     begin
      for j_273 = t_268 to (t_265 - 1) do
       let t_274 = (t_263.(j_273)).(t_269) in
       if (not (t_274 = 0)) then
        (match (! t_270) with
         | Some (i_275) ->
            if ((abs (snd i_275)) > (abs t_274)) then
             (t_270 := (Some (j_273, t_274)))
            else ()
         | None -> (t_270 := (Some (j_273, t_274))))
       else ()
      done;
      (match (! t_270) with
       | Some (i_271) ->
          if ((fst i_271) <> t_268) then begin
           let t_272 = t_263.(t_268) in
           t_263.(t_268) <- t_263.(fst i_271);
           t_263.(fst i_271) <- t_272;
           ()
          end else ();
          (Some (snd i_271))
       | None -> (None))
     end in
    (match t_276 with
     | Some (i_277) ->
        begin
         for j_278 = (t_268 + 1) to (t_265 - 1) do
          if (not ((t_263.(j_278)).(t_269) = 0)) then begin
           for j_279 = (t_269 + 1) to (t_264 - 1) do
            (t_263.(j_278)).(j_279) <-
             ((((t_263.(j_278)).(j_279) * (t_263.(t_268)).(t_269)) -
                ((t_263.(t_268)).(j_279) * (t_263.(j_278)).(t_268))) /
               (! t_266))
           done;
           (t_263.(j_278)).(t_269) <- 0
          end else ()
         done;
         (t_266 := i_277)
        end;
        (t_260 := ((! t_260) + 1))
     | None -> (t_267 := 0));
    (t_261 := ((! t_261) + 1))
   done;
   (t_263, (! t_260))>.
# val resIA4 :
  ('a,
   Funct4.GenIA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_280 ->
   let t_281 = (ref 0) in
   let t_282 = (ref 0) in
   let t_284 =
    (Array.map (fun x_283 -> (Array.copy x_283)) (Array.copy a_280)) in
   let t_285 = (Array.length a_280.(0)) in
   let t_286 = (Array.length a_280) in
   let t_287 = (ref 1) in
   let t_288 = (ref 1) in
   while (((! t_282) < t_285) && ((! t_281) < t_286)) do
    let t_289 = (! t_281) in
    let t_290 = (! t_282) in
    let t_291 = (ref (None)) in
    let t_297 =
     begin
      for j_294 = t_289 to (t_286 - 1) do
       let t_295 = (t_284.(j_294)).(t_290) in
       if (not (t_295 = 0)) then
        (match (! t_291) with
         | Some (i_296) ->
            if ((abs (snd i_296)) > (abs t_295)) then
             (t_291 := (Some (j_294, t_295)))
            else ()
         | None -> (t_291 := (Some (j_294, t_295))))
       else ()
      done;
      (match (! t_291) with
       | Some (i_292) ->
          if ((fst i_292) <> t_289) then begin
           let t_293 = t_284.(t_289) in
           t_284.(t_289) <- t_284.(fst i_292);
           t_284.(fst i_292) <- t_293;
           (t_288 := (~- (! t_288)))
          end else ();
          (Some (snd i_292))
       | None -> (None))
     end in
    (match t_297 with
     | Some (i_298) ->
        begin
         for j_299 = (t_289 + 1) to (t_286 - 1) do
          if (not ((t_284.(j_299)).(t_290) = 0)) then begin
           for j_300 = (t_290 + 1) to (t_285 - 1) do
            (t_284.(j_299)).(j_300) <-
             ((((t_284.(j_299)).(j_300) * (t_284.(t_289)).(t_290)) -
                ((t_284.(t_289)).(j_300) * (t_284.(j_299)).(t_289))) /
               (! t_287))
           done;
           (t_284.(j_299)).(t_290) <- 0
          end else ()
         done;
         (t_287 := i_298)
        end;
        (t_281 := ((! t_281) + 1))
     | None -> (t_288 := 0));
    (t_282 := ((! t_282) + 1))
   done;
   (t_284,
    if ((! t_288) = 0) then 0
    else if ((! t_288) = 1) then (! t_287)
    else (~- (! t_287)), (! t_281))>.
# val resIV1 :
  ('a,
   Funct4.GenIV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_301 ->
   let t_302 = (ref 0) in
   let t_303 = (ref 0) in
   let t_304 = {arr = (Array.copy a_301.arr)} (a_301) in
   let t_305 = a_301.m in
   let t_306 = a_301.n in
   let t_307 = (ref 1) in
   let t_308 = (ref 1) in
   while (((! t_303) < t_305) && ((! t_302) < t_306)) do
    let t_309 = (! t_302) in
    let t_310 = (! t_303) in
    let t_311 = (ref (None)) in
    let t_323 =
     begin
      for j_320 = t_309 to (t_306 - 1) do
       let t_321 = (t_304.arr).((j_320 * t_304.m) + t_310) in
       if (not (t_321 = 0)) then
        (match (! t_311) with
         | Some (i_322) ->
            if ((abs (snd i_322)) > (abs t_321)) then
             (t_311 := (Some (j_320, t_321)))
            else ()
         | None -> (t_311 := (Some (j_320, t_321))))
       else ()
      done;
      (match (! t_311) with
       | Some (i_312) ->
          if ((fst i_312) <> t_309) then begin
           let a_313 = t_304.arr
           and n_314 = t_304.n
           and m_315 = t_304.m in
           let i1_316 = (t_309 * m_315)
           and i2_317 = ((fst i_312) * m_315) in
           for i_318 = 0 to (m_315 - 1) do
            let t_319 = a_313.(i1_316 + i_318) in
            a_313.(i1_316 + i_318) <- a_313.(i2_317 + i_318);
            a_313.(i2_317 + i_318) <- t_319
           done;
           ()
          end else ();
          (Some (snd i_312))
       | None -> (None))
     end in
    (match t_323 with
     | Some (i_324) ->
        begin
         for j_325 = (t_309 + 1) to (t_306 - 1) do
          if (not ((t_304.arr).((j_325 * t_304.m) + t_310) = 0)) then begin
           for j_326 = (t_310 + 1) to (t_305 - 1) do
            (t_304.arr).((j_325 * t_304.m) + j_326) <-
             ((((t_304.arr).((j_325 * t_304.m) + j_326) *
                 (t_304.arr).((t_309 * t_304.m) + t_310)) -
                ((t_304.arr).((t_309 * t_304.m) + j_326) *
                  (t_304.arr).((j_325 * t_304.m) + t_309))) / (! t_307))
           done;
           (t_304.arr).((j_325 * t_304.m) + t_310) <- 0
          end else ()
         done;
         (t_307 := i_324)
        end;
        (t_302 := ((! t_302) + 1))
     | None -> (t_308 := 0));
    (t_303 := ((! t_303) + 1))
   done;
   t_304>.
# val resIV2 :
  ('a,
   Funct4.GenIV2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_327 ->
   let t_328 = (ref 0) in
   let t_329 = (ref 0) in
   let t_330 = {arr = (Array.copy a_327.arr)} (a_327) in
   let t_331 = a_327.m in
   let t_332 = a_327.n in
   let t_333 = (ref 1) in
   let t_334 = (ref 1) in
   while (((! t_329) < t_331) && ((! t_328) < t_332)) do
    let t_335 = (! t_328) in
    let t_336 = (! t_329) in
    let t_337 = (ref (None)) in
    let t_349 =
     begin
      for j_346 = t_335 to (t_332 - 1) do
       let t_347 = (t_330.arr).((j_346 * t_330.m) + t_336) in
       if (not (t_347 = 0)) then
        (match (! t_337) with
         | Some (i_348) ->
            if ((abs (snd i_348)) > (abs t_347)) then
             (t_337 := (Some (j_346, t_347)))
            else ()
         | None -> (t_337 := (Some (j_346, t_347))))
       else ()
      done;
      (match (! t_337) with
       | Some (i_338) ->
          if ((fst i_338) <> t_335) then begin
           let a_339 = t_330.arr
           and n_340 = t_330.n
           and m_341 = t_330.m in
           let i1_342 = (t_335 * m_341)
           and i2_343 = ((fst i_338) * m_341) in
           for i_344 = 0 to (m_341 - 1) do
            let t_345 = a_339.(i1_342 + i_344) in
            a_339.(i1_342 + i_344) <- a_339.(i2_343 + i_344);
            a_339.(i2_343 + i_344) <- t_345
           done;
           (t_334 := (~- (! t_334)))
          end else ();
          (Some (snd i_338))
       | None -> (None))
     end in
    (match t_349 with
     | Some (i_350) ->
        begin
         for j_351 = (t_335 + 1) to (t_332 - 1) do
          if (not ((t_330.arr).((j_351 * t_330.m) + t_336) = 0)) then begin
           for j_352 = (t_336 + 1) to (t_331 - 1) do
            (t_330.arr).((j_351 * t_330.m) + j_352) <-
             ((((t_330.arr).((j_351 * t_330.m) + j_352) *
                 (t_330.arr).((t_335 * t_330.m) + t_336)) -
                ((t_330.arr).((t_335 * t_330.m) + j_352) *
                  (t_330.arr).((j_351 * t_330.m) + t_335))) / (! t_333))
           done;
           (t_330.arr).((j_351 * t_330.m) + t_336) <- 0
          end else ()
         done;
         (t_333 := i_350)
        end;
        (t_328 := ((! t_328) + 1))
     | None -> (t_334 := 0));
    (t_329 := ((! t_329) + 1))
   done;
   (t_330,
    if ((! t_334) = 0) then 0
    else if ((! t_334) = 1) then (! t_333)
    else (~- (! t_333)))>.
# val resIV3 :
  ('a,
   Funct4.GenIV3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_353 ->
   let t_354 = (ref 0) in
   let t_355 = (ref 0) in
   let t_356 = {arr = (Array.copy a_353.arr)} (a_353) in
   let t_357 = a_353.m in
   let t_358 = a_353.n in
   let t_359 = (ref 1) in
   let t_360 = (ref 1) in
   while (((! t_355) < t_357) && ((! t_354) < t_358)) do
    let t_361 = (! t_354) in
    let t_362 = (! t_355) in
    let t_363 = (ref (None)) in
    let t_375 =
     begin
      for j_372 = t_361 to (t_358 - 1) do
       let t_373 = (t_356.arr).((j_372 * t_356.m) + t_362) in
       if (not (t_373 = 0)) then
        (match (! t_363) with
         | Some (i_374) ->
            if ((abs (snd i_374)) > (abs t_373)) then
             (t_363 := (Some (j_372, t_373)))
            else ()
         | None -> (t_363 := (Some (j_372, t_373))))
       else ()
      done;
      (match (! t_363) with
       | Some (i_364) ->
          if ((fst i_364) <> t_361) then begin
           let a_365 = t_356.arr
           and n_366 = t_356.n
           and m_367 = t_356.m in
           let i1_368 = (t_361 * m_367)
           and i2_369 = ((fst i_364) * m_367) in
           for i_370 = 0 to (m_367 - 1) do
            let t_371 = a_365.(i1_368 + i_370) in
            a_365.(i1_368 + i_370) <- a_365.(i2_369 + i_370);
            a_365.(i2_369 + i_370) <- t_371
           done;
           ()
          end else ();
          (Some (snd i_364))
       | None -> (None))
     end in
    (match t_375 with
     | Some (i_376) ->
        begin
         for j_377 = (t_361 + 1) to (t_358 - 1) do
          if (not ((t_356.arr).((j_377 * t_356.m) + t_362) = 0)) then begin
           for j_378 = (t_362 + 1) to (t_357 - 1) do
            (t_356.arr).((j_377 * t_356.m) + j_378) <-
             ((((t_356.arr).((j_377 * t_356.m) + j_378) *
                 (t_356.arr).((t_361 * t_356.m) + t_362)) -
                ((t_356.arr).((t_361 * t_356.m) + j_378) *
                  (t_356.arr).((j_377 * t_356.m) + t_361))) / (! t_359))
           done;
           (t_356.arr).((j_377 * t_356.m) + t_362) <- 0
          end else ()
         done;
         (t_359 := i_376)
        end;
        (t_354 := ((! t_354) + 1))
     | None -> (t_360 := 0));
    (t_355 := ((! t_355) + 1))
   done;
   (t_356, (! t_354))>.
# val resIV4 :
  ('a,
   Funct4.GenIV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_379 ->
   let t_380 = (ref 0) in
   let t_381 = (ref 0) in
   let t_382 = {arr = (Array.copy a_379.arr)} (a_379) in
   let t_383 = a_379.m in
   let t_384 = a_379.n in
   let t_385 = (ref 1) in
   let t_386 = (ref 1) in
   while (((! t_381) < t_383) && ((! t_380) < t_384)) do
    let t_387 = (! t_380) in
    let t_388 = (! t_381) in
    let t_389 = (ref (None)) in
    let t_401 =
     begin
      for j_398 = t_387 to (t_384 - 1) do
       let t_399 = (t_382.arr).((j_398 * t_382.m) + t_388) in
       if (not (t_399 = 0)) then
        (match (! t_389) with
         | Some (i_400) ->
            if ((abs (snd i_400)) > (abs t_399)) then
             (t_389 := (Some (j_398, t_399)))
            else ()
         | None -> (t_389 := (Some (j_398, t_399))))
       else ()
      done;
      (match (! t_389) with
       | Some (i_390) ->
          if ((fst i_390) <> t_387) then begin
           let a_391 = t_382.arr
           and n_392 = t_382.n
           and m_393 = t_382.m in
           let i1_394 = (t_387 * m_393)
           and i2_395 = ((fst i_390) * m_393) in
           for i_396 = 0 to (m_393 - 1) do
            let t_397 = a_391.(i1_394 + i_396) in
            a_391.(i1_394 + i_396) <- a_391.(i2_395 + i_396);
            a_391.(i2_395 + i_396) <- t_397
           done;
           (t_386 := (~- (! t_386)))
          end else ();
          (Some (snd i_390))
       | None -> (None))
     end in
    (match t_401 with
     | Some (i_402) ->
        begin
         for j_403 = (t_387 + 1) to (t_384 - 1) do
          if (not ((t_382.arr).((j_403 * t_382.m) + t_388) = 0)) then begin
           for j_404 = (t_388 + 1) to (t_383 - 1) do
            (t_382.arr).((j_403 * t_382.m) + j_404) <-
             ((((t_382.arr).((j_403 * t_382.m) + j_404) *
                 (t_382.arr).((t_387 * t_382.m) + t_388)) -
                ((t_382.arr).((t_387 * t_382.m) + j_404) *
                  (t_382.arr).((j_403 * t_382.m) + t_387))) / (! t_385))
           done;
           (t_382.arr).((j_403 * t_382.m) + t_388) <- 0
          end else ()
         done;
         (t_385 := i_402)
        end;
        (t_380 := ((! t_380) + 1))
     | None -> (t_386 := 0));
    (t_381 := ((! t_381) + 1))
   done;
   (t_382,
    if ((! t_386) = 0) then 0
    else if ((! t_386) = 1) then (! t_385)
    else (~- (! t_385)), (! t_380))>.
# val resIV5 :
  ('a,
   Funct4.GenIV5.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_405 ->
   let t_406 = (ref 0) in
   let t_407 = (ref 0) in
   let t_408 = {arr = (Array.copy a_405.arr)} (a_405) in
   let t_409 = a_405.m in
   let t_410 = a_405.n in
   let t_411 = (ref 1) in
   let t_412 = (ref 1) in
   while (((! t_407) < t_409) && ((! t_406) < t_410)) do
    let t_413 = (! t_406) in
    let t_414 = (! t_407) in
    let t_415 = (ref (None)) in
    let t_435 =
     begin
      for j_431 = t_413 to (t_410 - 1) do
       for j_432 = t_414 to (t_409 - 1) do
        let t_433 = (t_408.arr).((j_431 * t_408.m) + j_432) in
        if (not (t_433 = 0)) then
         (match (! t_415) with
          | Some (i_434) ->
             if ((abs (snd i_434)) > (abs t_433)) then
              (t_415 := (Some ((j_431, j_432), t_433)))
             else ()
          | None -> (t_415 := (Some ((j_431, j_432), t_433))))
        else ()
       done
      done;
      (match (! t_415) with
       | Some (i_416) ->
          if ((snd (fst i_416)) <> t_414) then begin
           let a_424 = t_408.arr
           and nm_425 = (t_408.n * t_408.m)
           and m_426 = t_408.m in
           let rec loop_427 =
            fun i1_428 ->
             fun i2_429 ->
              if (i2_429 < nm_425) then
               let t_430 = a_424.(i1_428) in
               a_424.(i1_428) <- a_424.(i2_429);
               a_424.(i2_429) <- t_430;
               (loop_427 (i1_428 + m_426) (i2_429 + m_426))
              else () in
           (loop_427 t_414 (snd (fst i_416)));
           (t_412 := (~- (! t_412)))
          end else ();
          if ((fst (fst i_416)) <> t_413) then begin
           let a_417 = t_408.arr
           and n_418 = t_408.n
           and m_419 = t_408.m in
           let i1_420 = (t_413 * m_419)
           and i2_421 = ((fst (fst i_416)) * m_419) in
           for i_422 = 0 to (m_419 - 1) do
            let t_423 = a_417.(i1_420 + i_422) in
            a_417.(i1_420 + i_422) <- a_417.(i2_421 + i_422);
            a_417.(i2_421 + i_422) <- t_423
           done;
           (t_412 := (~- (! t_412)))
          end else ();
          (Some (snd i_416))
       | None -> (None))
     end in
    (match t_435 with
     | Some (i_436) ->
        begin
         for j_437 = (t_413 + 1) to (t_410 - 1) do
          if (not ((t_408.arr).((j_437 * t_408.m) + t_414) = 0)) then begin
           for j_438 = (t_414 + 1) to (t_409 - 1) do
            (t_408.arr).((j_437 * t_408.m) + j_438) <-
             ((((t_408.arr).((j_437 * t_408.m) + j_438) *
                 (t_408.arr).((t_413 * t_408.m) + t_414)) -
                ((t_408.arr).((t_413 * t_408.m) + j_438) *
                  (t_408.arr).((j_437 * t_408.m) + t_413))) / (! t_411))
           done;
           (t_408.arr).((j_437 * t_408.m) + t_414) <- 0
          end else ()
         done;
         (t_411 := i_436)
        end;
        (t_406 := ((! t_406) + 1))
     | None -> (t_412 := 0));
    (t_407 := ((! t_407) + 1))
   done;
   (t_408,
    if ((! t_412) = 0) then 0
    else if ((! t_412) = 1) then (! t_411)
    else (~- (! t_411)), (! t_406))>.
# val resFA11 :
  ('a,
   Funct4.GenFA11.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_439 ->
   let t_440 = (ref 0) in
   let t_441 = (ref 0) in
   let t_443 =
    (Array.map (fun x_442 -> (Array.copy x_442)) (Array.copy a_439)) in
   let t_444 = (Array.length a_439.(0)) in
   let t_445 = (Array.length a_439) in
   while (((! t_441) < t_444) && ((! t_440) < t_445)) do
    let t_446 = (! t_440) in
    let t_447 = (! t_441) in
    let t_448 = (ref (None)) in
    let t_457 =
     begin
      for j_453 = t_446 to (t_445 - 1) do
       for j_454 = t_447 to (t_444 - 1) do
        let t_455 = (t_443.(j_453)).(j_454) in
        if (not (t_455 = 0.)) then
         (match (! t_448) with
          | Some (i_456) ->
             if ((abs_float (snd i_456)) < (abs_float t_455)) then
              (t_448 := (Some ((j_453, j_454), t_455)))
             else ()
          | None -> (t_448 := (Some ((j_453, j_454), t_455))))
        else ()
       done
      done;
      (match (! t_448) with
       | Some (i_449) ->
          if ((snd (fst i_449)) <> t_447) then begin
           for r_451 = 0 to ((Array.length t_443) - 1) do
            let t_452 = (t_443.(r_451)).(t_447) in
            (t_443.(r_451)).(t_447) <- (t_443.(r_451)).(snd (fst i_449));
            (t_443.(r_451)).(snd (fst i_449)) <- t_452
           done;
           ()
          end else ();
          if ((fst (fst i_449)) <> t_446) then begin
           let t_450 = t_443.(t_446) in
           t_443.(t_446) <- t_443.(fst (fst i_449));
           t_443.(fst (fst i_449)) <- t_450;
           ()
          end else ();
          (Some (snd i_449))
       | None -> (None))
     end in
    (match t_457 with
     | Some (i_458) ->
        begin
         for j_459 = (t_446 + 1) to (t_445 - 1) do
          if (not ((t_443.(j_459)).(t_447) = 0.)) then begin
           for j_460 = (t_447 + 1) to (t_444 - 1) do
            (t_443.(j_459)).(j_460) <-
             ((t_443.(j_459)).(j_460) -.
               (((t_443.(j_459)).(t_447) /. (t_443.(t_446)).(t_447)) *.
                 (t_443.(t_446)).(j_460)))
           done;
           (t_443.(j_459)).(t_447) <- 0.
          end else ()
         done;
         ()
        end;
        (t_440 := ((! t_440) + 1))
     | None -> ());
    (t_441 := ((! t_441) + 1))
   done;
   t_443>.
# val resFA12 :
  ('a,
   Funct4.GenFA12.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_461 ->
   let t_462 = (ref 0) in
   let t_463 = (ref 0) in
   let t_465 =
    (Array.map (fun x_464 -> (Array.copy x_464)) (Array.copy a_461)) in
   let t_466 = (Array.length a_461.(0)) in
   let t_467 = (Array.length a_461) in
   let t_468 = (ref 1.) in
   let t_469 = (ref 1) in
   while (((! t_463) < t_466) && ((! t_462) < t_467)) do
    let t_470 = (! t_462) in
    let t_471 = (! t_463) in
    let t_472 = (ref (None)) in
    let t_481 =
     begin
      for j_477 = t_470 to (t_467 - 1) do
       for j_478 = t_471 to (t_466 - 1) do
        let t_479 = (t_465.(j_477)).(j_478) in
        if (not (t_479 = 0.)) then
         (match (! t_472) with
          | Some (i_480) ->
             if ((abs_float (snd i_480)) < (abs_float t_479)) then
              (t_472 := (Some ((j_477, j_478), t_479)))
             else ()
          | None -> (t_472 := (Some ((j_477, j_478), t_479))))
        else ()
       done
      done;
      (match (! t_472) with
       | Some (i_473) ->
          if ((snd (fst i_473)) <> t_471) then begin
           for r_475 = 0 to ((Array.length t_465) - 1) do
            let t_476 = (t_465.(r_475)).(t_471) in
            (t_465.(r_475)).(t_471) <- (t_465.(r_475)).(snd (fst i_473));
            (t_465.(r_475)).(snd (fst i_473)) <- t_476
           done;
           (t_469 := (~- (! t_469)))
          end else ();
          if ((fst (fst i_473)) <> t_470) then begin
           let t_474 = t_465.(t_470) in
           t_465.(t_470) <- t_465.(fst (fst i_473));
           t_465.(fst (fst i_473)) <- t_474;
           (t_469 := (~- (! t_469)))
          end else ();
          (Some (snd i_473))
       | None -> (None))
     end in
    (match t_481 with
     | Some (i_482) ->
        begin
         for j_483 = (t_470 + 1) to (t_467 - 1) do
          if (not ((t_465.(j_483)).(t_471) = 0.)) then begin
           for j_484 = (t_471 + 1) to (t_466 - 1) do
            (t_465.(j_483)).(j_484) <-
             ((t_465.(j_483)).(j_484) -.
               (((t_465.(j_483)).(t_471) /. (t_465.(t_470)).(t_471)) *.
                 (t_465.(t_470)).(j_484)))
           done;
           (t_465.(j_483)).(t_471) <- 0.
          end else ()
         done;
         (t_468 := ((! t_468) *. i_482))
        end;
        (t_462 := ((! t_462) + 1))
     | None -> (t_469 := 0));
    (t_463 := ((! t_463) + 1))
   done;
   (t_465,
    if ((! t_469) = 0) then 0.
    else if ((! t_469) = 1) then (! t_468)
    else (~-. (! t_468)))>.
# val resFA13 :
  ('a,
   Funct4.GenFA13.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_485 ->
   let t_486 = (ref 0) in
   let t_487 = (ref 0) in
   let t_489 =
    (Array.map (fun x_488 -> (Array.copy x_488)) (Array.copy a_485)) in
   let t_490 = (Array.length a_485.(0)) in
   let t_491 = (Array.length a_485) in
   while (((! t_487) < t_490) && ((! t_486) < t_491)) do
    let t_492 = (! t_486) in
    let t_493 = (! t_487) in
    let t_494 = (ref (None)) in
    let t_503 =
     begin
      for j_499 = t_492 to (t_491 - 1) do
       for j_500 = t_493 to (t_490 - 1) do
        let t_501 = (t_489.(j_499)).(j_500) in
        if (not (t_501 = 0.)) then
         (match (! t_494) with
          | Some (i_502) ->
             if ((abs_float (snd i_502)) < (abs_float t_501)) then
              (t_494 := (Some ((j_499, j_500), t_501)))
             else ()
          | None -> (t_494 := (Some ((j_499, j_500), t_501))))
        else ()
       done
      done;
      (match (! t_494) with
       | Some (i_495) ->
          if ((snd (fst i_495)) <> t_493) then begin
           for r_497 = 0 to ((Array.length t_489) - 1) do
            let t_498 = (t_489.(r_497)).(t_493) in
            (t_489.(r_497)).(t_493) <- (t_489.(r_497)).(snd (fst i_495));
            (t_489.(r_497)).(snd (fst i_495)) <- t_498
           done;
           ()
          end else ();
          if ((fst (fst i_495)) <> t_492) then begin
           let t_496 = t_489.(t_492) in
           t_489.(t_492) <- t_489.(fst (fst i_495));
           t_489.(fst (fst i_495)) <- t_496;
           ()
          end else ();
          (Some (snd i_495))
       | None -> (None))
     end in
    (match t_503 with
     | Some (i_504) ->
        begin
         for j_505 = (t_492 + 1) to (t_491 - 1) do
          if (not ((t_489.(j_505)).(t_493) = 0.)) then begin
           for j_506 = (t_493 + 1) to (t_490 - 1) do
            (t_489.(j_505)).(j_506) <-
             ((t_489.(j_505)).(j_506) -.
               (((t_489.(j_505)).(t_493) /. (t_489.(t_492)).(t_493)) *.
                 (t_489.(t_492)).(j_506)))
           done;
           (t_489.(j_505)).(t_493) <- 0.
          end else ()
         done;
         ()
        end;
        (t_486 := ((! t_486) + 1))
     | None -> ());
    (t_487 := ((! t_487) + 1))
   done;
   (t_489, (! t_486))>.
# val resFA14 :
  ('a,
   Funct4.GenFA14.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_507 ->
   let t_508 = (ref 0) in
   let t_509 = (ref 0) in
   let t_511 =
    (Array.map (fun x_510 -> (Array.copy x_510)) (Array.copy a_507)) in
   let t_512 = (Array.length a_507.(0)) in
   let t_513 = (Array.length a_507) in
   let t_514 = (ref 1.) in
   let t_515 = (ref 1) in
   while (((! t_509) < t_512) && ((! t_508) < t_513)) do
    let t_516 = (! t_508) in
    let t_517 = (! t_509) in
    let t_518 = (ref (None)) in
    let t_527 =
     begin
      for j_523 = t_516 to (t_513 - 1) do
       for j_524 = t_517 to (t_512 - 1) do
        let t_525 = (t_511.(j_523)).(j_524) in
        if (not (t_525 = 0.)) then
         (match (! t_518) with
          | Some (i_526) ->
             if ((abs_float (snd i_526)) < (abs_float t_525)) then
              (t_518 := (Some ((j_523, j_524), t_525)))
             else ()
          | None -> (t_518 := (Some ((j_523, j_524), t_525))))
        else ()
       done
      done;
      (match (! t_518) with
       | Some (i_519) ->
          if ((snd (fst i_519)) <> t_517) then begin
           for r_521 = 0 to ((Array.length t_511) - 1) do
            let t_522 = (t_511.(r_521)).(t_517) in
            (t_511.(r_521)).(t_517) <- (t_511.(r_521)).(snd (fst i_519));
            (t_511.(r_521)).(snd (fst i_519)) <- t_522
           done;
           (t_515 := (~- (! t_515)))
          end else ();
          if ((fst (fst i_519)) <> t_516) then begin
           let t_520 = t_511.(t_516) in
           t_511.(t_516) <- t_511.(fst (fst i_519));
           t_511.(fst (fst i_519)) <- t_520;
           (t_515 := (~- (! t_515)))
          end else ();
          (Some (snd i_519))
       | None -> (None))
     end in
    (match t_527 with
     | Some (i_528) ->
        begin
         for j_529 = (t_516 + 1) to (t_513 - 1) do
          if (not ((t_511.(j_529)).(t_517) = 0.)) then begin
           for j_530 = (t_517 + 1) to (t_512 - 1) do
            (t_511.(j_529)).(j_530) <-
             ((t_511.(j_529)).(j_530) -.
               (((t_511.(j_529)).(t_517) /. (t_511.(t_516)).(t_517)) *.
                 (t_511.(t_516)).(j_530)))
           done;
           (t_511.(j_529)).(t_517) <- 0.
          end else ()
         done;
         (t_514 := ((! t_514) *. i_528))
        end;
        (t_508 := ((! t_508) + 1))
     | None -> (t_515 := 0));
    (t_509 := ((! t_509) + 1))
   done;
   (t_511,
    if ((! t_515) = 0) then 0.
    else if ((! t_515) = 1) then (! t_514)
    else (~-. (! t_514)), (! t_508))>.
# val resRA1 :
  ('a,
   Funct4.GenRA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res)
  code =
  .<fun a_553 ->
   let t_554 = (ref 0) in
   let t_555 = (ref 0) in
   let t_557 =
    (Array.map (fun x_556 -> (Array.copy x_556)) (Array.copy a_553)) in
   let t_558 = (Array.length a_553.(0)) in
   let t_559 = (Array.length a_553) in
   let t_560 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_561 = (ref 1) in
   while (((! t_555) < t_558) && ((! t_554) < t_559)) do
    let t_562 = (! t_554) in
    let t_563 = (! t_555) in
    let t_564 = (ref (None)) in
    let t_571 =
     begin
      let t_567 = (t_557.(t_562)).(t_563) in
      if (not (t_567 = (* cross-stage persistent value (as id: zero) *))) then
       (t_564 := (Some (t_562, t_567)))
      else
       let rec loop_568 =
        fun j_569 ->
         if (j_569 < t_559) then
          let bjc_570 = (t_557.(j_569)).(t_563) in
          if (bjc_570 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_568 (j_569 + 1))
          else (t_564 := (Some (j_569, bjc_570)))
         else () in
       (loop_568 (t_562 + 1));
      (match (! t_564) with
       | Some (i_565) ->
          if ((fst i_565) <> t_562) then begin
           let t_566 = t_557.(t_562) in
           t_557.(t_562) <- t_557.(fst i_565);
           t_557.(fst i_565) <- t_566;
           (t_561 := (~- (! t_561)))
          end else ();
          (Some (snd i_565))
       | None -> (None))
     end in
    (match t_571 with
     | Some (i_572) ->
        begin
         for j_573 = (t_562 + 1) to (t_559 - 1) do
          if (not
               ((t_557.(j_573)).(t_563) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_574 = (t_563 + 1) to (t_558 - 1) do
            (t_557.(j_573)).(j_574) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_557.(j_573)).(j_574)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_557.(j_573)).(t_563) (t_557.(t_562)).(t_563))
                 (t_557.(t_562)).(j_574)))
           done;
           (t_557.(j_573)).(t_563) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_560 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_560) i_572))
        end;
        (t_554 := ((! t_554) + 1))
     | None -> (t_561 := 0));
    (t_555 := ((! t_555) + 1))
   done;
   t_557>.
# val resRA2 :
  ('a,
   Funct4.GenRA2.Ctr.contr ->
   Funct4.OutDet(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res)
  code =
  .<fun a_575 ->
   let t_576 = (ref 0) in
   let t_577 = (ref 0) in
   let t_579 =
    (Array.map (fun x_578 -> (Array.copy x_578)) (Array.copy a_575)) in
   let t_580 = (Array.length a_575.(0)) in
   let t_581 = (Array.length a_575) in
   let t_582 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_583 = (ref 1) in
   while (((! t_577) < t_580) && ((! t_576) < t_581)) do
    let t_584 = (! t_576) in
    let t_585 = (! t_577) in
    let t_586 = (ref (None)) in
    let t_593 =
     begin
      let t_589 = (t_579.(t_584)).(t_585) in
      if (not (t_589 = (* cross-stage persistent value (as id: zero) *))) then
       (t_586 := (Some (t_584, t_589)))
      else
       let rec loop_590 =
        fun j_591 ->
         if (j_591 < t_581) then
          let bjc_592 = (t_579.(j_591)).(t_585) in
          if (bjc_592 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_590 (j_591 + 1))
          else (t_586 := (Some (j_591, bjc_592)))
         else () in
       (loop_590 (t_584 + 1));
      (match (! t_586) with
       | Some (i_587) ->
          if ((fst i_587) <> t_584) then begin
           let t_588 = t_579.(t_584) in
           t_579.(t_584) <- t_579.(fst i_587);
           t_579.(fst i_587) <- t_588;
           (t_583 := (~- (! t_583)))
          end else ();
          (Some (snd i_587))
       | None -> (None))
     end in
    (match t_593 with
     | Some (i_594) ->
        begin
         for j_595 = (t_584 + 1) to (t_581 - 1) do
          if (not
               ((t_579.(j_595)).(t_585) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_596 = (t_585 + 1) to (t_580 - 1) do
            (t_579.(j_595)).(j_596) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_579.(j_595)).(j_596)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_579.(j_595)).(t_585) (t_579.(t_584)).(t_585))
                 (t_579.(t_584)).(j_596)))
           done;
           (t_579.(j_595)).(t_585) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_582 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_582) i_594))
        end;
        (t_576 := ((! t_576) + 1))
     | None -> (t_583 := 0));
    (t_577 := ((! t_577) + 1))
   done;
   (t_579,
    if ((! t_583) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_583) = 1) then (! t_582)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_582)))>.
# val resRA3 :
  ('a,
   Funct4.GenRA3.Ctr.contr ->
   Funct4.OutRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_597 ->
   let t_598 = (ref 0) in
   let t_599 = (ref 0) in
   let t_601 =
    (Array.map (fun x_600 -> (Array.copy x_600)) (Array.copy a_597)) in
   let t_602 = (Array.length a_597.(0)) in
   let t_603 = (Array.length a_597) in
   let t_604 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_605 = (ref 1) in
   while (((! t_599) < t_602) && ((! t_598) < t_603)) do
    let t_606 = (! t_598) in
    let t_607 = (! t_599) in
    let t_608 = (ref (None)) in
    let t_615 =
     begin
      let t_611 = (t_601.(t_606)).(t_607) in
      if (not (t_611 = (* cross-stage persistent value (as id: zero) *))) then
       (t_608 := (Some (t_606, t_611)))
      else
       let rec loop_612 =
        fun j_613 ->
         if (j_613 < t_603) then
          let bjc_614 = (t_601.(j_613)).(t_607) in
          if (bjc_614 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_612 (j_613 + 1))
          else (t_608 := (Some (j_613, bjc_614)))
         else () in
       (loop_612 (t_606 + 1));
      (match (! t_608) with
       | Some (i_609) ->
          if ((fst i_609) <> t_606) then begin
           let t_610 = t_601.(t_606) in
           t_601.(t_606) <- t_601.(fst i_609);
           t_601.(fst i_609) <- t_610;
           ()
          end else ();
          (Some (snd i_609))
       | None -> (None))
     end in
    (match t_615 with
     | Some (i_616) ->
        begin
         for j_617 = (t_606 + 1) to (t_603 - 1) do
          if (not
               ((t_601.(j_617)).(t_607) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_618 = (t_607 + 1) to (t_602 - 1) do
            (t_601.(j_617)).(j_618) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_601.(j_617)).(j_618)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_601.(j_617)).(t_607) (t_601.(t_606)).(t_607))
                 (t_601.(t_606)).(j_618)))
           done;
           (t_601.(j_617)).(t_607) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_604 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_604) i_616))
        end;
        (t_598 := ((! t_598) + 1))
     | None -> (t_605 := 0));
    (t_599 := ((! t_599) + 1))
   done;
   (t_601, (! t_598))>.
# val resRA4 :
  ('a,
   Funct4.GenRA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet)(Funct4.Rank).res)
  code =
  .<fun a_619 ->
   let t_620 = (ref 0) in
   let t_621 = (ref 0) in
   let t_623 =
    (Array.map (fun x_622 -> (Array.copy x_622)) (Array.copy a_619)) in
   let t_624 = (Array.length a_619.(0)) in
   let t_625 = (Array.length a_619) in
   let t_626 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_627 = (ref 1) in
   while (((! t_621) < t_624) && ((! t_620) < t_625)) do
    let t_628 = (! t_620) in
    let t_629 = (! t_621) in
    let t_630 = (ref (None)) in
    let t_637 =
     begin
      let t_633 = (t_623.(t_628)).(t_629) in
      if (not (t_633 = (* cross-stage persistent value (as id: zero) *))) then
       (t_630 := (Some (t_628, t_633)))
      else
       let rec loop_634 =
        fun j_635 ->
         if (j_635 < t_625) then
          let bjc_636 = (t_623.(j_635)).(t_629) in
          if (bjc_636 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_634 (j_635 + 1))
          else (t_630 := (Some (j_635, bjc_636)))
         else () in
       (loop_634 (t_628 + 1));
      (match (! t_630) with
       | Some (i_631) ->
          if ((fst i_631) <> t_628) then begin
           let t_632 = t_623.(t_628) in
           t_623.(t_628) <- t_623.(fst i_631);
           t_623.(fst i_631) <- t_632;
           (t_627 := (~- (! t_627)))
          end else ();
          (Some (snd i_631))
       | None -> (None))
     end in
    (match t_637 with
     | Some (i_638) ->
        begin
         for j_639 = (t_628 + 1) to (t_625 - 1) do
          if (not
               ((t_623.(j_639)).(t_629) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_640 = (t_629 + 1) to (t_624 - 1) do
            (t_623.(j_639)).(j_640) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_623.(j_639)).(j_640)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_623.(j_639)).(t_629) (t_623.(t_628)).(t_629))
                 (t_623.(t_628)).(j_640)))
           done;
           (t_623.(j_639)).(t_629) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_626 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_626) i_638))
        end;
        (t_620 := ((! t_620) + 1))
     | None -> (t_627 := 0));
    (t_621 := ((! t_621) + 1))
   done;
   (t_623,
    if ((! t_627) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_627) = 1) then (! t_626)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_626)),
    (! t_620))>.
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
# val rFV5 :
  Funct4.GenFV5.Ctr.contr ->
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
# val rIV5 :
  Funct4.GenIV5.Ctr.contr ->
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
# val rRA1 :
  Funct4.GenRA1.Ctr.contr ->
  Funct4.OutJustMatrix(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res =
  <fun>
# val rRA2 :
  Funct4.GenRA2.Ctr.contr ->
  Funct4.OutDet(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res =
  <fun>
# val rRA3 :
  Funct4.GenRA3.Ctr.contr ->
  Funct4.OutRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res =
  <fun>
# val rRA4 :
  Funct4.GenRA4.Ctr.contr ->
  Funct4.OutDetRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet)(Funct4.Rank).res =
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
val resI11 :
  Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res
  list =
  [[|[|1|]|]; [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|];
   [|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|];
   [|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|];
   [|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|]]
# val resI12 :
  Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res
  list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 0)]
# val resI13 :
  Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res
  list =
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 3);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 2)]
# val resI14 :
  Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res
  list =
  [([|[|1|]|], 1, 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50, 3);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50, 3);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50, 3);
   ([|[|0; 2; 3|]; [|0; 0; 10|]; [|0; 0; 0|]|], 0, 2)]
#                 val iv0 : Funct4.IntegerDomain.v Funct4.container2dfromvector =
  {arr = [|1|]; n = 1; m = 1}
val iv1 : Funct4.IntegerDomain.v Funct4.container2dfromvector =
  {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3}
val iv2 : Funct4.IntegerDomain.v Funct4.container2dfromvector =
  {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4}
val iv4 : Funct4.IntegerDomain.v Funct4.container2dfromvector =
  {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}
val iv5 : Funct4.IntegerDomain.v Funct4.container2dfromvector list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 4; 13; 5; -1; 3; 0|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 4; 13; 5; 0; -1; 3; 0; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 13; 5; 0; 3; 0|]; n = 3; m = 3}]
val resI21 :
  Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res
  list =
  [{arr = [|1|]; n = 1; m = 1};
   {arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3};
   {arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4};
   {arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}]
# val resI22 :
  Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet).res
  list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50);
   ({arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}, 0)]
# val resI23 :
  Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res
  list =
  [({arr = [|1|]; n = 1; m = 1}, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 3);
   ({arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}, 2)]
# val resI24 :
  Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res
  list =
  [({arr = [|1|]; n = 1; m = 1}, 1, 1);
   ({arr = [|1; 2; 3; 0; 5; -7; 0; 0; 50|]; n = 3; m = 3}, 50, 3);
   ({arr = [|1; 2; 3; 0; 0; 5; -7; 0; 0; 0; 50; 0|]; n = 3; m = 4}, 50, 3);
   ({arr = [|0; 2; 3; 0; 0; 10; 0; 0; 0|]; n = 3; m = 3}, 0, 2)]
# val resI25 :
  Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res
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
#                                     val fa5 : Funct4.FloatDomain.v array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 10.; 5.|]; [|0.; 3.; 0.|]|]]
val resF1 :
  Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res
  list =
  [[|[|1.|]|]; [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]|];
   [|[|4.; 13.; 5.; 0.|]; [|0.; 6.25; 1.25; 0.|]; [|0.; 0.; 2.; 0.|]|];
   [|[|4.; 13.; 5.|]; [|0.; 6.25; 1.25|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 10.; 5.|]; [|0.; 0.; 2.|]; [|0.; 0.; 0.|]|]]
#                                                                                                 val a2v : 'a array array -> 'a Funct4.container2dfromvector = <fun>
val xxx : Funct4.FloatDomain.v Funct4.container2dfromvector list =
  [{arr = [|1.|]; n = 1; m = 1};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.|]; n = 3; m = 3};
   {arr = [|1.; 2.; 3.; 0.; 4.; 13.; 5.; 0.; -1.; 3.; 0.; 0.|]; n = 3; m = 4};
   {arr = [|1.; 2.; 3.; 4.; 13.; 5.; -1.; 3.; 0.; 0.; 0.; 0.|]; n = 4; m = 3};
   {arr = [|0.; 2.; 3.; 0.; 10.; 5.; 0.; 3.; 0.|]; n = 3; m = 3}]
val resFV5 :
  Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res
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
   [|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|]]
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
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 0.)]
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
   ([|[|10.; 5.; 0.|]; [|0.; 2.; 0.|]; [|0.; 0.; 0.|]|], 2)]
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
  Funct4.OutJustMatrix(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res
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
  Funct4.OutDet(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res
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
  Funct4.OutRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res
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
  Funct4.OutDetRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet)(Funct4.Rank).res
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
