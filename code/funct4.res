        Objective Caml version 3.09.1

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
    let t_102 =
     begin
      for j_99 = t_89 to (t_86 - 1) do
       let t_100 = (t_84.arr).((j_99 * t_84.m) + t_90) in
       if (not (t_100 = 0.)) then
        (match (! t_91) with
         | Some (i_101) ->
            if ((abs_float (snd i_101)) < (abs_float t_100)) then
             (t_91 := (Some (j_99, t_100)))
            else ()
         | None -> (t_91 := (Some (j_99, t_100))))
       else ()
      done;
      (match (! t_91) with
       | Some (i_92) ->
          if ((fst i_92) <> t_89) then begin
           let a_93 = t_84.arr
           and m_94 = t_84.m in
           let i1_95 = (t_89 * m_94)
           and i2_96 = ((fst i_92) * m_94) in
           for i_97 = 0 to (m_94 - 1) do
            let t_98 = a_93.(i1_95 + i_97) in
            a_93.(i1_95 + i_97) <- a_93.(i2_96 + i_97);
            a_93.(i2_96 + i_97) <- t_98
           done;
           (t_88 := (~- (! t_88)))
          end else ();
          (Some (snd i_92))
       | None -> (None))
     end in
    (match t_102 with
     | Some (i_103) ->
        begin
         for j_104 = (t_89 + 1) to (t_86 - 1) do
          if (not ((t_84.arr).((j_104 * t_84.m) + t_90) = 0.)) then begin
           for j_105 = (t_90 + 1) to (t_85 - 1) do
            (t_84.arr).((j_104 * t_84.m) + j_105) <-
             ((t_84.arr).((j_104 * t_84.m) + j_105) -.
               (((t_84.arr).((j_104 * t_84.m) + t_90) /.
                  (t_84.arr).((t_89 * t_84.m) + t_90)) *.
                 (t_84.arr).((t_89 * t_84.m) + j_105)))
           done;
           (t_84.arr).((j_104 * t_84.m) + t_90) <- 0.
          end else ()
         done;
         (t_87 := ((! t_87) *. i_103))
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
  .<fun a_106 ->
   let t_107 = (ref 0) in
   let t_108 = (ref 0) in
   let t_109 = {arr = (Array.copy a_106.arr)} (a_106) in
   let t_110 = a_106.m in
   let t_111 = a_106.n in
   let t_112 = (ref 1.) in
   let t_113 = (ref 1) in
   while (((! t_108) < t_110) && ((! t_107) < t_111)) do
    let t_114 = (! t_107) in
    let t_115 = (! t_108) in
    let t_116 = (ref (None)) in
    let t_127 =
     begin
      for j_124 = t_114 to (t_111 - 1) do
       let t_125 = (t_109.arr).((j_124 * t_109.m) + t_115) in
       if (not (t_125 = 0.)) then
        (match (! t_116) with
         | Some (i_126) ->
            if ((abs_float (snd i_126)) < (abs_float t_125)) then
             (t_116 := (Some (j_124, t_125)))
            else ()
         | None -> (t_116 := (Some (j_124, t_125))))
       else ()
      done;
      (match (! t_116) with
       | Some (i_117) ->
          if ((fst i_117) <> t_114) then begin
           let a_118 = t_109.arr
           and m_119 = t_109.m in
           let i1_120 = (t_114 * m_119)
           and i2_121 = ((fst i_117) * m_119) in
           for i_122 = 0 to (m_119 - 1) do
            let t_123 = a_118.(i1_120 + i_122) in
            a_118.(i1_120 + i_122) <- a_118.(i2_121 + i_122);
            a_118.(i2_121 + i_122) <- t_123
           done;
           (t_113 := (~- (! t_113)))
          end else ();
          (Some (snd i_117))
       | None -> (None))
     end in
    (match t_127 with
     | Some (i_128) ->
        begin
         for j_129 = (t_114 + 1) to (t_111 - 1) do
          if (not ((t_109.arr).((j_129 * t_109.m) + t_115) = 0.)) then begin
           for j_130 = (t_115 + 1) to (t_110 - 1) do
            (t_109.arr).((j_129 * t_109.m) + j_130) <-
             ((t_109.arr).((j_129 * t_109.m) + j_130) -.
               (((t_109.arr).((j_129 * t_109.m) + t_115) /.
                  (t_109.arr).((t_114 * t_109.m) + t_115)) *.
                 (t_109.arr).((t_114 * t_109.m) + j_130)))
           done;
           (t_109.arr).((j_129 * t_109.m) + t_115) <- 0.
          end else ()
         done;
         (t_112 := ((! t_112) *. i_128))
        end;
        (t_107 := ((! t_107) + 1))
     | None -> (t_113 := 0));
    (t_108 := ((! t_108) + 1))
   done;
   (t_109,
    if ((! t_113) = 0) then 0.
    else if ((! t_113) = 1) then (! t_112)
    else (~-. (! t_112)))>.
# val resFV3 :
  ('a,
   Funct4.GenFV3.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_131 ->
   let t_132 = (ref 0) in
   let t_133 = (ref 0) in
   let t_134 = {arr = (Array.copy a_131.arr)} (a_131) in
   let t_135 = a_131.m in
   let t_136 = a_131.n in
   while (((! t_133) < t_135) && ((! t_132) < t_136)) do
    let t_137 = (! t_132) in
    let t_138 = (! t_133) in
    let t_139 = (ref (None)) in
    let t_150 =
     begin
      for j_147 = t_137 to (t_136 - 1) do
       let t_148 = (t_134.arr).((j_147 * t_134.m) + t_138) in
       if (not (t_148 = 0.)) then
        (match (! t_139) with
         | Some (i_149) ->
            if ((abs_float (snd i_149)) < (abs_float t_148)) then
             (t_139 := (Some (j_147, t_148)))
            else ()
         | None -> (t_139 := (Some (j_147, t_148))))
       else ()
      done;
      (match (! t_139) with
       | Some (i_140) ->
          if ((fst i_140) <> t_137) then begin
           let a_141 = t_134.arr
           and m_142 = t_134.m in
           let i1_143 = (t_137 * m_142)
           and i2_144 = ((fst i_140) * m_142) in
           for i_145 = 0 to (m_142 - 1) do
            let t_146 = a_141.(i1_143 + i_145) in
            a_141.(i1_143 + i_145) <- a_141.(i2_144 + i_145);
            a_141.(i2_144 + i_145) <- t_146
           done;
           ()
          end else ();
          (Some (snd i_140))
       | None -> (None))
     end in
    (match t_150 with
     | Some (i_151) ->
        begin
         for j_152 = (t_137 + 1) to (t_136 - 1) do
          if (not ((t_134.arr).((j_152 * t_134.m) + t_138) = 0.)) then begin
           for j_153 = (t_138 + 1) to (t_135 - 1) do
            (t_134.arr).((j_152 * t_134.m) + j_153) <-
             ((t_134.arr).((j_152 * t_134.m) + j_153) -.
               (((t_134.arr).((j_152 * t_134.m) + t_138) /.
                  (t_134.arr).((t_137 * t_134.m) + t_138)) *.
                 (t_134.arr).((t_137 * t_134.m) + j_153)))
           done;
           (t_134.arr).((j_152 * t_134.m) + t_138) <- 0.
          end else ()
         done;
         ()
        end;
        (t_132 := ((! t_132) + 1))
     | None -> ());
    (t_133 := ((! t_133) + 1))
   done;
   (t_134, (! t_132))>.
# val resFV4 :
  ('a,
   Funct4.GenFV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_154 ->
   let t_155 = (ref 0) in
   let t_156 = (ref 0) in
   let t_157 = {arr = (Array.copy a_154.arr)} (a_154) in
   let t_158 = a_154.m in
   let t_159 = a_154.n in
   let t_160 = (ref 1.) in
   let t_161 = (ref 1) in
   while (((! t_156) < t_158) && ((! t_155) < t_159)) do
    let t_162 = (! t_155) in
    let t_163 = (! t_156) in
    let t_164 = (ref (None)) in
    let t_175 =
     begin
      for j_172 = t_162 to (t_159 - 1) do
       let t_173 = (t_157.arr).((j_172 * t_157.m) + t_163) in
       if (not (t_173 = 0.)) then
        (match (! t_164) with
         | Some (i_174) ->
            if ((abs_float (snd i_174)) < (abs_float t_173)) then
             (t_164 := (Some (j_172, t_173)))
            else ()
         | None -> (t_164 := (Some (j_172, t_173))))
       else ()
      done;
      (match (! t_164) with
       | Some (i_165) ->
          if ((fst i_165) <> t_162) then begin
           let a_166 = t_157.arr
           and m_167 = t_157.m in
           let i1_168 = (t_162 * m_167)
           and i2_169 = ((fst i_165) * m_167) in
           for i_170 = 0 to (m_167 - 1) do
            let t_171 = a_166.(i1_168 + i_170) in
            a_166.(i1_168 + i_170) <- a_166.(i2_169 + i_170);
            a_166.(i2_169 + i_170) <- t_171
           done;
           (t_161 := (~- (! t_161)))
          end else ();
          (Some (snd i_165))
       | None -> (None))
     end in
    (match t_175 with
     | Some (i_176) ->
        begin
         for j_177 = (t_162 + 1) to (t_159 - 1) do
          if (not ((t_157.arr).((j_177 * t_157.m) + t_163) = 0.)) then begin
           for j_178 = (t_163 + 1) to (t_158 - 1) do
            (t_157.arr).((j_177 * t_157.m) + j_178) <-
             ((t_157.arr).((j_177 * t_157.m) + j_178) -.
               (((t_157.arr).((j_177 * t_157.m) + t_163) /.
                  (t_157.arr).((t_162 * t_157.m) + t_163)) *.
                 (t_157.arr).((t_162 * t_157.m) + j_178)))
           done;
           (t_157.arr).((j_177 * t_157.m) + t_163) <- 0.
          end else ()
         done;
         (t_160 := ((! t_160) *. i_176))
        end;
        (t_155 := ((! t_155) + 1))
     | None -> (t_161 := 0));
    (t_156 := ((! t_156) + 1))
   done;
   (t_157,
    if ((! t_161) = 0) then 0.
    else if ((! t_161) = 1) then (! t_160)
    else (~-. (! t_160)), (! t_155))>.
# val resFV5 :
  ('a,
   Funct4.GenFV5.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_179 ->
   let t_180 = (ref 0) in
   let t_181 = (ref 0) in
   let t_182 = {arr = (Array.copy a_179.arr)} (a_179) in
   let t_183 = a_179.m in
   let t_184 = a_179.n in
   let t_185 = (ref 1.) in
   let t_186 = (ref 1) in
   while (((! t_181) < t_183) && ((! t_180) < t_184)) do
    let t_187 = (! t_180) in
    let t_188 = (! t_181) in
    let t_189 = (ref (None)) in
    let t_208 =
     begin
      for j_204 = t_187 to (t_184 - 1) do
       for j_205 = t_188 to (t_183 - 1) do
        let t_206 = (t_182.arr).((j_204 * t_182.m) + j_205) in
        if (not (t_206 = 0.)) then
         (match (! t_189) with
          | Some (i_207) ->
             if ((abs_float (snd i_207)) < (abs_float t_206)) then
              (t_189 := (Some ((j_204, j_205), t_206)))
             else ()
          | None -> (t_189 := (Some ((j_204, j_205), t_206))))
        else ()
       done
      done;
      (match (! t_189) with
       | Some (i_190) ->
          if ((snd (fst i_190)) <> t_188) then begin
           let a_197 = t_182.arr
           and nm_198 = (t_182.n * t_182.m)
           and m_199 = t_182.m in
           let rec loop_200 =
            fun i1_201 ->
             fun i2_202 ->
              if (i2_202 < nm_198) then
               let t_203 = a_197.(i1_201) in
               a_197.(i1_201) <- a_197.(i2_202);
               a_197.(i2_202) <- t_203;
               (loop_200 (i1_201 + m_199) (i2_202 + m_199))
              else () in
           (loop_200 t_188 (snd (fst i_190)));
           (t_186 := (~- (! t_186)))
          end else ();
          if ((fst (fst i_190)) <> t_187) then begin
           let a_191 = t_182.arr
           and m_192 = t_182.m in
           let i1_193 = (t_187 * m_192)
           and i2_194 = ((fst (fst i_190)) * m_192) in
           for i_195 = 0 to (m_192 - 1) do
            let t_196 = a_191.(i1_193 + i_195) in
            a_191.(i1_193 + i_195) <- a_191.(i2_194 + i_195);
            a_191.(i2_194 + i_195) <- t_196
           done;
           (t_186 := (~- (! t_186)))
          end else ();
          (Some (snd i_190))
       | None -> (None))
     end in
    (match t_208 with
     | Some (i_209) ->
        begin
         for j_210 = (t_187 + 1) to (t_184 - 1) do
          if (not ((t_182.arr).((j_210 * t_182.m) + t_188) = 0.)) then begin
           for j_211 = (t_188 + 1) to (t_183 - 1) do
            (t_182.arr).((j_210 * t_182.m) + j_211) <-
             ((t_182.arr).((j_210 * t_182.m) + j_211) -.
               (((t_182.arr).((j_210 * t_182.m) + t_188) /.
                  (t_182.arr).((t_187 * t_182.m) + t_188)) *.
                 (t_182.arr).((t_187 * t_182.m) + j_211)))
           done;
           (t_182.arr).((j_210 * t_182.m) + t_188) <- 0.
          end else ()
         done;
         (t_185 := ((! t_185) *. i_209))
        end;
        (t_180 := ((! t_180) + 1))
     | None -> (t_186 := 0));
    (t_181 := ((! t_181) + 1))
   done;
   (t_182,
    if ((! t_186) = 0) then 0.
    else if ((! t_186) = 1) then (! t_185)
    else (~-. (! t_185)), (! t_180))>.
# val resIA1 :
  ('a,
   Funct4.GenIA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_212 ->
   let t_213 = (ref 0) in
   let t_214 = (ref 0) in
   let t_216 =
    (Array.map (fun x_215 -> (Array.copy x_215)) (Array.copy a_212)) in
   let t_217 = (Array.length a_212.(0)) in
   let t_218 = (Array.length a_212) in
   let t_219 = (ref 1) in
   let t_220 = (ref 1) in
   while (((! t_214) < t_217) && ((! t_213) < t_218)) do
    let t_221 = (! t_213) in
    let t_222 = (! t_214) in
    let t_223 = (ref (None)) in
    let t_229 =
     begin
      for j_226 = t_221 to (t_218 - 1) do
       let t_227 = (t_216.(j_226)).(t_222) in
       if (not (t_227 = 0)) then
        (match (! t_223) with
         | Some (i_228) ->
            if ((abs (snd i_228)) > (abs t_227)) then
             (t_223 := (Some (j_226, t_227)))
            else ()
         | None -> (t_223 := (Some (j_226, t_227))))
       else ()
      done;
      (match (! t_223) with
       | Some (i_224) ->
          if ((fst i_224) <> t_221) then begin
           let t_225 = t_216.(t_221) in
           t_216.(t_221) <- t_216.(fst i_224);
           t_216.(fst i_224) <- t_225;
           (t_220 := (~- (! t_220)))
          end else ();
          (Some (snd i_224))
       | None -> (None))
     end in
    (match t_229 with
     | Some (i_230) ->
        begin
         for j_231 = (t_221 + 1) to (t_218 - 1) do
          if (not ((t_216.(j_231)).(t_222) = 0)) then begin
           for j_232 = (t_222 + 1) to (t_217 - 1) do
            (t_216.(j_231)).(j_232) <-
             ((((t_216.(j_231)).(j_232) * (t_216.(t_221)).(t_222)) -
                ((t_216.(t_221)).(j_232) * (t_216.(j_231)).(t_221))) /
               (! t_219))
           done;
           (t_216.(j_231)).(t_222) <- 0
          end else ()
         done;
         (t_219 := i_230)
        end;
        (t_213 := ((! t_213) + 1))
     | None -> (t_220 := 0));
    (t_214 := ((! t_214) + 1))
   done;
   t_216>.
# val resIA2 :
  ('a,
   Funct4.GenIA2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_233 ->
   let t_234 = (ref 0) in
   let t_235 = (ref 0) in
   let t_237 =
    (Array.map (fun x_236 -> (Array.copy x_236)) (Array.copy a_233)) in
   let t_238 = (Array.length a_233.(0)) in
   let t_239 = (Array.length a_233) in
   let t_240 = (ref 1) in
   let t_241 = (ref 1) in
   while (((! t_235) < t_238) && ((! t_234) < t_239)) do
    let t_242 = (! t_234) in
    let t_243 = (! t_235) in
    let t_244 = (ref (None)) in
    let t_250 =
     begin
      for j_247 = t_242 to (t_239 - 1) do
       let t_248 = (t_237.(j_247)).(t_243) in
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
          if ((fst i_245) <> t_242) then begin
           let t_246 = t_237.(t_242) in
           t_237.(t_242) <- t_237.(fst i_245);
           t_237.(fst i_245) <- t_246;
           (t_241 := (~- (! t_241)))
          end else ();
          (Some (snd i_245))
       | None -> (None))
     end in
    (match t_250 with
     | Some (i_251) ->
        begin
         for j_252 = (t_242 + 1) to (t_239 - 1) do
          if (not ((t_237.(j_252)).(t_243) = 0)) then begin
           for j_253 = (t_243 + 1) to (t_238 - 1) do
            (t_237.(j_252)).(j_253) <-
             ((((t_237.(j_252)).(j_253) * (t_237.(t_242)).(t_243)) -
                ((t_237.(t_242)).(j_253) * (t_237.(j_252)).(t_242))) /
               (! t_240))
           done;
           (t_237.(j_252)).(t_243) <- 0
          end else ()
         done;
         (t_240 := i_251)
        end;
        (t_234 := ((! t_234) + 1))
     | None -> (t_241 := 0));
    (t_235 := ((! t_235) + 1))
   done;
   (t_237,
    if ((! t_241) = 0) then 0
    else if ((! t_241) = 1) then (! t_240)
    else (~- (! t_240)))>.
# val resIA3 :
  ('a,
   Funct4.GenIA3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_254 ->
   let t_255 = (ref 0) in
   let t_256 = (ref 0) in
   let t_258 =
    (Array.map (fun x_257 -> (Array.copy x_257)) (Array.copy a_254)) in
   let t_259 = (Array.length a_254.(0)) in
   let t_260 = (Array.length a_254) in
   let t_261 = (ref 1) in
   let t_262 = (ref 1) in
   while (((! t_256) < t_259) && ((! t_255) < t_260)) do
    let t_263 = (! t_255) in
    let t_264 = (! t_256) in
    let t_265 = (ref (None)) in
    let t_271 =
     begin
      for j_268 = t_263 to (t_260 - 1) do
       let t_269 = (t_258.(j_268)).(t_264) in
       if (not (t_269 = 0)) then
        (match (! t_265) with
         | Some (i_270) ->
            if ((abs (snd i_270)) > (abs t_269)) then
             (t_265 := (Some (j_268, t_269)))
            else ()
         | None -> (t_265 := (Some (j_268, t_269))))
       else ()
      done;
      (match (! t_265) with
       | Some (i_266) ->
          if ((fst i_266) <> t_263) then begin
           let t_267 = t_258.(t_263) in
           t_258.(t_263) <- t_258.(fst i_266);
           t_258.(fst i_266) <- t_267;
           ()
          end else ();
          (Some (snd i_266))
       | None -> (None))
     end in
    (match t_271 with
     | Some (i_272) ->
        begin
         for j_273 = (t_263 + 1) to (t_260 - 1) do
          if (not ((t_258.(j_273)).(t_264) = 0)) then begin
           for j_274 = (t_264 + 1) to (t_259 - 1) do
            (t_258.(j_273)).(j_274) <-
             ((((t_258.(j_273)).(j_274) * (t_258.(t_263)).(t_264)) -
                ((t_258.(t_263)).(j_274) * (t_258.(j_273)).(t_263))) /
               (! t_261))
           done;
           (t_258.(j_273)).(t_264) <- 0
          end else ()
         done;
         (t_261 := i_272)
        end;
        (t_255 := ((! t_255) + 1))
     | None -> (t_262 := 0));
    (t_256 := ((! t_256) + 1))
   done;
   (t_258, (! t_255))>.
# val resIA4 :
  ('a,
   Funct4.GenIA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_275 ->
   let t_276 = (ref 0) in
   let t_277 = (ref 0) in
   let t_279 =
    (Array.map (fun x_278 -> (Array.copy x_278)) (Array.copy a_275)) in
   let t_280 = (Array.length a_275.(0)) in
   let t_281 = (Array.length a_275) in
   let t_282 = (ref 1) in
   let t_283 = (ref 1) in
   while (((! t_277) < t_280) && ((! t_276) < t_281)) do
    let t_284 = (! t_276) in
    let t_285 = (! t_277) in
    let t_286 = (ref (None)) in
    let t_292 =
     begin
      for j_289 = t_284 to (t_281 - 1) do
       let t_290 = (t_279.(j_289)).(t_285) in
       if (not (t_290 = 0)) then
        (match (! t_286) with
         | Some (i_291) ->
            if ((abs (snd i_291)) > (abs t_290)) then
             (t_286 := (Some (j_289, t_290)))
            else ()
         | None -> (t_286 := (Some (j_289, t_290))))
       else ()
      done;
      (match (! t_286) with
       | Some (i_287) ->
          if ((fst i_287) <> t_284) then begin
           let t_288 = t_279.(t_284) in
           t_279.(t_284) <- t_279.(fst i_287);
           t_279.(fst i_287) <- t_288;
           (t_283 := (~- (! t_283)))
          end else ();
          (Some (snd i_287))
       | None -> (None))
     end in
    (match t_292 with
     | Some (i_293) ->
        begin
         for j_294 = (t_284 + 1) to (t_281 - 1) do
          if (not ((t_279.(j_294)).(t_285) = 0)) then begin
           for j_295 = (t_285 + 1) to (t_280 - 1) do
            (t_279.(j_294)).(j_295) <-
             ((((t_279.(j_294)).(j_295) * (t_279.(t_284)).(t_285)) -
                ((t_279.(t_284)).(j_295) * (t_279.(j_294)).(t_284))) /
               (! t_282))
           done;
           (t_279.(j_294)).(t_285) <- 0
          end else ()
         done;
         (t_282 := i_293)
        end;
        (t_276 := ((! t_276) + 1))
     | None -> (t_283 := 0));
    (t_277 := ((! t_277) + 1))
   done;
   (t_279,
    if ((! t_283) = 0) then 0
    else if ((! t_283) = 1) then (! t_282)
    else (~- (! t_282)), (! t_276))>.
# val resIV1 :
  ('a,
   Funct4.GenIV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_296 ->
   let t_297 = (ref 0) in
   let t_298 = (ref 0) in
   let t_299 = {arr = (Array.copy a_296.arr)} (a_296) in
   let t_300 = a_296.m in
   let t_301 = a_296.n in
   let t_302 = (ref 1) in
   let t_303 = (ref 1) in
   while (((! t_298) < t_300) && ((! t_297) < t_301)) do
    let t_304 = (! t_297) in
    let t_305 = (! t_298) in
    let t_306 = (ref (None)) in
    let t_317 =
     begin
      for j_314 = t_304 to (t_301 - 1) do
       let t_315 = (t_299.arr).((j_314 * t_299.m) + t_305) in
       if (not (t_315 = 0)) then
        (match (! t_306) with
         | Some (i_316) ->
            if ((abs (snd i_316)) > (abs t_315)) then
             (t_306 := (Some (j_314, t_315)))
            else ()
         | None -> (t_306 := (Some (j_314, t_315))))
       else ()
      done;
      (match (! t_306) with
       | Some (i_307) ->
          if ((fst i_307) <> t_304) then begin
           let a_308 = t_299.arr
           and m_309 = t_299.m in
           let i1_310 = (t_304 * m_309)
           and i2_311 = ((fst i_307) * m_309) in
           for i_312 = 0 to (m_309 - 1) do
            let t_313 = a_308.(i1_310 + i_312) in
            a_308.(i1_310 + i_312) <- a_308.(i2_311 + i_312);
            a_308.(i2_311 + i_312) <- t_313
           done;
           ()
          end else ();
          (Some (snd i_307))
       | None -> (None))
     end in
    (match t_317 with
     | Some (i_318) ->
        begin
         for j_319 = (t_304 + 1) to (t_301 - 1) do
          if (not ((t_299.arr).((j_319 * t_299.m) + t_305) = 0)) then begin
           for j_320 = (t_305 + 1) to (t_300 - 1) do
            (t_299.arr).((j_319 * t_299.m) + j_320) <-
             ((((t_299.arr).((j_319 * t_299.m) + j_320) *
                 (t_299.arr).((t_304 * t_299.m) + t_305)) -
                ((t_299.arr).((t_304 * t_299.m) + j_320) *
                  (t_299.arr).((j_319 * t_299.m) + t_304))) / (! t_302))
           done;
           (t_299.arr).((j_319 * t_299.m) + t_305) <- 0
          end else ()
         done;
         (t_302 := i_318)
        end;
        (t_297 := ((! t_297) + 1))
     | None -> (t_303 := 0));
    (t_298 := ((! t_298) + 1))
   done;
   t_299>.
# val resIV2 :
  ('a,
   Funct4.GenIV2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_321 ->
   let t_322 = (ref 0) in
   let t_323 = (ref 0) in
   let t_324 = {arr = (Array.copy a_321.arr)} (a_321) in
   let t_325 = a_321.m in
   let t_326 = a_321.n in
   let t_327 = (ref 1) in
   let t_328 = (ref 1) in
   while (((! t_323) < t_325) && ((! t_322) < t_326)) do
    let t_329 = (! t_322) in
    let t_330 = (! t_323) in
    let t_331 = (ref (None)) in
    let t_342 =
     begin
      for j_339 = t_329 to (t_326 - 1) do
       let t_340 = (t_324.arr).((j_339 * t_324.m) + t_330) in
       if (not (t_340 = 0)) then
        (match (! t_331) with
         | Some (i_341) ->
            if ((abs (snd i_341)) > (abs t_340)) then
             (t_331 := (Some (j_339, t_340)))
            else ()
         | None -> (t_331 := (Some (j_339, t_340))))
       else ()
      done;
      (match (! t_331) with
       | Some (i_332) ->
          if ((fst i_332) <> t_329) then begin
           let a_333 = t_324.arr
           and m_334 = t_324.m in
           let i1_335 = (t_329 * m_334)
           and i2_336 = ((fst i_332) * m_334) in
           for i_337 = 0 to (m_334 - 1) do
            let t_338 = a_333.(i1_335 + i_337) in
            a_333.(i1_335 + i_337) <- a_333.(i2_336 + i_337);
            a_333.(i2_336 + i_337) <- t_338
           done;
           (t_328 := (~- (! t_328)))
          end else ();
          (Some (snd i_332))
       | None -> (None))
     end in
    (match t_342 with
     | Some (i_343) ->
        begin
         for j_344 = (t_329 + 1) to (t_326 - 1) do
          if (not ((t_324.arr).((j_344 * t_324.m) + t_330) = 0)) then begin
           for j_345 = (t_330 + 1) to (t_325 - 1) do
            (t_324.arr).((j_344 * t_324.m) + j_345) <-
             ((((t_324.arr).((j_344 * t_324.m) + j_345) *
                 (t_324.arr).((t_329 * t_324.m) + t_330)) -
                ((t_324.arr).((t_329 * t_324.m) + j_345) *
                  (t_324.arr).((j_344 * t_324.m) + t_329))) / (! t_327))
           done;
           (t_324.arr).((j_344 * t_324.m) + t_330) <- 0
          end else ()
         done;
         (t_327 := i_343)
        end;
        (t_322 := ((! t_322) + 1))
     | None -> (t_328 := 0));
    (t_323 := ((! t_323) + 1))
   done;
   (t_324,
    if ((! t_328) = 0) then 0
    else if ((! t_328) = 1) then (! t_327)
    else (~- (! t_327)))>.
# val resIV3 :
  ('a,
   Funct4.GenIV3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_346 ->
   let t_347 = (ref 0) in
   let t_348 = (ref 0) in
   let t_349 = {arr = (Array.copy a_346.arr)} (a_346) in
   let t_350 = a_346.m in
   let t_351 = a_346.n in
   let t_352 = (ref 1) in
   let t_353 = (ref 1) in
   while (((! t_348) < t_350) && ((! t_347) < t_351)) do
    let t_354 = (! t_347) in
    let t_355 = (! t_348) in
    let t_356 = (ref (None)) in
    let t_367 =
     begin
      for j_364 = t_354 to (t_351 - 1) do
       let t_365 = (t_349.arr).((j_364 * t_349.m) + t_355) in
       if (not (t_365 = 0)) then
        (match (! t_356) with
         | Some (i_366) ->
            if ((abs (snd i_366)) > (abs t_365)) then
             (t_356 := (Some (j_364, t_365)))
            else ()
         | None -> (t_356 := (Some (j_364, t_365))))
       else ()
      done;
      (match (! t_356) with
       | Some (i_357) ->
          if ((fst i_357) <> t_354) then begin
           let a_358 = t_349.arr
           and m_359 = t_349.m in
           let i1_360 = (t_354 * m_359)
           and i2_361 = ((fst i_357) * m_359) in
           for i_362 = 0 to (m_359 - 1) do
            let t_363 = a_358.(i1_360 + i_362) in
            a_358.(i1_360 + i_362) <- a_358.(i2_361 + i_362);
            a_358.(i2_361 + i_362) <- t_363
           done;
           ()
          end else ();
          (Some (snd i_357))
       | None -> (None))
     end in
    (match t_367 with
     | Some (i_368) ->
        begin
         for j_369 = (t_354 + 1) to (t_351 - 1) do
          if (not ((t_349.arr).((j_369 * t_349.m) + t_355) = 0)) then begin
           for j_370 = (t_355 + 1) to (t_350 - 1) do
            (t_349.arr).((j_369 * t_349.m) + j_370) <-
             ((((t_349.arr).((j_369 * t_349.m) + j_370) *
                 (t_349.arr).((t_354 * t_349.m) + t_355)) -
                ((t_349.arr).((t_354 * t_349.m) + j_370) *
                  (t_349.arr).((j_369 * t_349.m) + t_354))) / (! t_352))
           done;
           (t_349.arr).((j_369 * t_349.m) + t_355) <- 0
          end else ()
         done;
         (t_352 := i_368)
        end;
        (t_347 := ((! t_347) + 1))
     | None -> (t_353 := 0));
    (t_348 := ((! t_348) + 1))
   done;
   (t_349, (! t_347))>.
# val resIV4 :
  ('a,
   Funct4.GenIV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_371 ->
   let t_372 = (ref 0) in
   let t_373 = (ref 0) in
   let t_374 = {arr = (Array.copy a_371.arr)} (a_371) in
   let t_375 = a_371.m in
   let t_376 = a_371.n in
   let t_377 = (ref 1) in
   let t_378 = (ref 1) in
   while (((! t_373) < t_375) && ((! t_372) < t_376)) do
    let t_379 = (! t_372) in
    let t_380 = (! t_373) in
    let t_381 = (ref (None)) in
    let t_392 =
     begin
      for j_389 = t_379 to (t_376 - 1) do
       let t_390 = (t_374.arr).((j_389 * t_374.m) + t_380) in
       if (not (t_390 = 0)) then
        (match (! t_381) with
         | Some (i_391) ->
            if ((abs (snd i_391)) > (abs t_390)) then
             (t_381 := (Some (j_389, t_390)))
            else ()
         | None -> (t_381 := (Some (j_389, t_390))))
       else ()
      done;
      (match (! t_381) with
       | Some (i_382) ->
          if ((fst i_382) <> t_379) then begin
           let a_383 = t_374.arr
           and m_384 = t_374.m in
           let i1_385 = (t_379 * m_384)
           and i2_386 = ((fst i_382) * m_384) in
           for i_387 = 0 to (m_384 - 1) do
            let t_388 = a_383.(i1_385 + i_387) in
            a_383.(i1_385 + i_387) <- a_383.(i2_386 + i_387);
            a_383.(i2_386 + i_387) <- t_388
           done;
           (t_378 := (~- (! t_378)))
          end else ();
          (Some (snd i_382))
       | None -> (None))
     end in
    (match t_392 with
     | Some (i_393) ->
        begin
         for j_394 = (t_379 + 1) to (t_376 - 1) do
          if (not ((t_374.arr).((j_394 * t_374.m) + t_380) = 0)) then begin
           for j_395 = (t_380 + 1) to (t_375 - 1) do
            (t_374.arr).((j_394 * t_374.m) + j_395) <-
             ((((t_374.arr).((j_394 * t_374.m) + j_395) *
                 (t_374.arr).((t_379 * t_374.m) + t_380)) -
                ((t_374.arr).((t_379 * t_374.m) + j_395) *
                  (t_374.arr).((j_394 * t_374.m) + t_379))) / (! t_377))
           done;
           (t_374.arr).((j_394 * t_374.m) + t_380) <- 0
          end else ()
         done;
         (t_377 := i_393)
        end;
        (t_372 := ((! t_372) + 1))
     | None -> (t_378 := 0));
    (t_373 := ((! t_373) + 1))
   done;
   (t_374,
    if ((! t_378) = 0) then 0
    else if ((! t_378) = 1) then (! t_377)
    else (~- (! t_377)), (! t_372))>.
# val resIV5 :
  ('a,
   Funct4.GenIV5.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_396 ->
   let t_397 = (ref 0) in
   let t_398 = (ref 0) in
   let t_399 = {arr = (Array.copy a_396.arr)} (a_396) in
   let t_400 = a_396.m in
   let t_401 = a_396.n in
   let t_402 = (ref 1) in
   let t_403 = (ref 1) in
   while (((! t_398) < t_400) && ((! t_397) < t_401)) do
    let t_404 = (! t_397) in
    let t_405 = (! t_398) in
    let t_406 = (ref (None)) in
    let t_425 =
     begin
      for j_421 = t_404 to (t_401 - 1) do
       for j_422 = t_405 to (t_400 - 1) do
        let t_423 = (t_399.arr).((j_421 * t_399.m) + j_422) in
        if (not (t_423 = 0)) then
         (match (! t_406) with
          | Some (i_424) ->
             if ((abs (snd i_424)) > (abs t_423)) then
              (t_406 := (Some ((j_421, j_422), t_423)))
             else ()
          | None -> (t_406 := (Some ((j_421, j_422), t_423))))
        else ()
       done
      done;
      (match (! t_406) with
       | Some (i_407) ->
          if ((snd (fst i_407)) <> t_405) then begin
           let a_414 = t_399.arr
           and nm_415 = (t_399.n * t_399.m)
           and m_416 = t_399.m in
           let rec loop_417 =
            fun i1_418 ->
             fun i2_419 ->
              if (i2_419 < nm_415) then
               let t_420 = a_414.(i1_418) in
               a_414.(i1_418) <- a_414.(i2_419);
               a_414.(i2_419) <- t_420;
               (loop_417 (i1_418 + m_416) (i2_419 + m_416))
              else () in
           (loop_417 t_405 (snd (fst i_407)));
           (t_403 := (~- (! t_403)))
          end else ();
          if ((fst (fst i_407)) <> t_404) then begin
           let a_408 = t_399.arr
           and m_409 = t_399.m in
           let i1_410 = (t_404 * m_409)
           and i2_411 = ((fst (fst i_407)) * m_409) in
           for i_412 = 0 to (m_409 - 1) do
            let t_413 = a_408.(i1_410 + i_412) in
            a_408.(i1_410 + i_412) <- a_408.(i2_411 + i_412);
            a_408.(i2_411 + i_412) <- t_413
           done;
           (t_403 := (~- (! t_403)))
          end else ();
          (Some (snd i_407))
       | None -> (None))
     end in
    (match t_425 with
     | Some (i_426) ->
        begin
         for j_427 = (t_404 + 1) to (t_401 - 1) do
          if (not ((t_399.arr).((j_427 * t_399.m) + t_405) = 0)) then begin
           for j_428 = (t_405 + 1) to (t_400 - 1) do
            (t_399.arr).((j_427 * t_399.m) + j_428) <-
             ((((t_399.arr).((j_427 * t_399.m) + j_428) *
                 (t_399.arr).((t_404 * t_399.m) + t_405)) -
                ((t_399.arr).((t_404 * t_399.m) + j_428) *
                  (t_399.arr).((j_427 * t_399.m) + t_404))) / (! t_402))
           done;
           (t_399.arr).((j_427 * t_399.m) + t_405) <- 0
          end else ()
         done;
         (t_402 := i_426)
        end;
        (t_397 := ((! t_397) + 1))
     | None -> (t_403 := 0));
    (t_398 := ((! t_398) + 1))
   done;
   (t_399,
    if ((! t_403) = 0) then 0
    else if ((! t_403) = 1) then (! t_402)
    else (~- (! t_402)), (! t_397))>.
# val resFA11 :
  ('a,
   Funct4.GenFA11.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_429 ->
   let t_430 = (ref 0) in
   let t_431 = (ref 0) in
   let t_433 =
    (Array.map (fun x_432 -> (Array.copy x_432)) (Array.copy a_429)) in
   let t_434 = (Array.length a_429.(0)) in
   let t_435 = (Array.length a_429) in
   while (((! t_431) < t_434) && ((! t_430) < t_435)) do
    let t_436 = (! t_430) in
    let t_437 = (! t_431) in
    let t_438 = (ref (None)) in
    let t_447 =
     begin
      for j_443 = t_436 to (t_435 - 1) do
       for j_444 = t_437 to (t_434 - 1) do
        let t_445 = (t_433.(j_443)).(j_444) in
        if (not (t_445 = 0.)) then
         (match (! t_438) with
          | Some (i_446) ->
             if ((abs_float (snd i_446)) < (abs_float t_445)) then
              (t_438 := (Some ((j_443, j_444), t_445)))
             else ()
          | None -> (t_438 := (Some ((j_443, j_444), t_445))))
        else ()
       done
      done;
      (match (! t_438) with
       | Some (i_439) ->
          if ((snd (fst i_439)) <> t_437) then begin
           for r_441 = 0 to ((Array.length t_433) - 1) do
            let t_442 = (t_433.(r_441)).(t_437) in
            (t_433.(r_441)).(t_437) <- (t_433.(r_441)).(snd (fst i_439));
            (t_433.(r_441)).(snd (fst i_439)) <- t_442
           done;
           ()
          end else ();
          if ((fst (fst i_439)) <> t_436) then begin
           let t_440 = t_433.(t_436) in
           t_433.(t_436) <- t_433.(fst (fst i_439));
           t_433.(fst (fst i_439)) <- t_440;
           ()
          end else ();
          (Some (snd i_439))
       | None -> (None))
     end in
    (match t_447 with
     | Some (i_448) ->
        begin
         for j_449 = (t_436 + 1) to (t_435 - 1) do
          if (not ((t_433.(j_449)).(t_437) = 0.)) then begin
           for j_450 = (t_437 + 1) to (t_434 - 1) do
            (t_433.(j_449)).(j_450) <-
             ((t_433.(j_449)).(j_450) -.
               (((t_433.(j_449)).(t_437) /. (t_433.(t_436)).(t_437)) *.
                 (t_433.(t_436)).(j_450)))
           done;
           (t_433.(j_449)).(t_437) <- 0.
          end else ()
         done;
         ()
        end;
        (t_430 := ((! t_430) + 1))
     | None -> ());
    (t_431 := ((! t_431) + 1))
   done;
   t_433>.
# val resFA12 :
  ('a,
   Funct4.GenFA12.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_451 ->
   let t_452 = (ref 0) in
   let t_453 = (ref 0) in
   let t_455 =
    (Array.map (fun x_454 -> (Array.copy x_454)) (Array.copy a_451)) in
   let t_456 = (Array.length a_451.(0)) in
   let t_457 = (Array.length a_451) in
   let t_458 = (ref 1.) in
   let t_459 = (ref 1) in
   while (((! t_453) < t_456) && ((! t_452) < t_457)) do
    let t_460 = (! t_452) in
    let t_461 = (! t_453) in
    let t_462 = (ref (None)) in
    let t_471 =
     begin
      for j_467 = t_460 to (t_457 - 1) do
       for j_468 = t_461 to (t_456 - 1) do
        let t_469 = (t_455.(j_467)).(j_468) in
        if (not (t_469 = 0.)) then
         (match (! t_462) with
          | Some (i_470) ->
             if ((abs_float (snd i_470)) < (abs_float t_469)) then
              (t_462 := (Some ((j_467, j_468), t_469)))
             else ()
          | None -> (t_462 := (Some ((j_467, j_468), t_469))))
        else ()
       done
      done;
      (match (! t_462) with
       | Some (i_463) ->
          if ((snd (fst i_463)) <> t_461) then begin
           for r_465 = 0 to ((Array.length t_455) - 1) do
            let t_466 = (t_455.(r_465)).(t_461) in
            (t_455.(r_465)).(t_461) <- (t_455.(r_465)).(snd (fst i_463));
            (t_455.(r_465)).(snd (fst i_463)) <- t_466
           done;
           (t_459 := (~- (! t_459)))
          end else ();
          if ((fst (fst i_463)) <> t_460) then begin
           let t_464 = t_455.(t_460) in
           t_455.(t_460) <- t_455.(fst (fst i_463));
           t_455.(fst (fst i_463)) <- t_464;
           (t_459 := (~- (! t_459)))
          end else ();
          (Some (snd i_463))
       | None -> (None))
     end in
    (match t_471 with
     | Some (i_472) ->
        begin
         for j_473 = (t_460 + 1) to (t_457 - 1) do
          if (not ((t_455.(j_473)).(t_461) = 0.)) then begin
           for j_474 = (t_461 + 1) to (t_456 - 1) do
            (t_455.(j_473)).(j_474) <-
             ((t_455.(j_473)).(j_474) -.
               (((t_455.(j_473)).(t_461) /. (t_455.(t_460)).(t_461)) *.
                 (t_455.(t_460)).(j_474)))
           done;
           (t_455.(j_473)).(t_461) <- 0.
          end else ()
         done;
         (t_458 := ((! t_458) *. i_472))
        end;
        (t_452 := ((! t_452) + 1))
     | None -> (t_459 := 0));
    (t_453 := ((! t_453) + 1))
   done;
   (t_455,
    if ((! t_459) = 0) then 0.
    else if ((! t_459) = 1) then (! t_458)
    else (~-. (! t_458)))>.
# val resFA13 :
  ('a,
   Funct4.GenFA13.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_475 ->
   let t_476 = (ref 0) in
   let t_477 = (ref 0) in
   let t_479 =
    (Array.map (fun x_478 -> (Array.copy x_478)) (Array.copy a_475)) in
   let t_480 = (Array.length a_475.(0)) in
   let t_481 = (Array.length a_475) in
   while (((! t_477) < t_480) && ((! t_476) < t_481)) do
    let t_482 = (! t_476) in
    let t_483 = (! t_477) in
    let t_484 = (ref (None)) in
    let t_493 =
     begin
      for j_489 = t_482 to (t_481 - 1) do
       for j_490 = t_483 to (t_480 - 1) do
        let t_491 = (t_479.(j_489)).(j_490) in
        if (not (t_491 = 0.)) then
         (match (! t_484) with
          | Some (i_492) ->
             if ((abs_float (snd i_492)) < (abs_float t_491)) then
              (t_484 := (Some ((j_489, j_490), t_491)))
             else ()
          | None -> (t_484 := (Some ((j_489, j_490), t_491))))
        else ()
       done
      done;
      (match (! t_484) with
       | Some (i_485) ->
          if ((snd (fst i_485)) <> t_483) then begin
           for r_487 = 0 to ((Array.length t_479) - 1) do
            let t_488 = (t_479.(r_487)).(t_483) in
            (t_479.(r_487)).(t_483) <- (t_479.(r_487)).(snd (fst i_485));
            (t_479.(r_487)).(snd (fst i_485)) <- t_488
           done;
           ()
          end else ();
          if ((fst (fst i_485)) <> t_482) then begin
           let t_486 = t_479.(t_482) in
           t_479.(t_482) <- t_479.(fst (fst i_485));
           t_479.(fst (fst i_485)) <- t_486;
           ()
          end else ();
          (Some (snd i_485))
       | None -> (None))
     end in
    (match t_493 with
     | Some (i_494) ->
        begin
         for j_495 = (t_482 + 1) to (t_481 - 1) do
          if (not ((t_479.(j_495)).(t_483) = 0.)) then begin
           for j_496 = (t_483 + 1) to (t_480 - 1) do
            (t_479.(j_495)).(j_496) <-
             ((t_479.(j_495)).(j_496) -.
               (((t_479.(j_495)).(t_483) /. (t_479.(t_482)).(t_483)) *.
                 (t_479.(t_482)).(j_496)))
           done;
           (t_479.(j_495)).(t_483) <- 0.
          end else ()
         done;
         ()
        end;
        (t_476 := ((! t_476) + 1))
     | None -> ());
    (t_477 := ((! t_477) + 1))
   done;
   (t_479, (! t_476))>.
# val resFA14 :
  ('a,
   Funct4.GenFA14.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_497 ->
   let t_498 = (ref 0) in
   let t_499 = (ref 0) in
   let t_501 =
    (Array.map (fun x_500 -> (Array.copy x_500)) (Array.copy a_497)) in
   let t_502 = (Array.length a_497.(0)) in
   let t_503 = (Array.length a_497) in
   let t_504 = (ref 1.) in
   let t_505 = (ref 1) in
   while (((! t_499) < t_502) && ((! t_498) < t_503)) do
    let t_506 = (! t_498) in
    let t_507 = (! t_499) in
    let t_508 = (ref (None)) in
    let t_517 =
     begin
      for j_513 = t_506 to (t_503 - 1) do
       for j_514 = t_507 to (t_502 - 1) do
        let t_515 = (t_501.(j_513)).(j_514) in
        if (not (t_515 = 0.)) then
         (match (! t_508) with
          | Some (i_516) ->
             if ((abs_float (snd i_516)) < (abs_float t_515)) then
              (t_508 := (Some ((j_513, j_514), t_515)))
             else ()
          | None -> (t_508 := (Some ((j_513, j_514), t_515))))
        else ()
       done
      done;
      (match (! t_508) with
       | Some (i_509) ->
          if ((snd (fst i_509)) <> t_507) then begin
           for r_511 = 0 to ((Array.length t_501) - 1) do
            let t_512 = (t_501.(r_511)).(t_507) in
            (t_501.(r_511)).(t_507) <- (t_501.(r_511)).(snd (fst i_509));
            (t_501.(r_511)).(snd (fst i_509)) <- t_512
           done;
           (t_505 := (~- (! t_505)))
          end else ();
          if ((fst (fst i_509)) <> t_506) then begin
           let t_510 = t_501.(t_506) in
           t_501.(t_506) <- t_501.(fst (fst i_509));
           t_501.(fst (fst i_509)) <- t_510;
           (t_505 := (~- (! t_505)))
          end else ();
          (Some (snd i_509))
       | None -> (None))
     end in
    (match t_517 with
     | Some (i_518) ->
        begin
         for j_519 = (t_506 + 1) to (t_503 - 1) do
          if (not ((t_501.(j_519)).(t_507) = 0.)) then begin
           for j_520 = (t_507 + 1) to (t_502 - 1) do
            (t_501.(j_519)).(j_520) <-
             ((t_501.(j_519)).(j_520) -.
               (((t_501.(j_519)).(t_507) /. (t_501.(t_506)).(t_507)) *.
                 (t_501.(t_506)).(j_520)))
           done;
           (t_501.(j_519)).(t_507) <- 0.
          end else ()
         done;
         (t_504 := ((! t_504) *. i_518))
        end;
        (t_498 := ((! t_498) + 1))
     | None -> (t_505 := 0));
    (t_499 := ((! t_499) + 1))
   done;
   (t_501,
    if ((! t_505) = 0) then 0.
    else if ((! t_505) = 1) then (! t_504)
    else (~-. (! t_504)), (! t_498))>.
# val resRA1 :
  ('a,
   Funct4.GenRA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res)
  code =
  .<fun a_543 ->
   let t_544 = (ref 0) in
   let t_545 = (ref 0) in
   let t_547 =
    (Array.map (fun x_546 -> (Array.copy x_546)) (Array.copy a_543)) in
   let t_548 = (Array.length a_543.(0)) in
   let t_549 = (Array.length a_543) in
   let t_550 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_551 = (ref 1) in
   while (((! t_545) < t_548) && ((! t_544) < t_549)) do
    let t_552 = (! t_544) in
    let t_553 = (! t_545) in
    let t_554 = (ref (None)) in
    let t_561 =
     begin
      let t_557 = (t_547.(t_552)).(t_553) in
      if (not (t_557 = (* cross-stage persistent value (as id: zero) *))) then
       (t_554 := (Some (t_552, t_557)))
      else
       let rec loop_558 =
        fun j_559 ->
         if (j_559 < t_549) then
          let bjc_560 = (t_547.(j_559)).(t_553) in
          if (bjc_560 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_558 (j_559 + 1))
          else (t_554 := (Some (j_559, bjc_560)))
         else () in
       (loop_558 (t_552 + 1));
      (match (! t_554) with
       | Some (i_555) ->
          if ((fst i_555) <> t_552) then begin
           let t_556 = t_547.(t_552) in
           t_547.(t_552) <- t_547.(fst i_555);
           t_547.(fst i_555) <- t_556;
           (t_551 := (~- (! t_551)))
          end else ();
          (Some (snd i_555))
       | None -> (None))
     end in
    (match t_561 with
     | Some (i_562) ->
        begin
         for j_563 = (t_552 + 1) to (t_549 - 1) do
          if (not
               ((t_547.(j_563)).(t_553) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_564 = (t_553 + 1) to (t_548 - 1) do
            (t_547.(j_563)).(j_564) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_547.(j_563)).(j_564)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_547.(j_563)).(t_553) (t_547.(t_552)).(t_553))
                 (t_547.(t_552)).(j_564)))
           done;
           (t_547.(j_563)).(t_553) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_550 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_550) i_562))
        end;
        (t_544 := ((! t_544) + 1))
     | None -> (t_551 := 0));
    (t_545 := ((! t_545) + 1))
   done;
   t_547>.
# val resRA2 :
  ('a,
   Funct4.GenRA2.Ctr.contr ->
   Funct4.OutDet(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet).res)
  code =
  .<fun a_565 ->
   let t_566 = (ref 0) in
   let t_567 = (ref 0) in
   let t_569 =
    (Array.map (fun x_568 -> (Array.copy x_568)) (Array.copy a_565)) in
   let t_570 = (Array.length a_565.(0)) in
   let t_571 = (Array.length a_565) in
   let t_572 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_573 = (ref 1) in
   while (((! t_567) < t_570) && ((! t_566) < t_571)) do
    let t_574 = (! t_566) in
    let t_575 = (! t_567) in
    let t_576 = (ref (None)) in
    let t_583 =
     begin
      let t_579 = (t_569.(t_574)).(t_575) in
      if (not (t_579 = (* cross-stage persistent value (as id: zero) *))) then
       (t_576 := (Some (t_574, t_579)))
      else
       let rec loop_580 =
        fun j_581 ->
         if (j_581 < t_571) then
          let bjc_582 = (t_569.(j_581)).(t_575) in
          if (bjc_582 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_580 (j_581 + 1))
          else (t_576 := (Some (j_581, bjc_582)))
         else () in
       (loop_580 (t_574 + 1));
      (match (! t_576) with
       | Some (i_577) ->
          if ((fst i_577) <> t_574) then begin
           let t_578 = t_569.(t_574) in
           t_569.(t_574) <- t_569.(fst i_577);
           t_569.(fst i_577) <- t_578;
           (t_573 := (~- (! t_573)))
          end else ();
          (Some (snd i_577))
       | None -> (None))
     end in
    (match t_583 with
     | Some (i_584) ->
        begin
         for j_585 = (t_574 + 1) to (t_571 - 1) do
          if (not
               ((t_569.(j_585)).(t_575) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_586 = (t_575 + 1) to (t_570 - 1) do
            (t_569.(j_585)).(j_586) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_569.(j_585)).(j_586)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_569.(j_585)).(t_575) (t_569.(t_574)).(t_575))
                 (t_569.(t_574)).(j_586)))
           done;
           (t_569.(j_585)).(t_575) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_572 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_572) i_584))
        end;
        (t_566 := ((! t_566) + 1))
     | None -> (t_573 := 0));
    (t_567 := ((! t_567) + 1))
   done;
   (t_569,
    if ((! t_573) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_573) = 1) then (! t_572)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_572)))>.
# val resRA3 :
  ('a,
   Funct4.GenRA3.Ctr.contr ->
   Funct4.OutRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_587 ->
   let t_588 = (ref 0) in
   let t_589 = (ref 0) in
   let t_591 =
    (Array.map (fun x_590 -> (Array.copy x_590)) (Array.copy a_587)) in
   let t_592 = (Array.length a_587.(0)) in
   let t_593 = (Array.length a_587) in
   let t_594 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_595 = (ref 1) in
   while (((! t_589) < t_592) && ((! t_588) < t_593)) do
    let t_596 = (! t_588) in
    let t_597 = (! t_589) in
    let t_598 = (ref (None)) in
    let t_605 =
     begin
      let t_601 = (t_591.(t_596)).(t_597) in
      if (not (t_601 = (* cross-stage persistent value (as id: zero) *))) then
       (t_598 := (Some (t_596, t_601)))
      else
       let rec loop_602 =
        fun j_603 ->
         if (j_603 < t_593) then
          let bjc_604 = (t_591.(j_603)).(t_597) in
          if (bjc_604 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_602 (j_603 + 1))
          else (t_598 := (Some (j_603, bjc_604)))
         else () in
       (loop_602 (t_596 + 1));
      (match (! t_598) with
       | Some (i_599) ->
          if ((fst i_599) <> t_596) then begin
           let t_600 = t_591.(t_596) in
           t_591.(t_596) <- t_591.(fst i_599);
           t_591.(fst i_599) <- t_600;
           ()
          end else ();
          (Some (snd i_599))
       | None -> (None))
     end in
    (match t_605 with
     | Some (i_606) ->
        begin
         for j_607 = (t_596 + 1) to (t_593 - 1) do
          if (not
               ((t_591.(j_607)).(t_597) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_608 = (t_597 + 1) to (t_592 - 1) do
            (t_591.(j_607)).(j_608) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_591.(j_607)).(j_608)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_591.(j_607)).(t_597) (t_591.(t_596)).(t_597))
                 (t_591.(t_596)).(j_608)))
           done;
           (t_591.(j_607)).(t_597) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_594 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_594) i_606))
        end;
        (t_588 := ((! t_588) + 1))
     | None -> (t_595 := 0));
    (t_589 := ((! t_589) + 1))
   done;
   (t_591, (! t_588))>.
# val resRA4 :
  ('a,
   Funct4.GenRA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.RationalDomain)(Funct4.GenericArrayContainer)(Funct4.RDet)(Funct4.Rank).res)
  code =
  .<fun a_609 ->
   let t_610 = (ref 0) in
   let t_611 = (ref 0) in
   let t_613 =
    (Array.map (fun x_612 -> (Array.copy x_612)) (Array.copy a_609)) in
   let t_614 = (Array.length a_609.(0)) in
   let t_615 = (Array.length a_609) in
   let t_616 = (ref (* cross-stage persistent value (as id: one) *)) in
   let t_617 = (ref 1) in
   while (((! t_611) < t_614) && ((! t_610) < t_615)) do
    let t_618 = (! t_610) in
    let t_619 = (! t_611) in
    let t_620 = (ref (None)) in
    let t_627 =
     begin
      let t_623 = (t_613.(t_618)).(t_619) in
      if (not (t_623 = (* cross-stage persistent value (as id: zero) *))) then
       (t_620 := (Some (t_618, t_623)))
      else
       let rec loop_624 =
        fun j_625 ->
         if (j_625 < t_615) then
          let bjc_626 = (t_613.(j_625)).(t_619) in
          if (bjc_626 = (* cross-stage persistent value (as id: zero) *)) then
           (loop_624 (j_625 + 1))
          else (t_620 := (Some (j_625, bjc_626)))
         else () in
       (loop_624 (t_618 + 1));
      (match (! t_620) with
       | Some (i_621) ->
          if ((fst i_621) <> t_618) then begin
           let t_622 = t_613.(t_618) in
           t_613.(t_618) <- t_613.(fst i_621);
           t_613.(fst i_621) <- t_622;
           (t_617 := (~- (! t_617)))
          end else ();
          (Some (snd i_621))
       | None -> (None))
     end in
    (match t_627 with
     | Some (i_628) ->
        begin
         for j_629 = (t_618 + 1) to (t_615 - 1) do
          if (not
               ((t_613.(j_629)).(t_619) =
                 (* cross-stage persistent value (as id: zero) *))) then begin
           for j_630 = (t_619 + 1) to (t_614 - 1) do
            (t_613.(j_629)).(j_630) <-
             (((* cross-stage persistent value (as id: Num.sub_num) *))
               (t_613.(j_629)).(j_630)
               (((* cross-stage persistent value (as id: Num.mult_num) *))
                 (((* cross-stage persistent value (as id: Num.div_num) *))
                   (t_613.(j_629)).(t_619) (t_613.(t_618)).(t_619))
                 (t_613.(t_618)).(j_630)))
           done;
           (t_613.(j_629)).(t_619) <-
            (* cross-stage persistent value (as id: zero) *)
          end else ()
         done;
         (t_616 :=
           (((* cross-stage persistent value (as id: Num.mult_num) *))
             (! t_616) i_628))
        end;
        (t_610 := ((! t_610) + 1))
     | None -> (t_617 := 0));
    (t_611 := ((! t_611) + 1))
   done;
   (t_613,
    if ((! t_617) = 0) then (* cross-stage persistent value (as id: zero) *)
    else if ((! t_617) = 1) then (! t_616)
    else
     (((* cross-stage persistent value (as id: Num.minus_num) *)) (! t_616)),
    (! t_610))>.
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
