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
# val resFA11 :
  ('a,
   Funct4.GenFA11.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res)
  code =
  .<fun a_20 ->
   let t_21 = (ref 0) in
   let t_22 = (ref 0) in
   let t_24 = (Array.map (fun x_23 -> (Array.copy x_23)) (Array.copy a_20)) in
   let t_25 = (Array.length a_20.(0)) in
   let t_26 = (Array.length a_20) in
   while (((! t_22) < t_25) && ((! t_21) < t_26)) do
    let t_27 = (! t_21) in
    let t_28 = (! t_22) in
    let t_29 = (ref (None)) in
    let t_36 =
     begin
      for j_32 = t_27 to (t_26 - 1) do
       for j_33 = t_28 to (t_25 - 1) do
        let t_34 = (t_24.(j_32)).(j_33) in
        if (not (t_34 = 0.)) then
         (match (! t_29) with
          | Some (i_35) ->
             if ((abs_float (snd i_35)) < (abs_float t_34)) then
              (t_29 := (Some ((j_32, j_33), t_34)))
             else ()
          | None -> (t_29 := (Some ((j_32, j_33), t_34))))
        else ()
       done
      done;
      (match (! t_29) with
       | Some (i_30) ->
          if ((snd (fst i_30)) <> t_28) then begin
           (failwith "swap_cols_stmt not yet implemeted"); ()
          end else ();
          if ((snd (fst i_30)) <> t_28) then begin
           let t_31 = t_24.(t_27) in
           t_24.(t_27) <- t_24.(fst (fst i_30));
           t_24.(fst (fst i_30)) <- t_31;
           ()
          end else ();
          (Some (snd i_30))
       | None -> (None))
     end in
    (match t_36 with
     | Some (i_37) ->
        begin
         for j_38 = (t_27 + 1) to (t_26 - 1) do
          if (not ((t_24.(j_38)).(t_28) = 0.)) then begin
           for j_39 = (t_28 + 1) to (t_25 - 1) do
            (t_24.(j_38)).(j_39) <-
             ((t_24.(j_38)).(j_39) -.
               (((t_24.(j_38)).(t_28) /. (t_24.(t_27)).(t_28)) *.
                 (t_24.(t_27)).(j_39)))
           done;
           (t_24.(j_38)).(t_28) <- 0.
          end else ()
         done;
         ()
        end;
        (t_21 := ((! t_21) + 1))
     | None -> ());
    (t_22 := ((! t_22) + 1))
   done;
   t_24>.
# val resFA2 :
  ('a,
   Funct4.GenFA2.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet).res)
  code =
  .<fun a_40 ->
   let t_41 = (ref 0) in
   let t_42 = (ref 0) in
   let t_44 = (Array.map (fun x_43 -> (Array.copy x_43)) (Array.copy a_40)) in
   let t_45 = (Array.length a_40.(0)) in
   let t_46 = (Array.length a_40) in
   let t_47 = (ref 1.) in
   let t_48 = (ref 1) in
   while (((! t_42) < t_45) && ((! t_41) < t_46)) do
    let t_49 = (! t_41) in
    let t_50 = (! t_42) in
    let t_51 = (ref (None)) in
    let t_57 =
     begin
      for j_54 = t_49 to (t_46 - 1) do
       let t_55 = (t_44.(j_54)).(t_50) in
       if (not (t_55 = 0.)) then
        (match (! t_51) with
         | Some (i_56) ->
            if ((abs_float (snd i_56)) < (abs_float t_55)) then
             (t_51 := (Some (j_54, t_55)))
            else ()
         | None -> (t_51 := (Some (j_54, t_55))))
       else ()
      done;
      (match (! t_51) with
       | Some (i_52) ->
          if ((fst i_52) <> t_49) then begin
           let t_53 = t_44.(t_49) in
           t_44.(t_49) <- t_44.(fst i_52);
           t_44.(fst i_52) <- t_53;
           (t_48 := (~- (! t_48)))
          end else ();
          (Some (snd i_52))
       | None -> (None))
     end in
    (match t_57 with
     | Some (i_58) ->
        begin
         for j_59 = (t_49 + 1) to (t_46 - 1) do
          if (not ((t_44.(j_59)).(t_50) = 0.)) then begin
           for j_60 = (t_50 + 1) to (t_45 - 1) do
            (t_44.(j_59)).(j_60) <-
             ((t_44.(j_59)).(j_60) -.
               (((t_44.(j_59)).(t_50) /. (t_44.(t_49)).(t_50)) *.
                 (t_44.(t_49)).(j_60)))
           done;
           (t_44.(j_59)).(t_50) <- 0.
          end else ()
         done;
         (t_47 := ((! t_47) *. i_58))
        end;
        (t_41 := ((! t_41) + 1))
     | None -> (t_48 := 0));
    (t_42 := ((! t_42) + 1))
   done;
   (t_44,
    if ((! t_48) = 0) then 0.
    else if ((! t_48) = 1) then (! t_47)
    else (~-. (! t_47)))>.
# val resFA3 :
  ('a,
   Funct4.GenFA3.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_61 ->
   let t_62 = (ref 0) in
   let t_63 = (ref 0) in
   let t_65 = (Array.map (fun x_64 -> (Array.copy x_64)) (Array.copy a_61)) in
   let t_66 = (Array.length a_61.(0)) in
   let t_67 = (Array.length a_61) in
   while (((! t_63) < t_66) && ((! t_62) < t_67)) do
    let t_68 = (! t_62) in
    let t_69 = (! t_63) in
    let t_70 = (ref (None)) in
    let t_76 =
     begin
      for j_73 = t_68 to (t_67 - 1) do
       let t_74 = (t_65.(j_73)).(t_69) in
       if (not (t_74 = 0.)) then
        (match (! t_70) with
         | Some (i_75) ->
            if ((abs_float (snd i_75)) < (abs_float t_74)) then
             (t_70 := (Some (j_73, t_74)))
            else ()
         | None -> (t_70 := (Some (j_73, t_74))))
       else ()
      done;
      (match (! t_70) with
       | Some (i_71) ->
          if ((fst i_71) <> t_68) then begin
           let t_72 = t_65.(t_68) in
           t_65.(t_68) <- t_65.(fst i_71);
           t_65.(fst i_71) <- t_72;
           ()
          end else ();
          (Some (snd i_71))
       | None -> (None))
     end in
    (match t_76 with
     | Some (i_77) ->
        begin
         for j_78 = (t_68 + 1) to (t_67 - 1) do
          if (not ((t_65.(j_78)).(t_69) = 0.)) then begin
           for j_79 = (t_69 + 1) to (t_66 - 1) do
            (t_65.(j_78)).(j_79) <-
             ((t_65.(j_78)).(j_79) -.
               (((t_65.(j_78)).(t_69) /. (t_65.(t_68)).(t_69)) *.
                 (t_65.(t_68)).(j_79)))
           done;
           (t_65.(j_78)).(t_69) <- 0.
          end else ()
         done;
         ()
        end;
        (t_62 := ((! t_62) + 1))
     | None -> ());
    (t_63 := ((! t_63) + 1))
   done;
   (t_65, (! t_62))>.
# val resFA4 :
  ('a,
   Funct4.GenFA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_80 ->
   let t_81 = (ref 0) in
   let t_82 = (ref 0) in
   let t_84 = (Array.map (fun x_83 -> (Array.copy x_83)) (Array.copy a_80)) in
   let t_85 = (Array.length a_80.(0)) in
   let t_86 = (Array.length a_80) in
   let t_87 = (ref 1.) in
   let t_88 = (ref 1) in
   while (((! t_82) < t_85) && ((! t_81) < t_86)) do
    let t_89 = (! t_81) in
    let t_90 = (! t_82) in
    let t_91 = (ref (None)) in
    let t_97 =
     begin
      for j_94 = t_89 to (t_86 - 1) do
       let t_95 = (t_84.(j_94)).(t_90) in
       if (not (t_95 = 0.)) then
        (match (! t_91) with
         | Some (i_96) ->
            if ((abs_float (snd i_96)) < (abs_float t_95)) then
             (t_91 := (Some (j_94, t_95)))
            else ()
         | None -> (t_91 := (Some (j_94, t_95))))
       else ()
      done;
      (match (! t_91) with
       | Some (i_92) ->
          if ((fst i_92) <> t_89) then begin
           let t_93 = t_84.(t_89) in
           t_84.(t_89) <- t_84.(fst i_92);
           t_84.(fst i_92) <- t_93;
           (t_88 := (~- (! t_88)))
          end else ();
          (Some (snd i_92))
       | None -> (None))
     end in
    (match t_97 with
     | Some (i_98) ->
        begin
         for j_99 = (t_89 + 1) to (t_86 - 1) do
          if (not ((t_84.(j_99)).(t_90) = 0.)) then begin
           for j_100 = (t_90 + 1) to (t_85 - 1) do
            (t_84.(j_99)).(j_100) <-
             ((t_84.(j_99)).(j_100) -.
               (((t_84.(j_99)).(t_90) /. (t_84.(t_89)).(t_90)) *.
                 (t_84.(t_89)).(j_100)))
           done;
           (t_84.(j_99)).(t_90) <- 0.
          end else ()
         done;
         (t_87 := ((! t_87) *. i_98))
        end;
        (t_81 := ((! t_81) + 1))
     | None -> (t_88 := 0));
    (t_82 := ((! t_82) + 1))
   done;
   (t_84,
    if ((! t_88) = 0) then 0.
    else if ((! t_88) = 1) then (! t_87)
    else (~-. (! t_87)), (! t_81))>.
# val resFV1 :
  ('a,
   Funct4.GenFV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_101 ->
   let t_102 = (ref 0) in
   let t_103 = (ref 0) in
   let t_104 = {arr = (Array.copy a_101.arr)} (a_101) in
   let t_105 = a_101.m in
   let t_106 = a_101.n in
   let t_107 = (ref 1.) in
   let t_108 = (ref 1) in
   while (((! t_103) < t_105) && ((! t_102) < t_106)) do
    let t_109 = (! t_102) in
    let t_110 = (! t_103) in
    let t_111 = (ref (None)) in
    let t_123 =
     begin
      for j_120 = t_109 to (t_106 - 1) do
       let t_121 = (t_104.arr).((j_120 * t_104.n) + t_110) in
       if (not (t_121 = 0.)) then
        (match (! t_111) with
         | Some (i_122) ->
            if ((abs_float (snd i_122)) < (abs_float t_121)) then
             (t_111 := (Some (j_120, t_121)))
            else ()
         | None -> (t_111 := (Some (j_120, t_121))))
       else ()
      done;
      (match (! t_111) with
       | Some (i_112) ->
          if ((fst i_112) <> t_109) then begin
           let a_113 = t_104.arr
           and n_114 = t_104.n
           and m_115 = t_104.m in
           let i1_116 = (t_109 * n_114)
           and i2_117 = ((fst i_112) * n_114) in
           for i_118 = 0 to (m_115 - 1) do
            let t_119 = a_113.(i1_116 + i_118) in
            a_113.(i2_117 + i_118) <- a_113.(i1_116 + i_118);
            a_113.(i1_116 + i_118) <- t_119
           done;
           (t_108 := (~- (! t_108)))
          end else ();
          (Some (snd i_112))
       | None -> (None))
     end in
    (match t_123 with
     | Some (i_124) ->
        begin
         for j_125 = (t_109 + 1) to (t_106 - 1) do
          if (not ((t_104.arr).((j_125 * t_104.n) + t_110) = 0.)) then begin
           for j_126 = (t_110 + 1) to (t_105 - 1) do
            (t_104.arr).((j_125 * t_104.n) + j_126) <-
             ((t_104.arr).((j_125 * t_104.n) + j_126) -.
               (((t_104.arr).((j_125 * t_104.n) + t_110) /.
                  (t_104.arr).((t_109 * t_104.n) + t_110)) *.
                 (t_104.arr).((t_109 * t_104.n) + j_126)))
           done;
           (t_104.arr).((j_125 * t_104.n) + t_110) <- 0.
          end else ()
         done;
         (t_107 := ((! t_107) *. i_124))
        end;
        (t_102 := ((! t_102) + 1))
     | None -> (t_108 := 0));
    (t_103 := ((! t_103) + 1))
   done;
   t_104>.
# val resFV2 :
  ('a,
   Funct4.GenFV2.Ctr.contr ->
   Funct4.OutDet(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_127 ->
   let t_128 = (ref 0) in
   let t_129 = (ref 0) in
   let t_130 = {arr = (Array.copy a_127.arr)} (a_127) in
   let t_131 = a_127.m in
   let t_132 = a_127.n in
   let t_133 = (ref 1.) in
   let t_134 = (ref 1) in
   while (((! t_129) < t_131) && ((! t_128) < t_132)) do
    let t_135 = (! t_128) in
    let t_136 = (! t_129) in
    let t_137 = (ref (None)) in
    let t_149 =
     begin
      for j_146 = t_135 to (t_132 - 1) do
       let t_147 = (t_130.arr).((j_146 * t_130.n) + t_136) in
       if (not (t_147 = 0.)) then
        (match (! t_137) with
         | Some (i_148) ->
            if ((abs_float (snd i_148)) < (abs_float t_147)) then
             (t_137 := (Some (j_146, t_147)))
            else ()
         | None -> (t_137 := (Some (j_146, t_147))))
       else ()
      done;
      (match (! t_137) with
       | Some (i_138) ->
          if ((fst i_138) <> t_135) then begin
           let a_139 = t_130.arr
           and n_140 = t_130.n
           and m_141 = t_130.m in
           let i1_142 = (t_135 * n_140)
           and i2_143 = ((fst i_138) * n_140) in
           for i_144 = 0 to (m_141 - 1) do
            let t_145 = a_139.(i1_142 + i_144) in
            a_139.(i2_143 + i_144) <- a_139.(i1_142 + i_144);
            a_139.(i1_142 + i_144) <- t_145
           done;
           (t_134 := (~- (! t_134)))
          end else ();
          (Some (snd i_138))
       | None -> (None))
     end in
    (match t_149 with
     | Some (i_150) ->
        begin
         for j_151 = (t_135 + 1) to (t_132 - 1) do
          if (not ((t_130.arr).((j_151 * t_130.n) + t_136) = 0.)) then begin
           for j_152 = (t_136 + 1) to (t_131 - 1) do
            (t_130.arr).((j_151 * t_130.n) + j_152) <-
             ((t_130.arr).((j_151 * t_130.n) + j_152) -.
               (((t_130.arr).((j_151 * t_130.n) + t_136) /.
                  (t_130.arr).((t_135 * t_130.n) + t_136)) *.
                 (t_130.arr).((t_135 * t_130.n) + j_152)))
           done;
           (t_130.arr).((j_151 * t_130.n) + t_136) <- 0.
          end else ()
         done;
         (t_133 := ((! t_133) *. i_150))
        end;
        (t_128 := ((! t_128) + 1))
     | None -> (t_134 := 0));
    (t_129 := ((! t_129) + 1))
   done;
   (t_130,
    if ((! t_134) = 0) then 0.
    else if ((! t_134) = 1) then (! t_133)
    else (~-. (! t_133)))>.
# val resFV3 :
  ('a,
   Funct4.GenFV3.Ctr.contr ->
   Funct4.OutRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_153 ->
   let t_154 = (ref 0) in
   let t_155 = (ref 0) in
   let t_156 = {arr = (Array.copy a_153.arr)} (a_153) in
   let t_157 = a_153.m in
   let t_158 = a_153.n in
   while (((! t_155) < t_157) && ((! t_154) < t_158)) do
    let t_159 = (! t_154) in
    let t_160 = (! t_155) in
    let t_161 = (ref (None)) in
    let t_173 =
     begin
      for j_170 = t_159 to (t_158 - 1) do
       let t_171 = (t_156.arr).((j_170 * t_156.n) + t_160) in
       if (not (t_171 = 0.)) then
        (match (! t_161) with
         | Some (i_172) ->
            if ((abs_float (snd i_172)) < (abs_float t_171)) then
             (t_161 := (Some (j_170, t_171)))
            else ()
         | None -> (t_161 := (Some (j_170, t_171))))
       else ()
      done;
      (match (! t_161) with
       | Some (i_162) ->
          if ((fst i_162) <> t_159) then begin
           let a_163 = t_156.arr
           and n_164 = t_156.n
           and m_165 = t_156.m in
           let i1_166 = (t_159 * n_164)
           and i2_167 = ((fst i_162) * n_164) in
           for i_168 = 0 to (m_165 - 1) do
            let t_169 = a_163.(i1_166 + i_168) in
            a_163.(i2_167 + i_168) <- a_163.(i1_166 + i_168);
            a_163.(i1_166 + i_168) <- t_169
           done;
           ()
          end else ();
          (Some (snd i_162))
       | None -> (None))
     end in
    (match t_173 with
     | Some (i_174) ->
        begin
         for j_175 = (t_159 + 1) to (t_158 - 1) do
          if (not ((t_156.arr).((j_175 * t_156.n) + t_160) = 0.)) then begin
           for j_176 = (t_160 + 1) to (t_157 - 1) do
            (t_156.arr).((j_175 * t_156.n) + j_176) <-
             ((t_156.arr).((j_175 * t_156.n) + j_176) -.
               (((t_156.arr).((j_175 * t_156.n) + t_160) /.
                  (t_156.arr).((t_159 * t_156.n) + t_160)) *.
                 (t_156.arr).((t_159 * t_156.n) + j_176)))
           done;
           (t_156.arr).((j_175 * t_156.n) + t_160) <- 0.
          end else ()
         done;
         ()
        end;
        (t_154 := ((! t_154) + 1))
     | None -> ());
    (t_155 := ((! t_155) + 1))
   done;
   (t_156, (! t_154))>.
# val resFV4 :
  ('a,
   Funct4.GenFV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.FloatDomain)(Funct4.GenericVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_177 ->
   let t_178 = (ref 0) in
   let t_179 = (ref 0) in
   let t_180 = {arr = (Array.copy a_177.arr)} (a_177) in
   let t_181 = a_177.m in
   let t_182 = a_177.n in
   let t_183 = (ref 1.) in
   let t_184 = (ref 1) in
   while (((! t_179) < t_181) && ((! t_178) < t_182)) do
    let t_185 = (! t_178) in
    let t_186 = (! t_179) in
    let t_187 = (ref (None)) in
    let t_199 =
     begin
      for j_196 = t_185 to (t_182 - 1) do
       let t_197 = (t_180.arr).((j_196 * t_180.n) + t_186) in
       if (not (t_197 = 0.)) then
        (match (! t_187) with
         | Some (i_198) ->
            if ((abs_float (snd i_198)) < (abs_float t_197)) then
             (t_187 := (Some (j_196, t_197)))
            else ()
         | None -> (t_187 := (Some (j_196, t_197))))
       else ()
      done;
      (match (! t_187) with
       | Some (i_188) ->
          if ((fst i_188) <> t_185) then begin
           let a_189 = t_180.arr
           and n_190 = t_180.n
           and m_191 = t_180.m in
           let i1_192 = (t_185 * n_190)
           and i2_193 = ((fst i_188) * n_190) in
           for i_194 = 0 to (m_191 - 1) do
            let t_195 = a_189.(i1_192 + i_194) in
            a_189.(i2_193 + i_194) <- a_189.(i1_192 + i_194);
            a_189.(i1_192 + i_194) <- t_195
           done;
           (t_184 := (~- (! t_184)))
          end else ();
          (Some (snd i_188))
       | None -> (None))
     end in
    (match t_199 with
     | Some (i_200) ->
        begin
         for j_201 = (t_185 + 1) to (t_182 - 1) do
          if (not ((t_180.arr).((j_201 * t_180.n) + t_186) = 0.)) then begin
           for j_202 = (t_186 + 1) to (t_181 - 1) do
            (t_180.arr).((j_201 * t_180.n) + j_202) <-
             ((t_180.arr).((j_201 * t_180.n) + j_202) -.
               (((t_180.arr).((j_201 * t_180.n) + t_186) /.
                  (t_180.arr).((t_185 * t_180.n) + t_186)) *.
                 (t_180.arr).((t_185 * t_180.n) + j_202)))
           done;
           (t_180.arr).((j_201 * t_180.n) + t_186) <- 0.
          end else ()
         done;
         (t_183 := ((! t_183) *. i_200))
        end;
        (t_178 := ((! t_178) + 1))
     | None -> (t_184 := 0));
    (t_179 := ((! t_179) + 1))
   done;
   (t_180,
    if ((! t_184) = 0) then 0.
    else if ((! t_184) = 1) then (! t_183)
    else (~-. (! t_183)), (! t_178))>.
# val resIA1 :
  ('a,
   Funct4.GenIA1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_203 ->
   let t_204 = (ref 0) in
   let t_205 = (ref 0) in
   let t_207 =
    (Array.map (fun x_206 -> (Array.copy x_206)) (Array.copy a_203)) in
   let t_208 = (Array.length a_203.(0)) in
   let t_209 = (Array.length a_203) in
   let t_210 = (ref 1) in
   let t_211 = (ref 1) in
   while (((! t_205) < t_208) && ((! t_204) < t_209)) do
    let t_212 = (! t_204) in
    let t_213 = (! t_205) in
    let t_214 = (ref (None)) in
    let t_220 =
     begin
      for j_217 = t_212 to (t_209 - 1) do
       let t_218 = (t_207.(j_217)).(t_213) in
       if (not (t_218 = 0)) then
        (match (! t_214) with
         | Some (i_219) ->
            if ((abs (snd i_219)) > (abs t_218)) then
             (t_214 := (Some (j_217, t_218)))
            else ()
         | None -> (t_214 := (Some (j_217, t_218))))
       else ()
      done;
      (match (! t_214) with
       | Some (i_215) ->
          if ((fst i_215) <> t_212) then begin
           let t_216 = t_207.(t_212) in
           t_207.(t_212) <- t_207.(fst i_215);
           t_207.(fst i_215) <- t_216;
           (t_211 := (~- (! t_211)))
          end else ();
          (Some (snd i_215))
       | None -> (None))
     end in
    (match t_220 with
     | Some (i_221) ->
        begin
         for j_222 = (t_212 + 1) to (t_209 - 1) do
          if (not ((t_207.(j_222)).(t_213) = 0)) then begin
           for j_223 = (t_213 + 1) to (t_208 - 1) do
            (t_207.(j_222)).(j_223) <-
             ((((t_207.(j_222)).(j_223) * (t_207.(t_212)).(t_213)) -
                ((t_207.(t_212)).(j_223) * (t_207.(j_222)).(t_212))) /
               (! t_210))
           done;
           (t_207.(j_222)).(t_213) <- 0
          end else ()
         done;
         (t_210 := i_221)
        end;
        (t_204 := ((! t_204) + 1))
     | None -> (t_211 := 0));
    (t_205 := ((! t_205) + 1))
   done;
   t_207>.
# val resIA2 :
  ('a,
   Funct4.GenIA2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_224 ->
   let t_225 = (ref 0) in
   let t_226 = (ref 0) in
   let t_228 =
    (Array.map (fun x_227 -> (Array.copy x_227)) (Array.copy a_224)) in
   let t_229 = (Array.length a_224.(0)) in
   let t_230 = (Array.length a_224) in
   let t_231 = (ref 1) in
   let t_232 = (ref 1) in
   while (((! t_226) < t_229) && ((! t_225) < t_230)) do
    let t_233 = (! t_225) in
    let t_234 = (! t_226) in
    let t_235 = (ref (None)) in
    let t_241 =
     begin
      for j_238 = t_233 to (t_230 - 1) do
       let t_239 = (t_228.(j_238)).(t_234) in
       if (not (t_239 = 0)) then
        (match (! t_235) with
         | Some (i_240) ->
            if ((abs (snd i_240)) > (abs t_239)) then
             (t_235 := (Some (j_238, t_239)))
            else ()
         | None -> (t_235 := (Some (j_238, t_239))))
       else ()
      done;
      (match (! t_235) with
       | Some (i_236) ->
          if ((fst i_236) <> t_233) then begin
           let t_237 = t_228.(t_233) in
           t_228.(t_233) <- t_228.(fst i_236);
           t_228.(fst i_236) <- t_237;
           (t_232 := (~- (! t_232)))
          end else ();
          (Some (snd i_236))
       | None -> (None))
     end in
    (match t_241 with
     | Some (i_242) ->
        begin
         for j_243 = (t_233 + 1) to (t_230 - 1) do
          if (not ((t_228.(j_243)).(t_234) = 0)) then begin
           for j_244 = (t_234 + 1) to (t_229 - 1) do
            (t_228.(j_243)).(j_244) <-
             ((((t_228.(j_243)).(j_244) * (t_228.(t_233)).(t_234)) -
                ((t_228.(t_233)).(j_244) * (t_228.(j_243)).(t_233))) /
               (! t_231))
           done;
           (t_228.(j_243)).(t_234) <- 0
          end else ()
         done;
         (t_231 := i_242)
        end;
        (t_225 := ((! t_225) + 1))
     | None -> (t_232 := 0));
    (t_226 := ((! t_226) + 1))
   done;
   (t_228,
    if ((! t_232) = 0) then 0
    else if ((! t_232) = 1) then (! t_231)
    else (~- (! t_231)))>.
# val resIA3 :
  ('a,
   Funct4.GenIA3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.Rank).res)
  code =
  .<fun a_245 ->
   let t_246 = (ref 0) in
   let t_247 = (ref 0) in
   let t_249 =
    (Array.map (fun x_248 -> (Array.copy x_248)) (Array.copy a_245)) in
   let t_250 = (Array.length a_245.(0)) in
   let t_251 = (Array.length a_245) in
   let t_252 = (ref 1) in
   let t_253 = (ref 1) in
   while (((! t_247) < t_250) && ((! t_246) < t_251)) do
    let t_254 = (! t_246) in
    let t_255 = (! t_247) in
    let t_256 = (ref (None)) in
    let t_262 =
     begin
      for j_259 = t_254 to (t_251 - 1) do
       let t_260 = (t_249.(j_259)).(t_255) in
       if (not (t_260 = 0)) then
        (match (! t_256) with
         | Some (i_261) ->
            if ((abs (snd i_261)) > (abs t_260)) then
             (t_256 := (Some (j_259, t_260)))
            else ()
         | None -> (t_256 := (Some (j_259, t_260))))
       else ()
      done;
      (match (! t_256) with
       | Some (i_257) ->
          if ((fst i_257) <> t_254) then begin
           let t_258 = t_249.(t_254) in
           t_249.(t_254) <- t_249.(fst i_257);
           t_249.(fst i_257) <- t_258;
           ()
          end else ();
          (Some (snd i_257))
       | None -> (None))
     end in
    (match t_262 with
     | Some (i_263) ->
        begin
         for j_264 = (t_254 + 1) to (t_251 - 1) do
          if (not ((t_249.(j_264)).(t_255) = 0)) then begin
           for j_265 = (t_255 + 1) to (t_250 - 1) do
            (t_249.(j_264)).(j_265) <-
             ((((t_249.(j_264)).(j_265) * (t_249.(t_254)).(t_255)) -
                ((t_249.(t_254)).(j_265) * (t_249.(j_264)).(t_254))) /
               (! t_252))
           done;
           (t_249.(j_264)).(t_255) <- 0
          end else ()
         done;
         (t_252 := i_263)
        end;
        (t_246 := ((! t_246) + 1))
     | None -> (t_253 := 0));
    (t_247 := ((! t_247) + 1))
   done;
   (t_249, (! t_246))>.
# val resIA4 :
  ('a,
   Funct4.GenIA4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_266 ->
   let t_267 = (ref 0) in
   let t_268 = (ref 0) in
   let t_270 =
    (Array.map (fun x_269 -> (Array.copy x_269)) (Array.copy a_266)) in
   let t_271 = (Array.length a_266.(0)) in
   let t_272 = (Array.length a_266) in
   let t_273 = (ref 1) in
   let t_274 = (ref 1) in
   while (((! t_268) < t_271) && ((! t_267) < t_272)) do
    let t_275 = (! t_267) in
    let t_276 = (! t_268) in
    let t_277 = (ref (None)) in
    let t_283 =
     begin
      for j_280 = t_275 to (t_272 - 1) do
       let t_281 = (t_270.(j_280)).(t_276) in
       if (not (t_281 = 0)) then
        (match (! t_277) with
         | Some (i_282) ->
            if ((abs (snd i_282)) > (abs t_281)) then
             (t_277 := (Some (j_280, t_281)))
            else ()
         | None -> (t_277 := (Some (j_280, t_281))))
       else ()
      done;
      (match (! t_277) with
       | Some (i_278) ->
          if ((fst i_278) <> t_275) then begin
           let t_279 = t_270.(t_275) in
           t_270.(t_275) <- t_270.(fst i_278);
           t_270.(fst i_278) <- t_279;
           (t_274 := (~- (! t_274)))
          end else ();
          (Some (snd i_278))
       | None -> (None))
     end in
    (match t_283 with
     | Some (i_284) ->
        begin
         for j_285 = (t_275 + 1) to (t_272 - 1) do
          if (not ((t_270.(j_285)).(t_276) = 0)) then begin
           for j_286 = (t_276 + 1) to (t_271 - 1) do
            (t_270.(j_285)).(j_286) <-
             ((((t_270.(j_285)).(j_286) * (t_270.(t_275)).(t_276)) -
                ((t_270.(t_275)).(j_286) * (t_270.(j_285)).(t_275))) /
               (! t_273))
           done;
           (t_270.(j_285)).(t_276) <- 0
          end else ()
         done;
         (t_273 := i_284)
        end;
        (t_267 := ((! t_267) + 1))
     | None -> (t_274 := 0));
    (t_268 := ((! t_268) + 1))
   done;
   (t_270,
    if ((! t_274) = 0) then 0
    else if ((! t_274) = 1) then (! t_273)
    else (~- (! t_273)), (! t_267))>.
# val resIV1 :
  ('a,
   Funct4.GenIV1.Ctr.contr ->
   Funct4.OutJustMatrix(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_287 ->
   let t_288 = (ref 0) in
   let t_289 = (ref 0) in
   let t_290 = {arr = (Array.copy a_287.arr)} (a_287) in
   let t_291 = a_287.m in
   let t_292 = a_287.n in
   let t_293 = (ref 1) in
   let t_294 = (ref 1) in
   while (((! t_289) < t_291) && ((! t_288) < t_292)) do
    let t_295 = (! t_288) in
    let t_296 = (! t_289) in
    let t_297 = (ref (None)) in
    let t_309 =
     begin
      for j_306 = t_295 to (t_292 - 1) do
       let t_307 = (t_290.arr).((j_306 * t_290.n) + t_296) in
       if (not (t_307 = 0)) then
        (match (! t_297) with
         | Some (i_308) ->
            if ((abs (snd i_308)) > (abs t_307)) then
             (t_297 := (Some (j_306, t_307)))
            else ()
         | None -> (t_297 := (Some (j_306, t_307))))
       else ()
      done;
      (match (! t_297) with
       | Some (i_298) ->
          if ((fst i_298) <> t_295) then begin
           let a_299 = t_290.arr
           and n_300 = t_290.n
           and m_301 = t_290.m in
           let i1_302 = (t_295 * n_300)
           and i2_303 = ((fst i_298) * n_300) in
           for i_304 = 0 to (m_301 - 1) do
            let t_305 = a_299.(i1_302 + i_304) in
            a_299.(i2_303 + i_304) <- a_299.(i1_302 + i_304);
            a_299.(i1_302 + i_304) <- t_305
           done;
           ()
          end else ();
          (Some (snd i_298))
       | None -> (None))
     end in
    (match t_309 with
     | Some (i_310) ->
        begin
         for j_311 = (t_295 + 1) to (t_292 - 1) do
          if (not ((t_290.arr).((j_311 * t_290.n) + t_296) = 0)) then begin
           for j_312 = (t_296 + 1) to (t_291 - 1) do
            (t_290.arr).((j_311 * t_290.n) + j_312) <-
             ((((t_290.arr).((j_311 * t_290.n) + j_312) *
                 (t_290.arr).((t_295 * t_290.n) + t_296)) -
                ((t_290.arr).((t_295 * t_290.n) + j_312) *
                  (t_290.arr).((j_311 * t_290.n) + t_295))) / (! t_293))
           done;
           (t_290.arr).((j_311 * t_290.n) + t_296) <- 0
          end else ()
         done;
         (t_293 := i_310)
        end;
        (t_288 := ((! t_288) + 1))
     | None -> (t_294 := 0));
    (t_289 := ((! t_289) + 1))
   done;
   t_290>.
# val resIV2 :
  ('a,
   Funct4.GenIV2.Ctr.contr ->
   Funct4.OutDet(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_313 ->
   let t_314 = (ref 0) in
   let t_315 = (ref 0) in
   let t_316 = {arr = (Array.copy a_313.arr)} (a_313) in
   let t_317 = a_313.m in
   let t_318 = a_313.n in
   let t_319 = (ref 1) in
   let t_320 = (ref 1) in
   while (((! t_315) < t_317) && ((! t_314) < t_318)) do
    let t_321 = (! t_314) in
    let t_322 = (! t_315) in
    let t_323 = (ref (None)) in
    let t_335 =
     begin
      for j_332 = t_321 to (t_318 - 1) do
       let t_333 = (t_316.arr).((j_332 * t_316.n) + t_322) in
       if (not (t_333 = 0)) then
        (match (! t_323) with
         | Some (i_334) ->
            if ((abs (snd i_334)) > (abs t_333)) then
             (t_323 := (Some (j_332, t_333)))
            else ()
         | None -> (t_323 := (Some (j_332, t_333))))
       else ()
      done;
      (match (! t_323) with
       | Some (i_324) ->
          if ((fst i_324) <> t_321) then begin
           let a_325 = t_316.arr
           and n_326 = t_316.n
           and m_327 = t_316.m in
           let i1_328 = (t_321 * n_326)
           and i2_329 = ((fst i_324) * n_326) in
           for i_330 = 0 to (m_327 - 1) do
            let t_331 = a_325.(i1_328 + i_330) in
            a_325.(i2_329 + i_330) <- a_325.(i1_328 + i_330);
            a_325.(i1_328 + i_330) <- t_331
           done;
           (t_320 := (~- (! t_320)))
          end else ();
          (Some (snd i_324))
       | None -> (None))
     end in
    (match t_335 with
     | Some (i_336) ->
        begin
         for j_337 = (t_321 + 1) to (t_318 - 1) do
          if (not ((t_316.arr).((j_337 * t_316.n) + t_322) = 0)) then begin
           for j_338 = (t_322 + 1) to (t_317 - 1) do
            (t_316.arr).((j_337 * t_316.n) + j_338) <-
             ((((t_316.arr).((j_337 * t_316.n) + j_338) *
                 (t_316.arr).((t_321 * t_316.n) + t_322)) -
                ((t_316.arr).((t_321 * t_316.n) + j_338) *
                  (t_316.arr).((j_337 * t_316.n) + t_321))) / (! t_319))
           done;
           (t_316.arr).((j_337 * t_316.n) + t_322) <- 0
          end else ()
         done;
         (t_319 := i_336)
        end;
        (t_314 := ((! t_314) + 1))
     | None -> (t_320 := 0));
    (t_315 := ((! t_315) + 1))
   done;
   (t_316,
    if ((! t_320) = 0) then 0
    else if ((! t_320) = 1) then (! t_319)
    else (~- (! t_319)))>.
# val resIV3 :
  ('a,
   Funct4.GenIV3.Ctr.contr ->
   Funct4.OutRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.Rank).res)
  code =
  .<fun a_339 ->
   let t_340 = (ref 0) in
   let t_341 = (ref 0) in
   let t_342 = {arr = (Array.copy a_339.arr)} (a_339) in
   let t_343 = a_339.m in
   let t_344 = a_339.n in
   let t_345 = (ref 1) in
   let t_346 = (ref 1) in
   while (((! t_341) < t_343) && ((! t_340) < t_344)) do
    let t_347 = (! t_340) in
    let t_348 = (! t_341) in
    let t_349 = (ref (None)) in
    let t_361 =
     begin
      for j_358 = t_347 to (t_344 - 1) do
       let t_359 = (t_342.arr).((j_358 * t_342.n) + t_348) in
       if (not (t_359 = 0)) then
        (match (! t_349) with
         | Some (i_360) ->
            if ((abs (snd i_360)) > (abs t_359)) then
             (t_349 := (Some (j_358, t_359)))
            else ()
         | None -> (t_349 := (Some (j_358, t_359))))
       else ()
      done;
      (match (! t_349) with
       | Some (i_350) ->
          if ((fst i_350) <> t_347) then begin
           let a_351 = t_342.arr
           and n_352 = t_342.n
           and m_353 = t_342.m in
           let i1_354 = (t_347 * n_352)
           and i2_355 = ((fst i_350) * n_352) in
           for i_356 = 0 to (m_353 - 1) do
            let t_357 = a_351.(i1_354 + i_356) in
            a_351.(i2_355 + i_356) <- a_351.(i1_354 + i_356);
            a_351.(i1_354 + i_356) <- t_357
           done;
           ()
          end else ();
          (Some (snd i_350))
       | None -> (None))
     end in
    (match t_361 with
     | Some (i_362) ->
        begin
         for j_363 = (t_347 + 1) to (t_344 - 1) do
          if (not ((t_342.arr).((j_363 * t_342.n) + t_348) = 0)) then begin
           for j_364 = (t_348 + 1) to (t_343 - 1) do
            (t_342.arr).((j_363 * t_342.n) + j_364) <-
             ((((t_342.arr).((j_363 * t_342.n) + j_364) *
                 (t_342.arr).((t_347 * t_342.n) + t_348)) -
                ((t_342.arr).((t_347 * t_342.n) + j_364) *
                  (t_342.arr).((j_363 * t_342.n) + t_347))) / (! t_345))
           done;
           (t_342.arr).((j_363 * t_342.n) + t_348) <- 0
          end else ()
         done;
         (t_345 := i_362)
        end;
        (t_340 := ((! t_340) + 1))
     | None -> (t_346 := 0));
    (t_341 := ((! t_341) + 1))
   done;
   (t_342, (! t_340))>.
# val resIV4 :
  ('a,
   Funct4.GenIV4.Ctr.contr ->
   Funct4.OutDetRank(Funct4.IntegerDomain)(Funct4.GenericVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_365 ->
   let t_366 = (ref 0) in
   let t_367 = (ref 0) in
   let t_368 = {arr = (Array.copy a_365.arr)} (a_365) in
   let t_369 = a_365.m in
   let t_370 = a_365.n in
   let t_371 = (ref 1) in
   let t_372 = (ref 1) in
   while (((! t_367) < t_369) && ((! t_366) < t_370)) do
    let t_373 = (! t_366) in
    let t_374 = (! t_367) in
    let t_375 = (ref (None)) in
    let t_387 =
     begin
      for j_384 = t_373 to (t_370 - 1) do
       let t_385 = (t_368.arr).((j_384 * t_368.n) + t_374) in
       if (not (t_385 = 0)) then
        (match (! t_375) with
         | Some (i_386) ->
            if ((abs (snd i_386)) > (abs t_385)) then
             (t_375 := (Some (j_384, t_385)))
            else ()
         | None -> (t_375 := (Some (j_384, t_385))))
       else ()
      done;
      (match (! t_375) with
       | Some (i_376) ->
          if ((fst i_376) <> t_373) then begin
           let a_377 = t_368.arr
           and n_378 = t_368.n
           and m_379 = t_368.m in
           let i1_380 = (t_373 * n_378)
           and i2_381 = ((fst i_376) * n_378) in
           for i_382 = 0 to (m_379 - 1) do
            let t_383 = a_377.(i1_380 + i_382) in
            a_377.(i2_381 + i_382) <- a_377.(i1_380 + i_382);
            a_377.(i1_380 + i_382) <- t_383
           done;
           (t_372 := (~- (! t_372)))
          end else ();
          (Some (snd i_376))
       | None -> (None))
     end in
    (match t_387 with
     | Some (i_388) ->
        begin
         for j_389 = (t_373 + 1) to (t_370 - 1) do
          if (not ((t_368.arr).((j_389 * t_368.n) + t_374) = 0)) then begin
           for j_390 = (t_374 + 1) to (t_369 - 1) do
            (t_368.arr).((j_389 * t_368.n) + j_390) <-
             ((((t_368.arr).((j_389 * t_368.n) + j_390) *
                 (t_368.arr).((t_373 * t_368.n) + t_374)) -
                ((t_368.arr).((t_373 * t_368.n) + j_390) *
                  (t_368.arr).((j_389 * t_368.n) + t_373))) / (! t_371))
           done;
           (t_368.arr).((j_389 * t_368.n) + t_374) <- 0
          end else ()
         done;
         (t_371 := i_388)
        end;
        (t_366 := ((! t_366) + 1))
     | None -> (t_372 := 0));
    (t_367 := ((! t_367) + 1))
   done;
   (t_368,
    if ((! t_372) = 0) then 0
    else if ((! t_372) = 1) then (! t_371)
    else (~- (! t_371)), (! t_366))>.
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
