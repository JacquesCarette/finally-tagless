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
   while (((! t_3) < t_6) && ((! t_2) < t_7)) do
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
  .<fun a_15 ->
   let t_16 = (ref 0) in
   let t_17 = (ref 0) in
   let t_19 = (Array.map (fun x_18 -> (Array.copy x_18)) (Array.copy a_15)) in
   let t_20 = (Array.length a_15.(0)) in
   let t_21 = (Array.length a_15) in
   let t_22 = (ref 1.) in
   let t_23 = (ref 1.) in
   while (((! t_17) < t_20) && ((! t_16) < t_21)) do
    let t_24 = (ref (-1)) in
    let t_26 =
     begin
      for j_25 = (! t_16) to (t_21 - 1) do
       if (not ((t_19.(j_25)).(! t_17) = 0.)) then
        if (((! t_24) == (-1)) ||
             ((abs_float (t_19.(j_25)).(! t_17)) <
               (abs_float (t_19.(! t_24)).(! t_17)))) then
         (t_24 := j_25)
        else ()
       else ()
      done;
      if ((! t_24) == (-1)) then (None) else (Some (! t_24))
     end in
    (match t_26 with
     | Some (i_27) ->
        if (i_27 <> (! t_16)) then
         let t_28 = t_19.(! t_16) in
         t_19.(! t_16) <- t_19.(i_27);
         t_19.(i_27) <- t_28
        else ();
        begin
         for j_29 = ((! t_16) + 1) to (t_21 - 1) do
          if (not ((t_19.(j_29)).(! t_17) = 0.)) then begin
           for j_30 = ((! t_17) + 1) to (t_20 - 1) do
            (t_19.(j_29)).(j_30) <-
             ((((t_19.(j_29)).(! t_17) /. (t_19.(! t_16)).(! t_17)) *.
                (t_19.(! t_16)).(j_30)) -. (t_19.(j_29)).(j_30))
           done;
           (t_19.(j_29)).(! t_17) <- 0.
          end else ()
         done;
         (t_22 := ((! t_22) *. (t_19.(! t_16)).(! t_17)))
        end;
        (t_16 := ((! t_16) + 1))
     | None -> (t_23 := 0.));
    (t_17 := ((! t_17) + 1))
   done;
   (t_19, ((! t_23) *. (! t_22)))>.
# val resFA3 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutRank(Funct4.FArrayContainer)(Funct4.Rank)(Funct4.FloatDomain).res)
  code =
  .<fun a_31 ->
   let t_32 = (ref 0) in
   let t_33 = (ref 0) in
   let t_35 = (Array.map (fun x_34 -> (Array.copy x_34)) (Array.copy a_31)) in
   let t_36 = (Array.length a_31.(0)) in
   let t_37 = (Array.length a_31) in
   while (((! t_33) < t_36) && ((! t_32) < t_37)) do
    let t_38 = (ref (-1)) in
    let t_40 =
     begin
      for j_39 = (! t_32) to (t_37 - 1) do
       if (not ((t_35.(j_39)).(! t_33) = 0.)) then
        if (((! t_38) == (-1)) ||
             ((abs_float (t_35.(j_39)).(! t_33)) <
               (abs_float (t_35.(! t_38)).(! t_33)))) then
         (t_38 := j_39)
        else ()
       else ()
      done;
      if ((! t_38) == (-1)) then (None) else (Some (! t_38))
     end in
    (match t_40 with
     | Some (i_41) ->
        if (i_41 <> (! t_32)) then
         let t_42 = t_35.(! t_32) in
         t_35.(! t_32) <- t_35.(i_41);
         t_35.(i_41) <- t_42
        else ();
        begin
         for j_43 = ((! t_32) + 1) to (t_37 - 1) do
          if (not ((t_35.(j_43)).(! t_33) = 0.)) then begin
           for j_44 = ((! t_33) + 1) to (t_36 - 1) do
            (t_35.(j_43)).(j_44) <-
             ((((t_35.(j_43)).(! t_33) /. (t_35.(! t_32)).(! t_33)) *.
                (t_35.(! t_32)).(j_44)) -. (t_35.(j_43)).(j_44))
           done;
           (t_35.(j_43)).(! t_33) <- 0.
          end else ()
         done;
         ()
        end;
        (t_32 := ((! t_32) + 1))
     | None -> ());
    (t_33 := ((! t_33) + 1))
   done;
   (t_35, (! t_32))>.
# val resFA4 :
  ('a,
   Funct4.FArrayContainer.contr ->
   Funct4.OutDetRank(Funct4.FArrayContainer)(Funct4.FDet)(Funct4.Rank).res)
  code =
  .<fun a_45 ->
   let t_46 = (ref 0) in
   let t_47 = (ref 0) in
   let t_49 = (Array.map (fun x_48 -> (Array.copy x_48)) (Array.copy a_45)) in
   let t_50 = (Array.length a_45.(0)) in
   let t_51 = (Array.length a_45) in
   let t_52 = (ref 1.) in
   let t_53 = (ref 1.) in
   while (((! t_47) < t_50) && ((! t_46) < t_51)) do
    let t_54 = (ref (-1)) in
    let t_56 =
     begin
      for j_55 = (! t_46) to (t_51 - 1) do
       if (not ((t_49.(j_55)).(! t_47) = 0.)) then
        if (((! t_54) == (-1)) ||
             ((abs_float (t_49.(j_55)).(! t_47)) <
               (abs_float (t_49.(! t_54)).(! t_47)))) then
         (t_54 := j_55)
        else ()
       else ()
      done;
      if ((! t_54) == (-1)) then (None) else (Some (! t_54))
     end in
    (match t_56 with
     | Some (i_57) ->
        if (i_57 <> (! t_46)) then
         let t_58 = t_49.(! t_46) in
         t_49.(! t_46) <- t_49.(i_57);
         t_49.(i_57) <- t_58
        else ();
        begin
         for j_59 = ((! t_46) + 1) to (t_51 - 1) do
          if (not ((t_49.(j_59)).(! t_47) = 0.)) then begin
           for j_60 = ((! t_47) + 1) to (t_50 - 1) do
            (t_49.(j_59)).(j_60) <-
             ((((t_49.(j_59)).(! t_47) /. (t_49.(! t_46)).(! t_47)) *.
                (t_49.(! t_46)).(j_60)) -. (t_49.(j_59)).(j_60))
           done;
           (t_49.(j_59)).(! t_47) <- 0.
          end else ()
         done;
         (t_52 := ((! t_52) *. (t_49.(! t_46)).(! t_47)))
        end;
        (t_46 := ((! t_46) + 1))
     | None -> (t_53 := 0.));
    (t_47 := ((! t_47) + 1))
   done;
   (t_49, ((! t_53) *. (! t_52)), (! t_46))>.
# val resFV1 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutJustMatrix(Funct4.FVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_61 ->
   let t_62 = (ref 0) in
   let t_63 = (ref 0) in
   let t_64 = {arr = (Array.copy a_61.arr)} (a_61) in
   let t_65 = a_61.m in
   let t_66 = a_61.n in
   let t_67 = (ref 1.) in
   let t_68 = (ref 1.) in
   while (((! t_63) < t_65) && ((! t_62) < t_66)) do
    let t_69 = (ref (-1)) in
    let t_71 =
     begin
      for j_70 = (! t_62) to (t_66 - 1) do
       if (not ((t_64.arr).((j_70 * t_64.n) + (! t_63)) = 0.)) then
        if (((! t_69) == (-1)) ||
             ((abs_float (t_64.arr).((j_70 * t_64.n) + (! t_63))) <
               (abs_float (t_64.arr).(((! t_69) * t_64.n) + (! t_63))))) then
         (t_69 := j_70)
        else ()
       else ()
      done;
      if ((! t_69) == (-1)) then (None) else (Some (! t_69))
     end in
    (match t_71 with
     | Some (i_72) ->
        if (i_72 <> (! t_62)) then
         let a_73 = t_64.arr
         and n_74 = t_64.n
         and m_75 = t_64.m in
         let i1_76 = ((! t_62) * n_74)
         and i2_77 = (i_72 * n_74) in
         for i_78 = 0 to (m_75 - 1) do
          let t_79 = a_73.(i1_76 + i_78) in
          a_73.(i2_77 + i_78) <- a_73.(i1_76 + i_78);
          a_73.(i1_76 + i_78) <- t_79
         done
        else ();
        begin
         for j_80 = ((! t_62) + 1) to (t_66 - 1) do
          if (not ((t_64.arr).((j_80 * t_64.n) + (! t_63)) = 0.)) then begin
           for j_81 = ((! t_63) + 1) to (t_65 - 1) do
            (t_64.arr).((j_80 * t_64.n) + j_81) <-
             ((((t_64.arr).((j_80 * t_64.n) + (! t_63)) /.
                 (t_64.arr).(((! t_62) * t_64.n) + (! t_63))) *.
                (t_64.arr).(((! t_62) * t_64.n) + j_81)) -.
               (t_64.arr).((j_80 * t_64.n) + j_81))
           done;
           (t_64.arr).((j_80 * t_64.n) + (! t_63)) <- 0.
          end else ()
         done;
         (t_67 := ((! t_67) *. (t_64.arr).(((! t_62) * t_64.n) + (! t_63))))
        end;
        (t_62 := ((! t_62) + 1))
     | None -> (t_68 := 0.));
    (t_63 := ((! t_63) + 1))
   done;
   t_64>.
# val resFV2 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutDet(Funct4.FVectorContainer)(Funct4.FDet).res)
  code =
  .<fun a_82 ->
   let t_83 = (ref 0) in
   let t_84 = (ref 0) in
   let t_85 = {arr = (Array.copy a_82.arr)} (a_82) in
   let t_86 = a_82.m in
   let t_87 = a_82.n in
   let t_88 = (ref 1.) in
   let t_89 = (ref 1.) in
   while (((! t_84) < t_86) && ((! t_83) < t_87)) do
    let t_90 = (ref (-1)) in
    let t_92 =
     begin
      for j_91 = (! t_83) to (t_87 - 1) do
       if (not ((t_85.arr).((j_91 * t_85.n) + (! t_84)) = 0.)) then
        if (((! t_90) == (-1)) ||
             ((abs_float (t_85.arr).((j_91 * t_85.n) + (! t_84))) <
               (abs_float (t_85.arr).(((! t_90) * t_85.n) + (! t_84))))) then
         (t_90 := j_91)
        else ()
       else ()
      done;
      if ((! t_90) == (-1)) then (None) else (Some (! t_90))
     end in
    (match t_92 with
     | Some (i_93) ->
        if (i_93 <> (! t_83)) then
         let a_94 = t_85.arr
         and n_95 = t_85.n
         and m_96 = t_85.m in
         let i1_97 = ((! t_83) * n_95)
         and i2_98 = (i_93 * n_95) in
         for i_99 = 0 to (m_96 - 1) do
          let t_100 = a_94.(i1_97 + i_99) in
          a_94.(i2_98 + i_99) <- a_94.(i1_97 + i_99);
          a_94.(i1_97 + i_99) <- t_100
         done
        else ();
        begin
         for j_101 = ((! t_83) + 1) to (t_87 - 1) do
          if (not ((t_85.arr).((j_101 * t_85.n) + (! t_84)) = 0.)) then begin
           for j_102 = ((! t_84) + 1) to (t_86 - 1) do
            (t_85.arr).((j_101 * t_85.n) + j_102) <-
             ((((t_85.arr).((j_101 * t_85.n) + (! t_84)) /.
                 (t_85.arr).(((! t_83) * t_85.n) + (! t_84))) *.
                (t_85.arr).(((! t_83) * t_85.n) + j_102)) -.
               (t_85.arr).((j_101 * t_85.n) + j_102))
           done;
           (t_85.arr).((j_101 * t_85.n) + (! t_84)) <- 0.
          end else ()
         done;
         (t_88 := ((! t_88) *. (t_85.arr).(((! t_83) * t_85.n) + (! t_84))))
        end;
        (t_83 := ((! t_83) + 1))
     | None -> (t_89 := 0.));
    (t_84 := ((! t_84) + 1))
   done;
   (t_85, ((! t_89) *. (! t_88)))>.
# val resFV3 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutRank(Funct4.FVectorContainer)(Funct4.Rank)(Funct4.FloatDomain).res)
  code =
  .<fun a_103 ->
   let t_104 = (ref 0) in
   let t_105 = (ref 0) in
   let t_106 = {arr = (Array.copy a_103.arr)} (a_103) in
   let t_107 = a_103.m in
   let t_108 = a_103.n in
   while (((! t_105) < t_107) && ((! t_104) < t_108)) do
    let t_109 = (ref (-1)) in
    let t_111 =
     begin
      for j_110 = (! t_104) to (t_108 - 1) do
       if (not ((t_106.arr).((j_110 * t_106.n) + (! t_105)) = 0.)) then
        if (((! t_109) == (-1)) ||
             ((abs_float (t_106.arr).((j_110 * t_106.n) + (! t_105))) <
               (abs_float (t_106.arr).(((! t_109) * t_106.n) + (! t_105))))) then
         (t_109 := j_110)
        else ()
       else ()
      done;
      if ((! t_109) == (-1)) then (None) else (Some (! t_109))
     end in
    (match t_111 with
     | Some (i_112) ->
        if (i_112 <> (! t_104)) then
         let a_113 = t_106.arr
         and n_114 = t_106.n
         and m_115 = t_106.m in
         let i1_116 = ((! t_104) * n_114)
         and i2_117 = (i_112 * n_114) in
         for i_118 = 0 to (m_115 - 1) do
          let t_119 = a_113.(i1_116 + i_118) in
          a_113.(i2_117 + i_118) <- a_113.(i1_116 + i_118);
          a_113.(i1_116 + i_118) <- t_119
         done
        else ();
        begin
         for j_120 = ((! t_104) + 1) to (t_108 - 1) do
          if (not ((t_106.arr).((j_120 * t_106.n) + (! t_105)) = 0.)) then begin
           for j_121 = ((! t_105) + 1) to (t_107 - 1) do
            (t_106.arr).((j_120 * t_106.n) + j_121) <-
             ((((t_106.arr).((j_120 * t_106.n) + (! t_105)) /.
                 (t_106.arr).(((! t_104) * t_106.n) + (! t_105))) *.
                (t_106.arr).(((! t_104) * t_106.n) + j_121)) -.
               (t_106.arr).((j_120 * t_106.n) + j_121))
           done;
           (t_106.arr).((j_120 * t_106.n) + (! t_105)) <- 0.
          end else ()
         done;
         ()
        end;
        (t_104 := ((! t_104) + 1))
     | None -> ());
    (t_105 := ((! t_105) + 1))
   done;
   (t_106, (! t_104))>.
# val resFV4 :
  ('a,
   Funct4.FVectorContainer.contr ->
   Funct4.OutDetRank(Funct4.FVectorContainer)(Funct4.FDet)(Funct4.Rank).res)
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
    let t_130 = (ref (-1)) in
    let t_132 =
     begin
      for j_131 = (! t_123) to (t_127 - 1) do
       if (not ((t_125.arr).((j_131 * t_125.n) + (! t_124)) = 0.)) then
        if (((! t_130) == (-1)) ||
             ((abs_float (t_125.arr).((j_131 * t_125.n) + (! t_124))) <
               (abs_float (t_125.arr).(((! t_130) * t_125.n) + (! t_124))))) then
         (t_130 := j_131)
        else ()
       else ()
      done;
      if ((! t_130) == (-1)) then (None) else (Some (! t_130))
     end in
    (match t_132 with
     | Some (i_133) ->
        if (i_133 <> (! t_123)) then
         let a_134 = t_125.arr
         and n_135 = t_125.n
         and m_136 = t_125.m in
         let i1_137 = ((! t_123) * n_135)
         and i2_138 = (i_133 * n_135) in
         for i_139 = 0 to (m_136 - 1) do
          let t_140 = a_134.(i1_137 + i_139) in
          a_134.(i2_138 + i_139) <- a_134.(i1_137 + i_139);
          a_134.(i1_137 + i_139) <- t_140
         done
        else ();
        begin
         for j_141 = ((! t_123) + 1) to (t_127 - 1) do
          if (not ((t_125.arr).((j_141 * t_125.n) + (! t_124)) = 0.)) then begin
           for j_142 = ((! t_124) + 1) to (t_126 - 1) do
            (t_125.arr).((j_141 * t_125.n) + j_142) <-
             ((((t_125.arr).((j_141 * t_125.n) + (! t_124)) /.
                 (t_125.arr).(((! t_123) * t_125.n) + (! t_124))) *.
                (t_125.arr).(((! t_123) * t_125.n) + j_142)) -.
               (t_125.arr).((j_141 * t_125.n) + j_142))
           done;
           (t_125.arr).((j_141 * t_125.n) + (! t_124)) <- 0.
          end else ()
         done;
         (t_128 :=
           ((! t_128) *. (t_125.arr).(((! t_123) * t_125.n) + (! t_124))))
        end;
        (t_123 := ((! t_123) + 1))
     | None -> (t_129 := 0.));
    (t_124 := ((! t_124) + 1))
   done;
   (t_125, ((! t_129) *. (! t_128)), (! t_123))>.
# val resIA1 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutJustMatrix(Funct4.IArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_143 ->
   let t_144 = (ref 0) in
   let t_145 = (ref 0) in
   let t_147 =
    (Array.map (fun x_146 -> (Array.copy x_146)) (Array.copy a_143)) in
   let t_148 = (Array.length a_143.(0)) in
   let t_149 = (Array.length a_143) in
   let t_150 = (ref 1) in
   let t_151 = (ref 1) in
   while (((! t_145) < t_148) && ((! t_144) < t_149)) do
    let t_152 = (ref (-1)) in
    let t_154 =
     begin
      for j_153 = (! t_144) to (t_149 - 1) do
       if (not ((t_147.(j_153)).(! t_145) = 0)) then
        if (((! t_152) == (-1)) ||
             ((abs (t_147.(j_153)).(! t_145)) <
               (abs (t_147.(! t_152)).(! t_145)))) then
         (t_152 := j_153)
        else ()
       else ()
      done;
      if ((! t_152) == (-1)) then (None) else (Some (! t_152))
     end in
    (match t_154 with
     | Some (i_155) ->
        if (i_155 <> (! t_144)) then
         let t_156 = t_147.(! t_144) in
         t_147.(! t_144) <- t_147.(i_155);
         t_147.(i_155) <- t_156
        else ();
        begin
         for j_157 = ((! t_144) + 1) to (t_149 - 1) do
          if (not ((t_147.(j_157)).(! t_145) = 0)) then begin
           for j_158 = ((! t_145) + 1) to (t_148 - 1) do
            (t_147.(j_157)).(j_158) <-
             ((((t_147.(j_157)).(j_158) * (t_147.(! t_144)).(! t_145)) -
                ((t_147.(! t_144)).(j_158) * (t_147.(j_157)).(! t_144))) /
               (! t_150))
           done;
           (t_147.(j_157)).(! t_145) <- 0
          end else ()
         done;
         (t_150 := (t_147.(! t_144)).(! t_145))
        end;
        (t_144 := ((! t_144) + 1))
     | None -> (t_151 := 0));
    (t_145 := ((! t_145) + 1))
   done;
   t_147>.
# val resIA2 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutDet(Funct4.IArrayContainer)(Funct4.IDet).res)
  code =
  .<fun a_159 ->
   let t_160 = (ref 0) in
   let t_161 = (ref 0) in
   let t_163 =
    (Array.map (fun x_162 -> (Array.copy x_162)) (Array.copy a_159)) in
   let t_164 = (Array.length a_159.(0)) in
   let t_165 = (Array.length a_159) in
   let t_166 = (ref 1) in
   let t_167 = (ref 1) in
   while (((! t_161) < t_164) && ((! t_160) < t_165)) do
    let t_168 = (ref (-1)) in
    let t_170 =
     begin
      for j_169 = (! t_160) to (t_165 - 1) do
       if (not ((t_163.(j_169)).(! t_161) = 0)) then
        if (((! t_168) == (-1)) ||
             ((abs (t_163.(j_169)).(! t_161)) <
               (abs (t_163.(! t_168)).(! t_161)))) then
         (t_168 := j_169)
        else ()
       else ()
      done;
      if ((! t_168) == (-1)) then (None) else (Some (! t_168))
     end in
    (match t_170 with
     | Some (i_171) ->
        if (i_171 <> (! t_160)) then
         let t_172 = t_163.(! t_160) in
         t_163.(! t_160) <- t_163.(i_171);
         t_163.(i_171) <- t_172
        else ();
        begin
         for j_173 = ((! t_160) + 1) to (t_165 - 1) do
          if (not ((t_163.(j_173)).(! t_161) = 0)) then begin
           for j_174 = ((! t_161) + 1) to (t_164 - 1) do
            (t_163.(j_173)).(j_174) <-
             ((((t_163.(j_173)).(j_174) * (t_163.(! t_160)).(! t_161)) -
                ((t_163.(! t_160)).(j_174) * (t_163.(j_173)).(! t_160))) /
               (! t_166))
           done;
           (t_163.(j_173)).(! t_161) <- 0
          end else ()
         done;
         (t_166 := (t_163.(! t_160)).(! t_161))
        end;
        (t_160 := ((! t_160) + 1))
     | None -> (t_167 := 0));
    (t_161 := ((! t_161) + 1))
   done;
   (t_163, ((! t_167) * (! t_166)))>.
# val resIA3 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutRank(Funct4.IArrayContainer)(Funct4.Rank)(Funct4.IntegerDomain).res)
  code =
  .<fun a_175 ->
   let t_176 = (ref 0) in
   let t_177 = (ref 0) in
   let t_179 =
    (Array.map (fun x_178 -> (Array.copy x_178)) (Array.copy a_175)) in
   let t_180 = (Array.length a_175.(0)) in
   let t_181 = (Array.length a_175) in
   let t_182 = (ref 1) in
   let t_183 = (ref 1) in
   while (((! t_177) < t_180) && ((! t_176) < t_181)) do
    let t_184 = (ref (-1)) in
    let t_186 =
     begin
      for j_185 = (! t_176) to (t_181 - 1) do
       if (not ((t_179.(j_185)).(! t_177) = 0)) then
        if (((! t_184) == (-1)) ||
             ((abs (t_179.(j_185)).(! t_177)) <
               (abs (t_179.(! t_184)).(! t_177)))) then
         (t_184 := j_185)
        else ()
       else ()
      done;
      if ((! t_184) == (-1)) then (None) else (Some (! t_184))
     end in
    (match t_186 with
     | Some (i_187) ->
        if (i_187 <> (! t_176)) then
         let t_188 = t_179.(! t_176) in
         t_179.(! t_176) <- t_179.(i_187);
         t_179.(i_187) <- t_188
        else ();
        begin
         for j_189 = ((! t_176) + 1) to (t_181 - 1) do
          if (not ((t_179.(j_189)).(! t_177) = 0)) then begin
           for j_190 = ((! t_177) + 1) to (t_180 - 1) do
            (t_179.(j_189)).(j_190) <-
             ((((t_179.(j_189)).(j_190) * (t_179.(! t_176)).(! t_177)) -
                ((t_179.(! t_176)).(j_190) * (t_179.(j_189)).(! t_176))) /
               (! t_182))
           done;
           (t_179.(j_189)).(! t_177) <- 0
          end else ()
         done;
         (t_182 := (t_179.(! t_176)).(! t_177))
        end;
        (t_176 := ((! t_176) + 1))
     | None -> (t_183 := 0));
    (t_177 := ((! t_177) + 1))
   done;
   (t_179, (! t_176))>.
# val resIA4 :
  ('a,
   Funct4.IArrayContainer.contr ->
   Funct4.OutDetRank(Funct4.IArrayContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_191 ->
   let t_192 = (ref 0) in
   let t_193 = (ref 0) in
   let t_195 =
    (Array.map (fun x_194 -> (Array.copy x_194)) (Array.copy a_191)) in
   let t_196 = (Array.length a_191.(0)) in
   let t_197 = (Array.length a_191) in
   let t_198 = (ref 1) in
   let t_199 = (ref 1) in
   while (((! t_193) < t_196) && ((! t_192) < t_197)) do
    let t_200 = (ref (-1)) in
    let t_202 =
     begin
      for j_201 = (! t_192) to (t_197 - 1) do
       if (not ((t_195.(j_201)).(! t_193) = 0)) then
        if (((! t_200) == (-1)) ||
             ((abs (t_195.(j_201)).(! t_193)) <
               (abs (t_195.(! t_200)).(! t_193)))) then
         (t_200 := j_201)
        else ()
       else ()
      done;
      if ((! t_200) == (-1)) then (None) else (Some (! t_200))
     end in
    (match t_202 with
     | Some (i_203) ->
        if (i_203 <> (! t_192)) then
         let t_204 = t_195.(! t_192) in
         t_195.(! t_192) <- t_195.(i_203);
         t_195.(i_203) <- t_204
        else ();
        begin
         for j_205 = ((! t_192) + 1) to (t_197 - 1) do
          if (not ((t_195.(j_205)).(! t_193) = 0)) then begin
           for j_206 = ((! t_193) + 1) to (t_196 - 1) do
            (t_195.(j_205)).(j_206) <-
             ((((t_195.(j_205)).(j_206) * (t_195.(! t_192)).(! t_193)) -
                ((t_195.(! t_192)).(j_206) * (t_195.(j_205)).(! t_192))) /
               (! t_198))
           done;
           (t_195.(j_205)).(! t_193) <- 0
          end else ()
         done;
         (t_198 := (t_195.(! t_192)).(! t_193))
        end;
        (t_192 := ((! t_192) + 1))
     | None -> (t_199 := 0));
    (t_193 := ((! t_193) + 1))
   done;
   (t_195, ((! t_199) * (! t_198)), (! t_192))>.
# val resIV1 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutJustMatrix(Funct4.IVectorContainer)(Funct4.NoDet(Funct4.IntegerDomain)).res)
  code =
  .<fun a_207 ->
   let t_208 = (ref 0) in
   let t_209 = (ref 0) in
   let t_210 = {arr = (Array.copy a_207.arr)} (a_207) in
   let t_211 = a_207.m in
   let t_212 = a_207.n in
   let t_213 = (ref 1) in
   let t_214 = (ref 1) in
   while (((! t_209) < t_211) && ((! t_208) < t_212)) do
    let t_215 = (ref (-1)) in
    let t_217 =
     begin
      for j_216 = (! t_208) to (t_212 - 1) do
       if (not ((t_210.arr).((j_216 * t_210.n) + (! t_209)) = 0)) then
        if (((! t_215) == (-1)) ||
             ((abs (t_210.arr).((j_216 * t_210.n) + (! t_209))) <
               (abs (t_210.arr).(((! t_215) * t_210.n) + (! t_209))))) then
         (t_215 := j_216)
        else ()
       else ()
      done;
      if ((! t_215) == (-1)) then (None) else (Some (! t_215))
     end in
    (match t_217 with
     | Some (i_218) ->
        if (i_218 <> (! t_208)) then
         let a_219 = t_210.arr
         and n_220 = t_210.n
         and m_221 = t_210.m in
         let i1_222 = ((! t_208) * n_220)
         and i2_223 = (i_218 * n_220) in
         for i_224 = 0 to (m_221 - 1) do
          let t_225 = a_219.(i1_222 + i_224) in
          a_219.(i2_223 + i_224) <- a_219.(i1_222 + i_224);
          a_219.(i1_222 + i_224) <- t_225
         done
        else ();
        begin
         for j_226 = ((! t_208) + 1) to (t_212 - 1) do
          if (not ((t_210.arr).((j_226 * t_210.n) + (! t_209)) = 0)) then begin
           for j_227 = ((! t_209) + 1) to (t_211 - 1) do
            (t_210.arr).((j_226 * t_210.n) + j_227) <-
             ((((t_210.arr).((j_226 * t_210.n) + j_227) *
                 (t_210.arr).(((! t_208) * t_210.n) + (! t_209))) -
                ((t_210.arr).(((! t_208) * t_210.n) + j_227) *
                  (t_210.arr).((j_226 * t_210.n) + (! t_208)))) / (! t_213))
           done;
           (t_210.arr).((j_226 * t_210.n) + (! t_209)) <- 0
          end else ()
         done;
         (t_213 := (t_210.arr).(((! t_208) * t_210.n) + (! t_209)))
        end;
        (t_208 := ((! t_208) + 1))
     | None -> (t_214 := 0));
    (t_209 := ((! t_209) + 1))
   done;
   t_210>.
# val resIV2 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutDet(Funct4.IVectorContainer)(Funct4.IDet).res)
  code =
  .<fun a_228 ->
   let t_229 = (ref 0) in
   let t_230 = (ref 0) in
   let t_231 = {arr = (Array.copy a_228.arr)} (a_228) in
   let t_232 = a_228.m in
   let t_233 = a_228.n in
   let t_234 = (ref 1) in
   let t_235 = (ref 1) in
   while (((! t_230) < t_232) && ((! t_229) < t_233)) do
    let t_236 = (ref (-1)) in
    let t_238 =
     begin
      for j_237 = (! t_229) to (t_233 - 1) do
       if (not ((t_231.arr).((j_237 * t_231.n) + (! t_230)) = 0)) then
        if (((! t_236) == (-1)) ||
             ((abs (t_231.arr).((j_237 * t_231.n) + (! t_230))) <
               (abs (t_231.arr).(((! t_236) * t_231.n) + (! t_230))))) then
         (t_236 := j_237)
        else ()
       else ()
      done;
      if ((! t_236) == (-1)) then (None) else (Some (! t_236))
     end in
    (match t_238 with
     | Some (i_239) ->
        if (i_239 <> (! t_229)) then
         let a_240 = t_231.arr
         and n_241 = t_231.n
         and m_242 = t_231.m in
         let i1_243 = ((! t_229) * n_241)
         and i2_244 = (i_239 * n_241) in
         for i_245 = 0 to (m_242 - 1) do
          let t_246 = a_240.(i1_243 + i_245) in
          a_240.(i2_244 + i_245) <- a_240.(i1_243 + i_245);
          a_240.(i1_243 + i_245) <- t_246
         done
        else ();
        begin
         for j_247 = ((! t_229) + 1) to (t_233 - 1) do
          if (not ((t_231.arr).((j_247 * t_231.n) + (! t_230)) = 0)) then begin
           for j_248 = ((! t_230) + 1) to (t_232 - 1) do
            (t_231.arr).((j_247 * t_231.n) + j_248) <-
             ((((t_231.arr).((j_247 * t_231.n) + j_248) *
                 (t_231.arr).(((! t_229) * t_231.n) + (! t_230))) -
                ((t_231.arr).(((! t_229) * t_231.n) + j_248) *
                  (t_231.arr).((j_247 * t_231.n) + (! t_229)))) / (! t_234))
           done;
           (t_231.arr).((j_247 * t_231.n) + (! t_230)) <- 0
          end else ()
         done;
         (t_234 := (t_231.arr).(((! t_229) * t_231.n) + (! t_230)))
        end;
        (t_229 := ((! t_229) + 1))
     | None -> (t_235 := 0));
    (t_230 := ((! t_230) + 1))
   done;
   (t_231, ((! t_235) * (! t_234)))>.
# val resIV3 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutRank(Funct4.IVectorContainer)(Funct4.Rank)(Funct4.IntegerDomain).res)
  code =
  .<fun a_249 ->
   let t_250 = (ref 0) in
   let t_251 = (ref 0) in
   let t_252 = {arr = (Array.copy a_249.arr)} (a_249) in
   let t_253 = a_249.m in
   let t_254 = a_249.n in
   let t_255 = (ref 1) in
   let t_256 = (ref 1) in
   while (((! t_251) < t_253) && ((! t_250) < t_254)) do
    let t_257 = (ref (-1)) in
    let t_259 =
     begin
      for j_258 = (! t_250) to (t_254 - 1) do
       if (not ((t_252.arr).((j_258 * t_252.n) + (! t_251)) = 0)) then
        if (((! t_257) == (-1)) ||
             ((abs (t_252.arr).((j_258 * t_252.n) + (! t_251))) <
               (abs (t_252.arr).(((! t_257) * t_252.n) + (! t_251))))) then
         (t_257 := j_258)
        else ()
       else ()
      done;
      if ((! t_257) == (-1)) then (None) else (Some (! t_257))
     end in
    (match t_259 with
     | Some (i_260) ->
        if (i_260 <> (! t_250)) then
         let a_261 = t_252.arr
         and n_262 = t_252.n
         and m_263 = t_252.m in
         let i1_264 = ((! t_250) * n_262)
         and i2_265 = (i_260 * n_262) in
         for i_266 = 0 to (m_263 - 1) do
          let t_267 = a_261.(i1_264 + i_266) in
          a_261.(i2_265 + i_266) <- a_261.(i1_264 + i_266);
          a_261.(i1_264 + i_266) <- t_267
         done
        else ();
        begin
         for j_268 = ((! t_250) + 1) to (t_254 - 1) do
          if (not ((t_252.arr).((j_268 * t_252.n) + (! t_251)) = 0)) then begin
           for j_269 = ((! t_251) + 1) to (t_253 - 1) do
            (t_252.arr).((j_268 * t_252.n) + j_269) <-
             ((((t_252.arr).((j_268 * t_252.n) + j_269) *
                 (t_252.arr).(((! t_250) * t_252.n) + (! t_251))) -
                ((t_252.arr).(((! t_250) * t_252.n) + j_269) *
                  (t_252.arr).((j_268 * t_252.n) + (! t_250)))) / (! t_255))
           done;
           (t_252.arr).((j_268 * t_252.n) + (! t_251)) <- 0
          end else ()
         done;
         (t_255 := (t_252.arr).(((! t_250) * t_252.n) + (! t_251)))
        end;
        (t_250 := ((! t_250) + 1))
     | None -> (t_256 := 0));
    (t_251 := ((! t_251) + 1))
   done;
   (t_252, (! t_250))>.
# val resIV4 :
  ('a,
   Funct4.IVectorContainer.contr ->
   Funct4.OutDetRank(Funct4.IVectorContainer)(Funct4.IDet)(Funct4.Rank).res)
  code =
  .<fun a_270 ->
   let t_271 = (ref 0) in
   let t_272 = (ref 0) in
   let t_273 = {arr = (Array.copy a_270.arr)} (a_270) in
   let t_274 = a_270.m in
   let t_275 = a_270.n in
   let t_276 = (ref 1) in
   let t_277 = (ref 1) in
   while (((! t_272) < t_274) && ((! t_271) < t_275)) do
    let t_278 = (ref (-1)) in
    let t_280 =
     begin
      for j_279 = (! t_271) to (t_275 - 1) do
       if (not ((t_273.arr).((j_279 * t_273.n) + (! t_272)) = 0)) then
        if (((! t_278) == (-1)) ||
             ((abs (t_273.arr).((j_279 * t_273.n) + (! t_272))) <
               (abs (t_273.arr).(((! t_278) * t_273.n) + (! t_272))))) then
         (t_278 := j_279)
        else ()
       else ()
      done;
      if ((! t_278) == (-1)) then (None) else (Some (! t_278))
     end in
    (match t_280 with
     | Some (i_281) ->
        if (i_281 <> (! t_271)) then
         let a_282 = t_273.arr
         and n_283 = t_273.n
         and m_284 = t_273.m in
         let i1_285 = ((! t_271) * n_283)
         and i2_286 = (i_281 * n_283) in
         for i_287 = 0 to (m_284 - 1) do
          let t_288 = a_282.(i1_285 + i_287) in
          a_282.(i2_286 + i_287) <- a_282.(i1_285 + i_287);
          a_282.(i1_285 + i_287) <- t_288
         done
        else ();
        begin
         for j_289 = ((! t_271) + 1) to (t_275 - 1) do
          if (not ((t_273.arr).((j_289 * t_273.n) + (! t_272)) = 0)) then begin
           for j_290 = ((! t_272) + 1) to (t_274 - 1) do
            (t_273.arr).((j_289 * t_273.n) + j_290) <-
             ((((t_273.arr).((j_289 * t_273.n) + j_290) *
                 (t_273.arr).(((! t_271) * t_273.n) + (! t_272))) -
                ((t_273.arr).(((! t_271) * t_273.n) + j_290) *
                  (t_273.arr).((j_289 * t_273.n) + (! t_271)))) / (! t_276))
           done;
           (t_273.arr).((j_289 * t_273.n) + (! t_272)) <- 0
          end else ()
         done;
         (t_276 := (t_273.arr).(((! t_271) * t_273.n) + (! t_272)))
        end;
        (t_271 := ((! t_271) + 1))
     | None -> (t_277 := 0));
    (t_272 := ((! t_272) + 1))
   done;
   (t_273, ((! t_277) * (! t_276)), (! t_271))>.
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
  [([|[|1|]|], 1); ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]|], 50);
   ([|[|1; 2; 3; 0|]; [|0; 5; -7; 0|]; [|0; 0; 50; 0|]|], 50);
   ([|[|1; 2; 3|]; [|0; 5; -7|]; [|0; 0; 50|]; [|0; 0; 0|]|], 50);
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
#   val fa5 : Funct4.FArrayContainer.obj array array list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|4.; 13.; 5.; 0.|]; [|-1.; 3.; 0.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|4.; 13.; 5.|]; [|-1.; 3.; 0.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 13.; 5.|]; [|0.; 3.; 0.|]|]]
val resF1 :
  Funct4.OutJustMatrix(Funct4.FArrayContainer)(Funct4.NoDet(Funct4.FloatDomain)).res
  list =
  [[|[|1.|]|]; [|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]|];
   [|[|1.; 2.; 3.; 0.|]; [|0.; -5.; 7.; 0.|]; [|0.; 0.; 10.; 0.|]|];
   [|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]; [|0.; 0.; 0.|]|];
   [|[|0.; 2.; 3.|]; [|0.; 0.; 4.5|]; [|0.; 0.; 0.|]|]]
# val resF2 : Funct4.OutDet(Funct4.FArrayContainer)(Funct4.FDet).res list =
  [([|[|1.|]|], 1.);
   ([|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]|], -50.);
   ([|[|1.; 2.; 3.; 0.|]; [|0.; -5.; 7.; 0.|]; [|0.; 0.; 10.; 0.|]|], -50.);
   ([|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]; [|0.; 0.; 0.|]|],
    -50.);
   ([|[|0.; 2.; 3.|]; [|0.; 0.; 4.5|]; [|0.; 0.; 0.|]|], 0.)]
# val resF3 :
  Funct4.OutRank(Funct4.FArrayContainer)(Funct4.Rank)(Funct4.FloatDomain).res
  list =
  [([|[|1.|]|], 1);
   ([|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]|], 3);
   ([|[|1.; 2.; 3.; 0.|]; [|0.; -5.; 7.; 0.|]; [|0.; 0.; 10.; 0.|]|], 3);
   ([|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]; [|0.; 0.; 0.|]|], 3);
   ([|[|0.; 2.; 3.|]; [|0.; 0.; 4.5|]; [|0.; 0.; 0.|]|], 2)]
# val resF4 :
  Funct4.OutDetRank(Funct4.FArrayContainer)(Funct4.FDet)(Funct4.Rank).res
  list =
  [([|[|1.|]|], 1., 1);
   ([|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]|], -50., 3);
   ([|[|1.; 2.; 3.; 0.|]; [|0.; -5.; 7.; 0.|]; [|0.; 0.; 10.; 0.|]|], -50.,
    3);
   ([|[|1.; 2.; 3.|]; [|0.; -5.; 7.|]; [|0.; 0.; 10.|]; [|0.; 0.; 0.|]|],
    -50., 3);
   ([|[|0.; 2.; 3.|]; [|0.; 0.; 4.5|]; [|0.; 0.; 0.|]|], 0., 2)]
# 
