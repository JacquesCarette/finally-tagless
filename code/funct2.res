        MetaOCaml version 3.08.0 alpha 015

# - : ('a, Funct2.Domain.v -> Funct2.NoDetOUTPUT(Funct2.Domain).res) code =
.<fun a_1 -> let t_2 = 0 in let t_3 = 1 in ((t_2 + t_3) + a_1)>.
# - : ('a, Funct2.Domain.v -> Funct2.DetOUTPUT(Funct2.Domain).res) code =
.<fun a_1 ->
   let det_2 = (ref 1) in
   let t_3 = 0 in
   let t_4 = 1 in
   (det_2 := ((! det_2) + t_3));
   (((t_3 + t_4) + a_1), (! det_2))>.
# 
