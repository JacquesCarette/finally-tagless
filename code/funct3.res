        Objective Caml version 3.09.1

# - : ('a, Funct3.Domain.v -> Funct3.NoDetOUTPUT(Funct3.Domain).res) code =
.<fun a_1 -> let t_2 = 0 in let t_3 = 1 in ((t_2 + t_3) + a_1)>.
# - : ('a, Funct3.Domain.v -> Funct3.DetOUTPUT(Funct3.Domain).res) code =
.<fun a_1 ->
   let t_2 = (ref 1) in
   let t_3 = 0 in
   let t_4 = 1 in (t_2 := ((! t_2) + t_3)); (((t_3 + t_4) + a_1), (! t_2))>.
# 
