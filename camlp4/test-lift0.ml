module GIB (Num : NUM) = struct
  let gib =
    .<fun n x y ->
      let rec loop n =
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in loop n>.
end;;


let gib = fun n x y ->
      let rec loop n =
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in loop n
;;

let gib = fun n x y ->
      let loop self n =
	let loop x = self x in
          if n = 0 then x else
	  if n = 1 then y else
	  let r1 = loop (n-1) in
	  let r2 = loop (n-2) in
	  r1 + r2
      in let rec ym f n = f (ym f) n
      in let loop = ym loop 
      in loop n
;;

let testg = gib 8 1 1;;

module ENV1 = struct
  let ret x = x
  let bind m f = f m
  let rec ym f x = f (ym f) x
  let plus x y = x + y
end;;


let gib  =
    .<fun n x y ->
      ENV1.bind 
	   (fun self n -> 
	     let loop x = self x in
	     if n = 0 then (ENV1.ret x) else
	     if n = 1 then (ENV1.ret y) else
	     ENV1.bind (loop (n-1)) (fun r1 ->
	     ENV1.bind (loop (n-2)) (fun r2 ->
	       (ENV1.plus r1 r2))))
	   (fun loop ->
	     let loop = ENV1.ym loop in loop n)
	>.
;;

let testg = (.!gib) 8 1 1;;

module ENV2 = struct
  let ret x = x
  let bind m f = f m
  let ym fc = .<let rec ym f x = f (ym f) x in ym .~fc>.
  let plus x y = .<.~x + .~y>.
  let fif c t e = .<if .~c then .~t else .~e>.
end;;

let gib  =
    .<fun n x y ->
      .~(ENV2.bind 
	   (.<fun self n -> 
	     let loop x = self x in
	     .~(ENV2.fif .<n = 0>. (ENV2.ret .<x>.)
	       (ENV2.fif .<n = 1>. (ENV2.ret .<y>.)
	       (ENV2.bind .<loop (n-1)>. (fun r1 ->
		.<let r1 = .~r1 in
	        .~(ENV2.bind .<loop (n-2)>. (fun r2 ->
		  .<let r2 = .~r2 in 
	           .~(ENV2.plus .<r1>. .<r2>.)>.))>.))))>.)
	   (fun loop ->
	     .<let loop = .~loop in let loop = 
	       .~(ENV2.ym .<loop>.) in loop n>.))>.
	
;;

let testg = (.!gib) 8 1 1;;

