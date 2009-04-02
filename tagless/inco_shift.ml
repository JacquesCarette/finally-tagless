(* Interpreter and compiler for Hindley-Milner lambda-calculus
   with shift and reset: the system of Asai, Kameyama APLAS 2007
   paper.
*)

module type Symantics = sig
  type ('i,'o,'v) repr		(* expressions, generally effectful *)
  type 'v v			(* pure values *)
  val pure : 'v v -> ('w,'w,'v) repr
  val int' : int  -> int v
  val bool': bool -> bool v
  val str' : string -> string v

  val add : ('w,'w,int -> ('w1,'w1,int -> ('w2,'w2,int) repr) repr) repr

  val nil   : ('w,'w,'v list) repr
  val cons  : ('w,'w,'v -> 
                ('w1,'w1,'v list -> 
		  ('w2,'w2,'v list) repr) repr) repr
  val decon : ('i1,'o,'v list) repr ->	(* compare with if_ *)
              (unit -> ('i2,'i1,'b) repr) ->
	      ('v v -> 'v list v -> ('i2,'i1,'b) repr) ->
	      ('i2,'o,'b) repr

  val reset : (unit -> ('v,'o,'v) repr) -> ('w,'w,'o) repr
  val shift : (('a v -> 'i v) -> ('c,'o,'c) repr) -> ('i,'o,'a) repr

  (* mul, leq *)
  val if_ : ('i1,'o1,bool) repr ->
             (unit -> ('i2,'i1,'v) repr) ->
             (unit -> ('i2,'i1,'v) repr) ->
             ('i2,'o1,'v) repr

  val lam' : ('a v -> ('i,'o,'b) repr) -> ('a -> ('i,'o,'b) repr) v
  val (%) : ('i1,'o,'a -> ('i3,'i2,'b) repr) repr -> 
            ('i2,'i1,'a) repr ->
	    ('i3,'o,'b) repr
  val fix' : (('a -> ('i,'o,'b) repr) v -> 'a v -> ('i,'o,'b) repr) -> 
             ('a -> ('i,'o,'b) repr) v
end
;;

module Sugar(S:Symantics) = struct
  include S
  let int  x = pure (int' x)
  let bool x = pure (bool' x)
  let str x  = pure (str' x)
  let lam  x = pure (lam' x)
  let fix  x = pure (fix' x)

  let appk k x = lam (fun v -> pure (k v)) % x
  (* sample lists *)
  let l123 () = cons % (int 1) % (cons % (int 2) % (cons % (int 3) % nil))
  let l45  () = cons % (int 4) % (cons % (int 5) % nil)
end;;

(* The first example: list append, Sec 2.1 of Asai, Kameyama
   The example demonstrates answer-type modification
*)

(* Thunks below are for the sake of the value restriction... *)

module EX1(S:Symantics) = struct
  module SU = Sugar(S)
  open SU

  let append () = 
    fix (fun self l ->
      decon (pure l) 
	(fun () -> shift (fun k -> lam (fun v -> pure (k v))))
	(fun h t -> cons % (pure h) % (pure self % (pure t))))

  let append123 () = reset (fun () -> append () % l123 ())
  let result = append123 () % l45 ()
end;;

(* inferred type of append:
      val append :
        unit ->
        ('a, 'a,
         'b list -> ('c, 'b list -> ('d, 'd, 'c) SU.repr, 'b list) SU.repr)
        SU.repr
*)

(* The second example: variation on Danvy's visit, Sec 2.2 of Asai, Kameyama
   The example demonstrates answer-type polymorphism
*)

module EX2(S:Symantics) = struct
  module SU = Sugar(S)
  open SU

  let visit () = fix (fun self l ->
    decon (pure l)
      (fun () -> shift (fun h -> nil))
      (fun a rest -> cons % (pure a) % (shift (fun k ->
	cons % (appk k nil) % 
	       reset (fun () -> appk k (pure self % pure rest))))))
  let prefix () = lam (fun lst -> reset (fun () -> visit () % pure lst))
  let result = prefix () % l123 ()
end;;

(* Inferred types
      val visit :
        unit -> ('a, 'a, 'b list -> ('c, 'c list, 'b list) SU.repr) SU.repr
      val prefix :
        unit -> ('a, 'a, 'b list -> ('c, 'c, 'b list list) SU.repr) SU.repr
      val result : ('_a, '_a, int list list) SU.repr

*)

(* The printf-like example: Sec 2.3 of Asai, Kameyama. 
   To avoid introducing string concatenation and
   various conversions, we use integers instead of strings and
   addition instead of string concatenation
*)

module EXP(S:Symantics) = struct
  module SU = Sugar(S)
  open SU

  (* so-called conversion routines *)
  let fmt_bool () = lam (fun x -> 
                          if_ (pure x) (fun () -> int 0) (fun () -> int 1))

  let fmt_int () = lam (fun x -> pure x)

  let fmt () = lam (fun formatter -> 
    shift (fun k -> lam (fun x -> appk k (pure formatter % pure x))))

  let printi p = reset p  (* a `special' form in the object language *)

  let t1 = printi (fun () -> int 1101)  (* 1101 is sort of `Hello world!' *)
  let t2 = printi (fun () -> add % (int 1000) % 
                             (add % (fmt () % fmt_int ()) % int 1)) % int 100
  let t3 = printi (fun () -> add % (int 1000) % 
                             (add % (fmt () % fmt_int ()) %
			       (add % (int 5) %
				(fmt () % fmt_bool ()))))
      % (int 100) % (bool true)
end;;

(* Tagless typed interpreter. It uses CPS *)

module R  = struct
  type 'v v = 'v
  type ('i,'o,'v) repr = ('v -> 'i) -> 'o

  let pure v = fun k -> k v

  let int'  x = x
  let bool' x = x
  let str'  x = x

  let add k = k (fun e1 -> pure (fun e2 -> pure (e1 + e2)))

  let nil k = k []
  let cons k = k (fun x -> pure (fun l -> pure (x::l)))
  let decon e onempty onfull k = 
    e (fun l -> match l with
                | [] -> onempty () k
		| (h::t) -> onfull h t k)

  let reset e k = k (e () (fun x -> x))
  let shift e k = e k (fun x -> x)

  let if_ e et ef k = e (fun v -> if v then et () k else ef () k)

  let lam' f = f
  let (%) e1 e2 k = e1 (fun f -> e2 (fun v -> f v k))

  let fix' e = let rec self x = e self x in self 

  let get_res m = m (fun x -> x)
end;;

module M1R = EX1(R);;
let () = assert (R.get_res (M1R.result) = [1; 2; 3; 4; 5]);;

module M2R = EX2(R);;
let () = assert (R.get_res (M2R.result) = [[1]; [1; 2]; [1; 2; 3]]);;

module MPR = EXP(R);;
let () = assert (R.get_res (MPR.t1) = 1101);;
let () = assert (R.get_res (MPR.t2) = 1101);;
let () = assert (R.get_res (MPR.t3) = 1105);;


(* A different interpreter: compiler from the object language into Scheme *)

module C  = struct
  type 'v v = string
  type ('i,'o,'v) repr = string

  let gensym = let counter = ref 0 in
               fun () -> let v = !counter in incr counter; 
	                 "g" ^ string_of_int v

  let pure v = v

  let lam' f = let v = gensym () in
               "(lambda (" ^ v ^ ") " ^ f v ^ ")"
  let (%) e1 e2 = "(" ^ e1 ^ " " ^ e2 ^ ")"


  let int'  = string_of_int 
  let bool' x = if x then "#t" else "#f"
  let str'  x = failwith "na"

  let add = "(lambda (x) (lambda (y) (+ x y)))"

  let nil = "'()"
  let cons = "(lambda (x) (lambda (y) (cons x y)))"
  let decon e onempty onfull = 
     let v = gensym () in
    "(let ((" ^ v ^ " " ^ e ^ ")) " ^
      "(if (null? " ^ v ^ ")  " ^ (onempty ()) ^ " " ^
      onfull ("car" % v) ("cdr" % v) ^ "))"

  let reset e = "reset" % e ()
  let shift e = let k = gensym () in
   "(shift " ^ k ^ " " ^ e (fun v -> k % v) ^ ")"

  let if_ e et ef = 
    "(if " ^ e ^ " " ^ et () ^ " " ^ ef () ^ ")"

  let fix' e = 
    let self = gensym () in
    "(letrec ((" ^ self ^ " " ^ lam' (fun x -> e self x) ^ ")) " ^ self ^ ")"
end;;

module M1C = EX1(C);;
let test1c = M1C.result;;

module M2C = EX2(C);;
let test2c = M2C.result;;

(*
((lambda (g5) (reset ((letrec ((g6 (lambda (g7) (let ((g8 g7)) (if (null? g8)  (shift g12 '()) (((lambda (x) (lambda (y) (cons x y))) (car g8)) (shift g9 (((lambda (x) (lambda (y) (cons x y))) ((lambda (g11) (g9 g11)) '())) (reset ((lambda (g10) (g9 g10)) (g6 (cdr g8)))))))))))) g6) g5))) (((lambda (x) (lambda (y) (cons x y))) 1) (((lambda (x) (lambda (y) (cons x y))) 2) (((lambda (x) (lambda (y) (cons x y))) 3) '()))))

Petite Scheme evaluates this to
((1) (1 2) (1 2 3))
*)
