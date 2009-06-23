(* Automatic labeling of all sub-expressions (program points): the functor LAB
   Transforms a tagless final expression (a functor over Symantics)
   into the functor over Symantics with lab
*)

(* The same Symantics as in incope.ml *)
module type Symantics = sig
  type ('c,'sv,'dv) repr
  val int  : int  -> ('c,int,int) repr
  val bool : bool -> ('c,bool,bool) repr
  val add  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val mul  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr
  val leq  : ('c,int,int) repr -> ('c,int,int) repr -> ('c,bool,bool) repr
  val eql  : ('c,'sa,'da) repr -> ('c,'sa,'da) repr -> ('c,bool,bool) repr
  (* The last two arguments to [if_] are functional terms.
     One of them is applied to unit.
     The reason for this charade is to prevent evaluation
     of both arguments of if_ in a CBV language.
     if_ must be a syntactic form rather than just a function.
     Or, at least it shouldn't take just (undelayed) terms.
  *)
  val if_ : ('c,bool,bool) repr ->
             (unit -> 'x) ->
             (unit -> 'x) -> (('c,'sa,'da) repr as 'x)

  val lam : (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x) -> ('c,'x,'da->'db) repr
  val app : ('c,'x,'da->'db) repr -> (('c,'sa,'da) repr -> ('c,'sb,'db) repr as 'x)
  val fix : ('x -> 'x) -> (('c, ('c,'sa,'da) repr -> ('c,'sb,'db) repr, 'da->'db) repr as 'x)
end
;;

(* The Symantics with lab primitive to attach numeric labels to all code points *)

module type LSymantics = sig
  include Symantics
  val lab : int -> ('c,'sv,'dv) repr -> ('c,'sv,'dv) repr
end;;

(* The example, copied verbatim from incope.ml *)
module EX(S: Symantics) = struct
 open S

 let test1 () = add (int 1) (int 2)
 let test2 () = lam (fun x -> add x x)
 let test3 () = lam (fun x -> add (app x (int 1)) (int 2))

 let testgib () = lam (fun x -> lam (fun y ->
                  fix (fun self -> lam (fun n ->
                      if_ (leq n (int 0)) (fun () -> x)
                        (fun () ->
                          (if_ (leq n (int 1)) (fun () -> y)
                           (fun () ->
                             (add (app self (add n (int (-1))))
                                    (app self (add n (int (-2))))))))))))

 let testgib1 () = app (app (app (testgib ()) (int 1)) (int 1)) (int 5)
 let testgib2 () = lam (fun x -> (lam (fun y ->
   app (app (app (testgib ()) x) y) (int 5))))

 let testpowfix () = lam (fun x ->
                      fix (fun self -> lam (fun n ->
                        if_ (leq n (int 0)) (fun () -> int 1)
                            (fun () -> mul x (app self (add n (int (-1))))))))

 let testpowfix7 () = lam (fun x -> app (app (testpowfix ()) x) (int 7))
 let testpowfix0 () = lam (fun x -> app (app (testpowfix ()) (int 0)) x)

 let fact    () = 
   fix (fun self -> lam (fun n ->
     if_ (eql n (int 0))
           (fun () -> (int 1))
           (fun () -> (mul n (app self (add n (int (-1))))))))
 let testfact1   () = app (fact ()) (int 5)

 let fold zero succ = fix (fun self -> lam (fun i ->
     if_ (eql i (int 0))
         (fun () -> zero)
         (fun () -> app succ (app self (add i (int (-1)))))))
 let ack () = fold (lam (fun n -> add n (int 1)))
                    (lam (fun g -> fold (app g (int 1)) g))
 let testack1 () = app (ack ()) (int 2)
 let testack13 () = app (testack1 ()) (int 3)

 (* Ackermann's Higher-order (using Church numerals) *)
 let ackho () = lam (fun m -> lam (fun n -> 
   let succ = lam (fun n -> add n (int 1)) in
   app
     (app
        (app m (lam (fun g -> lam (fun n ->
          app (app n g) (app g (int 1))))))
        succ) n))

 let runit t = t () 

 let test1r () = runit test1
 let test2r () = runit test2
 let test3r () = runit test3

 let testgibr () = runit testgib
 let testgib1r () = runit testgib1
 let testgib2r () = runit testgib2

 let testpowfixr () = runit testpowfix
 let testpowfix7r () = runit testpowfix7
 let testpowfix0r () = runit testpowfix0

 let testfactr ()  = runit testfact1
 let testackr1 ()  = runit testack1
 let testackr13 () = runit testack13
end;;

(* Pure interpreter. It is essentially the identity transformer *)
(* Copied from incope.ml and added lab that does nothing.
   Used as a sample LSymantics so we can see the result of 
   the example.
*)
module R = struct
  type ('c,'sv,'dv) repr = 'dv    (* absolutely no wrappers *)
  let lab n x = x
  let int (x:int) = x
  let bool (b:bool) = b
  let add e1 e2 = e1 + e2
  let mul e1 e2 = e1 * e2
  let leq x y = x <= y
  let eql x y = x = y
  let if_ eb et ee = if eb then (et ()) else (ee ())

  let lam f = f
  let app e1 e2 = e1 e2
  let fix f = let rec self n = f self n in self
end;;

(* The labeller: LSymantics -> Symantics
   In the latter we set the 'c parameter (for code values) to unit
   Otherwise, we need one more type argument to repr.
   The absence of rank-2 types is jarring...
*)
module LAB(S: LSymantics) = struct
  type ('c,'sv,'dv) repr = ('c,(unit,'sv,'dv) S.repr) code

  let genlab = 
    let counter = ref 0 in
    fun () -> incr counter; let x = !counter in .<x>.

  let int  n = .<S.lab .~(genlab ()) (S.int n)>.
  let bool  b = .<S.lab .~(genlab ()) (S.bool b)>.
  let add e1 e2 = .<S.lab .~(genlab ()) (S.add .~e1 .~e2)>.
  let mul e1 e2 = .<S.lab .~(genlab ()) (S.mul .~e1 .~e2)>.
  let leq e1 e2 = .<S.lab .~(genlab ()) (S.leq .~e1 .~e2)>.
  let eql e1 e2 = .<S.lab .~(genlab ()) (S.eql .~e1 .~e2)>.

  let if_ t et ef = .<S.lab .~(genlab ()) 
      (S.if_ .~t (fun () -> .~(et ())) 
	         (fun () -> .~(ef ())))>.

  let lam f = .<S.lab .~(genlab ()) (S.lam (fun x -> .~(f .<x>.)))>.
  let app e1 e2 = .<S.lab .~(genlab ()) (S.app .~e1 .~e2)>.

  let fix f = .<S.lab .~(genlab ()) (S.fix (fun x -> .~(f .<x>.)))>.
end;;

module M = EX(LAB(R));;
M.test1r ();;
M.test2r ();;
M.test3r ();;
(*
.<(((* cross-stage persistent value (as id: S.lab) *)) 8
    (((* cross-stage persistent value (as id: S.lam) *))
      (fun x_1 ->
        (((* cross-stage persistent value (as id: S.lab) *)) 7
          (((* cross-stage persistent value (as id: S.add) *))
            (((* cross-stage persistent value (as id: S.lab) *)) 6
              (((* cross-stage persistent value (as id: S.app) *)) x_1
                (((* cross-stage persistent value (as id: S.lab) *)) 5
                  (((* cross-stage persistent value (as id: S.int) *)) 1))))
            (((* cross-stage persistent value (as id: S.lab) *)) 4
              (((* cross-stage persistent value (as id: S.int) *)) 2)))))))>.

*)

M.testgibr ();;
M.testgib1r ();;
M.testgib2r ();;

M.testpowfixr ();;
M.testpowfix7r ();;
M.testpowfix0r ();;

M.testfactr ();;
M.testackr1 ();;
(*
- : ('a, int, int) LAB(R).repr =
.<(((* cross-stage persistent value (as id: S.lab) *)) 157
    (((* cross-stage persistent value (as id: S.app) *))
      (((* cross-stage persistent value (as id: S.lab) *)) 156
        (((* cross-stage persistent value (as id: S.fix) *))
          (fun x_1 ->
            (((* cross-stage persistent value (as id: S.lab) *)) 155
              (((* cross-stage persistent value (as id: S.lam) *))
                (fun x_2 ->
                  (((* cross-stage persistent value (as id: S.lab) *)) 154
                    (((* cross-stage persistent value (as id: S.if_) *))
                      (((* cross-stage persistent value (as id: S.lab) *))
                        148
                        (((* cross-stage persistent value (as id: S.eql) *))
                          x_2
                          (((* cross-stage persistent value (as id: S.lab) *))
                            147
                            (((* cross-stage persistent value (as id: S.int) *))
                              0))))
                      (fun () ->
                        (((* cross-stage persistent value (as id: S.lab) *))
                          153
                          (((* cross-stage persistent value (as id: S.int) *))
                            1)))
                      (fun () ->
                        (((* cross-stage persistent value (as id: S.lab) *))
                          152
                          (((* cross-stage persistent value (as id: S.mul) *))
                            x_2
                            (((* cross-stage persistent value (as id: S.lab) *))
                              151
                              (((* cross-stage persistent value (as id: S.app) *))
                                x_1
                                (((* cross-stage persistent value (as id: S.lab) *))
                                  150
                                  (((* cross-stage persistent value (as id: S.add) *))
                                    x_2
                                    (((* cross-stage persistent value (as id: S.lab) *))
                                      149
                                      (((* cross-stage persistent value (as id: S.int) *))
                                        (-1))))))))))))))))))
      (((* cross-stage persistent value (as id: S.lab) *)) 146
        (((* cross-stage persistent value (as id: S.int) *)) 5))))>.
*)

M.testackr13 ();;


