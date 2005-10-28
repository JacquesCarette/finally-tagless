(* Delimited continuations: multi-prompt shift/reset with
   the polymorphic answertype and variously typed prompts.

  This code uses neither existentials nor universals, and relies on
  NO unsafe operations like Obj.magic.

  This code was a part of a constructive proof that if 
  reference cells are available, then the single-prompt shift/reset
  without polymoprhic answertype is sufficient to implement
  muplti-prompt shift/reset with the polymoprhic answertype --
  while fully preserving strong typing.
  In other words, if a type system is strong enough to support
  reference cells, it shall support multi-prompt delimited continuations.

  So, given typed delimited continuations one can get typed
  reference cells, and vice versa -- preserving the typing. This code
  proves the vice versa direction.

  This code is given with no explanations whatsoever. There is no
  published explanation. The one that exists is considered ``terse''
  by one of the co-authors. We are planning to submit the technique
  to a suitable venue. We are not sure which.
*)

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val (>>=) : 'a m -> ('a -> 'a m) -> 'a m
end ;;


(* Haskell's Cont monad _without_ the answer-type polymorphism *)

module CONT = struct
  (* type 'a m = {cont: 'w . ('a -> 'w) -> 'w } *)
  type ('w,'a) m = {cont: ('a -> 'w) -> 'w }
  let return x = {cont = fun k -> k x}
  let (>>=) m f = {cont = fun k -> m.cont (fun x -> (f x).cont k) }
end;;


open CONT

let id x = x
let (>>) m1 m2 = m1 >>= (fun _ -> m2)
;;


(* From  http://www.haskell.org/hawiki/MonadCont 

reset :: (Monad m) => ContT a m a -> ContT r m a
reset e = ContT $ \k -> runContT e return >>= k

shift :: (Monad m) =>
	 ((a -> ContT r m b) -> ContT b m b) 
          -> ContT b m a
shift e = ContT $ \k -> 
            runContT (e $ \v -> ContT $ \c -> k v >>= c) return

*)

let reset e = {cont = fun k -> k (e.cont id)}
let shift e = {cont = fun k -> 
   (e (fun v -> {cont = fun c -> c (k v)})).cont id}
;;


(* reading and writing of references lifted to the Cont monad *)
let lget p = {cont = fun k -> k !p}
let lset p v = {cont = fun k -> (p := v; k ())}
let lnewref x = {cont = fun k -> k (ref x)}
let fmap f m = {cont = fun k -> m.cont (fun v -> k (f v))}
;;


(* Our prompts and their primitive operations *)
type 'a promptFP = PromptFP of (bool * (unit -> 'a)) ref

let set'prompt (PromptFP p) v = lset p (false, fun () -> v)
let get'prompt (PromptFP p) = fmap (fun x -> snd x ()) (lget p)
let check'prompt (PromptFP p) 
    = lget p >>= (fun (mark,v) ->
                   if mark then (lset p (false, v) >> return true)
                   else return false)
let set'mark (PromptFP p) = lget p >>= (fun (mark,v) -> lset p (true,v))
;;


type 'a cc1 = (unit,'a) CONT.m 
type univs = unit
type hfp = HFP of (univs cc1 -> hfp cc1) * 
                  ((univs cc1 -> univs cc1) -> univs cc1)
         | HVFP of univs
type promptF0 = hfp ref
;;

(* Object-level single-prompt shift-reset with enough polymorphism *)
let oReset (p0:promptF0) m = reset (m >>= lset p0) >> lget p0
let oShift (p0:promptF0) f =
   shift (fun g -> f (fun v -> (g v) >> lget p0) >>= lset p0) >>= id
;;

let compose f g = fun x -> f (g x)
;;

let rec 
  pushPFP' p0 p m = hrStopF p0 p 
		   (oReset p0 (m >>= (fun x -> return (HVFP x))))
 and
  hrStopF p0 p m = m >>= hrStopF' p0 p
 and 
  hrStopF' p0 p = function (HVFP v) -> return v 
  | HFP (f, c) ->
   let handle = pushPFP' p0 p (c (compose (hrStopF p0 p) f)) 
   and relay = oShift p0 
               (fun g -> return 
                           (HFP ((compose g (compose (hrStopF p0 p) f)), c)))
   in
   check'prompt p >>= fun v -> if v then handle else relay
;;

let shiftFP' p0 p f = 
     (lnewref (fun () -> failwith "undefined")) >>=
     (fun ans ->
       let f' k =
          let k1 m = (k (m >>= (fun v -> lset ans (fun () -> v))))
                     >> get'prompt p
          in (f k1) >>= set'prompt p
       in
       (set'mark p) >> 
       (oShift p0 (fun k -> return (HFP (k, f')))) >>
       (lget ans >>= (fun vc -> return (vc ()))))
;;

module CCFP = struct
   type 'a m = 'a cc1
   let the'p0 = ref (HVFP ())(* Just avoid the Reader monad and use one P0 *)
   let return x = CONT.return x
   let (>>=) m f = CONT.(>>=) m f
   let run m =
       let ans = ref (fun () -> failwith "no prompt set") in
       let () = m.cont (fun v -> ans := (fun () -> v)) in
       !ans ()

   (* newPrompt *)
   let newPFP () = CONT.(>>=) 
                 (lnewref (false, fun () -> failwith "undefined"))
                 (fun x -> CONT.return (PromptFP x))

   let pushPFP p m = pushPFP' the'p0 p (m >>= set'prompt p) >> get'prompt p

   let shiftFP p f = shiftFP' the'p0 p f
end
;;

(*----------------------------------------------------------------------*)
(* Tests *)

open CCFP

let lplus x y = return (x + y)
let newP () = newPFP ()
let shiftP p e = shiftFP p e
let pushP p m = pushPFP p m
let abortP p e = shiftP p (fun _ -> e)
;;


let test1 = CCFP.run (return 1 >>= lplus 4)
;;
(* 5 *)

let test2 = CCFP.run (
  newP () >>= (fun p ->
    pushP p (pushP p (return 5)) >>= lplus 4))
;;
(* 9 *)

let test3 = CCFP.run (
  newP () >>= (fun p ->
    (pushP p (abortP p (return 5) >>= lplus 6)) >>= lplus 4))
;;
(* 9 *)

let test3' = CCFP.run (
  newP () >>= (fun p ->
    (pushP p (pushP p (abortP p (return 5) >>= lplus 6))) >>= lplus 4))
;;
(* 9 *)

let test3' = CCFP.run (
  newP () >>= (fun p ->
   (pushP p (
         (pushP p (abortP p (return 5) >>= lplus 6)) >>=
         (fun v1 -> abortP p (return 7)) >>= lplus 10))
   >>= lplus 20))
;;
(* 27 *)

let test3'' = 
 try
 CCFP.run (
  newP () >>= (fun p ->
   (pushP p (
         (pushP p (abortP p (return 5) >>= lplus 6)) >>=
         (fun v1 -> abortP p (return 7)) >>= lplus 10))
   >>= (fun _ -> abortP p (return 9)) >>= lplus 20))
 with Failure a -> print_string a; 0
;;
(* Must be an error *)


(* The Standard shift test
 (display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
 ; --> 117
*)

let test5 = CCFP.run (
  newP () >>= (fun p0 ->
  newP () >>= (fun p1 ->
  (pushP p0 (
    (shiftP p0 (fun sk -> sk (sk (return 3)) >>= lplus 100)) >>= lplus 2))
  >>= lplus 10)))
;;
(* 117 *)

let test5' = CCFP.run (
  newP () >>= (fun p0 ->
  newP () >>= (fun p1 ->
  (pushP p0 (
    (shiftP p0 (fun sk -> (sk (return 3)) >>= lplus 100)) >>= lplus 2))
  >>= lplus 10)))
;;
(* 115 *)

let test5'' = CCFP.run (
  newP () >>= (fun p0 ->
  newP () >>= (fun p1 ->
  (pushP p0 (
    (shiftP p0 (fun sk -> sk (pushP p1 (sk (abortP p1 (return 3))))
       >>= lplus 100)) >>= lplus 2))
  >>= lplus 10)))
;;
(* 115 *)

let test6 = CCFP.run (
  newP () >>= (fun p1 ->
  newP () >>= (fun p2 ->
  let pushtwice sk = sk (sk (return 3)) in
  (pushP p1 (pushP p2 (shiftP p1 pushtwice) >>= lplus 1) >>= lplus 10))))
;;

(* 15 *)

(* The most stringent test: capturing prompts in subcontinuations:
   or, overlapping control regions
 *)

let test7 = CCFP.run (
  newP () >>= (fun p1 ->
  newP () >>= (fun p2 ->
  newP () >>= (fun p3 ->
  let pushtwice sk = sk (sk (shiftP p2 (fun sk2 -> sk2 (sk2 (return 3))))) in
  (pushP p1 ((pushP p2 ((pushP p3 (shiftP p1 pushtwice)) >>= lplus 10))
             >>= lplus 1))
  >>= lplus 100))))
;;
(* 135 *)

(*
 (reset (let ((x (shift f (cons 'a (f '()))))) (shift g x))))
 ==> '(a)
*)

let testls = CCFP.run (
  newP () >>= (fun p ->
    pushP p (
       let x = shiftP p (fun f -> f (return []) >>= (fun t -> return ("a"::t)))
       in x >>= (fun xv -> shiftP p (fun _ -> return xv)))))
;;
		
(* ["a"] *)

