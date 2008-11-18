(* name:          cc.ml
 * synopsis:      Delimited continuation monad
 *                shift/reset with multiple prompts
 * authors:       Oleg Kiselyov, Chung-chieh Shan, and Amr Sabry
 * last revision: Thu Nov 13 10:05:14 UTC 2008
 * ocaml version: 3.11
 *
 * Copyright (C) 2006-2008  O. Kiselyov, C.-c. Shan, A. Sabry
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


(** This code uses neither existentials nor universals, and relies on
    NO unsafe operations like Obj.magic.

    This code was a part of a constructive proof that if reference
    cells are available, then the single-prompt shift/reset without
    polymorphic answertype is sufficient to implement multi-prompt
    shift/reset with the polymorphic answertype -- while fully
    preserving strong typing.  In other words, if a type system is
    strong enough to support reference cells, it shall support
    multi-prompt delimited continuations.

    So, given typed delimited continuations one can get typed
    reference cells, and vice versa -- preserving the typing.  This
    code proves the vice versa direction.

    This code is given with no explanations whatsoever.  There is no
    published explanation.  The one that exists is considered
    ``terse'' by one of the co-authors.  We are planning to submit the
    technique to a suitable venue.  We are not sure which.

    This is joint work with Chung-chieh Shan and Amr Sabry. *)


(** Haskell's Cont monad _without_ the answer-type polymorphism. *)
module CONT =
struct
  (* type 'a m = {cont: 'w . ('a -> 'w) -> 'w} *)
  type ('w, 'a) mc = {cont: ('a -> 'w) -> 'w}
  let return x = {cont = fun k -> k x}
  let (>>=) m f = {cont = fun k -> m.cont (fun x -> (f x).cont k)}
end

open CONT
let (>>) m1 m2 = m1 >>= (fun _ -> m2)

let id x = x
let compose f g = fun x -> f (g x)


(* See also http://www.haskell.org/haskellwiki/MonadCont_done_right
 *
 *  reset :: (Monad m) => ContT a m a -> ContT r m a
 *  reset e = ContT $ \k -> runContT e return >>= k
 *
 *  shift :: (Monad m) => ((a -> ContT r m b) -> ContT b m b) -> ContT b m a
 *  shift e = ContT $ \k ->
 *              runContT (e $ \v -> ContT $ \c -> k v >>= c) return *)

let reset e = {cont = fun k -> k (e.cont id)}
let shift e = {cont = fun k ->
                 (e (fun v -> {cont = fun c -> c (k v)})).cont id}


(* Reading and writing of references lifted to the Cont monad *)
let lget p    = {cont = fun k -> k !p}
let lset p v  = {cont = fun k -> p := v; k ()}
let lnewref x = {cont = fun k -> k (ref x)}
let fmap f m  = {cont = fun k -> m.cont (fun v -> k (f v))}


(* Our prompts and their primitive operations *)
type 'a prompt = PromptFP of (bool * (unit -> 'a)) ref

let set'prompt (PromptFP p) v = lset p (false, fun () -> v)
let get'prompt (PromptFP p) = fmap (fun x -> snd x ()) (lget p)
let check'prompt (PromptFP p) =
  lget p >>= (fun (mark,v) ->
                if mark then lset p (false, v) >> return true
                else return false)
let set'mark (PromptFP p) = lget p >>= (fun (mark,v) -> lset p (true,v))


type 'a cc1 = (unit, 'a) CONT.mc
type univs = unit
type hfp =
    HFP of (univs cc1 -> hfp cc1) * ((univs cc1 -> univs cc1) -> univs cc1)
  | HVFP of univs
type promptF0 = hfp ref


(* Object-level single-prompt shift-reset with enough polymorphism *)
let oReset (p0: promptF0) m = reset (m >>= lset p0) >> lget p0
let oShift (p0: promptF0) f =
  shift (fun g -> f (fun v -> (g v) >> lget p0) >>= lset p0) >>= id


let rec pushPFP' p0 p m =
  hrStopF p0 p (oReset p0 (m >>= (fun x -> return (HVFP x))))
and hrStopF p0 p m = m >>= hrStopF' p0 p
and hrStopF' p0 p = function
    HVFP v -> return v
  | HFP (f, c) ->
      let handle = pushPFP' p0 p (c (compose (hrStopF p0 p) f))
      and relay =
        oShift p0 (fun g ->
                     return (HFP ((compose g (compose (hrStopF p0 p) f)), c)))
      in
        check'prompt p >>= fun v -> if v then handle else relay


let shiftFP' p0 p f =
  (lnewref (fun () -> failwith "undefined")) >>=
    (fun ans ->
       let f' k =
         let k1 m =
           (k (m >>= (fun v -> lset ans (fun () -> v)))) >> get'prompt p in
           (f k1) >>= set'prompt p
       in
         (set'mark p) >>
           (oShift p0 (fun k -> return (HFP (k, f')))) >>
           (lget ans >>= (fun vc -> return (vc ()))))



(* The global prompt P0 *)
let the'p0 = ref (HVFP ())


(* Exported types and operations *)
type 'a m = 'a cc1

(* Our return and bind are the same as those in the CONT monad *)
let return x = CONT.return x
let bind m f = CONT.(>>=) m f

let run m =
  let ans = ref (fun () -> failwith "no prompt set") in
  let () = m.cont (fun v -> ans := (fun () -> v)) in
    !ans ()

let new_prompt () =
  CONT.(>>=)
    (lnewref (false, fun () -> failwith "undefined"))
    (fun x -> CONT.return (PromptFP x))

let pushP p m = pushPFP' the'p0 p (m >>= set'prompt p) >> get'prompt p

let shiftP p f = shiftFP' the'p0 p f
