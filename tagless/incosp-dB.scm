; Interpreter, Compiler, Self-interpreter, Specializer
; The `tagless-final' style, deBruijn indices

; The object language is the untyped lambda-calculus with fixpoint,
; integers and the conditional:
;
;  Var | Lam | App e e | Fix |
;  I Int | Add ie1 ie2 | Mul ie1 ie2
;  IFZ b e-then e-else
;  
;  Var ::= VZ | VS Var
;
; The language is just expressive enough for the Gibonacci and
; power functions, and to write the self-interpreter.
; To avoid cheating and confusing the meta-language (which is Scheme)
; with the object language, we deliberately defined the object
; language with deBruijn indices.

; All the terms in our language have the following form
; (lambda-c (vz vs int add mul ifz lam app fix) body)
; where (lambda-c (...) body) is the curried lambda-abstraction

; Two helper macros to make defining and applying curried functions easier
(define-syntax lambda-c
 (syntax-rules ()
   ((lambda-c () body) body)
   ((lambda-c (v . vs) body) (lambda (v) (lambda-c vs body)))))

(define-syntax @c
  (syntax-rules ()
    ((@c f) f)
    ((@c f arg . args) (@c (f arg) . args))))

; A useful helper
(define (cout . args) (for-each display args)) 
(define nl #\newline)


; the input to "spec" is annotated -- it distinguishes
; variables, applications, and lambdas that are static from those that are
; dynamic -- but the output is unannotated and can be read back using
; "unparse1". 

(define (spec t) ; specializer
  (let
     (; Church-encoding of pairs
      (rfst (lambda-c (a b) a))
      (rsnd (lambda-c (a b) b))
      (rcons (lambda-c (a b sel) (@c sel a b))))
    (let ((env0 (lambda (x) x))
          (Zs (lambda-c (env) (env rfst)))
          (Ss (lambda-c (v env) (v (env rsnd))))
          (Zd (lambda-c (env) (lambda-c (Z S A L) Z)))
          (Sd (lambda-c (e env) (lambda-c (Z S A L) (S (@c e env Z S A L)))))
          (As (lambda-c (e1 e2 env) ((e1 env) (e2 env))))
          (Ls (lambda-c (e env x) (e (@c rcons x env))))
          (Ad (lambda-c (e1 e2 env Z S A L) (@c A (@c e1 env Z S A L) 
	                                          (@c e2 env Z S A L))))
          (Ld (lambda-c (e env Z S A L) (@c L (@c e env Z S A L)))))
      (@c t Zs Ss Zd Sd As Ls Ad Ld env0))))



(define (unparse1 t) ; interpret a one-level term into an S-expression
  (@c t
      'Z
      (lambda-c (e1) `(S ,e1))
      (lambda-c (e1 e2) `(A ,e1 ,e2))
      (lambda-c (e1) `(L ,e1))))

;; Evaluating this Scheme expression specializes
;;     lambda_s x. lambda_d y. y @d x
;; to the argument
;;     lambda z. z
;; yielding
;;     lambda y. y @ (lambda z. z)
;; represented as
;;     (l (a z (l z)))

(cout "test1: " 
  (unparse1 ((spec (lambda-c (Zs Ss Zd Sd As Ls Ad Ld)
		     (Ls (Ld (@c Ad Zd Zs)))))
	      (lambda-c (Z S A L) (L Z))))
  nl)

;; Alas, this specializer still doesn't correctly handle mixing static and 
;; dynamic bindings.  Here's a counterexample:

(cout "test2: "
  (unparse1 (spec (lambda-c (Zs Ss Zd Sd As Ls Ad Ld)
		    (Ld (@c As (Ls (Ld Zs)) Zd)))))
  nl)

;; Evaluating this Scheme expression is supposed to specialize
;;     lambda_d x. ((lambda_s y. (lambda_d z. y)) @s x)
;; but the result is
;;     lambda x. lambda z. z
;; represented as
;;     (l (l z))
;; rather than
;;     lambda x. lambda z. x
;; represented as
;;     (l (l (s z)))


; A slightly better example
;;     lambda_d x. ((lambda_s y. (lambda_d z. z @d y)) @s x)

(cout "test21: "
  (unparse1 (spec (lambda-c (Zs Ss Zd Sd As Ls Ad Ld)
		    (Ld (@c As (Ls (Ld (@c Ad Zd Zs))) Zd)))))
  nl)
;; test21: (l (l (a z z))): not correct!


;;  (lambda_s id. id @d (lambda_d z. id)) @s (lambda_d x. x)

(cout "test3: "
  (unparse1 (spec (lambda-c (Zs Ss Zd Sd As Ls Ad Ld)
		    (@c As
		        (Ls (@c Ad Zs (Ld Zs)))
		        (Ld Zd)))))
  nl)

; test3: (a (l z) (l (l z)))


