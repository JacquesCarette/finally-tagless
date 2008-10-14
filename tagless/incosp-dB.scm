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

; There is no distinction between static and dynamic variables.
; Dynamic terms have the form (lambda-c (D Z S A L) body)
; where D is the function to shift the free variables in the body, if any
; That is, (l x. x) is represented as
; (lambda-c (D Z S A L) Z)
; whereas 
; (l x. y-one-level-up) as
; (lambda-c (D Z S A L) Z)


; better than Church numerals
(define P0 (lambda-c (f z) z))
(define P1 (lambda-c (f z) (@c f P0 z)))
(define succp (lambda-c (pn f z) (@c f pn (@c pn f z))))
(define predp (lambda-c (pn) (@c pn (lambda-c (pn1 x) pn1) (lambda (x) x))))
(define P2 (succp P1))
(define P3 (succp P2))
(define sub 				; assume p2 is smaller
  (lambda-c (p1 p2)
    (@c p2 (lambda-c (_ z) (predp z)) p1)))

(define (p->n p) (@c p (lambda-c (_ z) (+ 1 z)) 0))
(cout "Tests of P-numerals" nl
  "p0: " (p->n P0) nl
  "p1: " (p->n P1) nl
  "p2: " (p->n P2) nl
  "p1: " (p->n (predp (succp P1))) nl
  "p3-p0: " (p->n (@c sub P3 P0)) nl
  "p3-p1: " (p->n (@c sub P3 P1)) nl
  "p3-p2: " (p->n (@c sub P3 P2)) nl
  "p3-p3: " (p->n (@c sub P3 P3)) nl
)


 
; the input to "spec" is annotated -- it distinguishes
; variables, applications, and lambdas that are static from those that are
; dynamic -- but the output is unannotated and can be read back using
; "unparse1". 

(define (spec t) ; specializer
  (let
     (; Church-encoding of pairs
      (rfst (lambda-c (a b) a))
      (rsnd (lambda-c (a b) b))
      (rcons (lambda-c (a b sel) (@c sel a b)))
      (varref (lambda-c (D0 D1 Z S A L)
		(@c sub D1 (succp D0) (lambda-c (_ x) (S x)) Z))))
    (let ((env0 (lambda (x) x))
          (Z (lambda-c (env) (env rfst)))
          (S (lambda-c (v env) (v (env rsnd))))
          (As (lambda-c (e1 e2 env) ((e1 env) (e2 env))))
          (Ls (lambda-c (e env x) (e (@c rcons x env))))
          (Ad (lambda-c (e1 e2 env D Z S A L) 
		(@c A (@c e1 env D Z S A L) 
		      (@c e2 env D Z S A L))))
          (Ld (lambda-c (e env D Z S A L) 
		(@c L (@c e (@c rcons (varref D) env) (succp D) Z S A L)))))
      (@c t Z S As Ls Ad Ld env0))))



(define (unparse1 t) ; interpret a one-level term into an S-expression
  (@c t
      P0
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
  (unparse1 ((spec (lambda-c (Z S As Ls Ad Ld)
		     (Ls (Ld (@c Ad Z (S Z))))))
	      (lambda-c (D Z S A L) (L Z))))
  nl)
; (l (a z (l z)))

(unparse1 (spec (lambda-c (Z S As Ls Ad Ld)
		  (Ld (Ld Z)))))

(unparse1 (spec (lambda-c (Z S As Ls Ad Ld)
		  (Ld (Ld (S Z))))))


;; Alas, this specializer still doesn't correctly handle mixing static and 
;; dynamic bindings.  Here's a counterexample:

(cout "test2: "
  (unparse1 (spec (lambda-c (Z S As Ls Ad Ld)
		    (Ld (@c As (Ls (Ld (S Z))) Z)))))
  nl)
; (l (l (s z))), now, the result is correct

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
  (unparse1 (spec (lambda-c (Z S As Ls Ad Ld)
		    (Ld (@c As (Ls (Ld (@c Ad Z (S Z)))) Z)))))
  nl)
;; (l (l (a z (s z)))): seems correct


;;  (lambda_s id. id @d (lambda_d z. id)) @s (lambda_d x. x)

(cout "test3: "
  (unparse1 (spec (lambda-c (Z S As Ls Ad Ld)
		    (@c As
		        (Ls (@c Ad Z (Ld (S Z))))
		        (Ld Z)))))
  nl)

; test3: (a (l z) (l (l z))): seems correct


