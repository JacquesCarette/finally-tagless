; Interpreter, Compiler, Partial Evaluator
; deBruijn indices
;
;  The language is untyped lambda-calculus with fixpoint,
;  integers and conditional
;
;  Var | Lam | App e e | Fix |
;  I Int | Add ie1 ie2 | Mul ie1 ie2
;  IFZ b e-then e-else
;  
;  Var ::= VZ | VS Var
;
;  The language is just expressive enough for the Gibonacci and
;  power functions.

; All the terms in our language have the following form
; (cab-lambda (vz vs int add mul ifz lam app fix) body)
; where (cab-lambda () body) is the curried lambda-abstraction

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

(define test1
  (lambda-c (vz vs int add mul ifz lam app fix)
    (@c add (int 1) (int 2))))

(define test2
  (lambda-c (vz vs int add mul ifz lam app fix)
    (lam (@c add vz vz))))

(define test3
  (lambda-c (vz vs int add mul ifz lam app fix)
    (lam (@c add (@c app vz (int 1)) (int 2)))))

; here we test that if the procedure aqpplied to 0, the else branch
; is really not evaluated
(define t1
  (lambda-c (vz vs int add mul ifz lam app fix)
   (lam
    (@c ifz vz (int 1) (@c add 'er 'er)))))

(define t2
  (lambda-c (vz vs int add mul ifz lam app fix)
   (lam
    (@c ifz vz (int 1) (@c add vz vz)))))

(define t3
  (lambda-c (vz vs int add mul ifz lam app fix)
   (lam
    (lam
     (@c add vz (vs vz))))))

(define t4
  (lambda-c (vz vs int add mul ifz lam app fix)
   (fix
    (lam
     (@c ifz vz (int 1) (@c app (vs vz) (int 0)))))))

; The occurences of (let ((x ...)) ...) below are merely syntactic sugar,
; to give some meaningful names to deBruijn indices.
; The let-form is NOT part of the object language. We use meta-language
; to add syntactic sugar.
(define testpowfix
  (lambda-c (vz vs int add mul ifz lam app fix)
    (lam 
      (let ((x vz))
	(fix 
	  (let ((x (vs x)) (self vz))
	    (lam 
	      (let ((x (vs x)) (self (vs self)) (n vz))
		(@c ifz n (int 1)
		  (@c mul x (@c app self (@c add n (int -1)))))))))))))

(define testpowfix7 
  (lambda-c (vz vs int add mul ifz lam app fix)
    (lam (@c app (@c app 
		   (@c testpowfix vz vs int add mul ifz lam app fix) vz) (int 7)))))

(define testpowfix27 
  (lambda-c (vz vs int add mul ifz lam app fix)
    (@c app
      (@c testpowfix7 vz vs int add mul ifz lam app fix)
      (int 2))))

; let testpowfix0 () = lam (app (app (testpowfix ()) (int 0)) vz)

; Church-encoding of pairs: auxiliary functions
(define rfst (lambda-c (a b) a))
(define rsnd (lambda-c (a b) b))
(define rcons (lambda-c (a b sel) (@c sel a b)))


; The R-interpreter
(define intR
  (lambda (term)
    (@c term
      (lambda (h) (h rfst))				; vz
      (lambda (v) (lambda (h) (v (h rsnd))))		; vs
      (lambda (x) (lambda (h) x))			; int
      (lambda-c (e1 e2) (lambda (h) (+ (e1 h) (e2 h)))) ; add
      (lambda-c (e1 e2) (lambda (h) (* (e1 h) (e2 h)))) ; mul
      (lambda-c (e et ef)
	(lambda (h)
	  ((lambda (t) (if (zero? t) (@c et h) (@c ef h))) (e h)))) ; ifz
      (lambda (e) (lambda (h) (lambda (x) (e (@c rcons x h)))))	     ; lam
      (lambda-c (e1 e2) (lambda (h) ((e1 h) (e2 h)))) ; app
      (lambda (e) (lambda (h) 		      ; fix
	 (letrec ((self (lambda (n) (@c e (@c rcons self h) n)))) self)))
      (lambda (x) x)				      ; empty env
      )))
; #!eof

(cout "intR tests" nl nl
 "test1: " (intR test1) nl
 "test2: " (intR test2) nl
 "test2 5: " ((intR test2) 5) nl
 "testpw7: " (intR testpowfix7) nl
 "testpw27: " (intR testpowfix27) nl
)

; The C-interpreter
(define intC
  (lambda (term)
    (@c term
      (lambda (h) (h rfst))				; vz
      (lambda (v) (lambda (h) (v (h rsnd))))		; vs
      (lambda (x) (lambda (h) `,x))			; int
      (lambda-c (e1 e2) (lambda (h) `(+ ,(e1 h) ,(e2 h)))) ; add
      (lambda-c (e1 e2) (lambda (h) `(* ,(e1 h) ,(e2 h)))) ; mul
      (lambda-c (e et ef)
	(lambda (h) `(if (zero? ,(e h)) ,(@c et h) ,(@c ef h)))) ; ifz
      (lambda (e) (lambda (h) 
	  (let ((x (gensym))) `(lambda (,x) ,(e (@c rcons x h))))))	     ; lam
      (lambda-c (e1 e2) (lambda (h) `(,(e1 h) ,(e2 h)))) ; app
      (lambda (e) (lambda (h) 		      ; fix
	(let ((self (gensym)) (n (gensym)))
	 `(letrec ((,self (lambda (,n) (,(e (@c rcons self h)) ,n)))) ,self))))
      (lambda (x) x)				      ; empty env
      )))

(cout "intC tests" nl nl
 "test1: " (intC test1) nl
 "test2: " (intC test2) nl
 "test3: " (intC test3) nl
 "t2: " (intC t2) nl
 "t3: " (intC t3) nl
 "t4: " (intC t4) nl
 "testpw7: " (intC testpowfix7) nl
 "testpw27: " (intC testpowfix27) nl
 "testpw27 eval: " (eval (intC testpowfix27)) nl
)
