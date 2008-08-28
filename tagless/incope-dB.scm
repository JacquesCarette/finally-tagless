; Interpreter, Compiler, Self-interpreter
; The `tagless-final' style, deBruijn indices
;
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

; Sample terms of our object language, encoded in the meta-language
; They all have the form (lambda-c (vz vs ...) ...)

(define test1
  (lambda-c (vz vs int add mul ifz lam app fix)
    (@c add (int 1) (int 2))))

(define test2
  (lambda-c (vz vs int add mul ifz lam app fix)
    (lam (@c add vz vz))))

(define test3
  (lambda-c (vz vs int add mul ifz lam app fix)
    (lam (@c add (@c app vz (int 1)) (int 2)))))

; Applying test3 to the appropriate function
(define test3-app
  (lambda-c (vz vs int add mul ifz lam app fix)
    (@c app (@c test3 vz vs int add mul ifz lam app fix)
      (lam (@c add (int 4) vz)))))

; Here we test that if the procedure is applied to 0, the else branch
; is not evaluated
(define t1
  (lambda-c (vz vs int add mul ifz lam app fix)
   (lam
    (@c ifz vz (int 1) (@c add 'error 'error)))))

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

; The power function

; The occurrences of (let ((x ...)) ...) below are merely syntactic sugar,
; to give meaningful names to deBruijn indices.
; The let-form is NOT part of the object language. We take advantage of the
; meta-language to add syntactic sugar.

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
    (lam (@c app 
           (@c app 
             (@c testpowfix vz vs int add mul ifz lam app fix) vz) (int 7)))))

(define testpowfix27
  (lambda-c (vz vs int add mul ifz lam app fix)
    (@c app
      (@c testpowfix7 vz vs int add mul ifz lam app fix)
      (int 2))))


;------------------------------------------------------------------------
; Interpretation

; Church-encoding of pairs
(define rfst (lambda-c (a b) a))
(define rsnd (lambda-c (a b) b))
(define rcons (lambda-c (a b sel) (@c sel a b)))


; The R-interpreter
(define intR
  (lambda (term)
    (@c term
      (lambda (h) (h rfst))                             ; vz
      (lambda (v) (lambda (h) (v (h rsnd))))            ; vs
      (lambda (x) (lambda (h) x))                       ; int
      (lambda-c (e1 e2) (lambda (h) (+ (e1 h) (e2 h)))) ; add
      (lambda-c (e1 e2) (lambda (h) (* (e1 h) (e2 h)))) ; mul
      (lambda-c (e et ef)                               ; ifz
        (lambda (h) (if (zero? (e h)) (et h) (ef h))))
      (lambda (e) (lambda (h) (lambda (x) (e (@c rcons x h)))))             ; lam
      (lambda-c (e1 e2) (lambda (h) ((e1 h) (e2 h))))   ; app
      (lambda (e) (lambda (h)                           ; fix
         (letrec ((self (lambda (n) (@c e (@c rcons self h) n)))) self)))
      (lambda (x) x)                                    ; empty env
      )))

(cout "intR tests" nl
 "test1: "    (intR test1) nl
 "test2: "    (intR test2) nl
 "test2 5: "  ((intR test2) 5) nl
 "test3 (+4): "  ((intR test3) (lambda (x) (+ x 4))) nl    ; 7
 "test3-app: "(intR test3-app) nl           ; 7
 "testpw7: "  (intR testpowfix7) nl
 "testpw27: " (intR testpowfix27) nl        ; 128
)

; The C-interpreter, the compiler
(define intC
  (lambda (term)
    (@c term
      (lambda (h) (h rfst))                                ; vz
      (lambda (v) (lambda (h) (v (h rsnd))))               ; vs
      (lambda (x) (lambda (h) `,x))                        ; int
      (lambda-c (e1 e2) (lambda (h) `(+ ,(e1 h) ,(e2 h)))) ; add
      (lambda-c (e1 e2) (lambda (h) `(* ,(e1 h) ,(e2 h)))) ; mul
      (lambda-c (e et ef)
        (lambda (h) `(if (zero? ,(e h)) ,(et h) ,(ef h)))) ; ifz
      (lambda (e) (lambda (h)                              ; lam
          (let ((x (gensym))) `(lambda (,x) ,(e (@c rcons x h))))))
      (lambda-c (e1 e2) (lambda (h) `(,(e1 h) ,(e2 h))))   ; app
      (lambda (e) (lambda (h)                              ; fix
        (let ((self (gensym)) (n (gensym)))
         `(letrec ((,self (lambda (,n) (,(e (@c rcons self h)) ,n)))) ,self))))
      (lambda (x) x)                                       ; empty env
      )))

(cout nl nl "intC tests" nl
 "test1: "    (intC test1) nl
 "test2: "    (intC test2) nl
 "test3: "    (intC test3) nl
 "test3-app: "(intC test3-app) nl
 "t2: "       (intC t2) nl
 "t3: "       (intC t3) nl
 "t4: "       (intC t4) nl
 "testpw7: "  (intC testpowfix7) nl
 "testpw27: " (intC testpowfix27) nl
 "testpw27 eval: " (eval (intC testpowfix27)) nl ; 128
)

;------------------------------------------------------------------------
; The self-interpreter
; It is the implementation of the interpreter R (intR) in the object
; language. To be more precise, it is the straightforward encoding
; of the interpreter R in the object language, of the interpreter R.

; Curried application with `app' as the applicator
(define-syntax @@c
  (syntax-rules ()
    ((@@c (app) f) f)
    ((@@c (app) f arg . args) (@@c (app) (@c app f arg) . args))))

(define intS
  (lambda-c (vz vs int add mul ifz lam app fix)
    ; Church-encoding of pairs: auxiliary functions
    (let ((rfst (lam (lam (vs vz))))
          (rsnd (lam (lam vz)))
           (rcons (lam (lam (lam (@@c (app) vz (vs (vs vz)) (vs vz)))))))
   (lam
    (@@c (app) vz
      (lam (@c app vz rfst))                             ; vz
      (lam (lam (@c app (vs vz) (@c app vz rsnd))))      ; vs
      (lam (lam (vs vz)))                                ; int
      (lam (lam (lam (@c add (@c app (vs (vs vz)) vz) (@c app (vs vz) vz))))) ; add
      (lam (lam (lam (@c mul (@c app (vs (vs vz)) vz) (@c app (vs vz) vz))))) ; mul
      (lam (lam (lam (lam                                ; ifz
        ; the following let is just an abbreviation to avoid confusing
        ; deBruijn indices
        (let ((h vz) (ef (vs vz)) (et (vs (vs vz))) (e (vs (vs (vs vz)))))
          (@c ifz (@c app e h) (@c app et h) (@c app ef h)))))))
      (lam (lam (lam (@c app (vs (vs vz)) (@@c (app) rcons vz (vs vz)))))) ; lam
      (lam (lam (lam (@c app (@c app (vs (vs vz)) vz) (@c app (vs vz) vz))))) ; app
      (lam (lam                                          ; fix
         (fix (@c app (vs (vs vz)) (@@c (app) rcons vz (vs vz))))))
      (lam vz)                                           ; empty env
      )))))

; The self-interpreter interpreted
(define intSR (intR intS))

; The self-interpreter self-interpreted
(define intSSR (intSR intS))

(cout nl nl "intSR tests" nl
 "test1: "    (intSR test1) nl
 "test2: "    (intSR test2) nl
 "test2 5: "  ((intSR test2) 5) nl ; 10
 "test3-app: "(intSR test3-app) nl ; 7
 "t1 0: "     ((intSR t1) 0) nl    ; 1, no error is raised
 "t2 3: "     ((intSR t2) 3) nl    ; 6
 "t3 4 5: "   (@c (intSR t3) 4 5) nl    ; 9
 "t4 10: "    ((intSR t4) 10) nl    ; 1
 "testpw7 2: "  ((intSR testpowfix7) 2) nl ; 128
 "testpw27: " (intSR testpowfix27) nl        ; 128
)

; The self-interpreter compiled
(define intSC (eval (intC intS)))

; The self-interpreter self-compiled
(define intSSC (intSC intS))

(cout nl nl "intSC tests" nl
 "test1: "    (intSC test1) nl
 "test2: "    (intSC test2) nl
 "test2 5: "  ((intSC test2) 5) nl ; 10
 "test3 (+4): "  ((intSC test3) (lambda (x) (+ x 4))) nl    ; 7
 "test3-app: "(intSC test3-app) nl ; 7
 "t1 0: "     ((intSC t1) 0) nl    ; 1, no error is raised
 "t2 3: "     ((intSC t2) 3) nl    ; 6
 "t3 4 5: "   (@c (intSC t3) 4 5) nl    ; 9
 "t4 10: "    ((intSC t4) 10) nl    ; 1
 "testpw7 2: "  ((intSC testpowfix7) 2) nl ; 128
 "testpw27: " (intSC testpowfix27) nl        ; 128
)

; timing tests

(define testtiming
  (lambda-c (vz vs int add mul ifz lam app fix)
    (@@c (app)
      (@c testpowfix vz vs int add mul ifz lam app fix)
      (int 3) (int 30))))

(cout nl nl "Timing test" nl
 "testtiming with intR: " 
  (time (intR testtiming)) nl
;; (time (intr testtiming))
;;     no collections
;;     1 ms elapsed cpu time
;;     1 ms elapsed real time
;;     8256 bytes allocated

 "testtiming with intC: " 
  (time (eval (intC testtiming))) nl
;; (time (eval (intc testtiming)))
;;     no collections
;;     0 ms elapsed cpu time
;;     0 ms elapsed real time
;;     12968 bytes allocated

 "testtiming with intSR: " 
  (time (intSR testtiming)) nl
;; (time (intsr testtiming))
;;     no collections
;;     3 ms elapsed cpu time
;;     3 ms elapsed real time
;;     123928 bytes allocated

 "testtiming with intSSR: " 
  (time (intSSR testtiming)) nl
;; (time (intssr testtiming))
;;     no collections
;;     34 ms elapsed cpu time
;;     34 ms elapsed real time
;;     1867512 bytes allocated

 "testtiming with intSC: " 
  (time (intSC testtiming)) nl
;; (time (intsc testtiming))
;;     no collections
;;     0 ms elapsed cpu time
;;     0 ms elapsed real time
;;     8936 bytes allocated

 "testtiming with intSSC: " 
  (time (intSSC testtiming)) nl
;; (time (intssc testtiming))
;;     no collections
;;     2 ms elapsed cpu time
;;     1 ms elapsed real time
;;     130608 bytes allocated
)

(cout nl nl "Compiling the self-interpreter" (intC intS) nl)
