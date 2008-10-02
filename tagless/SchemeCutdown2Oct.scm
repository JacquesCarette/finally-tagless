; THINGS DONE RECENTLY:
  
; 1. Change syntax, so OL syntax = small subset of Scheme. This has effects:

;   a) avoid a level of interpretation, as  OL <= Sc
;   b) avoid need for two versions of syntax, eg  
;                   (app (lam (+ (X 0) 1) 2) and ((lambda(x)(+ x 1))2)
;   c) and so make parse, unparse, int, spec simpler
     
; 2. intR still uses De Bruijn indexes - no change here 

; 3. intS has been removed completely (no need now that OL <= Scheme)

; 4. A function "parse" was written: it takes an OL expression e and produces its representation as a function of form

;    \ Z S I P M Q L A F. <e>    (see below)

; 5. intC has been renamed  "unparse". It yields OL code from a representation

; 6. Lots of examples, including some with Church numerals

; 7. int-text = the interpreter intR as a complete OL program
;               It is essentially the old intR, plus inlined rfst, rsnd, rcons
;                and without the syntactic sugar of @, lambda-c etc
;
; 8. int-value = the parsed int-text
; __________________________________________________________
;
; The object language is the lambda-calculus with fixpoint,
; atomic values and the conditional. Its concrete syntax (Scheme data)
;
; e ::= id                     ; identifier 
;    |  number
;    | (+ e1 e2) 
;    | (* e1 e2)
;    | (if (zero? e) e-then e-else)    
;    | (lambda (id) e)
;    | (e e) 
;    | (letrec ((id e)) id)
;                    
; The language is just expressive enough for the Gibonacci and
; power functions, and to write the self-interpreter.

; All the terms in our language have the following form
; (lambda-c (vz vs int add mul ifz lam app fix) body)
; where (lambda-c (...) body) is the curried lambda-abstraction
;
;  Representation(e)  =  \ Z S I P M Q L A F. <e>  where
; 
; <iden>           =  Church numeral  S(S(...(S Z)...))
; <number>         =  I number 
; (+ e1 e2)>       =  P <e1> <e2>
; (* e1 e2)>       =  M <e1> <e2>
; (if e0 e1 e2)>   =  Q <e0> <e1> <e2>
; <(lambda (id) e> =  L <e>
; <(e1 e2)>        =  A <e1> <e2> 
; <(letrec ((id e)) id>  =  F <e>

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

; define gensym:

(define gensym
	  (let ((count 0))
	    (lambda ()
	      (set! count (+ 1 count))
	      (string->symbol (string-append "g-"
				(number->string count))))))



;------------------------------------------------------------------------
; Interpretation: The R-interpreter

(define intR

  (let 
     (; Church-encoding of pairs
      (rfst (lambda-c (a b) a))
      (rsnd (lambda-c (a b) b))
      (rcons (lambda-c (a b sel) (@c sel a b))))
    (let
     ((Z (lambda (env) (env rfst)))                   ; vz
      (S (lambda (v) (lambda (env) (v (env rsnd)))))  ; vs
      (I (lambda (x) (lambda (env) x)))               ; int
      (P (lambda-c (e1 e2) (lambda (env) (+ (e1 env) (e2 env))))) ; add
      (M (lambda-c (e1 e2) (lambda (env) (* (e1 env) (e2 env))))) ; mul
      (Q (lambda-c (e et ef)                                      ; ifz
       (lambda (env) (if (zero? (e env)) (et env) (ef env)))))
      (L (lambda (e) (lambda (env) (lambda (x) (e (@c rcons x env)))))) ; lam
      (A (lambda-c (e1 e2) (lambda (env) ((e1 env) (e2 env)))))    ; app
      (F (lambda (e) (lambda (env)                                 ; fix
        (letrec ((self (lambda (n) (@c e (@c rcons self env) n)))) self))))
      (env0 (lambda (x) x))                          ; empty env 
      )
    
    (lambda (term) (@c term Z S I P M Q L A F env0)))))

; The Unparser: from expression representation \ Z S I P M Q L A F... to exp

; This is an interpretation that produces code instead of evaluating.
;   It has exactly the same structure as intR above

(define unparse
  (let 
     ((Z (lambda (env) (env rfst)))                   ; vz
      (S (lambda (v) (lambda (env) (v (env rsnd)))))  ; vs
      (I (lambda (x) (lambda (env)`,x)))              ; int
      (P (lambda-c (e1 e2) (lambda (env) `(+ ,(e1 env) ,(e2 env))))) ; add
      (M (lambda-c (e1 e2) (lambda (env) `(* ,(e1 env) ,(e2 env)))))  ; mul
      (Q (lambda-c (e et ef)                                      ; ifz
         (lambda (env) `(if (zero? ,(e env)) ,(et env) ,(ef env))))) 
      (L (lambda (e) (lambda (env)                                              ; lam
         (let ((x (gensym))) `(lambda (,x) ,(e (@c rcons x env)))))))
      (A (lambda-c (e1 e2) (lambda (env) `(,(e1 env) ,(e2 env)))))    ; app
      (F (lambda (e) (lambda (env)                                 ; fix
       (let ((self (gensym)))
        `(letrec ((,self ,(e (@c rcons self env)) )) ,self)))))
      (env0 (lambda (x) x)) )                        ; empty env 
         
    (lambda (term) (@c term Z S I P M Q L A F env0))))

; This interpretation that produces  manifest form of the representation.
;   It has exactly the same structure as intR above

(define manifest
  (let 
     ((Z 'Z)                   ; vz
      (S (lambda-c (e1)`(S ,e1)))  ; vs
      (I (lambda-c (e1)`(I ,e1)))              ; int
      (P (lambda-c (e1 e2)`(P ,e1  ,e2 ))) ; add
      (M (lambda-c (e1 e2)`(M ,e1  ,e2 )))  ; mul
      (Q (lambda-c (e et ef)`(Q ,e ,et ,ef)))
      (L (lambda-c (e1)`(L ,e1)))
      (A (lambda-c (e1 e2)`(A ,e1 ,e2)))    ; app
      (F (lambda-c (e1)`(F ,e1)))
)                                 
    (lambda (term) (@c term Z S I P M Q L A F))))


; Church numeral representation as usual: <n> represents n

 ; Successor  succ = λm.λs.λz. m s (s z)
 
 (define succ (lambda-c (m s z) (@c m s (s z))))
 (define sum (lambda-c (m n s z) (@c m s (@c n s z))))
 (define prod (lambda-c (m n s z) (@c m (@c n s) z)))
 (define exp (lambda-c (m n s z) (@c n m s z)))
 (define ackermann (lambda-c (m n) (@c m (lambda-c (g n) (@c n g (g one))) succ n)))
 
; <0>, <1>, <2>, <3>
 (define zero    (lambda (S) (lambda (Z) Z)))
 (define one     (lambda (S) (lambda (Z) (S Z))))
 (define two     (lambda (S) (lambda (Z) (S (S Z)))))
 (define three   (lambda (S) (lambda (Z) (S (S (S Z))))))
 (define twenty   (lambda (S) (lambda (Z) (S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S Z
                                          )))))))))))))))))))))))
 (define forty   ((sum twenty) twenty))
 (define eighty   ((sum forty) forty))
 (define onesixty   ((sum eighty) eighty))
                
 (define church+succ+0    ;  Purpose: to be able to observe output from a C.N. result
 (lambda (num)
  (@c num (lambda(x)(+ x 1)) 0)))
 
; > (church+succ+0 (@c succ  two))
; 3
; > (church+succ+0 (@c exp three three))
; 27
 
 

; The Parser: from exp to expression representation \ Z S I P M Q L A F...

(define parse (lambda(e) (parse1 '() e))) 

(define lookup (lambda (iden names) 
                 (if (null? names)
                  (lambda-c (Z S I P M Q L A F) `(LAMBDA-VARIABLE-ERROR:,iden))
                  (if (equal? iden (car names))
                   (lambda-c (Z S I P M Q L A F) Z)
                   (lambda-c (Z S I P M Q L A F)
                             (@c S (@c (lookup iden (cdr names)) Z S I P M Q L A F)  
                              ))))))

(define parse1 (lambda(names e)
                    (if (number? e) (lambda-c (Z S I P M Q L A F) (I e))
                        
                    (if (not (pair? e)) (lookup e names)
                           
                    (if (equal? (car e) '+)
                           (lambda-c (Z S I P M Q L A F) 
                              (@c P (@c (parse1 names (cadr e)) Z S I P M Q L A F)
                                    (@c (parse1 names (caddr e)) Z S I P M Q L A F)))
                    (if  (equal? (car e) '*)
                           (lambda-c (Z S I P M Q L A F) 
                              (@c M  (@c (parse1 names (cadr e)) Z S I P M Q L A F)
                                     (@c (parse1 names(caddr e)) Z S I P M Q L A F)))
                           
                    (if  (equal? (car e) 'if)
                           (lambda-c (Z S I P M Q L A F) 
                              (@c Q (@c (parse1 names (cadadr e)) Z S I P M Q L A F)
                                    (@c (parse1 names (caddr e))  Z S I P M Q L A F)
                                    (@c (parse1 names (cadddr e)) Z S I P M Q L A F))) 
                    (if  (equal? (car e) 'lambda)
                           (lambda-c (Z S I P M Q L A F)
                              (@c L (@c (parse1 
                                         (cons (caadr e) names)
                                         (caddr e)) 
                                     Z S I P M Q L A F)))
                           
                    (if  (equal? (car e) 'letrec)
                      (lambda-c (Z S I P M Q L A F)
                        (@c F (@c (parse1 
                                  (cons (caar (cadr e)) names) 
                                   (cadar (cadr e))) 
                                Z S I P M Q L A F))) 
                           
                           (lambda-c (Z S I P M Q L A F) 
                              (@c A (@c (parse1 names (car e)) Z S I P M Q L A F)
                                  (@c (parse1 names (cadr e)) Z S I P M Q L A F)
                        )))))))))))


(define test0  (parse `1))
(define test1  (parse `(+ 1 2)))
(define test2  (parse `(lambda (x) (+ x x))))
(define test3  (parse `(lambda (x) (+ (x 1) 2))))

; Here we test that if the procedure is applied to 0, the else branch
; is not evaluated
(define t1  (parse `(lambda (x) (if (zero? x) 1 (+ error error)))))
(define t2  (parse `(lambda (x) (if (zero? x) 1 (+ x x)))))
(define t3  (parse `(lambda (x) (lambda (y) (+ x y)))))
(define t4  (parse `(letrec ((f (lambda (x) (if (zero? x) 1 (f 0))))) f) ))

; The power function

(define powfix
  `(lambda (x) (letrec ((f (lambda (n) (if
                    (zero? n)
                    1
                    (* x (f (+ n -1))))))))))

(define testpowfix (parse powfix))
(define testpowfix2  (parse `(,powfix 2)))
(define testpowfix27  (parse `((,powfix 2) 7))) 


; Here is the powfix in a readable representation:

(define powfixreadable (manifest testpowfix))

; > powfixreadable
; (l (f (l (q z (i 1) (m (s (s z)) (a (s z) (p z (i -1))))))))

; Here is the interpreter in its own language without syntactic sugar:

(define int-text
'(lambda (g-1)
  ((((((((((g-1 (lambda (g-2) (g-2 (lambda (g-3) (lambda (g-4) g-3)))))
           (lambda (g-5)
             (lambda (g-6) (g-5 (g-6 (lambda (g-7) (lambda (g-8) g-8)))))))
          (lambda (g-9) (lambda (g-10) g-9)))
         (lambda (g-11)
           (lambda (g-12) (lambda (g-13) (+ (g-11 g-13) (g-12 g-13))))))
        (lambda (g-14)
          (lambda (g-15) (lambda (g-16) (* (g-14 g-16) (g-15 g-16))))))
       (lambda (g-17)
         (lambda (g-18)
           (lambda (g-19)
             (lambda (g-20)
               (if (zero? (g-17 g-20)) (g-18 g-20) (g-19 g-20)))))))
      (lambda (g-21)
        (lambda (g-22)
          (lambda (g-23)
            (g-21
             (((lambda (g-24)
                 (lambda (g-25) (lambda (g-26) ((g-26 g-24) g-25))))
               g-23)
              g-22))))))
     (lambda (g-27) (lambda (g-28) (lambda (g-29) ((g-27 g-29) (g-28 g-29))))))
    (lambda (g-30)
      (lambda (g-31)
        (letrec ((g-32
                  (lambda (g-33)
                    ((g-30
                      (((lambda (g-34)
                          (lambda (g-35) (lambda (g-36) ((g-36 g-34) g-35))))
                        g-32)
                       g-31))
                     g-33))))
          g-32))))
   (lambda (g-37) g-37)))
  )

(define int-value (parse  int-text))

; The interpreter run

(cout nl nl "intR tests" nl
"test1: "    (intR test1) nl
"test2: "    (intR test2) nl
"test2 5: "  ((intR test2) 5) nl ; 10
"t1 0: "     ((intR t1) 0) nl    ; 1, no error is raised
"t2 3: "     ((intR t2) 3) nl    ; 6
"t3 4 5: "   (@c (intR t3) 4 5) nl    ; 9
"t4 10: "    ((intR t4) 10) nl    ; 1
"testpw2 7: "  ((intR testpowfix2) 2) nl ; 128
"testpw27: " (intR testpowfix27) nl        ; 128
)

; The interpreter self-interpreted

(define intint (intR int-value))

(cout nl nl "intint tests" nl
"test1: "    (intint test1) nl
"test2: "    (intint test2) nl
"test2 5: "  ((intint test2) 5) nl ; 10
"t1 0: "     ((intint t1) 0) nl    ; 1, no error is raised
"t2 3: "     ((intint t2) 3) nl    ; 6
"t3 4 5: "   (@c (intint t3) 4 5) nl    ; 9
"t4 10: "    ((intint t4) 10) nl    ; 1
"testpw2 7: "  ((intint testpowfix2) 7) nl ; 128
"testpw27: " (intint testpowfix27) nl        ; 128
)
; The interpreter doubly self-interpreted

(define intintint (intint int-value))

(cout nl nl "intintint tests" nl
"test1: "    (intintint test1) nl
"test2: "    (intintint test2) nl
"test2 5: "  ((intintint test2) 5) nl ; 10
"t1 0: "     ((intintint t1) 0) nl    ; 1, no error is raised
"t2 3: "     ((intintint t2) 3) nl    ; 6
"t3 4 5: "   (@c (intintint t3) 4 5) nl    ; 9
"t4 10: "    ((intintint t4) 10) nl    ; 1
"testpw2 7: "  ((intintint testpowfix2) 7) nl ; 128
"testpw27: " (intintint testpowfix27) nl        ; 128
)


; The self-interpreter compiled
(define intSC (eval int-text))

(cout nl nl "intSC tests" nl
"test1: "    (intSC test1) nl
"test2: "    (intSC test2) nl
"test2 5: "  ((intSC test2) 5) nl ; 10
"t1 0: "     ((intSC t1) 0) nl    ; 1, no error is raised
"t2 3: "     ((intSC t2) 3) nl    ; 6
"t3 4 5: "   (@c (intSC t3) 4 5) nl    ; 9
"t4 10: "    ((intSC t4) 10) nl    ; 1
"testpw2 7: "  ((intSC testpowfix2) 7) nl ; 128
"testpw27: " (intSC testpowfix27) nl        ; 128
)

; The self-interpreter self-compiled
(define intSSC (eval (intSC int-value)))

(cout nl nl "intSSC tests" nl
"test1: "    (intSSC test1) nl
"test2: "    (intSSC test2) nl
"test2 5: "  ((intSSC test2) 5) nl ; 10
"t1 0: "     ((intSSC t1) 0) nl    ; 1, no error is raised
"t2 3: "     ((intSSC t2) 3) nl    ; 6
"t3 4 5: "   (@c (intSSC t3) 4 5) nl    ; 9
"t4 10: "    ((intSSC t4) 10) nl    ; 1
"testpw2 7: "  ((intSSC testpowfix2) 7) nl ; 128
"testpw27: " (intSSC testpowfix27) nl        ; 128
)

