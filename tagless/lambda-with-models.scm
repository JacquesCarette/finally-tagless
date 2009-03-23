; __________________________________________________________ 
; --- first, A SHORT SCHEME-TECHNICAL DIVERSION ---
; Two helper macros to make defining and applying curried functions easier

(define-syntax lambda-c
  (syntax-rules () ((lambda-c () body) body)
                   ((lambda-c (v . vs) body) (lambda (v) (lambda-c vs body)))))
(define-syntax @c
 (syntax-rules () ((@c f) f)
                  ((@c f arg . args) (@c (f arg) . args))))
; define gensym:
(define gensym (let ((count 0)) (lambda ()
                   (set! count (+ 1 count))
                   (string->symbol (string-append "g-" (number->string count)))))) 
; __________________________________________________________ 
; --- END OF SCHEME-TECHNICAL DIVERSION ---


; OVERALL AIMS
; ============== 

; Realise the notations and concepts from PEAPG as literally as possible:

;  a) with interpretation, specilisation, compilation, 
;  b) with self-interpretation, and 
;  c) all three (4?) Futamura projections
;  d) in SCHEME (for convenience, using a tiny lambda-calculus subset)

; A DOGMA: avoid language-dependency, eg Scheme binding, recursion, etc.,
; in both formulations and implementations

; Use ideas of syntax-free tagless representation and evaluation using a model
  
; ________________________  
; THE PROGRAMMING LANGUAGE: this is the lambda-calculus with call by value, 
; numbers as atomic values, fixpoint, and conditional (unstrict as usual).

; Typical programs: e, p, q. Conceptual, not for computer processing.
                   
; The language is just expressive enough for the Gibonacci and
; power functions, and to write the self-interpreter.

; __________________________________________________________
; TEXTUAL programs Te: concrete syntax (Scheme values) as seen by a text editor
;         This is CONCRETE SYNTAX (chosen to be identical to a subset of Scheme)
;                  
; e ::= iden   | (lambda (id) e) | (e e) | (letrec ((id e)) id)           
;    |  number | (+ e1 e2) | (* e1 e2)   | (if (zero? e) e-then e-else)    
; 
; __________________________________________________________
; ENCODED TAGLESS REPRESENTATION Qe OF PROGRAM e is syntax-free Scheme value (no S-expressions!):

;    Qe = (lambda-c (V L A F I P M Q) <e>)  [actually: the VALUE of this...]
; 
; <iden>                     =  V value  (value is bound by "lambda")
; <(lambda (iden) e>         =  L f      where f v = <e>[ iden |-> v ] (so f: value -> term)
; <(e1 e2)>                  =  A <e1> <e2> 
; <(letrec ((iden e)) iden>  =  F f      where f v = <e>[ iden |-> (F f) ]
; <number>                   =  I number 
; (+ e1 e2>                  =  P <e1> <e2>
; (* e1 e2>                  =  M <e1> <e2>
; (if e0 e1 e2)>             =  Q <e0> (lambda() <e1>)  (lambda() <e2>)
;                        [thunks are needed for <e1> <e2>  since Q evaluates _all_ its arguments]

; __________________________________________________________
; TERM CONSTRUCTION OPERATORS: used to build <e> above using HOAS

;  V : value -> t     
;  L : (value -> t) -> t
;  A : t -> t -> t 
;  F : (value -> t) -> t
;  I : number -> t
;  P : t -> t -> t
;  M : t -> t -> t
;  Q : t -> (unit -> t) -> (unit -> t) -> t 

; _________________________
; EXAMPLE OF REPRESENTATION: encoding  Qe OF  e = ((lambda(x)(+ x 1)) 3) :


(define Qe  
    (lambda-c (V L A F I P M Q)  (@c A (L (lambda(v)(@c P v (I 1)))) (I 3))))


; __________________________________________________________
; SEMANTIC MODELS: A "model"  (or "interpretation" or "structure") binds to each term 
; construction operator in {V, L, A, F, I, P, M, Q} a corresponding operator on meanings.

; TRICK: evaluate this representation in an environment where V, L, A,... 
; have been bound to their meanings. This can be done by simply applying the term Qe to 
; suitable meanings assigned to A (application in the object language), 
; L (abstraction in the object language), etc. 

; MODEL EXAMPLE: the usual meanings of apply, abstract etc. can be got by defining:

(define Vi (lambda(value)value))
(define Li (lambda (a) a))
(define Ai (lambda (a) (lambda (b) (a b)))) 
(define Fi (lambda (a) (a (letrec ((self (lambda(n) ((a self)n)))) self ))))
(define Ii (lambda(num)num))
(define Pi (lambda (a) (lambda (b) (+ a b))))
(define Mi (lambda (a) (lambda (b) (* a b)))) 
(define Qi (lambda (a) (lambda (b) (lambda (c) 
                (if (zero? a) (b) (c) ) ; choose: activate thunk for then or else
                                                   ))))

"SEMANTICS EXAMPLE, standard evaluation using  Vi Li Ai Fi Ii Pi Mi Qi"

"   Instance: if e = ((lambda(x)(+ x 1)) 3) then (Qe Vi Li Ai Fi Ii Pi Mi Qi) should evaluate to 4"

(@c Qe Vi Li Ai Fi Ii Pi Mi Qi)

"NOW BUNDLE: MODEL := LIST OF THESE COMPONENTS: (define evalmodel (list Vi Li Ai Fi Ii Pi Mi Qi))"

(define evalmodel (list Vi Li Ai Fi Ii Pi Mi Qi))

; _____________________________
; MODEL-BASED PROGRAM EXECUTION This uss the packaged model as one object:

; For instance, (run Qe (list Vi Li Ai Fi Ii Pi Mi Qi)) computes the same as:
;                (@c Qe Vi Li Ai Fi Ii Pi Mi Qi))

(define run (lambda (code model)  (runaux code (reverse model))))

(define runaux (lambda (code as) (if (null? as) code ((runaux code (cdr as)) (car as)))))

"        Example: (run Qe evalmodel) should give 4"

(run Qe evalmodel)

"ELIDE evalmodel: Write (runL Qe) for (run Qe evalmodel), so (runL Qe) should give 4"

(define runL (lambda(code) (run code evalmodel)))

(runL Qe)

; ________________________________
; A SIMPLE model-based INTERPRETER.   This is easy to make, but no good for self-interpretation:

;       (define easyint (lambda(code)(@c code Vi Li Ai Fi Ii Pi Mi Qi)))

; Example: (easyint Qe)  gives 4.

; The problem is that easyint is not expressed in coded form, so it cannot be applied to itself.
; WHAT WE REALLY WANT: define Qint = the ENCODED form of easyint. This mess will work:

; (define Qint  (lambda-c (V L A F I P M Q) 
;  (L (lambda (u) (@c A (@c A (@c A (@c A (@c A (@c A (@c A (@c A u Vi) Li) Ai) Fi) Ii) Pi) Mi) Qi)))))

; then ((runL Qint) Qe) gives 4
 
; The following generates such an interpreter from the model:


(define genQint 
  (lambda (model) 
    (lambda-c (V L A F I P M Q)
           (L (lambda(u)
                (letrec ((gqaux (lambda (model)
                                  (if (null? model)
                                      u
                                      (@c A (gqaux (cdr model)) (car model)) )))) 
                  (gqaux (reverse model))))) )))
    
"-----------------------------------------------------------------------"
"INTERPRETER = semantics specialised to one model."
"E.G. (define Qint (genQint evalmodel))"

(define Qint (genQint evalmodel))


"EXAMPLE RUN OF INTERPRETER: ((runL Qint) Qe) should give 4"



((runL Qint) Qe)

"-----------------------------------------------------------------------"
"SELF-INTERPRETATION EXAMPLE: (((runL Qint) Qint) Qe)"

(((runL Qint) Qint) Qe)

"DOUBLE SELF-INTERPRETATION EXAMPLE: ((((runL Qint) Qint) Qint) Qe)"

((((runL Qint) Qint) Qint) Qe)


; A MODEL is the core of a PROGRAMMING LANGUAGE. Now several several models for this same syntax: 

"-----------------------------------------------------------------------"
"SEVERAL MODELS FOR THE SAME LAMBDA-SYNTAX (5 in all!)"
"    evalmodel; trivial model (all evaluates to 17); Sz for program size; Unparse; Parity"



(define Trivialmodel ; trivial model
  (list 
   (lambda-c (v)     17) 
   (lambda-c (a)     17) 
   (lambda-c (a b)   17)
   (lambda-c (a)     17)
   (lambda-c (num)   1)
   (lambda-c (a b)   17) 
   (lambda-c (a b)   17) 
   (lambda-c (a b c) 17)))  
  
(define SZmodel   ;;  Model for sizes: (HM, different order of evaluation!) 
  (list 
   (lambda-c (v)     1) 
   (lambda-c (a)     (+ (a 0) 1)) 
   (lambda-c (a b)   (+ a b 1))
   (lambda-c (a)     (+ a 1))
   (lambda-c (num)   1)
   (lambda-c (a b)   (+ a b 1)) 
   (lambda-c (a b)   (+ a b 1)) 
   (lambda-c (a b c) (+ a b c 1))))  ;  NOTE:  "if" is STRICT

(define Paritymodel   ;; Parity analysis:  HOAS working with abstract interpretation
  (list 
   (lambda-c (value)  value)
   (lambda-c (a)      a)
   (lambda-c (a b)    (a b)) 
   (lambda-c (a)      (+ a 1b)) 
   (lambda-c (num)    (numpar num))
   (lambda-c (a b)    (padd a b)) 
   (lambda-c (a b)    (pmul a b)) 
   (lambda-c (a b c)  (lub b c))))  

       (define numpar (lambda(n) (if (equal? (remainder n 2) 0) 'even 'odd)))
       (define padd (lambda(a b) (if (or (equal? a 'top) (equal? b 'top)) 'top 
                                 (if (equal? a b) 'even 'odd))))
       (define pmul (lambda(a b) (if (or (equal? a 'even) (equal? b 'even)) 'even
                                 (if (or (equal? a 'top) (equal? b 'top)) 'top 'odd))))
       (define lub (lambda(a b)  (if (equal? a b) a 'top)))

(define Unparsemodel      ;;  Model for unparsing from HOAS:  
  (list 
   (lambda-c (v)      v) 
   (lambda-c (a)      (let ((x (gensym))) `(lambda (,x) ,(a x))))
   (lambda-c (a b)    `(,a ,b))
   (lambda-c (a)      (let ((self (gensym)) (n (gensym)))
                         `(letrec ((,self (lambda (,n) (,a ,n)))) ,self)))
   (lambda-c (num)    num)
   (lambda-c (a b)   `(+ ,a ,b)) 
   (lambda-c (a b)   `(* ,a ,b)) 
   (lambda-c (a b c) `(if ,a ,b ,c))))  ;  NOTE:  "if" is STRICT


(define runT (lambda(code) (run code Trivialmodel)))
(define runZ (lambda(code) (run code SZmodel)))
(define runP (lambda(code) (run code Paritymodel)))
(define runU (lambda(code) (run code Unparsemodel)))

"(runL Qe) gives the standard value  (L = evalmodel = usual lambda calculus meaning)"
(runL Qe)
"(runT Qe) always gives the value 17 (T = Trivial)"
(runT Qe)
"(runZ Qe) gives the size of expression e (Z = siZe)"
(runZ Qe)
"(runP Qe) gives the Parity of expression e (P = Parity)"
(runP Qe)
"(runU Qe) should give a SCHEME expression encoded by Qe  (U = Unparse)"
(runU Qe)

 
; __________________________________________________________ 
; --- ANOTHER SHORT  DIVERSION, to MAKE A PARSER from Te (textual expression) 
 ; to tagless expression representation: Qe = (lambda-c (V L A F I P M Q) <e>)  

"-----------------------------------------------------------------------"
"PARSER takes a program in concrete syntax form to its syntax-free representation"

(define parse (lambda(e) (lambda-c (V L A F I P M Q)
              (letrec 
                 ((lookup (lambda (iden env)
                            (if (null? env) `(FREE-LAMBDA-VARIABLE-ERROR:,iden)
                                (if (equal? iden (caar env))
                                    (@c V (cdar env)) (lookup iden (cdr env))))))
                
                (parse1 (lambda(e env)
                    (if (number? e) (I e)                       
                    (if (not (pair? e)) (lookup e env)  
                    (if  (equal? (car e) 'lambda)
                         (@c L (lambda(v) (parse1 (caddr e) 
                                                  (cons (cons (caadr e) v) env))))
                    (if  (equal? (car e) 'letrec)
                         (@c F (lambda(f) (parse1 (cadar(cadr e)) 
                                                  (cons (cons (caddr e) f) env))))
                    (if (equal? (car e) '+)
                        (@c P (parse1  (cadr e) env) (parse1  (caddr e) env))
                    (if (equal? (car e) '*)
                        (@c M (parse1  (cadr e) env) (parse1  (caddr e) env))
                    (if (equal? (car e) 'if)
            (@c Q 
                (parse1 (cadadr e) env) 
                (lambda() (parse1 (caddr e) env))    ; make thunk for then
                (lambda() (parse1 (cadddr e) env)))  ; make thunk for else
                                                   
                    (@c A (parse1  (car e) env) (parse1  (cadr e) env)) 
                                                                     ))))))))) )
              (parse1 e '())  )))) 
; END OF PARSER 

; EXAMPLE: factorial function with explicit recursion

(define Tfac '(lambda (x) ((letrec((f (lambda(n) (if (zero? n) 1 (* n (f (+ n -1))))))) f) x)))

"  Parse concrete syntax of factorial function:"
"   (define Tfac '(lambda (x) ((letrec((f (lambda(n) (if (zero? n) 1 (* n (f (+ n -1))))))) f) x)))"

(define Tfac '(lambda (x) ((letrec((f (lambda(n) (if (zero? n) 1 (* n (f (+ n -1))))))) f) x)))
(define  Qfactorial (parse Tfac))

"Parse and then run the factorial function:   ((runL (parse Tfac)) 7) gives 5040"
((run (parse Tfac) evalmodel) 7)
; __________________________________________________________ 
; END OF SCHEME DIVERSION for PARSER
