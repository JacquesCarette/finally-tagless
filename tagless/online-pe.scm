; Observers on lambda-terms as S-expressions

(define (lam? m)
  (and (eq? (car m) 'lambda)
       (symbol? (caadr m))
       (null? (cdadr m))
       (null? (cdddr m))))
(define lam-var caadr)
(define lam-body caddr)

(define (oper? m)
  (and (not (eq? (car m) 'lambda))
       (null? (cddr m))))
(define operator car)
(define operand cadr)

; Rename lambda-terms using de Bruijn levels

(define (rename m)
  (let loop ((m m) (i 0) (env '()))
    (cond
      ((symbol? m)
       (cdr (assq m env)))
      ((lam? m)
       (let ((x (string->symbol (number->string i))))
         `(lambda (,x)
            ,(loop (lam-body m) (+ i 1) (cons (cons (lam-var m) x) env)))))
      ((oper? m)
       `(,(loop (operator m) i env) ,(loop (operand m) i env))))))

; Mogensen's "new representation schema"

(define (q m)
  (let ((a (gensym))
        (b (gensym)))
    `(lambda (,a) (lambda (,b)
       ,(let loop ((m m))
          (cond
            ((symbol? m) m)
            ((lam? m) `(,b (lambda (,(lam-var m)) ,(loop (lam-body m)))))
            ((oper? m) `((,a ,(loop (operator m))) ,(loop (operand m))))))))))

(define (unq m)
  (((eval m) (lambda (m) (lambda (n) `(,m ,n))))
   (lambda (g) (let ((x (gensym))) `(lambda (,x) ,(g x))))))

; Mogensen's self-interpreter

(define E
  (let ((I '(lambda (x) x)))
    `(lambda (m) ((m ,I) ,I))))

; An online self-reducer a la Mogensen,
; but with optional static terms, a la Sperber,
; and without passing redundant constructors of the output term syntax

(define R
  (let (; Church encoding of pairs
        (fst '(lambda (a) (lambda (b) a))) ; Mogensen's T
        (snd '(lambda (a) (lambda (b) b))) ; Mogensen's F
        (cons '(lambda (a) (lambda (b) (lambda (sel) ((sel a) b)))))
        ; Church encoding of option
        (none '(lambda (selnone) (lambda (selsome) selnone)))
        (some '(lambda (s) (lambda (selnone) (lambda (selsome) (selsome s))))))
    `(lambda (m) (lambda (app) (lambda (lam)
       (((m
          ; Static+dynamic application (revised from Mogensen's A)
          (lambda (m) (m (lambda (mfst) (lambda (msnd)
            ((mfst (lambda (n) ((,cons ,none) ((app msnd) (n ,snd)))))
             (lambda (g) g)))))))
         ; Static+dynamic lambda (revised from Mogensen's B)
         (lambda (g)
           ((,cons (,some g))
            (lam (lambda (x) ((g ((,cons ,none) x)) ,snd))))))
        ,snd))))))

(define P
  `(lambda (m) (lambda (n)
     (,R (lambda (a) (lambda (b) ((a ((m a) b)) ((n a) b))))))))
        ; Mogensen writes "b" here ^ but "a" is correct

; Church numerals

(define (num i)
  `(lambda (f) (lambda (x)
     ,(let loop ((i i) (y 'x))
        (if (= i 0) y (loop (- i 1) `(f ,y)))))))

(define (unnum m)
  (((eval m) (lambda (i) (+ 1 i))) 0))

; Ackermann's function

(define Ackermann
  '(lambda (m) (lambda (n)
     (((m (lambda (f) (lambda (m) (f ((m f) (lambda (x) x))))))
       (lambda (m) (lambda (f) (lambda (x) (f ((m f) x))))))
      n))))

; For testing, we reduce terms to normal form through call-by-need reduction
; and count the number of beta reductions required

(define (reduce m expected-result)
  ; A denotation is a promise for either a static function
  ; (from promises to promises) or a dynamic term
  (letrec ((beta 0)
           (dyn (lambda (m)
                  (let ((m (force m)))
                    (if (procedure? m)
                      (let ((x (gensym)))
                        `(lambda (,x) ,(dyn (m (delay x)))))
                      m))))
           (app (lambda (m) (lambda (n) (delay
                  (let ((m (force m)))
                    (if (procedure? m)
                      (begin (set! beta (+ 1 beta))
                             (force (m n)))
                      `(,m ,(dyn n))))))))
           (lam (lambda (g) (delay g))))
    (let ((result (dyn (((eval (q m)) app) lam))))
      (and (equal? (rename result) (rename expected-result))
           beta))))

; A simple test: reducing ((lambda (x) (x x)) (lambda (x) x))
; > (reduce '((lambda (x) (x x)) (lambda (x) x)) '(lambda (x) x))
; 2

(define (timings)
  (let* ((_gen (lambda (m) (unq `((,P ,(q P)) ,(q (q m))))))
         (Ackermann_gen (_gen Ackermann))
         (Ackermann_ (lambda (i) (unq `(,Ackermann_gen ,(q (num i))))))
         (Ackermann_0 (Ackermann_ 0))
         (Ackermann_1 (Ackermann_ 1))
         (Ackermann_2 (Ackermann_ 2))
         (Ackermann_3 (Ackermann_ 3))
         (E_gen (_gen E))
         (P_gen (_gen P)))
    (list

      (reduce `((,Ackermann ,(num 0)) ,(num 3)) (num 4))
      (reduce `((,Ackermann ,(num 1)) ,(num 3)) (num 5))
      (reduce `((,Ackermann ,(num 2)) ,(num 3)) (num 9))
      (reduce `((,Ackermann ,(num 3)) ,(num 3)) (num 61))

      (reduce `(,Ackermann_0 ,(num 3)) (num 4))
      (reduce `(,Ackermann_1 ,(num 3)) (num 5))
      (reduce `(,Ackermann_2 ,(num 3)) (num 9))
      (reduce `(,Ackermann_3 ,(num 3)) (num 61))

      (reduce `(((,E ,(q Ackermann)) ,(num 0)) ,(num 3)) (num 4))
      (reduce `(((,E ,(q Ackermann)) ,(num 1)) ,(num 3)) (num 5)) ; Why do we get 54 reductions here, where Mogensen gets only 53?
      (reduce `(((,E ,(q Ackermann)) ,(num 2)) ,(num 3)) (num 9))
      (reduce `(((,E ,(q Ackermann)) ,(num 3)) ,(num 3)) (num 61))

      ; Skipped a group here because E_Ackermann = Ackermann

      (reduce `((,P ,(q Ackermann)) ,(q (num 0))) (q Ackermann_0))
      (reduce `((,P ,(q Ackermann)) ,(q (num 1))) (q Ackermann_1))
      (reduce `((,P ,(q Ackermann)) ,(q (num 2))) (q Ackermann_2))
      (reduce `((,P ,(q Ackermann)) ,(q (num 3))) (q Ackermann_3))

      (reduce `(,Ackermann_gen ,(q (num 0))) (q Ackermann_0))
      (reduce `(,Ackermann_gen ,(q (num 1))) (q Ackermann_1))
      (reduce `(,Ackermann_gen ,(q (num 2))) (q Ackermann_2))
      (reduce `(,Ackermann_gen ,(q (num 3))) (q Ackermann_3))

      (reduce `((,P ,(q E)) ,(q (q Ackermann))) (q Ackermann))
      (reduce `(,E_gen ,(q (q Ackermann))) (q Ackermann))

      (reduce `((,P ,(q P)) ,(q (q Ackermann))) (q Ackermann_gen))
      (reduce `((,P ,(q P)) ,(q (q E))) (q E_gen))
      (reduce `((,P ,(q P)) ,(q (q P))) (q P_gen))

      (reduce `(,P_gen ,(q (q Ackermann))) (q Ackermann_gen))
      (reduce `(,P_gen ,(q (q E))) (q E_gen))
      (reduce `(,P_gen ,(q (q P))) (q P_gen)))))
