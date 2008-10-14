; Two helper macros to make defining and applying curried functions easier

(define-syntax lambda-c
(syntax-rules ()
  ((lambda-c () body) body)
  ((lambda-c (v . vs) body) (lambda (v) (lambda-c vs body)))))

(define-syntax @c
 (syntax-rules ()
   ((@c f) f)
   ((@c f arg . args) (@c (f arg) . args))))

; Church-encoding of pairs

(define rfst (lambda-c (a b) a))
(define rsnd (lambda-c (a b) b))
(define rcons (lambda-c (a b sel) (@c sel a b)))

; Operations on lists that are Church-encoded as folds
; http://okmij.org/ftp/Computation/Continuations.html#cdr-fstream

(define church-nil
  (lambda-c (cons nil) nil))

(define church-cons
  (lambda-c (car cdr cons nil)
    (@c cons car (@c cdr cons nil))))

(define (church-car l)
  (@c l (lambda-c (car cadr) car)
        'undefined))

(define (church-case l)
  (@c l (lambda-c (car case-cdr sk fk)
          (@c sk car (lambda-c (cons nil)
                       (@c case-cdr (lambda-c (cadr cddr)
                                      (@c cons cadr (@c cddr cons nil)))
                                    nil))))
        (lambda-c (sk fk) fk)))

(define (church-cdr l)
  (@c church-case l (lambda-c (car cdr) cdr)
                    'undefined))

(define church-append
  (lambda-c (l1 l2 cons nil)
    (@c l1 cons (@c l2 cons nil))))

(define church-map
  (lambda-c (f l cons)
    (l (lambda-c (car map-f-cdr) (@c cons (f car) map-f-cdr)))))

(define church-ref-length
  (lambda-c (l-index l)
    (church-car (@c l-index (lambda-c (car) church-cdr) l))))

; To convert HOAS to de Bruijn indices use appD and lamD below to build the
; expression and apply the result to church-nil, then Z, S, A, and L.  The
; Church-encoded list argument ("envD" below), initialized thus to church-nil,
; contains the variables currently in scope, from outermost to innermost (!).

; Example:
; > (unparse1 (@c lamD (lambda (x) (let ((axx (@c appD x x))) (@c appD axx (lamD (lambda (y) (@c appD y axx)))))) church-nil))
; (L (A (A Z Z) (L (A Z (A (S Z) (S Z))))))

(define appD
  (lambda-c (c1 c2 envD)
    (let ((e1 (c1 envD))
          (e2 (c2 envD)))
      (lambda-c (Z S A L)
        (@c A (@c e1 Z S A L) (@c e2 Z S A L))))))

(define lamD
  (lambda-c (f envD)
    (let ((body
           (@c f (church-ref-length envD)
                 (@c church-append (@c church-map (lambda-c (c Z S A L)
                                                    (S (@c c Z S A L)))
                                                  envD)
                                   (@c church-cons (lambda-c (Z S A L) Z)
                                                   church-nil)))))
      (lambda-c (Z S A L) (L (@c body Z S A L))))))

(define (unparse1 t)
  (@c t
      'Z
      (lambda-c (e1) `(S ,e1))
      (lambda-c (e1 e2) `(A ,e1 ,e2))
      (lambda-c (e1) `(L ,e1))))

; Offline specializer for a two-level language using de Bruijn indices

(define (spec t)
  (let ((env0 (lambda (x) x))
        (Z (lambda-c (env) (env rfst)))
        (S (lambda-c (v env) (v (env rsnd))))
        (As (lambda-c (e1 e2 env) ((e1 env) (e2 env))))
        (Ls (lambda-c (e env x) (e (@c rcons x env))))
        (Ad (lambda-c (e1 e2 env) (@c appD (e1 env) (e2 env))))
        (Ld (lambda-c (e env) (lamD (lambda (x) (e (@c rcons x env)))))))
    (@c t Z S As Ls Ad Ld env0)))
