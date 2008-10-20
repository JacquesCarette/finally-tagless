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

; Church-encoding of option

(define rnone (lambda-c (selnone selsome) selnone))
(define (rsome s) (lambda-c (selnone selsome) (selsome s)))

; Online PE of the pure lambda calculus using HOAS, a la Mogensen,
; but with optional static terms, a la Sperber,
; and without passing redundant constructors of the output term syntax

(define appPE
  (lambda-c (app m n)
    (@c m rfst (@c rcons rnone (@c app (m rsnd) (n rsnd)))
               (lambda (g) (g n)))))

(define lamPE
  (lambda-c (lam g)
    (@c rcons (rsome g)
              (lam (lambda (x) (@c g (@c rcons rnone x) rsnd))))))

; For testing, let us resort to gensym for now

(define app (appPE (lambda-c (m n) `(,m ,n))))
(define lam (lamPE (lambda (g) (let ((x (gensym))) `(lambda (,x) ,(g x))))))

; A simple test: reducing ((lambda (x) (x x)) (lambda (x) x))

((@c app (lam (lambda (x) (@c app x x))) (lam (lambda (x) x))) rsnd)
; (lambda (g52) g52)

