;; Tagged Maybe Monad
;; Edward Kmett (8/28/09)

(load "base.scm")

;; type maybe a = ('just . a) | 'nothing

;; just : a -> maybe a
(define-curried (just x) (cons 'just x))

;; nothing : maybe a
(define nothing 'nothing)

;; maybe-star : (a -> maybe b) -> maybe a -> maybe b
(define-curried (maybe-star f m)
  (if (eqv? 'nothing m) nothing (f (cdr x))))

;; maybe-plus : maybe a -> maybe a -> maybe a
(define-curried (maybe-plus m n)
  (if (eqv? m 'nothing) n m))

;; maybe : monad-plus maybe
(define maybe
  (monad-plus just maybe-star nothing maybe-plus))
