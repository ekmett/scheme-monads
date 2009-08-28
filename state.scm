;; Edward Kmett (8/29/08)
;; The state monad

(load "base.scm")

;; type state s a = s -> (a . s)

;; state-unit : a -> state s a
(define-curried (state-unit a s) (cons a s))

;; state-star : (a -> state s b) ~> state s a -> state s b
;; state-star : (a -> s -> (b . s)) -> (s -> (a . s)) -> s -> (b . s)
(define-curried (state-star f ma s)
  (let ((t (ma s)))
    ((f (car t)) (cdr t))))

;; state-get : state s s
(define-curried (state-get s) (cons s s))

;; state-put : s -> state s s 
(define-curried (state-put a _) (cons a a))

(define state-monad
  (with-attribute (cons 'get state-get)
    (with-attribute (cons 'put state-put)
      (monad state-unit state-star))))