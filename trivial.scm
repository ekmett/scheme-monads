;; The Trivial Monad
;; Edward Kmett (8/28/09)

(load "base.scm")

(define nil '())

;; does nothing
(define trivial-monad
  (monad-plus 
     (lambda (x) nil)
     (lambda (k m) nil)
     nil
     (lambda (m n) nil)))
