;; The Identity Monad
;; Edward Kmett (8/28/09)

(load "base.scm")

(define id-monad (monad id id))
(define id-comonad (comonad id id))