;; A Church-encoded maybe monad
;; Edward Kmett (8/28/09)

(load "base.scm")

;; type church-maybe a = forall b. b * (a -> b) -> b

;; church-maybe-unit : a -> church-maybe a
(define-curried (church-maybe-unit x _ f) 
  (f x))

;; church-maybe-star : (a -> church-maybe b) -> church-maybe a -> church-maybe b
(define-curried (church-maybe-star k m z f)
  ((m z k) z f))

;; church-maybe-zero : church-maybe a
(define-curried (church-maybe-zero z _) 
  z)

;; church-maybe-plus : church-maybe a -> church-maybe a -> church-maybe a
(define-curried (church-maybe-plus m n z f)
  (m (n z f) f))

;; church-maybe : monad church-maybe
(define church-maybe 
  (monad-plus church-maybe-unit church-maybe-star church-maybe-zero church-maybe-plus))

;; run-church-maybe : b -> (a -> b) -> church-maybe a -> b
(define-curried (run-church-maybe z f m)
  (m z f))
