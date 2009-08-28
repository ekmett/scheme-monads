;; The Reader Monad
;; Edward Kmett (8/28/09)

(load "base.scm")

;; type reader e a = e -> a

;; const :: a -> e -> a 
;; K combinator
(define-curried (const x _) x)
  
;; reader-star :: (a -> reader e b) -> reader e a -> reader e b
;; reader-star :: (a -> e -> b) -> (e -> a) -> e -> b

;; note the relation of reader-star to the S combinator:
;; first argument flipped
;; S :: (e -> a -> b) -> (e -> a) -> e -> b
;; (define-curried (S x y z) (x z (y z)))

(define-curried (reader-star f r e)
  ((f (r e)) e))
  
;; reader :: monad (reader e)
(define reader
  (with-attribute (cons 'ask id) 
     (monad const reader-star)))

(assert-eq 3
    ((do* reader ((x (unit reader 1)) (y (ask reader))) (const (+ x y))) 2))
