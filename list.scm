;; The List Monad
;; Edward Kmett (8/28/09)

(load "base.scm")

;; append-map :: (a -> [b]) -> [a] -> [b]
(define-curried (append-map f xs)
  (apply append (map f xs)))

;; list-monad : monad-plus []
(define list-monad 
  (monad-plus list append-map '() append))

(assert-eq 
 '(2 4 4 5) 
 (do* list-monad ((x '(1 2))) 
      (list (* 2 x) (+ 3 x))))