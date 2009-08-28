;; The Writer Monad
;; Edward Kmett (8/28/09)

(load "base.scm")

;; make a writer monad given a monoid specified by its unit
;; and associative binary operation. The binary operation should
;; be able to take both arguments in one application. So should
;; be defined either using the curried macro or as taking two 
;; arguments
(define-curried (make-writer e op)
  (with-attribute (cons 'tell (lambda (x) (cons x '())))
    (monad
          ;; unit
          (lambda (x) (cons e x))
          ;; star
          (lambda (f m) 
              (let ((r (f (cdr m))))
                (cons (op (car m) (car r)) (cdr r)))))))

(assert-eq '(5)
   (let ((w (make-writer 0 (lambda (x y) (+ x y)))))
       (do* w ((x (unit w 1)) (y (tell w x))) (tell w 4))))