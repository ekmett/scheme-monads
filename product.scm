;; The Product Comonad
;; Edward Kmett (8/28/09)

(load "base.scm")

(define product-comonad
  (comonad 
    ;; tinu
    cdr 
    ;; rats
    (lambda (f p)
      (cons (car p) (f p)))))
  
(assert-eq 
    (cons 6 90)
    (rats product-comonad (lambda (p) (* (car p) (tinu product-comonad p))) (cons 6 15)))
