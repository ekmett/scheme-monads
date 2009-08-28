;; Edward Kmett (8/28/09)
;; A poor man's testing framework

(define-syntax assert
  (syntax-rules ()
    ((assert proposition) 
       (if (not proposition) 
         (begin
           (display "Assertion Failed: ")
           (display 'proposition)
           (newline))))))

(define-syntax assert-eq
  (syntax-rules ()
    ((assert-eq x y) (assert (equal? x y)))))
