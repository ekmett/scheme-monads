(load "base.scm")

(define-curried (make-cofree-comonad functor)
  (let ((map (fmap functor)))
    (comonad
      ;; tinu
      car
      ;; rats
      (curried (f)
        (letrec ((alpha (lambda (w)
                          (cons (f w) (map alpha (cdr w))))))
          alpha)))))