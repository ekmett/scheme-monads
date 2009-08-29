(load "base.scm")
(load "list.scm") ;; for test


;; the cofree comonad has the form (a . f (a . f (a . f (....))))
;; the prod-type ideal comonad is categorically dual to the free monad.

;; the cofree comonad of a functor
(define-curried (make-cofree-comonad functor)
  (let ((map (fmap functor)))
    (comonad
      ;; tinu
      car
      ;; rats 
      (curried (f) ;; the use of curried propagates second argument to lambda (w) below.
        (letrec ((alpha (lambda (w)
                          (cons (f w) (map alpha (cdr w))))))
          alpha)))))

;; when the functor f has a natural notion of zero and plus, we can build a monad out of it as well!

;; The prod-type monad given rise to by a functor w/ a monoidal structure (functor-plus)
(define-curried (make-cofree-monad fp)
  (let ((base-map (fmap fp)) (base-zero (zero fp)) (base-plus (plus fp)))
    (monad
      ;; unit 
      (lambda (x) (cons x base-zero))
      ;; star
      (curried (f) ;; use of curried propagates second argument to lambda (m) below.
        (letrec
          ((alpha (lambda (m)
                    (let ((r (f (car m))))
                         (cons (car r) (base-plus (base-map alpha (cdr m)) (cdr r))))))) 
          alpha)))))
     
(assert-eq
   '(1 (2 (3) (4)) (3 (4) (5)) (2) (3))
   (let ((cofree-list (make-cofree-monad list-monad)))
     (let ((act ((star cofree-list (lambda (x) (cons x (list (unit cofree-list (+ x 1)) (unit cofree-list (+ x 2)))))))))
       (act (act (unit cofree-list 1))))))