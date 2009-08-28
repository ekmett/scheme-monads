(load "base.scm")
(load "list.scm") ;; for test below

;; yoneda-fmap : (a -> c) -> ((a -> b) -> f b) -> (c -> b) -> f b
(define-curried (yoneda-fmap f m k)
  (m (compose k f)))

;; yoneda f a = (forall b. (a -> b) -> f b)
;; 'yoneda f' is a functor regardless of the definition of f!
(define yoneda-functor (functor yoneda-fmap))

;; construct the unit of a yoneda monad of a given monad
(define-curried (make-yoneda-unit base-monad f)
  (unit base-monad (f a)))

;; construct the star of a yoneda monad for a given monad
;; make-yoneda-star : monad f ~> (a -> yoneda f b) ~> yoneda f a -> yoneda f b
;; make-yoneda-star : monad f ~> (a -> forall b. (b -> c) -> f c) ~> (forall d. (a -> d) -> f d) ~> forall e. (a -> e) -> f e
(define-curried (make-yoneda-star base-monad k m f)
  (star base-monad (lambda (a) ((k a) f)) (m id)))

;; lower-yoneda : yoneda f a -> f a
;; lower-yoneda : (forall b. (a -> b) -> f b) -> f a
(define-curried (yoneda-lower f)
  (f id))

;; make-yoneda-ap : applicative f => yoneda f (a -> b) -> yoneda f a -> yoneda f b
;; make-yoneda-ap : (forall c. ((a -> b) -> c) -> f c) -> 
;;                  (forall d. (a -> d) -> f d) ->
;;                  (forall e. (a -> e) -> f e)
(define-curried (make-yoneda-ap a mg mx f)
  (ap a (mg (compose f)) (mx id)))

;; to-yoneda : functor f => f a -> yoneda f a
(define-curried (make-yoneda-lift base-functor fa ab)
  (fmap base-functor ab fa))

(define-curried (make-yoneda-zero base-monad _)
  (zero base-monad))

(define-curried (make-yoneda-plus base-monad m n f)
  (fmap base-monad f (plus base-monad (m id) (n id))))
      
;; 'yoneda f' is a monad if f is a monad.
;; TODO: lift the underlying attributes
;; TODO: make the resulting dictionary only have entries the source dictionary has
(define-curried (make-yoneda m)
  (list 
    (list 
      ;; TODO lift the other operations of the base monad
      (cons 'lift (make-yoneda-lift m))) 
    yoneda-fmap
    (list
      (make-yoneda-unit m)
      (make-yoneda-ap m)
      (make-yoneda-star m))
    (list
      (make-yoneda-zero m)
      (make-yoneda-plus m))))

(assert-eq 
  '(2 4 4 5) 
  (let ((yoneda (make-yoneda list-monad)))
    (yoneda-lower 
      (do* yoneda 
           ((x (lift yoneda '(1 2))))
           (lift yoneda (list (* 2 x) (+ 3 x))))))) 

