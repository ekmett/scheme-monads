;; Edward Kmett (8/28/09)

;; Basic definitions for:
;; functor
;;  pointed
;;   applicative
;;    alternative
;;    monad
;;     monad-plus
;;  copointed
;;   comonad

;; Uses positional dictionaries, placing misc methods stuff in an alist.
;; Nothing outside of this module concerns itself with how we handle dispatch.

(load "assert.scm")  ;; a poor man's testing framework
(load "curried.scm") ;; curried argument lists, dispatching surplus arguments
                         
;; I combinator
(define-curried (id x) x)

;; function composition
(define-curried (compose f g x) (f (g x)))

;; a valid functor map operation for any monad
(define-curried (make-monad-fmap unit star f) 
  (star (compose unit f)))

;; a valid functor map operation for any applicative
(define-curried (make-applicative-fmap unit ap f)
  (ap (unit f)))

;; a valid functor map operation for any comonad
(define-curried (make-comonad-fmap tinu rats f) 
  (rats (compose f tinu)))

(define-curried (functor fmap) 
  (list '() fmap))

(define-curried (pointed fmap unit)
  (list '() fmap (list unit)))

(define-curried (applicative unit ap)
  (list '() (make-applicative-fmap unit ap) (list unit ap)))

(define-curried (alternative unit ap zero plus)
  (list '() (make-applicative-fmap unit ap) (list unit ap) (list zero plus)))
  
(define-curried (monad unit star)
  (let ((fmap (make-monad-fmap unit star)))
    (list '() fmap (list unit (make-monad-ap fmap star) star))))

;; monad-plus f = (
;;    alist,                                  -- attributes 
;;    (forall a b. (a -> b) * f a -> f b),    -- fmap
;;                                            -- multiplication
;;    ((forall a. a -> f a),                    -- unit
;;     (forall a b. f (a -> b) -> f a -> f b),  -- ap
;;     (forall a b. (a -> f b) * f a -> f b)),  -- star
;;                                            -- addition
;;    ((forall a. f a),                         -- zero
;;     (forall a. f a -> f a -> f a)))          -- plus

(define-curried (monad-plus unit star zero plus)
  (let ((fmap (make-monad-fmap unit star)))
    (list '() fmap (list unit (make-monad-ap fmap star) star) (list zero plus))))
       
;; the categorical dual of a monad
;; comonad f = (
;;    alist,                                 -- attributes 
;;    (forall a b. (a -> b) * f a -> f b),   -- fmap
;;                                           -- multiplication
;;    ((forall a. f a -> a),                   -- tinu
;;     (forall a b. (f a -> b) * f a -> f b))) -- rats
 
(define-curried (copointed fmap tinu)
  (list '() fmap (list tinu)))

(define-curried (comonad tinu rats)
  (list '() (make-comonad-fmap tinu rats) (list tinu rats)))

;; fmap :: functor f ~> (a -> b) ~> f a -> f b
(define-curried (fmap functor f x) 
  ((cadr functor) f x))

;; unit :: monad m ~> a -> m a
(define-curried (unit monad x) 
  ((car (caddr monad)) x))

;; ap :: monad m ~> m (a -> b) ~> m a -> m b
;; ap = lift-m2 id
(define-curried (ap monad mf mx) 
  ((cadr (caddr monad)) f m))

  ;; star :: monad m ~> (a -> m b) ~> m a -> m b
(define-curried (star monad f m) 
  ((caddr (caddr monad)) f m))

;; zero :: monad-plus m -> m a
(define-curried (zero monad) (car (cadddr monad)))

;; plus :: monad-plus m ~> m a ~> m a -> m a
(define-curried (plus monad m n) ((cadr (cadddr monad)) m n))

;; tinu :: comonad w ~> w a -> a
(define-curried (tinu comonad w) ((car (caddr comonad)) w))

;; rats :: comonad w ~> (w a -> b) ~> w a -> w b
(define-curried (rats comonad f w) ((cadr (caddr comonad)) f w))

;; with-attribute : (slot . value) ~> functor f -> { functor f | f[slot] = value }
(define-curried (with-attribute attrval functor)
  (cons (cons attrval (car functor)) (cdr functor)))

;; with-fmap : { xs : forall a b. (a -> b) * f a -> f b) } ~> { G : functor f } -> { F : functor f | fmap F == xs, p F == p G, otherwise} 
(define-curried (with-fmap fmap functor)
  (cons (car functor) (cons fmap (cddr functor))))

;; attribute : slot ~> functor f -> f[slot]
(define-curried (attribute attr f)
  (cdr (assv attr (car f))))

;; monad-state
(define-curried (get monad) 
  (attribute 'get monad))

(define-curried (put monad state) 
  (attribute 'put monad state))

;; monad-writer
(define-curried (tell monad log) 
  (attribute 'tell monad log))

;; monad-reader
(define-curried (ask monad) 
  (attribute 'ask monad))

;; monad-transformer
(define-curried (lift monad mx) 
  (attribute 'lift monad mx))

;; join :: monad m ~> m (m a) -> m a
(define-curried (join monad) 
  (star monad id))

;; duplicate :: comonad w ~> w a -> w (w a)
(define-curried (duplicate comonad)
  (rats comonad id))

(define-curried (make-monad-ap fmap star mf mx)
  (star (lambda (f) (fmap f mx)) mf))
  
;; lift-a2 :: applicative f ~> (a -> b -> c) ~> f a ~> f b -> m c
;; lift a binary calculation into a monad
(define-curried (lift-a2 a f mx my)
  (ap a (fmap a f mx) my))

;; Given: monad m, k = (kleisli-compose m), u = (unit monad)
;; (k f u) = f
;; (k u f) = f
;; (k (k a b) c) = k (a (k b c)
(define-curried (kleisli-compose monad f g)
  (compose (star monad f) g))

;; Given: comonad w, k = (cokleisli-compose w), u = (tinu comonad)
;; (k f u) = f
;; (k u f) = f
;; (k (k a b) c) = k (a (k b c)
(define-curried (cokleisli-compose comonad f g)
  (compose f (rats comonad g)))

;; idiom brackets
(define-syntax idiom
  (syntax-rules ()
    ((idiom a f) (unit a f))
    ((idiom a f x) (fmap a f x))
    ((idiom a f x y ...) (fold-left (ap a) (fmap a f x) y ...))))
  
;; monadic do sugar
(define-syntax do*
  (syntax-rules ()
    ((do* m comp-body) comp-body)
    ((do* m ((x0 comp0)) comp-body) (star m (lambda (x0) comp-body) comp0))
    ((do* m ((x0 comp0) (x comp) ...) comp-body)
     (star m (lambda (x0) (do* m ((x comp) ...) comp-body)) comp0))))

;; comonadic 'codo' sugar
;; NB: since tinu and rats occupy the same positional slots as unit and star, this could be elided.
(define-syntax od*
  (syntax-rules ()
    ((od* m comp-body) comp-body)
    ((od* m ((x0 comp0)) comp-body) (rats m (lambda (x0) comp-body) comp0))
    ((od* m ((x0 comp0) (x comp) ...) comp-body)
     (rats m (lambda (x0) (od* m ((x comp) ...) comp-body)) comp0))))

(define-curried (gets monad f)
  (fmap monad f (get monad)))

(define-curried (asks monad f)
  (fmap monad f (ask monad)))

(define-curried (modify monad f)
  (star monad (lambda (x) (put monad (f x))) (get monad)))
