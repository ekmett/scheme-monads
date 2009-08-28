;; We'll need Oleg's pmatch macro to get clean Haskell-style monads
(load "pmatch.scm")

;; poor man's testing framework
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
                         
;; I combinator
(define id 
  (lambda (x) x))

;; function composition
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

;; a valid functor map operation for any monad
(define make-monad-fmap 
  (lambda (unit star) 
    (lambda (f) 
      (star (lambda (x) (unit (f x)))))))

;; a valid functor map operation for any comonad
(define make-comonad-fmap 
  (lambda (tinu rats) 
    (lambda (f) 
      (rats (lambda (x) (f (tinu x)))))))

;; simple positional dictionaries for the monad methods

;; TODO: consider moving everything into the alist dictionary

;; type functor f = (alist, (forall a b. (a -> b) -> f a -> f b))
;; the following laws should be satisfied by any 'instance':

;; 1.) fmap functor f . fmap functor g = fmap functor (compose f g)
;; 2.) fmap functor id = id

;; Under Hindley-Milner, the former is a free theorem given the latter.

(define functor
  (lambda (fmap) 
    (list '() fmap)))

;; type monad f = (
;;    alist,                                  -- attributes 
;;    (forall a b. (a -> b) -> f a -> f b),   -- fmap
;;    (forall a. a -> f a),                   -- unit
;;    (forall a b. (a -> f b) -> f a -> f b)) -- star

;; Required to satisfy the monad laws, which can be stated (more cleanly) at the site of kleisli-compose below
(define (monad unit star) 
  (list '() (make-monad-fmap unit star) unit star))

;; the categorical dual of a monad
;; type comonad f = (
;;    alist,                                  -- attributes 
;;    (forall a b. (a -> b) -> f a -> f b),   -- fmap
;;    (forall a. f a -> a),                   -- tinu
;;    (forall a b. (f a -> b) -> f a -> f b)) -- rats
(define (comonad tinu rats) 
  (list '() (make-comonad-fmap tinu rats) tinu rats))

(define (monad-plus unit star zero plus)
  (list '() (make-monad-fmap unit star) unit star zero plus))
  
;; fmap :: functor f => (a -> b) -> f a -> f b
(define fmap cadr)
;; unit :: monad m => a -> m a
(define unit caddr)
;; star :: monad m => (a -> m b) -> m a -> m b
(define star cadddr)
;; zero :: monad-plus m => m a
(define zero (compose cadddr cdr))
;; plus :: monad-plus m => m a -> m a -> m a
(define plus (compose cadddr cddr))

;; tinu :: comonad w => w a -> a
(define tinu caddr)
;; rats :: comonad w => (w a -> b) -> w a -> w b
(define rats cadddr)

;; attribute :: slot -> functor f -> f[slot]
(define attribute
  (lambda (attr)
    (lambda (f)
     (cdr (assv attr (car f))))))

;; (slot * value) -> functor f -> functor f | f[slot] = value
(define with-attribute
  (lambda (attrval functor)
      (cons (cons attrval (car functor)) (cdr functor))))

;; monad-state
(define get (attribute 'get))
(define put (attribute 'put))

;; monad-writer
(define tell (attribute 'tell))

;; monad-reader
(define ask (attribute 'ask))

;; monad-transformer
(define lift (attribute 'lift))

;; join :: monad m => m (m a) -> m a
(define join
  (lambda (monad)
    ((star monad) id)))

;; duplicate :: comonad w => w a -> w (w a)
(define duplicate
  (lambda (comonad)
    ((rats comonad) id)))

;; ap :: monad m => m (a -> b) -> m a -> m b
;; ap = lift-m2 id
(define ap
  (lambda (monad)
    (lambda (mf)
      (lambda (mx)
        (do* monad ((f mf) 
                    (x mx)) 
                   ((unit monad) (f x)))))))

;; lift-m2 :: monad m => (a -> b -> c) -> m a -> m b -> m c
;; lift a binary non-monadic calculation into a monad
(define lift-m2
  (lambda (monad)
    (lambda (f)
      (lambda (mx)
        (lambda (my)
          (do* monad ((x mx)
                      (y my))
                     ((unit monad) ((f x) y))))))))

;; Given: k = (kleisli-compose monad), u = (unit monad)
;; (k f u) = f
;; (k u f) = f
;; (k (k a b) c) = k (a (k b c)
(define kleisli-compose
  (lambda (monad)
    (lambda (f g)
      (compose ((star monad) f) g))))

;; Given: k = (cokleisli-compose comonad), u = (tinu comonad)
;; (k f u) = f
;; (k u f) = f
;; (k (k a b) c) = k (a (k b c)
(define cokleisli-compose
  (lambda (comonad)
    (lambda f g)
       (compose (f ((rats comonad) g)))))

;; monadic do sugar
(define-syntax do*
  (syntax-rules ()
    ((do* m comp-body) comp-body)
    ((do* m ((x0 comp0)) comp-body) (((star m) (lambda (x0) comp-body)) comp0))
    ((do* m ((x0 comp0) (x comp) ...) comp-body)
     (((star m) (lambda (x0) (do* m ((x comp) ...) comp-body))) comp0))))

;; comonadic 'codo' sugar
(define-syntax od*
  (syntax-rules ()
    ((od* m comp-body) comp-body)
    ((od* m ((x0 comp0)) comp-body) (((rats m) (lambda (x0) comp-body)) comp0))
    ((od* m ((x0 comp0) (x comp) ...) comp-body)
     (((rats m) (lambda (x0) (od* m ((x comp) ...) comp-body))) comp0))))

;; the identity (co)monad
(define id-monad (monad id id))
(define id-comonad (comonad id id))

;; append-map (a -> [b]) -> [a] -> [b]
(define append-map
  (lambda (f)
    (lambda (xs)
      (apply append (map f xs)))))

;; list-monad : monad-plus []
(define list-monad
  (monad-plus
      list
      append-map
      '()
      append))

(assert-eq '(2 4 4 5) (do* list-monad ((x '(1 2))) (list (* 2 x) (+ 3 x))))

;; Haskell-style tagged maybe monad
;; type maybe a = ('just . a) | 'nothing
;; just : a -> maybe a
(define just (lambda (x) (cons 'just x)))
;; nothing : maybe a
(define nothing 'nothing)
;; maybe-star : (a -> maybe b) -> maybe a -> maybe b
(define maybe-star
  (lambda (f)
    (lambda (m) 
      (pmatch m
        ('nothing nothing)
        (('just . ,x) (f x))))))

;; maybe-plus : maybe a -> maybe a -> maybe a
(define maybe-plus
  (lambda (m)
    (lambda (n)
      (cond
        ((eqv? m 'nothing) n)
        (else m)))))

;; maybe : monad-plus maybe
(define maybe
  (monad-plus just maybe-star nothing maybe-plus))


;; A Church-encoded maybe monad
;; type church-maybe a = forall b. b * (a -> b) -> b

;; church-maybe-unit : a -> church-maybe a
(define church-maybe-unit (lambda (x) (lambda (_ f) (f x))))

;; church-maybe-star : (a -> church-maybe b) -> church-maybe a -> church-maybe b
(define church-maybe-star
    (lambda (k)
      (lambda (m)
         (lambda (z f)
           ((m z k) z f)))))

;; church-maybe-zero : church-maybe a
(define church-maybe-zero (lambda (z _) z))

;; church-maybe-plus : church-maybe a -> church-maybe a -> church-maybe a
(define church-maybe-plus
     (lambda (m)
       (lambda (n)
         (lambda (z f)
           (m (n z f) f)))))

;; church-maybe : monad church-maybe
(define church-maybe 
  (monad-plus church-maybe-unit church-maybe-star church-maybe-zero church-maybe-plus))

;; run-church-maybe : b -> (a -> b) -> church-maybe a -> b
(define run-church-maybe
   (lambda (z f m)
      (m z f)))


;; yoneda-fmap : (a -> c) -> ((a -> b) -> f b) -> (c -> b) -> f b
(define yoneda-fmap
  (lambda (f)
     (lambda (m)
       (lambda (k)
         (m (compose k f))))))

;; yoneda f a = (forall b. (a -> b) -> f b)
;; 'yoneda f' is a functor regardless of the definition of f.
(define yoneda-functor (functor yoneda-fmap))

;; construct the unit of a yoneda monad of a given monad
(define make-yoneda-unit
  (lambda (base-monad)
    (lambda (f)
      (unit base-monad (f a)))))

;; construct the star of a yoneda monad for a given monad
;; monad f => (a -> yoneda f b) -> yoneda f a -> yoneda f b
;; monad f => (a -> forall b. (b -> c) -> f c) -> (forall d. (a -> d) -> f d) -> forall e. (a -> e) -> f e
(define make-yoneda-star
  (lambda (base-monad)
    (lambda (k)
      (lambda (m)
        (lambda (f)
          (((star base-monad) (lambda (a) ((k a) f))) (m id)))))))

;; lower-yoneda : yoneda f a -> f a
;; lower-yoneda : (forall b. (a -> b) -> f b) -> f a
(define yoneda-lower
  (lambda (f)
    (f id)))

;; to-yoneda : functor f => f a -> yoneda f a
(define make-yoneda-lift
  (lambda (base-functor)
    (lambda (fa)
      (lambda (ab)
        (((fmap base-functor) ab) fa)))))

;; 'yoneda f' is a monad if f is a monad.
;; TODO: lift the underlying attributes
(define make-yoneda-monad
  (lambda (base-monad)
     (list 
        (list 
           (cons 'lift (make-yoneda-lift base-monad))) 
         yoneda-fmap 
         (make-yoneda-unit base-monad) 
         (make-yoneda-star base-monad))))

;; type reader e a = e -> a

;; const :: a -> e -> a 
(define const
   (lambda (x) (lambda (_) x)))
  
;; reader-star :: (a -> reader e b) -> reader e a -> reader e b
(define reader-star 
  (lambda (f) (lambda (r) (lambda (e) ((f (r e)) e)))))
  
;; reader :: monad (reader e)
(define reader
  (with-attribute (cons 'ask id) 
     (monad const reader-star)))

;; run-reader :: reader e a -> e -> a
(define run-reader id)
        

(define make-writer
  (lambda (e op)
    (with-attribute (cons 'tell (lambda (x) (cons x '())))
       (monad
          ;; unit
          (lambda (x) (cons e x))
          ;; star
          (lambda (f) 
            (lambda (m) 
              (let ((r (f (cdr m))))
                (cons (op (car m) (car r)) (cdr r)))))))))


(assert-eq 
  '(2 4 4 5) 
  (let ((yoneda (make-yoneda-monad list-monad)))
    (yoneda-lower 
      (do* yoneda 
           ((x ((lift yoneda) '(1 2))))
           ((lift yoneda) (list (* 2 x) (+ 3 x))))))) 

(assert-eq 3
    ((do* reader ((x ((unit reader) 1)) (y (ask reader))) ((unit reader) (+ x y))) 2))

(assert-eq '(5)
   (let ((w (make-writer 0 (lambda (x y) (+ x y)))))
       (do* w ((x ((unit w) 1)) (y ((tell w) x))) ((tell w) 4))))

(define product-comonad
  (comonad 
     cdr
    (lambda (f) 
       (lambda (p)
          (cons (car p) (f p))))))

(assert-eq 
    (cons 6 90)
    (((rats product-comonad) (lambda (p) (* (car p) ((tinu product-comonad) p)))) (cons 6 15)))

;; does nothing
(define trivial-monad
  (monad-plus 
     (const '())
     (const (const '()))
     '()
     (const (const '()))))