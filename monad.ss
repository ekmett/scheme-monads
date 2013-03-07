(module monad scheme
  (require "curry.ss")

  (define (id a) a)

  ;; i'm too lazy to repeat this pattern for now.
  (define-syntax init-public
    (syntax-rules ()
      ((_) (begin))
      ((_ (m default) ms ...) (begin
                        (init-field (m default))
                        (public (internal-m m))
                        (define (internal-m . rest) (apply (get-field m this) rest))
                        (init-public ms ...)
                        ))
      ((_ m ms ...) (begin
                        (init-field m)
                        (public (internal-m m))
                        (define (internal-m . rest) (apply (get-field m this) rest))
                        (init-public ms ...)
                        ))))

  ;; functors
  (provide functor-interface functor% fmap)
  (define functor-interface (interface () fmap))
  (define functor% (class* object% (functor-interface)
                     (init-public fmap)
                     (super-new)))
  (define-curried (fmap f x m) (send m fmap f (x m)))

  ;; pointed functors
  (provide pointed-interface pointed% return)
  (define pointed-interface (interface (functor-interface) return))
  (define pointed% (class* functor% (pointed-interface)
                     (init-public return)
                     (init fmap)
                     (super-new (fmap fmap))))
  (define-curried (return x m) (send m return x))

  ;; applicative functors and idiom brackets
  (provide applicative-interface applicative% ap idiom)
  (define applicative-interface  (interface (pointed-interface) ap))
  (define applicative% (class* pointed% (applicative-interface)
                         (init-public ap)
                         (init return
                               (fmap (lambda (f x) (ap (return f) x))))
                         (super-new (fmap fmap) (return return))))
  (define-curried (ap mf mx m) (send m ap (mf m) (mx m)))


  (define-syntax idiom
    (syntax-rules ()
      ((idiom f) (unit f))
      ((idiom f x) (fmap f x))
      ((idiom f x y ...) (fold-left ap (fmap f x) y ...))))

  ;; monads and do sugar
  (provide monad-interface monad% bind then when unless do do*)
  (define monad-interface (interface (applicative-interface) bind))
  (define monad% (class* applicative% (monad-interface)
                   (init-public bind)
                   (init return)
                   (init (fmap (lambda (f mx) (bind (lambda (x) (return (f x))) mx)))
                         (ap (lambda (mf mx) (bind (lambda (f) (fmap f mx)) mf))))
                   (init-public (then (lambda (m n) (bind (lambda (_) n) m))))
                   (super-new (fmap fmap) (ap ap) (return return))))

  (define-curried (bind f x m) (send m bind (lambda (a) ((f a) m)) (x m)))
  (define-curried (then a b m) (send m then (a m) b m))
  (define-curried (when p s m) (if p (s m) (return '() m)))
  (define-curried (unless p s m) (if p (return '() m) (s m)))
  ;; join :: m (m a) -> m a -- before the reader transformation
  ;; join :: (monad m -> m (monad m -> m a) -> monad m -> m a
  (define-curried (join f ma m)
     (send m bind (lambda (g) (g m)) (f m)))


  ;; something closer to let*
  (define-syntax do*
    (syntax-rules ()
      ((do* comp-body) comp-body)
      ((do* ((x0 comp0)) comp-body) (bind (lambda (x0) comp-body) comp0))
      ((do* ((x0 comp0) (x comp) ...) comp-body)
       (bind (lambda (x0) (do* ((x comp) ...) comp-body)) comp0))))

  ;; a richer, more haskell-like do sugar. todo: pmatch support for destructuring binds with monad-fail?
  (define-syntax do
    (syntax-rules (let let* letrec letrec* <-)
      ((do s)                         s)
      ((do (x <- s ...) ss ...)           (bind (lambda (x) (do ss ...)) (s ...)))
      ((do (let bs) ss ...)           (let bs (do ss ...)))
      ((do (let* bs) ss ...)          (let* bs (do ss ...)))
      ((do (letrec bs) ss ...)        (letrec bs (do ss ...)))
      ((do (letrec* bs) ss ...)       (letrec* bs (do ss ...)))
      ((do s ss ...)                  (then s (do ss ...)))))

  ;; simple mixins
  (define-syntax define-mixin
    (syntax-rules ()
      ((_ mixin (interfaces ... ) (ip ...)) (define (mixin %) (class* % (interfaces ...)
                                                                 (init-public ip ...)
                                                                 (super-new))))))

  ;; functor-plus. TODO: just allow variadic 'return'?
  (provide amb-interface amb-mixin amb)
  (define amb-interface (interface () amb))
  (define-mixin amb-mixin (amb-interface) (amb))
  (define (amb . args) (lambda (m) (send/apply m amb args)))
  (define (plus . args) (lambda (m) (send/apply m plus (map (lambda (f) (f m)) args))))
  (define zero (amb))
  (define-curried (guard b m) (if b (send m return '()) (send m amb)))

  ;; monad/applicative/functor-transformer
  (provide trans-interface trans-mixin lift base)
  (define trans-interface  (interface () lift base))
  (define-mixin trans-mixin (trans-interface) (lift base))
  (define-curried (lift b m) (send m lift (b (send m base))))
  (define-curried (base m) (send m base))

  ;; state monad: TODO: consider making get a field?
  (provide state-interface state-mixin get put gets modify)
  (define state-interface  (interface () get put))
  (define-mixin state-mixin (state-interface) (get put))

  (define-curried (get m) (send m get))
  (define-curried (put s m) (send m put s))

  ; requires: state-mixin %monad
  (define-curried (modify f) (do (x <- get)
                         (put (f x))))
  ; requires: state-mixin %functor
  (define-curried (gets f) (fmap f get))

  ;; reader monad
  (provide reader-interface reader-mixin ask asks)

  (define reader-interface (interface () ask))
  (define-mixin reader-mixin (reader-interface) (ask))

  (define-curried (ask m)  (send m ask))

  ; requires: reader-mixin %functor
  (define-curried (asks f) (fmap f ask))

  ;; writer monad
  (provide writer-interface writer-mixin tell)
  (define writer-interface (interface () tell))
  (define-mixin writer-mixin (writer-interface) (tell))
  (define-curried (tell x m) (send m tell x)))
