(module reader scheme
   (require "monad.ss")
   (provide reader-monad reader-trans)

   (define reader-monad
     (new (reader-mixin monad%)
          (bind (lambda (f g) (lambda (e) (f (g e) e))))
          (return (lambda (a) (lambda (e) a)))
          (ask (lambda () (lambda (e) e)))))

   (define (reader-trans m)
     (new (trans-mixin (reader-mixin monad%))
          (return (lambda (a) (lambda (e) (return a m))))
          (bind (lambda (f g) (lambda (e) (do (a <- g e) (f a e)) m)))
          (ask (lambda () (lambda (e) (return e m))))
          (lift (lambda (n) (lambda (e) n)))
          (base (lambda () m)))))


