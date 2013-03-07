(module list scheme
   (require "monad.ss")
   (provide list-monad)
   (define list-monad
     (new (amb-mixin monad%)
          (bind (lambda (f xs) (apply append (map f xs))))
          (return list)
          (amb list)))

;  ((do (x <- amb 1 2 3) (y <- amb 4 5) (return (* x y))) list-monad) => (4 5 8 10 12 15)
  )
