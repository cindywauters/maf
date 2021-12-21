;; renamed lambdas/lets: 1
 
(define mmap (<change>
      (lambda (f lst)
         (if (null? lst)
            ()
            (cons (f (car lst)) (mmap f (cdr lst)))))
      (lambda (_f0 _lst0)
         (if (null? _lst0)
            ()
            (cons (_f0 (car _lst0)) (mmap _f0 (cdr _lst0)))))))
 
(mmap car (__toplevel_cons (__toplevel_cons 'a (__toplevel_cons 1 ())) ()))
 
(mmap cdr (__toplevel_cons (__toplevel_cons 'b (__toplevel_cons 2 ())) ()))
 
