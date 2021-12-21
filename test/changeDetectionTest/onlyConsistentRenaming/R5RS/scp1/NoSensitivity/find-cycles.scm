;; renamed lambdas/lets: 3
 
(define ret4 (<change>
      (let ((last (cons 'c ())))
         (cons last (cons 'b last)))
      (let ((_last0 (cons 'c ())))
         (cons _last0 (cons 'b _last0)))))
 
(define ret7 (let* ((last (cons 'c ()))
          (middle (cons last last)))
      (cons middle middle)))
 
(define retno (<change>
      (let* ((last (cons 'c ()))
             (lst (cons 'a (cons 'b last))))
         (set-cdr! last lst)
         lst)
      (let* ((_last0 (cons 'c ()))
             (_lst0 (cons 'a (cons 'b _last0))))
         (set-cdr! _last0 _lst0)
         _lst0)))
 
(define cycles? (<change>
      (lambda (lst)
         (define find-cycles? (lambda (current path)
               (if (null? current)
                  #f
                  (if (memq current path)
                     #t
                     (find-cycles? (cdr current) (cons current path))))))
         (find-cycles? lst ()))
      (lambda (_lst0)
         (define find-cycles? (lambda (_current0 _path0)
               (if (null? _current0)
                  #f
                  (if (memq _current0 _path0)
                     #t
                     (find-cycles? (cdr _current0) (cons _current0 _path0))))))
         (find-cycles? _lst0 ()))))
 
(if (not (cycles? ()))
   (if (not (cycles? (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))))
      (if (not (cycles? ret4))
         (if (cycles? retno)
            (if (not (cycles? ret7))
               (cycles? (cons 'a (cons 'b retno)))
               #f)
            #f)
         #f)
      #f)
   #f)
 
