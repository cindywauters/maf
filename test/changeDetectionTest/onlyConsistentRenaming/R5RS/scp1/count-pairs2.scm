;; renamed lambdas/lets: 2
 
(define count-pairs (<change>
      (lambda (lst)
         (let ((path ()))
            (define count (lambda (current)
                  (if (null? current)
                     0
                     (if (not (pair? current))
                        0
                        (if (memq current path)
                           0
                           (begin
                              (set! path (cons current path))
                              (+ 1 (count (car current)) (count (cdr current)))))))))
            (count lst)))
      (lambda (_lst0)
         (let ((_path0 ()))
            (define count (lambda (_current0)
                  (if (null? _current0)
                     0
                     (if (not (pair? _current0))
                        0
                        (if (memq _current0 _path0)
                           0
                           (begin
                              (set! _path0 (cons _current0 _path0))
                              (+ 1 (count (car _current0)) (count (cdr _current0)))))))))
            (count _lst0)))))
 
(define ret3 (cons 'a (cons 'b (cons 'c ()))))
 
(define ret4 (let ((last (cons 'c ())))
      (cons last (cons 'b last))))
 
(define ret7 (<change>
      (let* ((last (cons 'c ()))
             (middle (cons last last)))
         (cons middle middle))
      (let* ((_last0 (cons 'c ()))
             (_middle0 (cons _last0 _last0)))
         (cons _middle0 _middle0))))
 
(define retno (let* ((last (cons 'c ()))
          (lst (cons 'a (cons 'b last))))
      (set-cdr! last lst)
      lst))
 
(= 3 (count-pairs ret3) (count-pairs ret4) (count-pairs ret7) (count-pairs retno))
 
