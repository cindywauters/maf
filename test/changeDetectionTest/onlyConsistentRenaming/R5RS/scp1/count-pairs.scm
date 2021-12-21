;; renamed lambdas/lets: 2
 
(define count-pairs (<change>
      (lambda (x)
         (if (not (pair? x))
            0
            (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))
      (lambda (_x0)
         (if (not (pair? _x0))
            0
            (+ (count-pairs (car _x0)) (count-pairs (cdr _x0)) 1)))))
 
(define ret3 (cons 'a (cons 'b (cons 'c ()))))
 
(define ret4 (<change>
      (let ((last (cons 'c ())))
         (cons last (cons 'b last)))
      (let ((_last0 (cons 'c ())))
         (cons _last0 (cons 'b _last0)))))
 
(define ret7 (let* ((last (cons 'c ()))
          (middle (cons last last)))
      (cons middle middle)))
 
(if (= (count-pairs ret3) 3)
   (if (= (count-pairs ret4) 4)
      (= (count-pairs ret7) 7)
      #f)
   #f)
 
