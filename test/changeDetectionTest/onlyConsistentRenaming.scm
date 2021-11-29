(define (fifth-test a)
  (<change>
   (let ((a 1)
         (b 2))
     (let* ((c (+ a b)))
       ((lambda (x) (+ x c ((lambda () (+ 5 6))))) 5)))
   (let ((a 1)
         (b 2))
     (let* ((f (+ a b)))
       ((lambda (x) (+ x f ((lambda () (+ 5 6))))) 5)))))

(fifth-test 10)

;(define first-test
;  (<change> (lambda (a b) (+ a b))
;            (lambda (a d) (+ a d))))

;(first-test 5 10)


;(define (second-test c)
;   (<change> (let ((a 5) (b 10)) (+ a b c))
;             (let ((a 5) (d 10)) (+ a d c))))

;(second-test 5)