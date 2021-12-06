(define (test-with-list)
  (<change>
   (list 1 (lambda (a) a))
   (list 1 (lambda (b) b))))

(test-with-list)