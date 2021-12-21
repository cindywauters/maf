;; renamed lambdas/lets: 1
 
(define bubble-sort (lambda (vector)
      (define swap (lambda (vector index1 index2)
            (<change>
               (let ((temp (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 temp))
               (let ((_temp0 (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 _temp0)))))
      (define bubble (lambda (index)
            (define bubble-iter (lambda (index1 changed)
                  (if (<= index1 index)
                     (begin
                        (if (> (vector-ref vector index1) (vector-ref vector (+ index1 1)))
                           (begin
                              (swap vector index1 (+ index1 1))
                              (set! changed #t))
                           #f)
                        (bubble-iter (+ index1 1) changed))
                     changed)))
            (bubble-iter 0 #f)))
      (define bubble-sort-iter (lambda (index)
            (if (>= index 0)
               (if (bubble index)
                  (bubble-sort-iter (- index 1))
                  #f)
               #f)))
      (bubble-sort-iter (- (vector-length vector) 2))))
 
(define vect (vector 9 5 1 7 8 9 4 6 2 3))
 
(bubble-sort vect)
 
(equal? vect (vector 1 2 3 4 5 6 7 8 9 9))
 
