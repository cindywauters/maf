(define (something)
  (define vec (make-vector 1))
  (vector-set! vec 0 #t)
  (<change>
   (vector-set! vec 0 (lambda (a) a))
   (vector-set! vec 0 (lambda (b) b)))
  (vector-ref vec 0))

(something)

;(define (something)
;  (define vec (make-vector 1))
;   (vector-set! vec 0 (<change>
;      (lambda (a) a)
;      (lambda (b) b)))
;  (vector-ref vec 0))

;(something)