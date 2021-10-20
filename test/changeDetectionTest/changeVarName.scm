;; change name variable
(define (test n)
  (<change>
   (begin
    (define res (+ n 1))
    res)
   (begin
    (define result (+ n 1))
    result)))
(test 8)

;; change name variable + other change
;(define (test n)
;  (<change>
;   (begin
;    (define res (+ n 2))
;    res)
;   (begin
;    (define result (+ n 1))
;    result)))
;(test 8)