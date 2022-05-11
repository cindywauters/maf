(define (close-enough-guess? guess x)
  (< (abs (-  (* guess guess) x)) (<change> 0.0001 0.01)))

(define (sqrt guess x)
  (let ((next-guess (/ (+ guess (/ x guess)) 2.0)))
    (if (close-enough-guess? next-guess x)
         next-guess
        (sqrt next-guess x))))

(sqrt 1.0 9)