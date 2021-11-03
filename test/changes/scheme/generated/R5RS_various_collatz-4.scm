; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 1
(letrec ((div2* (lambda (n s)
                  (if (= (* 2 n) s)
                     n
                     (if (= (+ (* 2 n) 1) s) n (div2* (- n 1) s)))))
         (div2 (lambda (n)
                 (div2* n n)))
         (hailstone* (lambda (n count)
                       (<change>
                          ()
                          (display (div2 n)))
                       (<change>
                          ()
                          +)
                       (if (<change> (= n 1) (not (= n 1)))
                          count
                          (if (even? n)
                             (hailstone* (div2 n) (+ count 1))
                             (hailstone* (+ (* 3 n) 1) (+ count 1))))))
         (hailstone (lambda (n)
                      (hailstone* n 0))))
   (hailstone 5))