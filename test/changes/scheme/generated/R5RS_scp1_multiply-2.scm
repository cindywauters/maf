; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((rec-multiply (lambda (a b)
                         (if (zero? b) 0 (+ a (rec-multiply a (- b 1))))))
         (iter-multiply (lambda (a b)
                          (letrec ((iter (lambda (result counter)
                                           (<change>
                                              ()
                                              counter)
                                           (if (zero? counter)
                                              result
                                              (iter (+ result a) (- counter 1))))))
                             (iter 0 b)))))
   (= 10 (rec-multiply 5 2) (iter-multiply 5 2)))