; Changes:
; * removed: 0
; * added: 1
; * swaps: 0
; * negated predicates: 0
(letrec ((zero (lambda (f x)
                 x))
         (inc (lambda (n)
                (lambda (f x)
                   (f (n f x)))))
         (plus (lambda (m n)
                 (lambda (f x)
                    (m f (n f x))))))
   ((plus (inc (inc (inc zero))) (plus (inc (inc zero)) (inc zero)))
      (lambda (x)
         (<change>
            ()
            (display +))
         (+ x 1))
      0))