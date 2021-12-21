;; renamed lambdas/lets: 1
 
(define derde-machtswortel (<change>
      (lambda (x)
         (define epsilon 1.000000e-02)
         (define hulp-derde-machtswortel (lambda (y)
               (if (< (abs (- (* y y y) x)) epsilon)
                  y
                  (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3)))))
         (hulp-derde-machtswortel (/ x 3)))
      (lambda (_x0)
         (define epsilon 1.000000e-02)
         (define hulp-derde-machtswortel (lambda (_y0)
               (if (< (abs (- (* _y0 _y0 _y0) _x0)) epsilon)
                  _y0
                  (hulp-derde-machtswortel (/ (+ (/ _x0 (* _y0 _y0)) _y0 _y0) 3)))))
         (hulp-derde-machtswortel (/ _x0 3)))))
 
(= 3.000000e+00 (exact->inexact (derde-machtswortel 27)))
 
