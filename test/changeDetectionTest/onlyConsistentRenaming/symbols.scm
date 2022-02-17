(define ((<update> oldRec newRec) a)
 (if (< a 1)
     (display a)
     (begin
      (display a)
      ((<update> oldRec newRec) (- a 1)))))

((<update> oldRec newRec)  5)


