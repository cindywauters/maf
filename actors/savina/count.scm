;; Adapted from Savina benchmarks ("Counting Actor" benchmarks, coming from Theron)
(letrec ((N 10)
         (producer-actor
          (actor "producer" (counter)
                 (increment ()
                            (letrec ((loop (lambda (n)
                                             (if (> n 0)
                                                 (begin
                                                   (send counter increment)
                                                   (loop (- n 1)))
                                                 'done))))
                              (loop N)
                              (send counter retrieve self)
                              (become producer-actor counter)))
                 (result (count)
                         (if (= count N)
                             (displayln "Success!")
                             (displayln "Error!"))
                         (terminate))))
         (counting-actor
          (actor "counting" (count)
                 (increment ()
                            (become counting-actor (+ count 1)))
                 (retrieve (to)
                           (send to result count)
                           (terminate))))
         (counter (create counting-actor 0))
         (producer (create producer-actor counter)))
  (send producer increment))
