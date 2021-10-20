(define (create-string s1 s2)
  (string-append s1 " " s2))

(define (insert-fun-display n)
  (display (create-string "something" "else"))
  (<change> (display "new test")(display (create-string "new" "test")))
  (+ n 1))

(insert-fun-display 5)