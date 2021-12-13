(define test-with-strings
  (lambda (a)
    (if (= a 0)
        (display "all strings printed")
       (<change>
         (let ((aString "A string"))
           (display aString)
           (test-with-strings (- a 1)))
         (let ((testString "A string"))
           (display testString)
           (test-with-strings (- a 1)))))))

(test-with-strings 5)