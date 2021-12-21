;; renamed lambdas/lets: 0
 
(define inport #f)
 
(define outport #f)
 
(define readline (lambda (port line-so-far)
      (let ((x (read-char port)))
         (if (eof-object? x)
            x
            (if (char=? x #\
)
               (list->string (reverse (cons x line-so-far)))
               (readline port (cons x line-so-far)))))))
 
(define tail-r-aux (lambda (port file-so-far)
      (let ((x (readline port ())))
         (if (eof-object? x)
            (begin
               (display file-so-far outport)
               (close-output-port outport))
            (tail-r-aux port (cons x file-so-far))))))
 
(define tail-r (lambda (port)
      (tail-r-aux port ())))
 
(define go (lambda ()
      (set! inport (open-input-file "input.txt"))
      (set! outport (open-output-file "output.txt"))
      (tail-r inport)
      (close-input-port inport)))
 
(go)
 
