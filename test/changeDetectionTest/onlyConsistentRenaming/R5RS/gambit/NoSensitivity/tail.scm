;; renamed lambdas/lets: 4
 
(define inport #f)
 
(define outport #f)
 
(define readline (<change>
      (lambda (port line-so-far)
         (let ((x (read-char port)))
            (if (eof-object? x)
               x
               (if (char=? x #\
)
                  (list->string (reverse (cons x line-so-far)))
                  (readline port (cons x line-so-far))))))
      (lambda (_port0 _line-so-far0)
         (let ((_x0 (read-char _port0)))
            (if (eof-object? _x0)
               _x0
               (if (char=? _x0 #\
)
                  (list->string (reverse (cons _x0 _line-so-far0)))
                  (readline _port0 (cons _x0 _line-so-far0))))))))
 
(define tail-r-aux (<change>
      (lambda (port file-so-far)
         (let ((x (readline port ())))
            (if (eof-object? x)
               (begin
                  (display file-so-far outport)
                  (close-output-port outport))
               (tail-r-aux port (cons x file-so-far)))))
      (lambda (_port0 _file-so-far0)
         (let ((_x0 (readline _port0 ())))
            (if (eof-object? _x0)
               (begin
                  (display _file-so-far0 outport)
                  (close-output-port outport))
               (tail-r-aux _port0 (cons _x0 _file-so-far0)))))))
 
(define tail-r (<change>
      (lambda (port)
         (tail-r-aux port ()))
      (lambda (_port0)
         (tail-r-aux _port0 ()))))
 
(define go (<change>
      (lambda ()
         (set! inport (open-input-file "input.txt"))
         (set! outport (open-output-file "output.txt"))
         (tail-r inport)
         (close-input-port inport))
      (lambda ()
         (set! inport (open-input-file "input.txt"))
         (set! outport (open-output-file "output.txt"))
         (tail-r inport)
         (close-input-port inport))))
 
(go)
 
