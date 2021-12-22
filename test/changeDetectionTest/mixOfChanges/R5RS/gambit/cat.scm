;; renamed lambdas/lets: 1
;; Of which consistent renamings: 1

(define inport #f)
 
(define outport #f)
 
(define catport (lambda (port)
      (<change>
         (let ((x (read-char port)))
            (if (eof-object? x)
               (close-output-port outport)
               (begin
                  (write-char x outport)
                  (catport port))))
         (let ((_x0 (read-char port)))
            (if (eof-object? _x0)
               (close-output-port outport)
               (begin
                  (write-char _x0 outport)
                  (catport port)))))))
 
(define go (lambda ()
      (set! inport (open-input-file "input.txt"))
      (set! outport (open-output-file "output.txt"))
      (catport inport)
      (close-input-port inport)))
 
(go)
 
