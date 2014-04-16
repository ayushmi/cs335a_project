(define compile-port
  (make-parameter
    (current-output-port)
    (lambda (p)
       (unless (output-port? p)
         (error 'compile-port (format "not an output port ~s" p)))
       p)))


(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))
