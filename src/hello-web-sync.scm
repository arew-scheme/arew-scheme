(import (scheme base))
(import (arew network socket))
(import (arew network http))
(import (scheme process-context))


(define (handle socket)
  (let ((generator (socket-generator socket))
        (accumulator (socket-accumulator socket)))
    (pk 'handle...)
    (call-with-values (lambda () (http-request-read generator))
      (lambda (method uri version headers body)
        (pk 'write...)
        (http-response-write accumulator
                             '(1 . 1)
                             200
                             "Found"
                             '()
                             "hello schemer!")
        (close socket)))))

(define (hello port)
  (define sock (socket 'inet 'stream 'ip))

  (bind sock `((family . inet) (ip . "0.0.0.0") (port . ,port)))
  (listen sock 1024)

  (guard (obj (else (close sock) (raise obj)))
    (let loop ()
      (let ((client (accept sock)))
        (handle client)
        (loop)))))


(hello (string->number (cadr (command-line))))
