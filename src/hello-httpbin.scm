(import (scheme base))
(import (arew network socket))
(import (arew network http))


(define (ref alist symbol)
  (let loop ((alist alist))
    (if (null? alist)
        (error "Oops")
        (if (symbol=? (caar alist) symbol)
            (cdar alist)
            (loop (cdr alist))))))

(define sock (socket 'inet 'stream 'ip))

(define-values (ok addressinfos) (getaddrinfo "httpbin.org" "http" #f))
(connect sock (ref (car addressinfos) 'address))

(define accumulator (socket-accumulator sock))
(define generator (socket-generator sock))

(http-request-write accumulator
                    "GET"
                    "http://httpbin.org/html"
                    '(1 . 1)
                    '(("Host" . "httpbin.org"))
                    "")

(call-with-values (lambda () (http-response-read generator)) pk)
(close sock)
