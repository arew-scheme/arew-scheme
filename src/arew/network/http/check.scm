(library (arew network http check)

  (export check-001
          check-002
          check-003
          check-004
          check-005
          check-006
          check-007
          check-008
          check-009
          check-010
          check-011
          check-012
          check-013
          check-014
          check-015
          ;;check-016
          check-017
          check-018
          check-019
          check-020
          )

  (import (scheme base)
          (scheme file)
          (scheme char)
          (scheme fixnum)
          (scheme generator)
          (check)
          (arew network http))

  (begin

    ;; helpers

    (define-syntax switch=
      (syntax-rules ()
        ((switch= key (value out ...) ... (else expr ...))
         (let ((key* key))
           (cond
            ((= key* value) out ...)
            ...
            (else expr ...))))))

    (define (http-file->generator filename)
      (define (port->generator port)
        ;; Generate bytes from FILENAME
        (let ((eof? #f))
          (lambda ()
            (if eof?
                (eof-object)
                (let ((byte (read-u8 port)))
                  (if (eof-object? byte)
                      (begin
                        (close-port port)
                        (set! eof? #t)
                        (eof-object))
                      byte))))))

      (define (http->scheme generator)
        (define continue '())
        (lambda ()
          (let loop ()
            (cond
             ((null? continue)
              (let ((byte (generator)))
                (switch= byte
                  ((char->integer #\newline) (loop))
                  ((char->integer #\\) (let ((next (generator)))
                                         (switch= next
                                           ((char->integer #\r) (char->integer #\return))
                                           ((char->integer #\n) (char->integer #\newline))
                                           (else (set! continue next)
                                                 byte))))
                  (else byte))))
             ((not continue) (eof-object))
             (else (let ((byte continue))
                     (set! continue '())
                     byte))))))

        (define port (open-binary-input-file (string-append "src/arew/network/http/files/" filename)))

        (http->scheme (port->generator port)))

    (define check-001
      (check-values (values "PUT" "/stuff/here?foo=bar" '(1 . 0)
                            '(("Content-Length" . "14")
                              ("Content-Type" . "application/json")
                              ("Server" . "http://127.0.0.1:5984"))
                            "{\"nom\": \"nom\"}")
                    (http-request-read (http-file->generator "requests/valid/001.http"))))

    (define check-002
      (check-values (values "GET" "/test" '(1 . 1)
                            '(("Accept" . "*/*")
                              ("Host" . "0.0.0.0=5000")
                              ("User-Agent" . "curl/7.18.0 (i486-pc-linux-gnu) libcurl/7.18.0 OpenSSL/0.9.8g zlib/1.2.3.3 libidn/1.1"))
                            #f)
                    (http-request-read (http-file->generator "requests/valid/002.http"))))

    (define check-003
      (check-values (values "GET" "/favicon.ico" '(1 . 1)
                            '(("Connection" . "keep-alive")
                              ("Keep-Alive" . "300")
                              ("Accept-Charset" . "ISO-8859-1,utf-8;q=0.7,*;q=0.7")
                              ("Accept-Encoding" . "gzip,deflate")
                              ("Accept-Language" . "en-us,en;q=0.5")
                              ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                              ("User-Agent" . "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9) Gecko/2008061015 Firefox/3.0")
                              ("Host" . "0.0.0.0=5000"))
                            #f)
                    (http-request-read (http-file->generator "requests/valid/003.http"))))

    (define check-004
      (check-values (values "GET" "/silly" '(1 . 1)
                            '(("aaaaaaaaaaaaa" . "++++++++++"))
                            #f)
                    (http-request-read (http-file->generator "requests/valid/004.http"))))

    (define check-005
      (check-values (values "GET" "/forums/1/topics/2375?page=1#posts-17408" '(1 . 1)
                            '()
                            #f)
                    (http-request-read (http-file->generator "requests/valid/005.http"))))

    (define check-006
      (check-values (values "GET" "/get_no_headers_no_body/world" '(1 . 1)
                            '()
                            #f)
                    (http-request-read (http-file->generator "requests/valid/006.http"))))

    (define check-007
      (check-values (values "GET" "/get_one_header_no_body" '(1 . 1)
                            '(("Accept" . "*/*"))
                            #f)
                    (http-request-read (http-file->generator "requests/valid/007.http"))))

    (define check-008
      (check-values (values "GET" "/unusual_content_length" '(1 . 0)
                            '(("conTENT-Length" . "5"))
                            "HELLO")
                    (http-request-read (http-file->generator "requests/valid/008.http"))))

    (define check-009
      (check-values (values "POST" "/post_identity_body_world?q=search#hey" '(1 . 1)
                            '(("Content-Length" . "5")
                              ("Transfer-Encoding" . "identity")
                              ("Accept" . "*/*"))
                            "World")
                    (http-request-read (http-file->generator "requests/valid/009.http"))))

    (define check-010
      (check-values (values "POST" "/post_chunked_all_your_base" '(1 . 1)
                            '(("Transfer-Encoding" . "chunked"))
                            "all your base are belong to us")
                    (http-request-read (http-file->generator "requests/valid/010.http"))))

    (define check-011
      (check-values (values "POST" "/two_chunks_mult_zero_end" '(1 . 1)
                            '(("Transfer-Encoding" . "chunked"))
                            "hello world")
                    (http-request-read (http-file->generator "requests/valid/011.http"))))

    (define check-012
      (check-values (values "POST" "/chunked_w_trailing_headers" '(1 . 1)
                            '(("Transfer-Encoding" . "chunked"))
                            "hello world")
                    (http-request-read (http-file->generator "requests/valid/012.http"))))

    (define check-013
      (check-values (values "POST" "/chunked_w_extensions" '(1 . 1)
                            '(("Transfer-Encoding" . "chunked"))
                            "hello world")
                    (http-request-read (http-file->generator "requests/valid/013.http"))))

    (define check-014
      (check-values (values "GET" "/with_\"quotes\"?foo=\"bar\"" '(1 . 1)
                            '()
                            #f)
                    (http-request-read (http-file->generator "requests/valid/014.http"))))

    (define check-015
      (check-values (values "GET" "/test" '(1 . 0)
                            '(("Accept" . "*/*")
                              ("User-Agent" . "ApacheBench/2.3")
                              ("Host" . "0.0.0.0:5000"))
                            #f)
                    (http-request-read (http-file->generator "requests/valid/015.http"))))

    #;(define check-016
      (check-values (values)
                    (http-request-read (http-file->generator "requests/valid/016.http"))))

    (define check-017
      (check-values (values "GET" "/stuff/here?foo=bar" '(1 . 0)
                            '(("If-Match" . "large-sound")
                              ("If-Match" . "bazinga!"))
                            #f)
                    (http-request-read (http-file->generator "requests/valid/017.http"))))

    (define check-018
      (check-values (values "GET" "/first" '(1 . 0) '() #f)
                    (http-request-read (http-file->generator "requests/valid/019.http"))))

    (define check-019
      (check-values (values "GET" "/first" '(1 . 0) '() #f)
                    (http-request-read (http-file->generator "requests/valid/019.http"))))

    (define check-020
      (check-values (values "GET" "/first" '(1 . 0)
                            '(("Content-Length" . "24"))
                            "GET /second HTTP/1.1\r\n\r\n")
                    (http-request-read (http-file->generator "requests/valid/020.http"))))

    (define check-021
      (check-values (values)
                    (http-request-read (http-file->generator "requests/valid/021.http"))))



    ))
