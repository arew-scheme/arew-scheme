(library (arew network http)

  (export http-request-read
          http-request-chunks-generator
          http-request-chunked-body-generator
          http-request-write
          http-request-chunked-body-accumulator)

  (import (scheme base)
          (scheme generator)
          (scheme fixnum))

  (begin

    (define-record-type <http-error>
      (make-http-error message payload)
      http-error?
      (message http-error-message)
      (payload http-error-payload))

    (define (http-line-maximum-length)
      8192)

    (define (http-headers-maximum-count)
      8192)

    (define (request-line-generator generator)
      ;; TODO: what happens if GENERATOR produce EOF before the end of a line?!

      ;; TODO: replace `max` and `count` with `max` that will describe
      ;; what is the maximum number of bytes that can be read, if (fx=?
      ;; max 0) then no more byte can be read.
      (let ((max (http-line-maximum-length))
            (count 0)
            (continue '()))
        (lambda ()
          (if (= count max)
              (raise (make-http-error "Line too long!" #f))
              (set! count (fx+ count 1)))
          ;; TODO: avoid cond by making continue a lambda
          (cond ;; TODO: maybe use `case`
           ((null? continue)
            (let ((byte (generator)))
              (if (= byte  13)
                  (let ((other (generator)))
                    (if (= other 10)
                        (begin
                          ;; that is the end of the line
                          (set! continue #f)
                          (eof-object))
                        (begin
                          ;; there is #\return byte in the middle of the line
                          (set! continue other)
                          13)))
                  ;; that is not a control byte, return it
                  byte)))
           ((not continue) (eof-object))
           (else (let ((byte continue))
                   (set! continue '())
                   byte))))))

    (define (request-lines-generator generator)
      ;; TODO: replace `max` and `count` with `max` that will describe
      ;; what is the maximum number of headers that can be read.

      (let ((max (http-headers-maximum-count))
            (count 0))
        ;; generate at most `max` lines as generators. Yes, it is a
        ;; generator of generators.
        (lambda ()
          (if (= count max)
              (raise (make-http-error "There is too much header lines" #f))
              (set! count (fx+ count 1)))
          (request-line-generator generator))))

    (define (generator-until generator predicate?)
      (lambda ()
        (let ((item (generator)))
          (if (predicate? item)
              (eof-object)
              item))))

    (define (space? byte)
      (= byte 32))

    (define (http-request-line-item-read generator)
      (generator->string (gmap integer->char (generator-until generator space?))))

    (define (two-dots? byte)
      (= byte 58))

    (define (http-header-read first-byte rest)
      (let* ((key (string-append (list->string (list (integer->char first-byte)))
                                 (generator->string
                                  (gmap integer->char
                                        (generator-until rest two-dots?)))))
             (value (list->string (map integer->char (generator->list rest)))))
        (cons key value)))

    (define (maybe-http-header-read line)
      (let ((maybe-byte (line)))
        (if (eof-object? maybe-byte)
            #f
            (http-header-read maybe-byte line))))

    (define (http-headers-read lines)
      (let loop ((out '()))
        (let ((header (maybe-http-header-read (lines))))
          (if header
              (loop (cons header out))
              out))))

    (define (http-request-read generator)
      (define lines (request-lines-generator generator))

      (define request-line (lines))

      (define method (http-request-line-item-read request-line))

      (define uri (http-request-line-item-read request-line))

      (define version (list->string (map integer->char (generator->list request-line))))

      (define headers (http-headers-read lines))

      (values method uri version headers))

    (define (http-request-chunks-generator generator)
      (define (return? x)
        (fx=? x 13))

      (define (newline? x)
        (fx=? x 10))

      (define (semi-colon? x)
        (fx=? x 59))

      (define (or* pred1 pred2)
        (lambda (obj)
          (or (pred1 obj) (pred2 obj))))

      (define (read-chunk-line generator)
        (define length (string->number
                        (generator->string
                         (gmap integer->char
                               (generator-until generator (or* return? semi-colon?))))
                               16))

        (define extensions "")

        (define continuation (generator))

        (unless (newline? continuation)
          (set! extensions (string-append ";" (generator->string
                                               (gmap integer->char
                                                     (generator-until generator return?)))))
          ;; consume the newline byte at the end of the line
          (generator))

        (values length extensions))

      (define (read-headers generator)
        (values 'headers (http-headers-read (request-lines-generator generator))))

      (lambda ()

        (define-values (length extensions)  (read-chunk-line generator))

        (if (fx=? length 0)
            (read-headers generator)
            (values 'chunk extensions (generator/continuation
                                       (gtake generator length)
                                       (lambda ()
                                         (generator)
                                         (generator)
                                         (eof-object)))))))

    (define (generator/continuation generator k)
      (lambda ()
        (let ((out (generator)))
          (if (eof-object? out)
              (k)
              out))))

    (define (http-request-chunked-body-generator generator)

      (define chunks #f)

      (define (next-chunk)
        (call-with-values chunks
          (lambda (symbol extensions-or-headers . maybe-chunk)
            (case symbol
              ((chunk)
               (set! continue (generator/continuation (car maybe-chunk)
                                                      next-chunk))
               (continue))
              ((headers) (set! continue eof-object?) (eof-object))))))

      (define (start)
        (set! chunks (http-request-chunks-generator generator))
        (next-chunk))

      (define continue start)

      (lambda ()
        (continue)))

    (define http-request-write
      (lambda args 'not-implemented))

    (define http-request-chunked-body-accumulator
      (lambda args 'not-implemented))))
