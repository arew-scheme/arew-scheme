(import (scheme base))
(import (only (scheme list) take))
(import (scheme cxr))
(import (scheme write))
(import (scheme read))
(import (scheme file))
(import (scheme charset))
(import (scheme char))
(import (scheme set))
(import (scheme generator))
(import (scheme comparator))
(import (scheme fixnum))
(import (scheme time))
(import (scheme process-context))
(import (arew network http))

(import (srfi srfi-167 pack))
(import (arew data base okvslite))

(import (only (chezscheme) fxsrl fxlogor fxsll fxlogand))
(import (only (chezscheme) random random-seed))

(define *forward* #vu8(0))
(define *bag* #vu8(1))
(define *df* #vu8(2)) ;; document frequency
(define *backward* #vu8(2)) ;; document frequency

(define default-comparator (make-default-comparator))

(define (engine-open filename)
  (let ((db (okvslite-new)))
    (okvslite-config db 8 0) ;; no log
    (okvslite-config db 11 0) ;; no multiple process
    (okvslite-open db filename)
    db))

(define (engine-open-read-only filename)
  (let ((db (okvslite-new)))
    (okvslite-config db 8 0) ;; no log
    (okvslite-config db 11 0) ;; no multiple process
    (okvslite-config db 16 1) ;; readonly
    (okvslite-open db filename)
    db))

(define (engine-forward-set! db uid snippet url)
  (okvslite-insert db (pack *forward* uid) (pack snippet url)))

(define (okvslite-ref db key)
  (let ((cursor (okvslite-cursor-open db)))
    (okvslite-cursor-seek cursor key 'equal)
    (if (okvslite-cursor-valid? cursor)
        (let ((out (unpack (okvslite-cursor-value cursor))))
          (okvslite-cursor-close cursor)
          out)
        (begin
          (okvslite-cursor-close cursor)
          #f))))

(define (engine-forward-ref db uid)
  (okvslite-ref db (pack *forward* uid)))

(define (call-with-output-string proc)
  (define port (open-output-string))
  (proc port)
  (let ((string (get-output-string port)))
    (close-port port)
    string))

(define (call-with-input-string string proc)
  (define port (open-input-string string))
  (let ((out (proc port)))
    (close-port port)
    out))

(define (scm->string scm)
  (call-with-output-string
   (lambda (port)
     (write scm port))))

(define (string->scm string)
  (call-with-input-string string read))

(define (engine-bag-update! db uid bag)
  (let* ((bag* (engine-bag-ref db uid))
         (bag** (bag-sum! bag bag*)))
    (okvslite-insert db (pack *bag* uid) (pack (scm->string (bag->alist bag**))))))

(define (engine-bag-ref db uid)
  (let ((bag (or (okvslite-ref db (pack *bag* uid)) (list "()"))))
    (alist->bag default-comparator (string->scm (car bag)))))

(define (engine-document-frequency! db bag)
  (engine-bag-update! db #vu8(0) bag))

(define (engine-document-frequency db)
  (engine-bag-ref db #vu8(0)))

(define (engine-backward-add! db token uid)
  (okvslite-insert db (pack *backward* token uid) #vu8(0)))

(define (engine-backward-ref db token)
  (let ((cursor (okvslite-cursor-open db))
        (key (pack *backward* token)))
    (okvslite-cursor-seek cursor key 'greater-than-or-equal)
    (let loop ((out '()))
      (if (not (okvslite-cursor-valid? cursor))
          (begin (okvslite-cursor-close cursor) out)
          (let ((key (unpack (okvslite-cursor-key cursor))))
            (if (string=? (cadr key) token)
                (begin
                  (okvslite-cursor-next cursor)
                  (loop (cons (caddr key) out)))
                (begin (okvslite-cursor-close cursor) out)))))))

(define (engine-close db)
  (okvslite-close db))

;; tokenize

(define stopword (list->set default-comparator
                            (call-with-input-file "data/stopwords.txt"
                              (lambda (port)
                                (let loop ((out '()))
                                  (let ((line (read-line port)))
                                    (if (eof-object? line)
                                        out
                                        (loop (cons line out)))))))))

(define (stopword? token)
  (set-contains? stopword token))

(define (string->bag string)
  (define culture-generator (string->generator string))

  (define char-set:spacy (char-set-union char-set:punctuation
                                         char-set:whitespace
                                         char-set:empty))

  (define (spacy char)
    ;; replace punctuation, blank, and whitespace with #\space
    (if (char-set-contains? char-set:spacy char)
        #\space
        char))

  (define culture-generator* (gmap char-downcase (gmap spacy culture-generator)))

  (define (unspace generator)
    ;; merge space characters into a single space
    (define previous-char-is-space? #t)
    (lambda ()
      (let loop ((char (generator)))
        (if (eof-object? char)
            char
            (if (char=? char #\space)
                (if previous-char-is-space?
                    (loop (generator))
                    (begin
                      (set! previous-char-is-space? #t)
                      #\space))
                (begin
                  (set! previous-char-is-space? #f)
                  char))))))

  (define culture-generator** (unspace culture-generator*))

  (define (tokenize generator)
    (lambda ()
      (let loop ((out '()))
        (let ((char (generator)))
          (if (eof-object? char)
              char
              (if (char=? char #\space)
                  (list->string (reverse out))
                  (loop (cons char out))))))))

  (define (sane? string)
    (<= 3 (string-length string) 255))

  (define tokens (gremove stopword? (gfilter sane? (tokenize culture-generator**))))

  (define bag (list->bag default-comparator (generator->list tokens)))

  bag)

(define (string->snippet string)

  (define culture-generator (string->generator string))

  (define char-set:spacy (char-set-union char-set:punctuation
                                         char-set:whitespace
                                         char-set:empty))

  (define (spacy char)
    ;; replace punctuation, blank, and whitespace with #\space
    (if (char-set-contains? char-set:spacy char)
        #\space
        char))

  (define culture-generator* (gmap char-downcase (gmap spacy culture-generator)))

  (define (unspace generator)
    ;; merge space characters into a single space
    (define previous-char-is-space? #t)
    (lambda ()
      (let loop ((char (generator)))
        (if (eof-object? char)
            char
            (if (char=? char #\space)
                (if previous-char-is-space?
                    (loop (generator))
                    (begin
                      (set! previous-char-is-space? #t)
                      #\space))
                (begin
                  (set! previous-char-is-space? #f)
                  char))))))

  (define culture-generator** (unspace culture-generator*))

  (define (tokenize generator)
    (lambda ()
      (let loop ((out '()))
        (let ((char (generator)))
          (if (eof-object? char)
              char
              (if (char=? char #\space)
                  (list->string (reverse out))
                  (loop (cons char out))))))))

  (define (gtake generator count)
    ;; replacement for scheme generator's gtake that does not rely on
    ;; call/cc
    (lambda ()
      (if (fx=? count 0)
          (eof-object)
          (begin
            (set! count (fx- count 1))
            (generator)))))

  (define snippet (gtake (tokenize culture-generator**) 100))

  (define (generator-interleave generator inter)
    (define inter? #f)
    (lambda ()
      (if inter?
          (begin
            (set! inter? #f)
            inter)
          (begin
            (set! inter? #t)
            (generator)))))

  (define snippet* (generator-interleave snippet " "))

  (apply string-append (generator->list snippet*)))

(define (random-uid)
  (let ((uid (make-bytevector 16)))
    (let loop ((index 16))
      (unless (zero? index)
        (bytevector-u8-set! uid (fx- index 1) (random 256))
        (loop (fx- index 1))))
    uid))

(define (random-seed!)
  (random-seed (modulo (current-second) (expt 2 32))))

(define counter 0)

(define (%index db url string)
  (define foobar (pk counter))
  (define foobar2 (set! counter (fx+ counter 1)))
  (define uid (random-uid))
  (define bag (string->bag string))
  (define snippet (string->snippet string))

  (define x0 (engine-forward-set! db uid snippet url))
  (define x1 (engine-bag-update! db uid bag))
  (define x2 (engine-document-frequency! db bag))

  (define (add token count)
    (engine-backward-add! db token uid))

  (bag-for-each-unique add bag))


;; wet reader


(define (generator-consume generator)
  (let loop ((byte (generator)))
    (unless (eof-object? byte)
      (loop (generator)))))

(define (file-port-generator port)
  (lambda ()
    (read-u8 port)))

(define (generator-consume-line generator)
  (let loop ((byte (generator)))
    (unless (fx=? byte 10)
      (loop (generator)))))

(define (generator->line generator)
  (let loop ((byte (generator))
             (out '()))
    (if (fx=? byte 10)
        (cdr out)
        (loop (generator) (cons byte out)))))

(define (gtake* generator count)
  (lambda ()
    (if (zero? count)
        (eof-object)
        (begin (set! count (fx- count 1)) (generator)))))

;; WARC-Target-URI

(define (warc-record-read generator)
  ;; consume WARC/1.0
  (generator-consume-line generator)

  (let ((headers (http-headers-read generator)))
    (let loop ((headers headers)
               (uri #f)
               (content-length #f))
      (if (null? headers)
          (begin 'null (values uri
                               (gtake* generator content-length)))
          (let ((key (string-downcase (caar headers))))
            (cond
             ((string=? key "warc-target-uri") (loop (cdr headers)
                                                     (cdar headers)
                                                     content-length))
             ((string=? key "content-length")
              (loop (cdr headers)
                    uri
                    (string->number (cdar headers))))
             (else (loop (cdr headers) uri content-length))))))))

(define (utf8->char generator)
  (define end? #f)
  (define (fini)
    (set! end? #t)
    #xfffd)
  (lambda ()
    (if end?
        (eof-object)
        (let ((b1 (generator)))
          (if (eof-object? b1)
              (begin (set! end? #t) (eof-object))
              (cond
               ((fx<=? b1 #x7f) ;; one-byte encoding
                (integer->char b1))
               ((fx<=? #xc0 b1 #xdf) ;; two-byte encoding
                (let ((b2 (generator)))
                  (if (not (eof-object? b2))
                      (if (fx=? (fxsrl b2 6) #b10) ;; second byte a continuation byte?
                          (let ((x (fxlogor (fxsll (fxlogand b1 #b11111) 6) (fxlogand b2 #b111111))))
                            (if (fx<=? x #x7f) #\xfffd (integer->char x)))
                          ;; second byte is not a continuation byte
                          #\xfffd)
                      ;; have only one byte
                      (fini))))
               ((fx<=? #xe0 b1 #xef) ;; three-byte encoding
                (let ((b2 (generator)))
                  (if (not (eof-object? b2))
                      (if (fx=? (fxsrl b2 6) #b10) ;; second byte a continuation byte?
                          (let ((b3 (generator)))
                            (if (not (eof-object? b3))
                                (if (fx=? (fxsrl b3 6) #b10) ;; third byte a continuation byte?
                                    (let ((x (fxlogor
                                              (fxsll (fxlogand b1 #b1111) 12)
                                              (fxsll (fxlogand b2 #b111111) 6)
                                              (fxlogand b3 #b111111))))
                                      (if (and (fx>=? x #x800) (not (fx<=? #xd800 x #xdfff)))
                                          (integer->char x)
                                          #\xfffd))
                                    ;; third byte is not a continuation byte
                                    #\xfffd)
                                ;; have only two bytes
                                (fini)))
                          ;; second byte is not a continuation byte
                          #\xfff)
                      ;; have only one byte
                      (fini))))
               ((fx<=? #xf0 b1 #xf4) ;; four-byte encoding
                (let ((b2 (generator)))
                  (if (not (eof-object? b2))
                      (if (fx=? (fxsrl b2 6) #b10) ;; second byte a continuation byte?
                          (let ((b3 (generator)))
                            (if (not (eof-object? b3))
                                (if (fx=? (fxsrl b3 6) #b10) ;; third byte a continuation byte?
                                    (let ((b4 (generator)))
                                      (if (not (eof-object? b4))
                                          (if (fx=? (fxsrl b4 6) #b10) ;; fourth byte a continuation byte?
                                              (let ((x (fxlogor
                                                        (fxsll (fxlogand b1 #b111) 18)
                                                        (fxsll (fxlogand b2 #b111111) 12)
                                                        (fxsll (fxlogand b3 #b111111) 6)
                                                        (fxlogand b4 #b111111))))
                                                (if (fx<=? #x10000 x #x10ffff)
                                                    (integer->char x)
                                                    #\xfffd))
                                              ;; fourth byte is not a continuation byte
                                              #\xfffd)
                                          (fini)))
                                      #\xfffd)
                                (fini)))
                          ;; second byte is not a continuation byte
                           #\xfffd)
                      (fini))))
               (else
                 #\xfffd)))))))

(define (warc-record-generator generator)
  ;; (call-with-values (lambda () (warc-record-read wet-generator))
  ;;   (lambda (_ body)
  ;;     (generator-consume body)))

  ;; ;; consume some CRLF
  ;; (generator) (generator) (generator) (generator)

  (lambda ()
    (call-with-values (lambda () (warc-record-read wet-generator))
      (lambda (uri body)
        (values uri (gmap char-downcase (utf8->char body)))))))

(define wet-generator #f)

(define (index db filename)
  (define db (engine-open "db.okvslite"))

  (define x0 (set! wet-generator
                   (file-port-generator (open-binary-input-file filename))))

  (define warc-record-reader (warc-record-generator wet-generator))

  (let loop ()
    (call-with-values warc-record-reader
      (lambda (uri body)
        (%index db uri (generator->string body))))
    (wet-generator)
    (wet-generator)
    (wet-generator)
    (wet-generator)
    (unless (eof-object? (wet-generator))
      (loop)))

  (engine-close db))


(define (serve db)
  (pk 'serve db))

(define (query db . tokens)

  (define db (engine-open "db.okvslite"))
  (define df (engine-document-frequency db))

  (define (lct tokens) ;; least frequent token
    (let loop ((tokens tokens)
               (out '()))
      (if (null? tokens)
          (if (null? out)
              #f
              (caar (sort (lambda (a b) (< (cdr a) (cdr b))) out)))
          (loop (cdr tokens)
                (cons (cons (car tokens) (bag-element-count df (car tokens))) out)))))

  (define seed (pk 'seed (lct tokens)))

  (when seed
    (let ((candidates (engine-backward-ref db seed)))

      (define (%query uid)
        (define bag (engine-bag-ref db uid))
        (define url (cadr (engine-forward-ref db uid)))
        (cons url (apply + (map (lambda (token) (bag-element-count bag token)) tokens))))

      (pk (length candidates))

      (for-each pk (reverse (take (sort (lambda (a b) (> (cdr a) (cdr b))) (map %query candidates)) 30)))))

  (engine-close db))


(case (string->symbol (cadr (command-line)))
  ((index) (apply index (cddr (command-line))))
  ((query) (apply query (cddr (command-line))))
  ((serve) (apply serve (cddr (command-line)))))
