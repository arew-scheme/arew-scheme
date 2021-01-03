(library (babelia-engine)
  (export engine-open
          engine-open-read-only
          engine-forward-set!
          engine-forward-ref
          engine-bag-set!
          engine-bag-ref
          engine-document-frequency!
          engine-document-frequency
          engine-backward-add!
          engine-backward-ref
          engine-close)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme charset)
          (scheme char)
          (scheme set)
          (scheme generator)
          (scheme comparator)
          (srfi srfi-167 pack))


(define *forward* #u8(0))
(define *bag* #u8(1))
(define *df* #u8(2)) ;; document frequency
(define *backward* #u8(2)) ;; document frequency

(define (engine-open filename)
  (let ((db (okvslite-new)))
    (okvslite-config db 11 0) ;; no multiple process
    (okvslite-open db filename)
    db))

(define (engine-open-read-only filename)
  (let ((db (okvslite-new)))
    (okvslite-config db 11 0) ;; no multiple process
    (okvslite-config db 16 1) ;; readonly
    (okvslite-open db filename)
    db))

(define (engine-forward-set! db uid snippet url)
  (okvslite-insert (pack *forward* uid) (pack snippet url)))


(define (okvslite-ref db key)
  (let ((cursor (okvs-cursor-open db)))
    (okvslite-cursor-seek cursor key 'equal)
    (and (okvslite-cursor-valid cursor)
         (let ((out (unpack (okvslite-cursor-value cursor))))
           (okvslite-cursor-close cursor)
           out))))

(define (engine-forward-ref db uid)
  (okvslite-ref (pack *forward* uid)))

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

(define (engine-bag-set! db uid bag)
  (okvslite-insert db (pack *bag* uid) (scm->string (bag->alist bag))))

(define (engine-bag-ref db uid)
  (alist->bag (make-default-comparator) (string->scm (okvslite-ref db (pack *bag* uid)))))

(define (engine-document-frequency! db bag)
  (okvslite-bag-set! db #u8(0) bag))

(define (engine-document-frequency db)
  (okvslite-bag-set! db #u8(0)))

(define (engine-backward-add! db token uid)
  (okvslite-insert db (pack *backward* token uid) #u8(0)))

(define (engine-backward-ref db token)
  (let ((cursor (okvs-cursor-open db)))
    (okvslite-cursor-seek cursor key 'greater-than-or-equal)
    (let loop ((out '()))
      (if (not (okvslite-cursor-valid cursor))
          (begin (okvslite-cursor-close cursor) out)
          (let ((key (unpack (okvslite-cursor-key cursor))))
            (if (string=? (cadr key token))
                (loop (cons (caddr key) out))
                (begin (okvslite-cursor-close cursor) out)))))))

(define (engine-close db)
  (okvslite-close db))
