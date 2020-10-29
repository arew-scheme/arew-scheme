(library (arew data base foundationdb untangle-check)

  (export check-000
          check-001
          check-002
          check-003
          )

  (import (scheme base)
          (only (arew data base foundationdb) fdb-select-api-version)
          (arew data base foundationdb untangle)
          (arew network untangle)
          (check))

  (define (with-fdb proc)
    (define fdb (make-fdb))
    (call-with-values (lambda () (proc fdb))
      (lambda args
        (fdb-close fdb)
        (apply values args))))

  (define (with-loop thunk)
    (define out '(no-set))
    (untangle (lambda () (set! out (thunk))))
    out)

  (define (with-txn proc)
    (with-fdb
     (lambda (fdb)
       (with-loop (lambda () (fdb-in-transaction fdb proc))))))

  (define init (fdb-init! 620))

  (define check-000
    (check #t (with-fdb (lambda (fdb) #t))))

  (define check-001
    (check #t (with-txn (lambda (txn) #t))))

  (define check-002
    (check #vu8(37)
           (begin
             (with-txn (lambda (txn)
                         (fdb-set! txn #vu8(13) #vu8(37))
                         (fdb-ref txn #vu8(13))))
             (with-txn (lambda (txn)
                         (fdb-ref txn #vu8(13)))))))

  (define check-003
    (check '((#vu8(1) . #vu8(101))
             (#vu8(2) . #vu8(102))
             (#vu8(3) . #vu8(103)))
           (begin
             (with-txn (lambda (txn)
                         (fdb-set! txn #vu8(1) #vu8(101))
                         (fdb-set! txn #vu8(2) #vu8(102))
                         (fdb-set! txn #vu8(3) #vu8(103))))
             (with-txn (lambda (txn)
                         (fdb-range txn #vu8(1) #t #vu8(3) #t #f #f))))))

  )
