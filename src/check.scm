(library (check)
  (export check check-raise check-skip check-values run-check)

  (import (chezscheme))
  (import (only (scheme base)  pk))

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define-syntax check
      (syntax-rules ()
        ((check predicate? expected actual)
         (lambda ()
           (let ((expected* expected)
                 (actual* actual))
             (if (predicate? expected* actual*)
                 (vector #t)
                 (vector #f 'unexpected-value expected* actual*)))))
        ((check expected actual)
         (check equal? expected actual))
        ((check actual)
         (check (lambda (x y) (if x #t #f)) #t actual))
        ((check predicate? v0 v1 ...)
         (check (predicate? v0 v1 ...)))))

    (define-syntax check-raise
      (syntax-rules ()
        ((check-raise predicate? expr)
         (lambda ()
           (let ((predicate?* predicate?))
             (guard (ex ((predicate?* ex) (vector #t))
                        (else (vector #f 'unexpected-exception predicate?* ex)))
               (let ((expr* expr))
                 (vector #f 'no-exception predicate?* expr*))))))
         ((check-raise expr)
          (check-raise (lambda (x) #t) expr))))

    (define-syntax-rule (check-skip test expected actual)
      (lambda ()
        (vector #t)))

    (define-syntax-rule (check-values expected actual)
      (check (call-with-values (lambda () expected)
               (lambda args args))
             (call-with-values (lambda () actual)
               (lambda args args))))

    ;; how to run the test suite

    (define (error-format vector)
      (case (vector-ref vector 1)
        ((unexpected-value)
         (display "*** expected: ") (write (vector-ref vector 2)) (newline)
         (display "*** given: ") (write (vector-ref vector 3)) (newline))
        (else (write vector))))
    
    (define-syntax-rule (run-check library-name check-name check)
      (begin
        (guard (obj
                ((condition? obj)
                 (display (vector #f 'error (list (condition-message obj)
                                                  (condition-irritants obj))))
                 (newline)
                 (exit 1))
                (else (display (vector #f 'error obj))
                      (newline)
                      (exit 1)))
          (display "** ")
          (display 'library-name)
          (display " ")
          (display 'check-name)
          (newline)
          (let ((out (check)))
            (when (not (vector-ref out 0))
              (error-format out)
              (newline)
              (exit 1))))))))
