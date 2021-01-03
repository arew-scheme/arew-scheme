(import (scheme base))
(import (scheme write))
(import (scheme file))
(import (scheme charset))
(import (scheme char))
(import (scheme set))
(import (scheme generator))
(import (scheme comparator))


(define stopword (list->set (make-default-comparator)
                            (call-with-input-file "data/stopwords.txt"
                              (lambda (port)
                                (let loop ((out '()))
                                  (let ((line (read-line port)))
                                    (if (eof-object? line)
                                        out
                                        (loop (cons line out)))))))))

(define (stopword? token)
  (set-contains? stopword token))

(define culture (call-with-input-file "data/Culture.md"
                  (lambda (port)
                    (let loop ((out '()))
                      (let ((char (read-char port)))
                        (if (eof-object? char)
                            (list->string (reverse out))
                            (loop (cons char out))))))))

(define culture-generator (string->generator culture))

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

(define bag (list->bag (make-default-comparator) (generator->list tokens)))

(define alist (sort (lambda (a b) (< (cdr a) (cdr b)))
                    (bag->alist bag)))

(for-each pk alist)
