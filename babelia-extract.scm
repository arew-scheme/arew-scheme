(import (scheme base))
(import (scheme write))
(import (scheme file))
(import (scheme charset))
(import (scheme char))
(import (scheme set))
(import (scheme generator))
(import (scheme comparator))
(import (scheme fixnum))


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

(display (apply string-append (generator->list snippet*)))
(newline)
