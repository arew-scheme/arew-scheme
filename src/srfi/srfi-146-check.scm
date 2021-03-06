;; Copyright (C) Marc Nieper-Wißkirchen (2016, 2017).  All Rights
;; Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
(library (srfi srfi-146-check)
  (export check-000 check-001 check-002 check-003 check-004 check-005
check-006 check-007 check-008 check-009 check-010 check-011 check-012
check-013 check-014 check-015 check-016 check-017 check-018 check-019
check-020 check-021 check-022 check-023 check-024 check-025 check-026
check-027 check-028 check-029 check-030 check-031 check-032 check-033
check-034 check-035 check-036 check-037 check-038 check-039 check-040
check-041 check-042 check-043 check-044 check-045 check-046 check-047
check-048 check-049 check-050 check-051 check-052 check-053 check-054
check-055 check-056 check-057 check-058 check-059 check-060 check-061
check-062 check-063 check-064 check-065 check-066 check-067 check-068
check-069 check-070 check-071 check-072 check-073 check-074 check-075
check-076 check-077 check-078 check-079 check-080 check-081 check-082
check-083 check-084 check-085 check-086 check-087 check-088 check-089
check-090 check-091 check-092 check-093 check-094 check-095)

  (import (scheme base)
          (check)
          (srfi srfi-1)
          (srfi srfi-8)
          (srfi srfi-128)
          (srfi srfi-146))

  (define comparator (make-default-comparator))

  (define mapping0 (mapping comparator))
  (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping2 (mapping comparator 'c 1 'd 2 'e 3))
  (define mapping3 (mapping comparator 'd 1 'e 2 'f 3))

  (define check-000
    (check #t (mapping? (mapping comparator))))

  (define check-001
    (check #t (not (mapping? (list 1 2 3)))))

  (define check-002
    (check #t (mapping-empty? mapping0)))

  (define check-003
    (check #t (not (mapping-empty? mapping1))))

  (define check-004
    (check #t (mapping-contains? mapping1 'b)))

  (define check-005
    (check #t (not (mapping-contains? mapping1 '2))))

  (define check-006
    (check #t (mapping-disjoint? mapping1 mapping3)))

  (define check-007
    (check #t (not (mapping-disjoint? mapping1 mapping2))))

  ;; Accessors
  (define mapping11 (mapping comparator 'a 1 'b 2 'c 3))

  (define check-008
    (check 2 (mapping-ref mapping11 'b)))

  (define check-009
    (check
     42
     (mapping-ref mapping11 'd (lambda () 42))))

  ;; TODO: make it a test
  ;; (check-error "mapping-ref: key not found/without failure"
  ;;   (mapping-ref mapping11 'd))

  (define check-010
    (check
     (* 2 2)
     (mapping-ref mapping11 'b (lambda () #f) (lambda (x) (* x x)))))

  (define check-011
    (check
     3
     (mapping-ref/default mapping11 'c 42)))

  (define check-012
    (check
     42
     (mapping-ref/default mapping11 'd 42)))

  (define check-013
    (check
     comparator
     (mapping-key-comparator mapping11)))

  ;; Updaters
  (define mapping21 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping22 (mapping-set mapping1 'c 4 'd 4 'd 5))
  (define mapping23 (mapping-update mapping1 'b (lambda (x) (* x x))))
  (define mapping24 (mapping-update/default mapping1 'd (lambda (x) (* x x)) 4))
  (define mapping25 (mapping-adjoin mapping1 'c 4 'd 4 'd 5))
  (define mapping20 (mapping comparator))

  (define check-014
    (check
     3
     (mapping-ref mapping25 'c)))

  (define check-015
    (check
     4
     (mapping-ref mapping25 'd)))

  (define check-016
    (check
     4
     (mapping-ref mapping22 'c)))

  (define check-017
    (check
     5
     (mapping-ref mapping22 'd)))

  (define check-018
    (check
     #f
     (mapping-ref/default (mapping-replace mapping21 'd 4) 'd #f)))

  (define check-019
    (check
     6
     (mapping-ref (mapping-replace mapping21 'c 6) 'c)))

  (define check-020
    (check
     42
     (mapping-ref/default (mapping-delete mapping21 'b) 'b 42)))

  (define check-021
    (check
     42
     (mapping-ref/default (mapping-delete-all mapping21 '(a b)) 'b 42)))

  (define check-022
    (check
     (list mapping21 2)
     (receive result
         (mapping-intern mapping21 'b (lambda () (error "should not have been invoked")))
       result)))

  (define check-023
    (check
     (list 42 42)
     (receive (mapping value)
         (mapping-intern mapping21 'd (lambda () 42))
       (list value (mapping-ref mapping 'd)))))

  (define check-024
    (check
     4
     (mapping-ref mapping23 'b)))

  (define check-025
    (check
     16
     (mapping-ref mapping24 'd)))

  (define check-026
    (check
     'empty
     (mapping-pop mapping20 (lambda () 'empty))))

  (define check-027
    (check
     (list 2 'a 1)
     (receive (mapping key value)
         (mapping-pop mapping21)
       (list (mapping-size mapping) key value))))

  (define check-028
    (check
     '("success updated"
       "failure ignored"
       ((0 . "zero") (1 . "one") (2 . "two [seen]") (3 . "three")
        (4 . "four") (5 . "five")))
     (let ((m1 (mapping (make-default-comparator)
                        1 "one"
                        3 "three"
                        0 "zero"
                        4 "four"
                        2 "two"
                        5 "five")))
       (define (f/ignore insert ignore)
         (ignore "failure ignored"))
       (define (s/update key val update remove)
         (update key
                 (string-append val " [seen]")
                 "success updated"))
       (let*-values (((m2 v2) (mapping-search m1 2 f/ignore s/update))
                     ((m3 v3) (mapping-search m2 42 f/ignore s/update)))
         (list v2 v3 (mapping->alist m3))))))

  ;; The whole mapping

  (define mapping30 (mapping comparator))
  (define mapping31 (mapping comparator 'a 1 'b 2 'c 3))

  (define check-029
    (check
     0
     (mapping-size mapping30)))

  (define check-030
    (check
     3
     (mapping-size mapping31)))

  (define check-031
    (check
     (list 'b 2)
     (receive result
         (mapping-find (lambda (key value)
                         (and (eq? key 'b)
                              (= value 2)))
                       mapping31
                       (lambda () (error "should not have been called")))
       result)))

  (define check-032
    (check
     (list 42)
     (receive result
         (mapping-find (lambda (key value)
                         (eq? key 'd))
                       mapping31
                       (lambda ()
                         42))
       result)))

  (define check-033
    (check
     2
     (mapping-count (lambda (key value)
                      (>= value 2))
                    mapping31)))

  (define check-034
    (check #t
           (mapping-any? (lambda (key value)
                           (= value 3))
                         mapping31)))

  (define check-035
    (check #t
           (not (mapping-any? (lambda (key value)
                                (= value 4))
                              mapping31))))

  (define check-036
    (check #t
           (mapping-every? (lambda (key value)
                             (<= value 3))
                           mapping31)))

  (define check-037
    (check #t
           (not (mapping-every? (lambda (key value)
                                  (<= value 2))
                                mapping31))))

  (define check-038
    (check
     3
     (length (mapping-keys mapping31))))

  (define check-039
    (check
     6
     (fold + 0 (mapping-values mapping31))))

  (define check-040
    (check
     (list 3 6)
     (receive (keys values)
         (mapping-entries mapping31)
       (list (length keys) (fold + 0 values)))))

  (define mapping41 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping42 (mapping-map (lambda (key value)
                                   (values (symbol->string key)
                                           (* 10 value)))
                                 comparator
                                 mapping1))
  (define check-041
    (check
     20
     (mapping-ref mapping42 "b")))

  (define check-042
    (check
     6
     (let ((counter 0))
       (mapping-for-each (lambda (key value)
                           (set! counter (+ counter value)))
                         mapping41)
       counter)))

  (define check-043
    (check
     6
     (mapping-fold (lambda (key value acc)
                     (+ value acc))
                   0
                   mapping41)))

  (define check-044
    (check
     (+ (* 1 1) (* 2 2) (* 3 3))
     (fold + 0 (mapping-map->list (lambda (key value)
                                    (* value value))
                                  mapping41))))

  (define check-045
    (check
     2
     (mapping-size (mapping-filter (lambda (key value)
                                     (<= value 2))
                                   mapping41))))

  (define check-046
    (check
     1
     (mapping-size (mapping-remove (lambda (key value)
                                     (<= value 2))
                                   mapping41))))

  (define check-047
    (check
     (list 1 2)
     (receive result
         (mapping-partition (lambda (key value)
                              (eq? 'b key))
                            mapping41)
       (map mapping-size result))))

  ;; Copying and conversion
  (define mapping51 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping52 (alist->mapping comparator '((a . 1) (b . 2) (c . 3))))
  (define mapping53 (alist->mapping! (mapping-copy mapping1) '((d . 4) '(c . 5))))

  (define check-048
    (check
     3
     (mapping-size (mapping-copy mapping51))))

  (define check-049
    (check
     comparator
     (mapping-key-comparator (mapping-copy mapping51))))

  (define check-050
    (check
     (cons 'b 2)
     (assq 'b (mapping->alist mapping51))))

  (define check-051
    (check
     2
     (mapping-ref mapping52 'b)))

  (define check-052
    (check
     4
     (mapping-ref mapping53 'd)))

  (define check-053
    (check
     3
     (mapping-ref mapping53 'c)))

  ;; Submappings
  (define mapping61 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping62 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping63 (mapping comparator 'a 1 'c 3))
  (define mapping64 (mapping comparator 'a 1 'c 3 'd 4))
  (define mapping65 (mapping comparator 'a 1 'b 2 'c 6))
  (define mapping66 (mapping (make-comparator (comparator-type-test-predicate comparator)
                                              (comparator-equality-predicate comparator)
                                              (comparator-ordering-predicate comparator)
                                              (comparator-hash-function comparator))
                             'a 1 'b 2 'c 3))


  (define check-054
    (check #t
           (mapping=? comparator mapping61 mapping62)))

  (define check-055
    (check #t
           (not (mapping=? comparator mapping61 mapping64))))

  (define check-056
    (check #t
           (not (mapping=? comparator mapping61 mapping66))))

  (define check-057
    (check #t
           (mapping<? comparator mapping63 mapping61)))

  (define check-058
    (check #t
           (not (mapping<? comparator mapping3 mapping61 mapping62))))

  (define check-059
    (check #t
           (mapping>? comparator mapping62 mapping63)))

  (define check-060
    (check #t
           (not (mapping>? comparator mapping1 mapping62 mapping63))))

  (define check-061
    (check #t
           (mapping<=? comparator mapping63 mapping62 mapping61)))

  (define check-062
    (check #t
           (not (mapping<=? comparator mapping63 mapping65))))

  (define check-063
    (check #t
           (not (mapping<=? comparator mapping62 mapping64))))

  (define check-064
    (check #t
           (mapping>=? comparator mapping64 mapping63)))

  (define check-065
    (check #t
           (not (mapping>=? comparator mapping65 mapping63))))

  ;; Set theory operations
  (define mapping71 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping72 (mapping comparator 'a 1 'b 2 'd 4))
  (define mapping73 (mapping comparator 'a 1 'b 2))
  (define mapping74 (mapping comparator 'a 1 'b 2 'c 4))
  (define mapping75 (mapping comparator 'a 1 'c 3))
  (define mapping76 (mapping comparator 'd 4 'e 5 'f 6))

  (define check-066
    (check
     4
     (mapping-ref (mapping-union mapping71 mapping72) 'd)))

  (define check-067
    (check
     3
     (mapping-ref (mapping-union mapping71 mapping74) 'c)))

  (define check-068
    (check
     6
     (mapping-size (mapping-union mapping71 mapping72 mapping76))))

  (define check-069
    (check
     3
     (mapping-ref (mapping-intersection mapping71 mapping74) 'c)))

  (define check-070
    (check
     42
     (mapping-ref/default (mapping-intersection mapping71 mapping75) 'b 42)))

  (define check-071
    (check
     2
     (mapping-size (mapping-difference mapping72 mapping76))))

  (define check-072
    (check
     4
     (mapping-size (mapping-xor mapping72 mapping76))))

  ;; Additional procedures for mappings with ordered keys

  (define mapping81 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping82 (mapping comparator 'a 1 'b 2 'c 3 'd 4))
  (define mapping83 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5))
  (define mapping84 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6))
  (define mapping85 (mapping comparator 'f 6 'g 7 'h 8))

  (define check-073
    (check
     '(a a a a)
     (map mapping-min-key (list mapping81 mapping82 mapping83 mapping84))))

  (define check-074
    (check
     '(c d e f)
     (map mapping-max-key (list mapping81 mapping82 mapping83 mapping84))))

  (define check-075
    (check
     '(1 1 1 1)
     (map mapping-min-value (list mapping81 mapping82 mapping83 mapping84))))

  (define check-076
    (check
     '(3 4 5 6)
     (map mapping-max-value (list mapping81 mapping82 mapping83 mapping84))))

  (define check-077
    (check
     '(c d d d)
     (map (lambda (mapping)
            (mapping-key-predecessor mapping 'e (lambda () #f)))
          (list mapping81 mapping82 mapping83 mapping84))))

  (define check-078
    (check
     '(#f #f e e)
     (map (lambda (mapping)
            (mapping-key-successor mapping 'd (lambda () #f)))
          (list mapping81 mapping82 mapping83 mapping84))))

  (define check-079
    (check '(4)
           (mapping-values (mapping-range= mapping84 'd))))

  (define check-080
    (check
     '()
     (mapping-values (mapping-range= mapping84 'z))))

  (define check-081
    (check '(1 2 3)
           (mapping-values (mapping-range< mapping84 'd))))

  (define check-082
    (check
     '(1 2 3 4)
     (mapping-values (mapping-range<= mapping84 'd))))

  (define check-083
    (check
     '(5 6)
     (mapping-values (mapping-range> mapping84 'd))))

  (define check-084
    (check
     '(4 5 6)
     (mapping-values (mapping-range>= mapping84 'd))))

  (define check-085
    (check
     '((1 2 3) (1 2 3 4) (4) (4 5 6) (5 6))
     (receive mappings
         (mapping-split mapping84 'd)
       (map mapping-values mappings))))

  (define check-086
    (check
     '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8))
     (mapping->alist (mapping-catenate comparator mapping82 'e 5 mapping85))))

  (define check-087
    (check
     '((1 . 1) (2 . 4) (3 . 9))
     (mapping->alist
      (mapping-map/monotone (lambda (key value)
                              (values value (* value value)))
                            comparator
                            mapping81))))

  (define check-088
    (check
     '(1 2 3)
     (mapping-fold/reverse (lambda (key value acc)
                             (cons value acc))
                           '() mapping81)))

  ;; Comparators

  (define mapping91 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping92 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping93 (mapping comparator 'a 1 'b 2))
  (define mapping94 (mapping comparator 'a 1 'b 2 'c 4))
  (define mapping95 (mapping comparator 'a 1 'c 3))
  (define mapping90 (mapping comparator mapping91 "a" mapping92 "b" mapping93 "c" mapping94 "d" mapping95 "e"))

  (define check-089
    (check #t
           (comparator? mapping-comparator)))

  (define check-090
    (check
     (list "a" "a" "c" "d" "e")
     (list (mapping-ref mapping90 mapping91)
           (mapping-ref mapping90 mapping92)
           (mapping-ref mapping90 mapping93)
           (mapping-ref mapping90 mapping94)
           (mapping-ref mapping90 mapping95))))

  ;; Ordering comparators
  (define check-091
    (check #t
           (=? comparator mapping91 mapping92)))

  (define check-092
    (check #t
           (not (=? comparator mapping91 mapping94))))

  (define check-093
    (check #t
           (<? comparator mapping93 mapping94)))

  (define check-094
    (check #t
           (<? comparator mapping91 mapping94)))

  (define check-095
    (check #t
           (<? comparator mapping91 mapping95))))
