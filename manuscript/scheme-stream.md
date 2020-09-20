# `(scheme stream)`

This is based on [SRFI-41](https://srfi.schemers.org/srfi-41/).

Streams, sometimes called lazy lists, are a sequential data structure
containing elements computed only on demand. A stream is either null
or is a pair with a stream in its cdr. Since elements of a stream are
computed only when accessed, streams can be infinite. Once computed,
the value of a stream element is cached in case it is needed again.

Streams without memoization were first described by Peter Landin
in 1965. Memoization became accepted as an essential feature of
streams about a decade later. Today, streams are the signature data
type of functional programming languages such as Haskell.

This Scheme Request for Implementation describes two libraries for
operating on streams: a canonical set of stream primitives and a set
of procedures and syntax derived from those primitives that permits
convenient expression of stream operations. They rely on facilities
provided by R6RS, including libraries, records, and error
reporting. To load both stream libraries, say:

```
(import (scheme stream))
```

## `stream-null`

Stream-null is a promise that, when forced, is a single object,
distinguishable from all other objects, that represents the null
stream. Stream-null is immutable and unique.

## `(stream-cons object stream)`

Stream-cons is a macro that accepts an object and a stream and creates
a newly-allocated stream containing a promise that, when forced, is a
stream-pair with the object in its stream-car and the stream in its
stream-cdr. Stream-cons must be syntactic, not procedural, because
neither object nor stream is evaluated when stream-cons is
called. Since stream is not evaluated, when the stream-pair is
created, it is not an error to call stream-cons with a stream that is
not of type stream; however, doing so will cause an error later when
the stream-cdr of the stream-pair is accessed. Once created, a
stream-pair is immutable; there is no stream-set-car! or
stream-set-cdr! that modifies an existing stream-pair. There is no
dotted-pair or improper stream as with lists.

## `(stream? object)`

Stream? is a procedure that takes an object and returns #t if the
object is a stream and #f otherwise. If object is a stream, stream?
does not force its promise. If (stream? obj) is #t, then one of
(stream-null? obj) and (stream-pair? obj) will be #t and the other
will be #f; if (stream? obj) is #f, both (stream-null? obj) and
(stream-pair? obj) will be #f.

## `(stream-null? object)`

Stream-null? is a procedure that takes an object and returns #t if the
object is the distinguished null stream and #f otherwise. If object is
a stream, stream-null? must force its promise in order to distinguish
stream-null from stream-pair.

## `(stream-pair? object)`

Stream-pair? is a procedure that takes an object and returns #t if the
object is a stream-pair constructed by stream-cons and #f
otherwise. If object is a stream, stream-pair? must force its promise
in order to distinguish stream-null from stream-pair.

## `(stream-car stream)`

Stream-car is a procedure that takes a stream and returns the object
stored in the stream-car of the stream. Stream-car signals an error if
the object passed to it is not a stream-pair. Calling stream-car
causes the object stored there to be evaluated if it has not yet been;
the object’s value is cached in case it is needed again.

## `(stream-cdr stream)

Stream-cdr is a procedure that takes a stream and returns the stream
stored in the stream-cdr of the stream. Stream-cdr signals an error if
the object passed to it is not a stream-pair. Calling stream-cdr does
not force the promise containing the stream stored in the stream-cdr
of the stream.

## `(stream-lambda args body)`

Stream-lambda creates a procedure that returns a promise to evaluate
the body of the procedure. The last body expression to be evaluated
must yield a stream. As with normal lambda, args may be a single
variable name, in which case all the formal arguments are collected
into a single list, or a list of variable names, which may be null if
there are no arguments, proper if there are an exact number of
arguments, or dotted if a fixed number of arguments is to be followed
by zero or more arguments collected into a list. Body must contain at
least one expression, and may contain internal definitions preceding
any expressions to be evaluated.

```scheme
(define strm123
  (stream-cons 1
    (stream-cons 2
      (stream-cons 3
        stream-null))))

(stream-car strm123) ⇒ 1

(stream-car (stream-cdr strm123) ⇒ 2

(stream-pair?
  (stream-cdr
    (stream-cons (/ 1 0) stream-null))) ⇒ #f

(stream? (list 1 2 3)) ⇒ #f

(define iter
  (stream-lambda (f x)
    (stream-cons x (iter f (f x)))))

(define nats (iter (lambda (x) (+ x 1)) 0))

(stream-car (stream-cdr nats)) ⇒ 1

(define stream-add
  (stream-lambda (s1 s2)
    (stream-cons
      (+ (stream-car s1) (stream-car s2))
      (stream-add (stream-cdr s1)
                  (stream-cdr s2)))))

(define evens (stream-add nats nats))

(stream-car evens) ⇒ 0

(stream-car (stream-cdr evens)) ⇒ 2

(stream-car (stream-cdr (stream-cdr evens))) ⇒ 4
```

## `(define-stream (name args) body)` syntax

Define-stream creates a procedure that returns a stream, and may
appear anywhere a normal define may appear, including as an internal
definition, and may have internal definitions of its own, including
other define-streams. The defined procedure takes arguments in the
same way as stream-lambda. Define-stream is syntactic sugar on
stream-lambda; see also stream-let, which is also a sugaring of
stream-lambda.

A simple version of stream-map that takes only a single input stream
calls itself recursively:

```
(define-stream (stream-map proc strm)
  (if (stream-null? strm)
      stream-null
      (stream-cons
        (proc (stream-car strm))
        (stream-map proc (stream-cdr strm))))))
```

## `(list->stream list-of-objects)`

[α] → {α}

List->stream takes a list of objects and returns a newly-allocated
stream containing in its elements the objects in the list. Since the
objects are given in a list, they are evaluated when list->stream is
called, before the stream is created. If the list of objects is null,
as in (list->stream '()), the null stream is returned. See also
stream.

```scheme
(define strm123 (list->stream '(1 2 3)))

; fails with divide-by-zero error
(define s (list->stream (list 1 (/ 1 0) -1)))
```

## `(port->stream [port])`

port → {char}

Port->stream takes a port and returns a newly-allocated stream
containing in its elements the characters on the port. If port is not
given it defaults to the current input port. The returned stream has
finite length and is terminated by stream-null.

It looks like one use of port->stream would be this:

```scheme
(define s ;wrong!
  (with-input-from-file filename
    (lambda () (port->stream))))
```

But that fails, because with-input-from-file is eager, and closes the
input port prematurely, before the first character is read. To read a
file into a stream, say:

```scheme
(define-stream (file->stream filename)
  (let ((p (open-input-file filename)))
    (stream-let loop ((c (read-char p)))
      (if (eof-object? c)
          (begin (close-input-port p)
                 stream-null)
          (stream-cons c
            (loop (read-char p)))))))
```

## `(stream object ...)`

Stream is syntax that takes zero or more objects and creates a
newly-allocated stream containing in its elements the objects, in
order. Since stream is syntactic, the objects are evaluated when they
are accessed, not when the stream is created. If no objects are given,
as in (stream), the null stream is returned. See also list->stream.

```scheme
(define strm123 (stream 1 2 3))

; (/ 1 0) not evaluated when stream is created
(define s (stream 1 (/ 1 0) -1))
```

## `(stream->list [n] stream)`

nat × {α} → [α]

Stream->list takes a natural number n and a stream and returns a
newly-allocated list containing in its elements the first n items in
the stream. If the stream has less than n items all the items in the
stream will be included in the returned list. If n is not given it
defaults to infinity, which means that unless stream is finite
stream->list will never return.

```scheme
(stream->list 10
  (stream-map (lambda (x) (* x x))
    (stream-from 0)))
  ⇒ (0 1 4 9 16 25 36 49 64 81)
```

## `(stream-append stream ...)`

{α} ... → {α}

Stream-append returns a newly-allocated stream containing in its
elements those elements contained in its input streams, in order of
input. If any of the input streams is infinite, no elements of any of
the succeeding input streams will appear in the output stream; thus,
if x is infinite, (stream-append x y) ≡ x. See also stream-concat.

Quicksort can be used to sort a stream, using stream-append to build
the output; the sort is lazy; so if only the beginning of the output
stream is needed, the end of the stream is never sorted.

```scheme
(define-stream (qsort lt? strm)
  (if (stream-null? strm)
      stream-null
      (let ((x (stream-car strm))
            (xs (stream-cdr strm)))
        (stream-append
          (qsort lt?
            (stream-filter
              (lambda (u) (lt? u x))
              xs))
          (stream x)
          (qsort lt?
            (stream-filter
              (lambda (u) (not (lt? u x)))
              xs))))))
```

Note also that, when used in tail position as in qsort, stream-append
does not suffer the poor performance of append on lists. The list
version of append requires re-traversal of all its list arguments
except the last each time it is called. But stream-append is
different. Each recursive call to stream-append is suspended; when it
is later forced, the preceding elements of the result have already
been traversed, so tail-recursive loops that produce streams are
efficient even when each element is appended to the end of the result
stream. This also implies that during traversal of the result only one
promise needs to be kept in memory at a time.

## `(stream-concat stream)`

{{α}} ... → {α}

Stream-concat takes a stream consisting of one or more streams and
returns a newly-allocated stream containing all the elements of the
input streams. If any of the streams in the input stream is infinite,
any remaining streams in the input stream will never appear in the
output stream. See also stream-append.

```scheme
(stream->list
  (stream-concat
    (stream
      (stream 1 2) (stream) (stream 3 2 1))))
  ⇒ (1 2 3 2 1)
```

The permutations of a finite stream can be determined by interleaving
each element of the stream in all possible positions within each
permutation of the other elements of the stream. Interleave returns a
stream of streams with x inserted in each possible position of yy:

```scheme
(define-stream (interleave x yy)
  (stream-match yy
    (() (stream (stream x)))
    ((y . ys)
      (stream-append
        (stream (stream-cons x yy))
        (stream-map
          (lambda (z) (stream-cons y z))
          (interleave x ys))))))

(define-stream (perms xs)
  (if (stream-null? xs)
      (stream (stream))
      (stream-concat
        (stream-map
          (lambda (ys)
            (interleave (stream-car xs) ys))
          (perms (stream-cdr xs))))))
```

## `(stream-constant object ...)`

α ... → {α}

Stream-constant takes one or more objects and returns a
newly-allocated stream containing in its elements the objects,
repeating the objects in succession forever.

```scheme
(stream-constant 1) ⇒ 1 1 1 ...


(stream-constant #t #f) ⇒ #t #f #t #f #t #f ...
```

## `(stream-drop n stream) procedure`

nat × {α} → {α}

Stream-drop returns the suffix of the input stream that starts at the
next element after the first n elements. The output stream shares
structure with the input stream; thus, promises forced in one instance
of the stream are also forced in the other instance of the stream. If
the input stream has less than n elements, stream-drop returns the
null stream. See also stream-take.

```scheme
(define (stream-split n strm)
  (values (stream-take n strm)
          (stream-drop n strm)))
```

## `(stream-drop-while pred? stream)`

(α → boolean) × {α} → {α}

Stream-drop-while returns the suffix of the input stream that starts
at the first element x for which (pred? x) is #f. The output stream
shares structure with the input stream. See also stream-take-while.

Stream-unique creates a new stream that retains only the first of any
sub-sequences of repeated elements.

```scheme
(define-stream (stream-unique eql? strm)
  (if (stream-null? strm)
      stream-null
      (stream-cons (stream-car strm)
        (stream-unique eql?
          (stream-drop-while
            (lambda (x)
              (eql? (stream-car strm) x))
            strm)))))
```

## `(stream-filter pred? stream)`

(α → boolean) × {α} → {α}

Stream-filter returns a newly-allocated stream that contains only
those elements x of the input stream for which (pred? x) is non-#f.

```scheme
(stream-filter odd? (stream-from 0))
   ⇒ 1 3 5 7 9 ...
```

## `(stream-fold proc base stream)`

(α × β → α) × α × {β} → α

Stream-fold applies a binary procedure to base and the first element
of stream to compute a new base, then applies the procedure to the new
base and the next element of stream to compute a succeeding base, and
so on, accumulating a value that is finally returned as the value of
stream-fold when the end of the stream is reached. Stream must be
finite, or stream-fold will enter an infinite loop. See also
stream-scan, which is similar to stream-fold, but useful for infinite
streams. For readers familiar with other functional languages, this is
a left-fold; there is no corresponding right-fold, since right-fold
relies on finite streams that are fully-evaluated, at which time they
may as well be converted to a list.

Stream-fold is often used to summarize a stream in a single value, for
instance, to compute the maximum element of a stream.

```scheme
(define (stream-maximum lt? strm)
  (stream-fold
    (lambda (x y) (if (lt? x y) y x))
    (stream-car strm)
    (stream-cdr strm)))
```

Sometimes, it is useful to have stream-fold defined only on non-null
streams:

```scheme
(define (stream-fold-one proc strm)
  (stream-fold proc
    (stream-car strm)
    (stream-cdr strm)))
```

Stream-minimum can then be defined as:

```scheme
(define (stream-minimum lt? strm)
  (stream-fold-one
    (lambda (x y) (if (lt? x y) x y))
    strm))
```

Stream-fold can also be used to build a stream:

```scheme
(define-stream (isort lt? strm)
    (define-stream (insert strm x)
      (stream-match strm
        (() (stream x))
        ((y . ys)
          (if (lt? y x)
              (stream-cons y (insert ys x))
              (stream-cons x strm)))))
    (stream-fold insert stream-null strm))
```

## `(stream-for-each proc stream ...)`

(α × β × ...) × {α} × {β} ...

Stream-for-each applies a procedure element-wise to corresponding
elements of the input streams for its side-effects; it returns
nothing. Stream-for-each stops as soon as any of its input streams is
exhausted.

The following procedure displays the contents of a file:

```scheme
(define (display-file filename)
  (stream-for-each display
    (file->stream filename)))
```

## `(stream-from first [step])`

number × number → {number}

Stream-from creates a newly-allocated stream that contains first as
its first element and increments each succeeding element by step. If
step is not given it defaults to 1. First and step may be of any
numeric type. Stream-from is frequently useful as a generator in
stream-of expressions. See also stream-range for a similar procedure
that creates finite streams.

Stream-from could be implemented as (stream-iterate (lambda (x) (+ x
step)) first).

```scheme
(define nats (stream-from 0)) ⇒ 0 1 2 ...

(define odds (stream-from 1 2)) ⇒ 1 3 5 ...
```

## `(stream-iterate proc base)`

(α → α) × α → {α}

Stream-iterate creates a newly-allocated stream containing base in its
first element and applies proc to each element in turn to determine
the succeeding element. See also stream-unfold and stream-unfolds.

```scheme
(stream-iterate (lambda (x) (+ x 1)) 0)
  ⇒ 0 1 2 3 4 ...

(stream-iterate (lambda (x) (* x 2)) 1)
  ⇒ 1 2 4 8 16 ...
```

Given a seed between 0 and 232, exclusive, the following expression
creates a stream of pseudo-random integers between 0 and 232,
exclusive, beginning with seed, using the method described by Stephen
Park and Keith Miller:

```scheme
(stream-iterate
  (lambda (x) (modulo (* x 16807) 2147483647))
  seed)
```

Successive values of the continued fraction shown below approach the
value of the “golden ratio” φ ≈ 1.618:

Continued fraction

The fractions can be calculated by the stream

```scheme
(stream-iterate (lambda (x) (+ 1 (/ x))) 1)
```

## `(stream-length stream)`

{α} → nat

Stream-length takes an input stream and returns the number of elements
in the stream; it does not evaluate its elements. Stream-length may
only be used on finite streams; it enters an infinite loop with
infinite streams.

```scheme
(stream-length strm123) ⇒ 3
```

## `(stream-let tag ((var expr) ...) body)` syntax

Stream-let creates a local scope that binds each variable to the value
of its corresponding expression. It additionally binds tag to a
procedure which takes the bound variables as arguments and body as its
defining expressions, binding the tag with stream-lambda. Tag is in
scope within body, and may be called recursively. When the expanded
expression defined by the stream-let is evaluated, stream-let
evaluates the expressions in its body in an environment containing the
newly-bound variables, returning the value of the last expression
evaluated, which must yield a stream.

Stream-let provides syntactic sugar on stream-lambda, in the same
manner as normal let provides syntactic sugar on normal
lambda. However, unlike normal let, the tag is required, not optional,
because unnamed stream-let is meaningless.

Stream-member returns the first stream-pair of the input strm with a
stream-car x that satisfies (eql? obj x), or the null stream if x is
not present in strm.

```scheme
(define-stream (stream-member eql? obj strm)
  (stream-let loop ((strm strm))
    (cond ((stream-null? strm) strm)
          ((eql? obj (stream-car strm)) strm)
          (else (loop (stream-cdr strm))))))
```

## `(stream-map proc stream ...)`

(α × β ... → ω) × {α} × {β} ... → {ω}

Stream-map applies a procedure element-wise to corresponding elements
of the input streams, returning a newly-allocated stream containing
elements that are the results of those procedure applications. The
output stream has as many elements as the minimum-length input stream,
and may be infinite.

```scheme
(define (square x) (* x x))

(stream-map square (stream 9 3)) ⇒ 81 9

(define (sigma f m n)
  (stream-fold + 0
    (stream-map f (stream-range m (+ n 1)))))

(sigma square 1 100) ⇒ 338350
```

In some functional languages, stream-map takes only a single input
stream, and stream-zipwith provides a companion function that takes
multiple input streams.

## `(stream-match stream clause ...)` syntax

Stream-match provides the syntax of pattern-matching for streams. The
input stream is an expression that evaluates to a stream. Clauses are
of the form (pattern [fender] expr), consisting of a pattern that
matches a stream of a particular shape, an optional fender that must
succeed if the pattern is to match, and an expression that is
evaluated if the pattern matches. There are four types of patterns:

- () — Matches the null stream.
- (pat0 pat1 ...) — Matches a finite stream with length exactly equal
  to the number of pattern elements.
- (pat0 pat1 ... . patrest) — Matches an infinite stream, or a finite
  stream with length at least as great as the number of pattern
  elements before the literal dot.
- pat — Matches an entire stream. Should always appear last in the
  list of clauses; it’s not an error to appear elsewhere, but
  subsequent clauses could never match.

Each pattern element pati may be either:

- An identifier — Matches any stream element. Additionally, the value
  of the stream element is bound to the variable named by the
  identifier, which is in scope in the fender and expression of the
  corresponding clause. Each identifier in a single pattern must be
  unique.
- A literal underscore — Matches any stream element, but creates no
  bindings.

The patterns are tested in order, left-to-right, until a matching
pattern is found; if fender is present, it must evaluate as non-#f for
the match to be successful. Pattern variables are bound in the
corresponding fender and expression. Once the matching pattern is
found, the corresponding expression is evaluated and returned as the
result of the match. An error is signaled if no pattern matches the
input stream.

Stream-match is often used to distinguish null streams from non-null
streams, binding head and tail:

```scheme
(define (len strm)
  (stream-match strm
    (() 0)
    ((head . tail) (+ 1 (len tail)))))
```

Fenders can test the common case where two stream elements must be
identical; the else pattern is an identifier bound to the entire
stream, not a keyword as in cond.

```scheme
(stream-match strm
  ((x y . _) (equal? x y) 'ok)
  (else 'error))
```

A more complex example uses two nested matchers to match two different
stream arguments; (stream-merge lt? . strms) stably merges two or more
streams ordered by the lt? predicate:

```scheme
(define-stream (stream-merge lt? . strms)
  (define-stream (merge xx yy)
    (stream-match xx (() yy) ((x . xs)
      (stream-match yy (() xx) ((y . ys)
        (if (lt? y x)
            (stream-cons y (merge xx ys))
            (stream-cons x (merge xs yy))))))))
  (stream-let loop ((strms strms))
    (cond ((null? strms) stream-null)
          ((null? (cdr strms)) (car strms))
          (else (merge (car strms)
                       (apply stream-merge lt?
                         (cdr strms)))))))
```

## `(stream-of expr clause ...)` syntax

Stream-of provides the syntax of stream comprehensions, which generate
streams by means of looping expressions. The result is a stream of
objects of the type returned by expr. There are four types of clauses:


- (var in stream-expr) — Loop over the elements of stream-expr, in
  order from the start of the stream, binding each element of the
  stream in turn to var. Stream-from and stream-range are frequently
  useful as generators for stream-expr.
- (var is expr) — Bind var to the value obtained by evaluating expr.
- (pred? expr) — Include in the output stream only those elements x
  for which (pred? x) is non-#f.

The scope of variables bound in the stream comprehension is the
clauses to the right of the binding clause (but not the binding clause
itself) plus the result expression.

When two or more generators are present, the loops are processed as if
they are nested from left to right; that is, the rightmost generator
varies fastest. A consequence of this is that only the first generator
may be infinite and all subsequent generators must be finite. If no
generators are present, the result of a stream comprehension is a
stream containing the result expression; thus, (stream-of 1) produces
a finite stream containing only the element 1.

```scheme
(stream-of (* x x)
  (x in (stream-range 0 10))
  (even? x))
  ⇒ 0 4 16 36 64

(stream-of (list a b)
  (a in (stream-range 1 4))
  (b in (stream-range 1 3)))
  ⇒ (1 1) (1 2) (2 1) (2 2) (3 1) (3 2)

(stream-of (list i j)
  (i in (stream-range 1 5))
  (j in (stream-range (+ i 1) 5)))
  ⇒ (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)
```

## `(stream-range first past [step])`

number × number × number → {number}

Stream-range creates a newly-allocated stream that contains first as
its first element and increments each succeeding element by step. The
stream is finite and ends before past, which is not an element of the
stream. If step is not given it defaults to 1 if first is less than
past and -1 otherwise. First, past and step may be of any numeric
type. Stream-range is frequently useful as a generator in stream-of
expressions. See also stream-from for a similar procedure that creates
infinite streams.

```scheme
(stream-range 0 10) ⇒ 0 1 2 3 4 5 6 7 8 9

(stream-range 0 10 2) → 0 2 4 6 8
```

Successive elements of the stream are calculated by adding step to
first, so if any of first, past or step are inexact, the length of the
output stream may differ from (ceiling (- (/ (- past first) step) 1).

## `(stream-ref stream n)`

{α} × nat → α

Stream-ref returns the nth element of stream, counting from zero. An
error is signaled if n is greater than or equal to the length of
stream.

```scheme
(define (fact n)
  (stream-ref
    (stream-scan * 1 (stream-from 1))
    n))
```

## `(stream-reverse stream)`

{α} → {α}

Stream-reverse returns a newly-allocated stream containing the
elements of the input stream but in reverse order. Stream-reverse may
only be used with finite streams; it enters an infinite loop with
infinite streams. Stream-reverse does not force evaluation of the
elements of the stream.

```scheme
> (define s (stream 1 (/ 1 0) -1))
> (define r (stream-reverse s))
> (stream-ref r 0)
> (stream-ref r 2)
1
> (stream-ref r 1)
error: division by zero
```

## `(stream-scan proc base stream)`

(α × β → α) × α × {β} → {α}

Stream-scan accumulates the partial folds of an input stream into a
newly-allocated output stream. The output stream is the base followed
by (stream-fold proc base (stream-take i stream)) for each of the
first i elements of stream.

```scheme
(stream-scan + 0 (stream-from 1))
  ⇒ (stream 0 1 3 6 10 15 ...)

(stream-scan * 1 (stream-from 1))
  ⇒ (stream 1 1 2 6 24 120 ...)
```

## `(stream-take n stream)`

nat × {α} → {α}

Stream-take takes a non-negative integer n and a stream and returns a
newly-allocated stream containing the first n elements of the input
stream. If the input stream has less than n elements, so does the
output stream. See also stream-drop.

Mergesort splits a stream into two equal-length pieces, sorts them
recursively and merges the results:

```scheme
(define-stream (msort lt? strm)
  (let* ((n (quotient (stream-length strm) 2))
         (ts (stream-take n strm))
         (ds (stream-drop n strm)))
    (if (zero? n)
        strm
        (stream-merge lt?
          (msort < ts) (msort < ds)))))
```

## `(stream-take-while pred? stream)`

(α → boolean) × {α} → {α}

Stream-take-while takes a predicate and a stream and returns a
newly-allocated stream containing those elements x that form the
maximal prefix of the input stream for which (pred? x) is non-#f. See
also stream-drop-while.

```scheme
(stream-car
  (stream-reverse
    (stream-take-while
      (lambda (x) (< x 1000))
        primes))) ⇒ 997
```

## `(stream-unfold map pred? gen base)`

(α → β) × (α → boolean) × (α → α) × α → {β}

Stream-unfold is the fundamental recursive stream constructor. It
constructs a stream by repeatedly applying gen to successive values of
base, in the manner of stream-iterate, then applying map to each of
the values so generated, appending each of the mapped values to the
output stream as long as (pred? base) is non-#f. See also
stream-iterate and stream-unfolds.

The expression below creates the finite stream 0 1 4 9 16 25 36 49
64 81. Initially the base is 0, which is less than 10, so map squares
the base and the mapped value becomes the first element of the output
stream. Then gen increments the base by 1, so it becomes 1; this is
less than 10, so map squares the new base and 1 becomes the second
element of the output stream. And so on, until the base becomes 10,
when pred? stops the recursion and stream-null ends the output stream.

```scheme
(stream-unfold
  (lambda (x) (expt x 2)) ; map
  (lambda (x) (< x 10))   ; pred?
  (lambda (x) (+ x 1))    ; gen
  0)                      ; base
```

## `(stream-unfolds proc seed)`

(α → (values α × β ...)) × α → (values {β} ...)

Stream-unfolds returns n newly-allocated streams containing those
elements produced by successive calls to the generator proc, which
takes the current seed as its argument and returns n+1 values

```scheme
(proc seed → seed result0 ... resultn-1
```

where the returned seed is the input seed to the next call to the
generator and resulti indicates how to produce the next element of the
ith result stream:

- (value) — value is the next car of the result stream
- #f — no value produced by this iteration of the generator proc for
  the result stream
- () — the end of the result stream

It may require multiple calls of proc to produce the next element of
any particular result stream. See also stream-iterate and
stream-unfold.

Stream-unfolds is especially useful when writing expressions that
return multiple streams. For instance, (stream-partition pred? strm)
is equivalent to

```scheme
(values
  (stream-filter pred? strm)
  (stream-filter
    (lambda (x) (not (pred? x))) strm))
```

but only tests pred? once for each element of strm.

```scheme
(define (stream-partition pred? strm)
  (stream-unfolds
    (lambda (s)
      (if (stream-null? s)
          (values s '() '())
          (let ((a (stream-car s))
                (d (stream-cdr s)))
            (if (pred? a)
                (values d (list a) #f)
                (values d #f (list a))))))
    strm))

(call-with-values
  (lambda ()
    (stream-partition odd?
      (stream-range 1 6)))
  (lambda (odds evens)
    (list (stream->list odds)
          (stream->list evens))))
  ⇒ ((1 3 5) (2 4))
```

## `(stream-zip stream ...)`

{α} × {β} × ... → {[α β ...]}

Stream-zip takes one or more input streams and returns a
newly-allocated stream in which each element is a list (not a stream)
of the corresponding elements of the input streams. The output stream
is as long as the shortest input stream, if any of the input streams
is finite, or is infinite if all the input streams are infinite.

A common use of stream-zip is to add an index to a stream, as in
(stream-finds eql? obj strm), which returns all the zero-based indices
in strm at which obj appears; (stream-find eql? obj strm) returns the
first such index, or #f if obj is not in strm.

```scheme
(define-stream (stream-finds eql? obj strm)
  (stream-of (car x)
    (x in (stream-zip (stream-from 0) strm))
    (eql? obj (cadr x))))

(define (stream-find eql? obj strm)
  (stream-car
    (stream-append
      (stream-finds eql? obj strm)
      (stream #f))))

(stream-find char=? #\l
  (list->stream
    (string->list "hello"))) ⇒ 2

(stream-find char=? #\l
  (list->stream
    (string->list "goodbye"))) ⇒ #f
```

Stream-find is not as inefficient as it looks; although it calls
stream-finds, which finds all matching indices, the matches are
computed lazily, and only the first match is needed for stream-find.
