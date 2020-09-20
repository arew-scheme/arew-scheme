# `(scheme vector)`

This library is based on
[SRFI-133](https://srfi.schemers.org/srfi-133/).

## `(make-vector size [fill])`

[R7RS-small] Creates and returns a vector of size size. If fill is
specified, all the elements of the vector are initialized to
fill. Otherwise, their contents are indeterminate.

Example:

```scheme
(make-vector 5 3)
#(3 3 3 3 3)
```

## `(vector x ...)`

[R7RS-small] Creates and returns a vector whose elements are x ....

Example:

```scheme
(vector 0 1 2 3 4)
#(0 1 2 3 4)
```

## `(vector-unfold f length initial-seed ...)`

The fundamental vector constructor. Creates a vector whose length is
length and iterates across each index k between 0 and length, applying
f at each iteration to the current index and current seeds, in that
order, to receive n + 1 values: first, the element to put in the kth
slot of the new vector and n new seeds for the next iteration. It is
an error for the number of seeds to vary between iterations. Note that
the termination condition is different from the unfold procedure of
SRFI 1.

Examples:

``scheme
    (vector-unfold (λ (i x) (values x (- x 1)))
                     10 0)
    #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
```

Construct a vector of the sequence of integers in the range [0,n).

```scheme
(vector-unfold values n)
#(0 1 2 ... n-2 n-1)
```

Copy vector.

```scheme
    (vector-unfold (λ (i) (vector-ref vector i))
                     (vector-length vector))
```

## `(vector-unfold-right f length initial-seed ...)`

Like vector-unfold, but it uses f to generate elements from
right-to-left, rather than left-to-right. The first index used is
length - 1. Note that the termination condition is different from the
unfold-right procedure of SRFI 1.

Examples:

Construct a vector of pairs of non-negative integers whose values sum
to 4.

```scheme
(vector-unfold-right (λ (i x) (values (cons i x) (+ x 1))) 5 0)
#((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
```

Reverse vector.

```scheme
    (vector-unfold-right (λ (i x) (values (vector-ref vector x) (+ x 1)))
                           (vector-length vector)
                           0)
```

## `(vector-copy vec [start [end]])`

[R7RS-small] Allocates a new vector whose length is end - start and
fills it with elements from vec, taking elements from vec starting at
index start and stopping at index end. Start defaults to 0 and end
defaults to the value of (vector-length vec). SRFI 43 provides an
optional fill argument to supply values if end is greater than the
length of vec. Neither R7RS-small nor this SRFI requires support for
this argument.

Examples:

```scheme
    (vector-copy '#(a b c d e f g h i))
    #(a b c d e f g h i)

    (vector-copy '#(a b c d e f g h i) 6)
    #(g h i)

    (vector-copy '#(a b c d e f g h i) 3 6)
    #(d e f)
```

## `(vector-reverse-copy vec [start [end]])`

Like vector-copy, but it copies the elements in the reverse order from
vec.

Example:

```scheme
(vector-reverse-copy '#(5 4 3 2 1 0) 1 5)
#(1 2 3 4)
```

## `(vector-append vec ...)`

[R7RS-small] Returns a newly allocated vector that contains all
elements in order from the subsequent locations in vec ....

Examples:

```scheme
(vector-append '#(x) '#(y))
#(x y)

(vector-append '#(a) '#(b c d))
#(a b c d)

(vector-append '#(a #(b)) '#(#(c)))
#(a #(b) #(c))
```

## `(vector-concatenate list-of-vectors)`

Appends each vector in list-of-vectors. This is equivalent to:

```scheme
(apply vector-append list-of-vectors)
```

However, it may be implemented better.

Example:

```scheme
(vector-concatenate '(#(a b) #(c d)))
#(a b c d)
```

## `(vector-append-subvectors [vec start end] ...)`

Returns a vector that contains every element of each vec from start to
end in the specified order. This procedure is a generalization of
vector-append.

Example:

```scheme
(vector-append-subvectors '#(a b c d e) 0 2 '#(f g h i j) 2 4)
#(a b h i)
```

## `(vector? x)`

[R7RS-small] Disjoint type predicate for vectors: this returns #t if x
is a vector, and #f if otherwise.

Examples:

```scheme
(vector? '#(a b c))
#t

(vector? '(a b c))
#f

(vector? #t)
#f

(vector? '#())
#t

(vector? '())
#f
```

## `(vector-empty? vec)`

Returns #t if vec is empty, i.e. its length is 0, and #f if not.

Examples:

```scheme
(vector-empty? '#(a))
#f

(vector-empty? '#(()))
#f

(vector-empty? '#(#()))
#f

(vector-empty? '#())
#t
```

## `(vector= elt=? vec ...)`

Vector structure comparator, generalized across user-specified element
comparators. Vectors a and b are considered equal by vector= iff their
lengths are the same, and for each respective element Ea and Eb,
(elt=? Ea Eb) returns a true value. Elt=? is always applied to two
arguments.

If there are only zero or one vector arguments, #t is automatically
returned. The dynamic order in which comparisons of elements and of
vectors are performed is left completely unspecified; do not rely on a
particular order.

Examples:

```scheme
(vector= eq? '#(a b c d) '#(a b c d))
#t

(vector= eq? '#(a b c d) '#(a b d c))
#f

(vector= = '#(1 2 3 4 5) '#(1 2 3 4))
#f

(vector= = '#(1 2 3 4) '#(1 2 3 4))
#t

The two trivial cases.

(vector= eq?)
#t

(vector= eq? '#(a))
#t

Note the fact that we don't use vector literals in the next two — it is unspecified whether or not literal vectors with the same external representation are eq?.

(vector= eq? (vector (vector 'a)) (vector (vector 'a)))
#f

(vector= equal? (vector (vector 'a)) (vector (vector 'a)))
#t
```

## `(vector-ref vec i)`

[R7RS-small] Vector element dereferencing: returns the value that the
location in vec at i is mapped to in the store. Indexing is based on
zero. I must be within the range [0, (vector-length vec)).

Example:

```scheme
(vector-ref '#(a b c d) 2)
c
```

## `(vector-length vec)`

[R7RS-small] Returns the length of vec, the number of locations
reachable from vec. (The careful word 'reachable' is used to allow for
'vector slices,' whereby vec refers to a larger vector that contains
more locations that are unreachable from vec. This SRFI does not
define vector slices, but later SRFIs may.)

Example:

```scheme
(vector-length '#(a b c))
3
```

## `(vector-fold kons knil vec1 vec2 ...)`

The fundamental vector iterator. Kons is iterated over each value in
all of the vectors, stopping at the end of the shortest; kons is
applied as (kons state (vector-ref vec1 i) (vector-ref vec2 i) ...)
where state is the current state value — the current state value
begins with knil, and becomes whatever kons returned on the previous
iteration —, and i is the current index.

The iteration is strictly left-to-right.

Examples:

Find the longest string's length in vector-of-strings.
```scheme
    (vector-fold (λ (len str) (max (string-length str) len))
                   0 vector-of-strings)
```

Produce a list of the reversed elements of vec.

```
    (vector-fold (λ (tail elt) (cons elt tail))
                   '() vec)
```

Count the number of even numbers in vec.

```scheme
    (vector-fold (λ (counter n)
                     (if (even? n) (+ counter 1) counter))
                   0 vec)
```

## `(vector-fold-right kons knil vec1 vec2 ...)`

Similar to vector-fold, but it iterates right to left instead of left
to right.

Example:

Convert a vector to a list.

```scheme
    (vector-fold-right (λ (tail elt) (cons elt tail))
                         '() '#(a b c d))
    (a b c d)
```

## `(vector-map f vec1 vec2 ...)`

[R7RS-small] Constructs a new vector of the shortest size of the
vector arguments. Each element at index i of the new vector is mapped
from the old vectors by (f (vector-ref vec1 i) (vector-ref vec2 i)
...). The dynamic order of application of f is unspecified.

Examples:

```scheme
    (vector-map (λ (x) (* x x))
                  (vector-unfold (λ (i x) (values x (+ x 1))) 4 1))
    #(1 4 9 16)

    (vector-map (λ (x y) (* x y))
                  (vector-unfold (λ (x) (values x (+ x 1))) 5 1)
                  (vector-unfold (λ (x) (values x (- x 1))) 5 5))
    #(5 8 9 8 5)

    (let ((count 0))
       (vector-map (λ (ignored-elt)
                     (set! count (+ count 1))
                     count)
                   '#(a b)))
    #(1 2) OR #(2 1)
```

## `(vector-map! f vec1 vec2 ...)`

Similar to vector-map, but rather than mapping the new elements into a
new vector, the new mapped elements are destructively inserted into
vec1. Again, the dynamic order of application of f unspecified, so it
is dangerous for f to apply either vector-ref or vector-set! to vec1
in f.

## `(vector-for-each f vec1 vec2 ...)`

[R7RS-small] Simple vector iterator: applies f to the corresponding
list of parallel elements from vec1 vec2 ... in the range [0, length),
where length is the length of the smallest vector argument passed, In
contrast with vector-map, f is reliably applied to each subsequent
element, starting at index 0, in the vectors.

Example:

```scheme
    (vector-for-each (λ (x) (display x) (newline))
                     '#("foo" "bar" "baz" "quux" "zot"))
```

Displays:

```
foo
bar
baz
quux
zot
```

## `(vector-count pred? vec1 vec2 ...)`

Counts the number of parallel elements in the vectors that satisfy
pred?, which is applied, for each index i in the range [0, length)
where length is the length of the smallest vector argument, to each
parallel element in the vectors, in order.

Examples:

```scheme
(vector-count even? '#(3 1 4 1 5 9 2 5 6))
3

(vector-count < '#(1 3 6 9) '#(2 4 6 8 10 12))
2
```

## `(vector-cumulate f knil vec)`

Returns a newly allocated vector new with the same length as vec. Each
element i of new is set to the result of invoking f on newi-1 and
veci, except that for the first call on f, the first argument is
knil. The new vector is returned.

Note that the order of arguments to vector-cumulate was changed by
errata-3 on 2016-09-02.

Example:

```scheme
(vector-cumulate + 0 '#(3 1 4 1 5 9 2 5 6))
#(3 4 8 9 14 23 25 30 36)
```

## `(vector-index pred? vec1 vec2 ...)`

Finds & returns the index of the first elements in vec1 vec2 ... that
satisfy pred?. If no matching element is found by the end of the
shortest vector, #f is returned.

Examples:

```scheme
(vector-index even? '#(3 1 4 1 5 9))
2

(vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
1

(vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
#f
```

## `(vector-index-right pred? vec1 vec2 ...)`

Like vector-index, but it searches right-to-left, rather than
left-to-right, and all of the vectors must have the same length.


## `(vector-skip pred? vec1 vec2 ...)`

Finds & returns the index of the first elements in vec1 vec2 ... that
do not satisfy pred?. If all the values in the vectors satisfy pred?
until the end of the shortest vector, this returns #f. This is
equivalent to:

``scheme
    (vector-index (λ (x1 x2 ...) (not (pred? x1 x1 ...)))
                        vec1 vec2 ...)
```

Example:

```scheme
(vector-skip number? '#(1 2 a b 3 4 c d))
2
```

## `(vector-skip-right pred? vec1 vec2 ...)`

Like vector-skip, but it searches for a non-matching element
right-to-left, rather than left-to-right, and it is an error if all of
the vectors do not have the same length. This is equivalent to:

```scheme
    (vector-index-right (λ (x1 x2 ...) (not (pred? x1 x1 ...)))
                              vec1 vec2 ...)
```

## `(vector-binary-search vec value cmp)`

Similar to vector-index and vector-index-right, but instead of
searching left to right or right to left, this performs a binary
search. If there is more than one element of vec that matches value in
the sense of cmp, vector-binary-search may return the index of any of
them.

cmp should be a procedure of two arguments and return a negative
integer, which indicates that its first argument is less than its
second, zero, which indicates that they are equal, or a positive
integer, which indicates that the first argument is greater than the
second argument. An example cmp might be:

```scheme
    (λ (char1 char2)
      (cond ((char<? char1 char2) -1)
            ((char=? char1 char2) 0)
            (else 1)))
```

## `(vector-any pred? vec1 vec2 ...)`

Finds the first set of elements in parallel from vec1 vec2 ... for
which pred? returns a true value. If such a parallel set of elements
exists, vector-any returns the value that pred? returned for that set
of elements. The iteration is strictly left-to-right.

## `(vector-every pred? vec1 vec2 ...)`

If, for every index i between 0 and the length of the shortest vector
argument, the set of elements (vector-ref vec1 i) (vector-ref vec2 i)
... satisfies pred?, vector-every returns the value that pred?
returned for the last set of elements, at the last index of the
shortest vector. The iteration is strictly left-to-right.

## `(vector-partition pred? vec)`

A vector the same size as vec is newly allocated and filled with all
the elements of vec that satisfy pred? in their original order
followed by all the elements that do not satisfy pred?, also in their
original order.

Two values are returned, the newly allocated vector and the index of
the leftmost element that does not satisfy pred?.

## `(vector-set! vec i value)`

[R7RS-small] Assigns the contents of the location at i in vec to
value.

## `(vector-swap! vec i j)`

Swaps or exchanges the values of the locations in vec at i & j.

## `(vector-fill! vec fill [start [end]])`

[R7RS-small] Assigns the value of every location in vec between start,
which defaults to 0 and end, which defaults to the length of vec, to
fill.

## `(vector-reverse! vec [start [end]])`

Destructively reverses the contents of the sequence of locations in
vec between start and end. Start defaults to 0 and end defaults to the
length of vec. Note that this does not deeply reverse.

## `(vector-copy! to at from [start [end]])`

[R7RS-small] Copies the elements of vector from between start and end
to vector to, starting at at. The order in which elements are copied
is unspecified, except that if the source and destination overlap,
copying takes place as if the source is first copied into a temporary
vector and then into the destination. This can be achieved without
allocating storage by making sure to copy in the correct direction in
such circumstances.

## `(vector-reverse-copy! to at from [start [end]])`

Like vector-copy!, but the elements appear in to in reverse order.

## `(vector-unfold! f vec start end initial-seed ...)`

Like vector-unfold, but the elements are copied into the vector vec
starting at element start rather than into a newly allocated
vector. Terminates when end-start elements have been generated.

## `(vector-unfold-right! f vec start end initial-seed ...)`

Like vector-unfold!, but the elements are copied in reverse order into
the vector vec starting at the index preceding end.

## `(vector->list vec [start [end]])`

[R7RS-small] Creates a list containing the elements in vec between
start, which defaults to 0, and end, which defaults to the length of
vec.

## `(reverse-vector->list vec [start [end]])`

Like vector->list, but the resulting list contains the elements in
reverse of vec.

## `(list->vector proper-list)`

[R7RS-small] Creates a vector of elements from proper-list.

## `(reverse-list->vector proper-list)`

Like list->vector, but the resulting vector contains the elements in
reverse of proper-list.

## `(string->vector string [start [end]])`

[R7RS-small] Creates a vector containing the elements in string
between start, which defaults to 0, and end, which defaults to the
length of string.

## `(vector->string vec [start [end]])`

[R7RS-small] Creates a string containing the elements in vec between
start, which defaults to 0, and end, which defaults to the length of
vec. It is an error if the elements are not characters.
