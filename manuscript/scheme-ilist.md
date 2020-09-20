# `(scheme ilist)`

This library is based on [SRFI-116](https://srfi.schemers.org/srfi-116/).

Scheme currently does not provide immutable pairs corresponding to its
existing mutable pairs, although most uses of pairs do not exploit
their mutability. The Racket system takes the radical approach of
making Scheme's pairs immutable, and providing a minimal library of
mutable pairs with procedures named mpair?, mcons, mcar, mcdr,
set-mcar!, set-mcdr!. This SRFI takes the opposite approach of leaving
Scheme's pairs unchanged and providing a full set of routines for
creating and dealing with immutable pairs. The sample implementation
is portable (to systems with SRFI 9) and efficient.


## `(ipair a d)`

The primitive constructor. Returns a newly allocated ipair whose icar
is a and whose icdr is d. The ipair is guaranteed to be different (in
the sense of eqv?) from every existing object.

```schemme
(ipair 'a '())        => (a)
(ipair (iq a) (iq b c d)) => ((a) b c d)
(ipair "a" (iq b c))    => ("a" b c)
(ipair 'a 3)          => (a . 3)
(ipair (iq a b) 'c)     => ((a b ) . c)
```

## `(ilist object ...)`

Returns a newly allocated ilist of its arguments.

```scheme
(ilist 'a (+ 3 4) 'c) =>  (a 7 c)
(ilist)               =>  ()
```

## `(xipair d a)`

```scheme
(lambda (d a) (ipair a d))
```

Of utility only as a value to be conveniently passed to higher-order
procedures.

```scheme
(xipair (iq b c) 'a) => (a b c)
```
The name stands for "eXchanged Immutable PAIR."

## `(ipair* elt1 elt2 ...)

Like ilist, but the last argument provides the tail of the constructed ilist, returning

```scheme
(ipair elt1 (ipair elt2 (ipair ... eltn)))

(ipair* 1 2 3 4) => (1 2 3 . 4)
(ipair* 1) => 1
```

## `(make-ilist n [fill])`

Returns an n-element ilist, whose elements are all the value fill. If
the fill argument is not given, the elements of the ilist may be
arbitrary values.

```scheme
(make-ilist 4 'c) => (c c c c)
```

## `(ilist-tabulate n init-proc)`

Returns an n-element ilist. Element i of the ilist, where 0 <= i < n, is produced by (init-proc i). No guarantee is made about the dynamic order in which init-proc is applied to these indices.

```scheme
(ilist-tabulate 4 values) => (0 1 2 3)
```

## `(ilist-copy dilist)`

Copies the spine of the argument, including the ilist tail.

## `(iiota count [start step])`

Returns an ilist containing the elements

```scheme
(start start+step ... start+(count-1)*step)
```

The start and step parameters default to 0 and 1, respectively. This
procedure takes its name from the APL primitive.

```scheme
(iiota 5) => (0 1 2 3 4)
(iiota 5 0 -0.1) => (0 -0.1 -0.2 -0.3 -0.4)
```

## `(proper-ilist? x)`
## `(ilist? x)`

These identifiers are bound either to the same procedure, or to
procedures of equivalent behavior. In either case, true is returned
iff x is a proper ilist — a ()-terminated ilist.

More carefully: The empty list is a proper ilist. An ipair whose icdr
is a proper ilist is also a proper ilist. Everything else is a dotted
ilist. This includes non-ipair, non-() values (e.g. symbols, numbers,
mutable pairs), which are considered to be dotted ilists of length 0.

## `(dotted-ilist? x)`

True if x is a finite, non-nil-terminated ilist. That is, there exists
an n >= 0 such that icdrn(x) is neither an ipair nor (). This includes
non-ipair, non-() values (e.g. symbols, numbers), which are considered
to be dotted ilists of length 0.

```scheme
(dotted-ilist? x) = (not (proper-ilist? x))
```

## `(ipair? object)`

Returns #t if object is an ipair; otherwise, #f.

```scheme
(ipair? (ipair 'a 'b)) =>  #t
(ipair? (iq a b c)) =>  #t
(ipair? (cons 1 2)) =>  #f
(ipair? '())        =>  #f
(ipair? '#(a b))    =>  #f
(ipair? 7)          =>  #f
(ipair? 'a)         =>  #f
```

## `(null-ilist? ilist)

Ilist is a proper ilist. This procedure returns true if the argument
is the empty list (), and false otherwise. It is an error to pass this
procedure a value which is not a proper ilist. This procedure is
recommended as the termination condition for ilist-processing
procedures that are not defined on dotted ilists.

## `(not-ipair? x)`

```scheme
(lambda (x) (not (ipair? x)))
```

Provided as a procedure as it can be useful as the termination
condition for ilist-processing procedures that wish to handle all
ilists, both proper and dotted.

## `(ilist= elt= ilist1 ...)`

Determines ilist equality, given an element-equality procedure. Proper
ilist A equals proper ilist B if they are of the same length, and
their corresponding elements are equal, as determined by elt=. If the
element-comparison procedure's first argument is from ilisti, then its
second argument is from ilisti+1, i.e. it is always called as (elt= a
b) for a an element of ilist A, and b an element of ilist B.

In the n-ary case, every ilisti is compared to ilisti+1 (as opposed,
for example, to comparing ilist1 to ilisti, for i>1). If there are no
ilist arguments at all, ilist= simply returns true.

It is an error to apply ilist= to anything except proper ilists. It
cannot reasonably be extended to dotted ilists, as it provides no way
to specify an equality procedure for comparing the ilist terminators.

Note that the dynamic order in which the elt= procedure is applied to
pairs of elements is not specified. For example, if ilist= is applied
to three ilists, A, B, and C, it may first completely compare A to B,
then compare B to C, or it may compare the first elements of A and B,
then the first elements of B and C, then the second elements of A and
B, and so forth.

The equality procedure must be consistent with eq?. That is, it must
be the case that:

```scheme
(eq? x y) => (elt= x y).
```

Note that this implies that two ilists which are eq? are always
ilist=, as well; implementations may exploit this fact to "short-cut"
the element-by-element comparisons.

```scheme
(ilist= eq?) => #t       ; Trivial cases
(ilist= eq? (iq a)) => #t
```

## `(icar ipair)`
## `(icdr ipair)`

These procedures return the contents of the icar and icdr field of
their argument, respectively. Note that it is an error to apply them
to the empty ilist.

```scheme
(icar (iq a b c))       =>  a        (icdr (iq a b c))     =>  (b c)
(icar (iq (a) b c d))   =>  (a)	     (icdr (iq (a) b c d)) =>  (b c d)
(icar (ipair 1 2))      =>  1	     (icdr (ipair 1 2))    =>  2
(icar '())              =>  *error*  (icdr '())            =>  *error*
```

## `(icaar ipair)`
## `(icadr ipair)`
...

## `(icdddar ipair)`
## `(icddddr ipair)

These procedures are compositions of icar and icdr, where for example
icaddr could be defined by

```scheme
(define icaddr (lambda (x) (icar (icdr (icdr x)))))
```

Arbitrary compositions, up to four deep, are provided. There are
twenty-eight of these procedures in all.

## `(ilist-ref ilist i)`

Returns the ith element of ilist. (This is the same as the icar of
(idrop ilist i).) It is an error if i >= n, where n is the length of
ilist.

```scheme
(ilist-ref (iq a b c d) 2) => c
```

## `(ifirst ipair)`
## `(isecond ipair)`
## `(ithird ipair)`
## `(ifourth ipair)`
## `(ififth ipair)`
## `(isixth ipair)`
## `(iseventh ipair)`
## `(ieighth ipair)`
## `(ininth ipair)`
## `(itenth ipair)`

Synonyms for car, cadr, caddr, ...

```scheme
(ithird '(a b c d e)) => c
```

## `(icar+icdr ipair)`

The fundamental ipair deconstructor:

```scheme
(lambda (p) (values (icar p) (icdr p)))
```

This can, of course, be implemented more efficiently by a compiler.

## `(itake x i)`
## `(idrop x i)`
## `(ilist-tail x i)`

`itake` returns the first i elements of ilist x.

`idrop` returns all but the first i elements of ilist x.

`ilist-tail` is either the same procedure as idrop or else a procedure
with the same behavior.

``scheme
(itake (iq a b c d e)  2) => (a b)
(idrop (iq a b c d e)  2) => (c d e)
```

x may be any value — a proper or dotted ilist:

```scheme
(itake (ipair 1 (ipair 2 (ipair 3 'd)))    => (1 2)
(idrop (ipair 1 (ipair 2 (ipair 3 'd))) 2) => (3 . d)
(itake (ipair 1 (ipair 2 (ipair 3 'd))) 3) => (1 2 3)
(idrop (ipair 1 (ipair 2 (ipair 3 'd))) 3) => d
```

For a legal i, itake and idrop partition the ilist in a manner which
can be inverted with iappend:

```scheme
(iappend (itake x i) (idrop x i)) = x
```

idrop is exactly equivalent to performing i icdr operations on x; the returned value shares a common tail with x.

## `(itake-right dilist i)`
## `(idrop-right dilist i)`

itake-right returns the last i elements of dilist.
idrop-right returns all but the last i elements of dilist.

```scheme
(itake-right (iq a b c d e) 2) => (d e)
(idrop-right (iq a b c d e) 2) => (a b c)
```

The returned ilist may share a common tail with the argument ilist.

dilist may be any ilist, either proper or dotted:

```scheme
(itake-right (iq ipair 1 (ipair 2 (ipair 3 'd))) 2) => (2 3 . d)
(idrop-right (ipair 1 (ipair 2 (ipair 3 'd))) 2)    => (1)
(itake-right (ipair 1 (ipair 2 (ipair 3 'd))) 0)    => d
(idrop-right (ipair 1 (ipair 2 (ipair 3 'd))) 0)    => (1 2 3)
```

For a legal i, itake-right and idrop-right partition the ilist in a
manner which can be inverted with iappend:

```scheme
(iappend (itake dilist i) (idrop dilist i)) = dilist
```

itake-right's return value is guaranteed to share a common tail with
dilist.

## `(isplit-at  x i)`

isplit-at splits the ilist x at index i, returning an ilist of the
first i elements, and the remaining tail. It is equivalent to

```scheme
(values (itake x i) (idrop x i))
```

## `(ilast ipair)`
## `(last-ipair ipair)`

Returns the last element of the non-empty, possibly dotted, ilist
ipair. last-ipair returns the last ipair in the non-empty ilist pair.

```scheme
(ilast (iq a b c))      => c
(last-ipair (iq a b c)) => (c)
```

## `(ilength  ilist)`

Returns the length of its argument. It is an error to pass a value to
ilength which is not a proper ilist (()-terminated).

The length of a proper ilist is a non-negative integer n such that
icdr applied n times to the ilist produces the empty list.

## `(iappend  ilist1 ...)`

Returns an ilist consisting of the elements of ilist1 followed by the
elements of the other ilist parameters.

```scheme
(iappend (iq x) (iq y))        =>  (x y)
(iappend (iq a) (iq b c d))    =>  (a b c d)
(iappend (iq a (b)) (iq (c)))  =>  (a (b) (c))
```

The resulting ilist is always newly allocated, except that it shares
structure with the final ilisti argument. This last argument may be
any value at all; an improper ilist results if it is not a proper
ilist. All other arguments must be proper ilists.

```scheme
(iappend (iq a b) (ipair 'c 'd))  =>  (a b c . d)
(iappend '() 'a)           =>  a
(iappend (iq x y))         =>  (x y)
(iappend)                  =>  ()
```

## `(iconcatenate  ilist-of-ilists)`

Appends the elements of its argument together. That is, iconcatenate returns

```scheme
(iapply iappend ilist-of-ilists)
```

or, equivalently,

```scheme
(ireduce-right iappend '() ilist-of-ilists)
```

Note that some Scheme implementations do not support passing more than
a certain number (e.g., 64) of arguments to an n-ary procedure. In
these implementations, the (iapply iappend ...) idiom would fail when
applied to long lists, but iconcatenate would continue to function
properly.

As with iappend, the last element of the input list may be any value at all.

## `(ireverse  ilist)`

Returns a newly allocated ilist consisting of the elements of ilist in reverse order.

```scheme
(ireverse (iq a b c)) =>  (c b a)
(ireverse (iq a (b c) d (e (f))))
        =>  ((e (f)) d (b c) a)
```

## `(iappend-reverse  rev-head tail)`

iappend-reverse returns (iappend (ireverse rev-head) tail). It is
provided because it is a common operation — a common list-processing
style calls for this exact operation to transfer values accumulated in
reverse order onto the front of another ilist, and because the
implementation is significantly more efficient than the simple
composition it replaces. (But note that this pattern of iterative
computation followed by a reverse can frequently be rewritten as a
recursion, dispensing with the reverse and iappend-reverse steps, and
shifting temporary, intermediate storage from the heap to the stack,
which is typically a win for reasons of cache locality and eager
storage reclamation.)

## `(izip ilist1 ilist2 ...)`

```scheme
(lambda ilists (iapply imap ilist ilists))
```

If izip is passed n ilists, it returns an ilist as long as the
shortest of these ilists, each element of which is an n-element ilist
comprised of the corresponding elements from the parameter ilists.

```scheme
(izip (iq one two three)
  (iq 1 2 3)
  (iq odd even odd even odd even odd even))
   ;; => ((one 1 odd) (two 2 even) (three 3 odd))

(izip (iq 1 2 3)) => ((1) (2) (3))
```

## `(iunzip1 ilist)`
## `(iunzip2 ilist)`
## `(iunzip3 ilist)`
## `(iunzip4 ilist)`
## `(iunzip5 ilist)`

iunzip1 takes an ilist of ilists, where every ilist must contain at
least one element, and returns an ilist containing the initial element
of each such ilist. That is, it returns (imap icar ilists). iunzip2
takes an ilist of ilists, where every ilist must contain at least two
elements, and returns two values: an ilist of the first elements, and
an ilist of the second elements. iunzip3 does the same for the first
three elements of the ilists, and so forth.

```scheme
(iunzip2 (iq (1 one) (2 two) (3 three))) =>
  (1 2 3)
  (one two three)
```

## `(icount pred ilist1 ilist2 ...)`

pred is a procedure taking as many arguments as there are ilists and
returning a single value. It is applied element-wise to the elements
of the ilists, and a count is tallied of the number of elements that
produce a true value. This count is returned. count is "iterative" in
that it is guaranteed to apply pred to the ilist elements in a
left-to-right order. The counting stops when the shortest ilist
expires.

```scheme
(count even? (iq 3 1 4 1 5 9 2 5 6)) => 3
(count < (iq 1 2 4 8) (iq 2 4 6 8 10 12 14 16)) => 3
```

## `(ifold kons knil ilist1 ilist2 ...)`

The fundamental ilist iterator.

First, consider the single ilist-parameter case. If ilist1 = (e1 e2 ... en), then this procedure returns

```scheme
(kons en ... (kons e2 (kons e1 knil)) ... )
```

That is, it obeys the (tail) recursion

```scheme
(ifold kons knil lis) = (ifold kons (kons (icar lis) knil) (icdr lis))
(ifold kons knil '()) = knil
```

Examples:

```scheme
(ifold + 0 lis)			; Add up the elements of LIS.

(ifold ipair '() lis)		; Reverse LIS.

(ifold ipair tail rev-head)	; See APPEND-REVERSE.

;; How many symbols in LIS?
(ifold (lambda (x count) (if (symbol? x) (+ count 1) count))
       0
       lis)

;; Length of the longest string in LIS:
(ifold (lambda (s max-len) (max max-len (string-length s)))
       0
       lis)
```

If n ilist arguments are provided, then the kons function must take
n+1 parameters: one element from each ilist, and the "seed" or fold
state, which is initially knil. The fold operation terminates when the
shortest ilist runs out of values:

```scheme
(ifold ipair* '() (iq a b c) (iq 1 2 3 4 5)) => (c 3 b 2 a 1)
```

## `(ifold-right kons knil ilist1 ilist2 ...)`

The fundamental ilist recursion operator.

First, consider the single ilist-parameter case. If ilist1 = (e1 e2 ... en), then this procedure returns

```scheme
(kons e1 (kons e2 ... (kons en knil)))
```

That is, it obeys the recursion

```scheme
(ifold-right kons knil lis) = (kons (icar lis) (ifold-right kons knil (icdr lis)))
(ifold-right kons knil '()) = knil
```

Examples:

```scheme
(ifold-right ipair '() lis)		; Copy LIS.

;; Filter the even numbers out of LIS.
(ifold-right (lambda (x l) (if (even? x) (ipair x l) l)) '() lis))
```

If n ilist arguments are provided, then the kons procedure must take
n+1 parameters: one element from each ilist, and the "seed" or fold
state, which is initially knil. The fold operation terminates when the
shortest ilist runs out of values:

```scheme
(ifold-right ipair* '() (iq a b c) (iq 1 2 3 4 5)) => (a 1 b 2 c 3)
```

## `(ipair-fold kons knil ilist1 ilist2 ...)`

Analogous to fold, but kons is applied to successive sub-ilists of the
ilists, rather than successive elements — that is, kons is applied to
the ipairs making up the lists, giving this (tail) recursion:

```scheme
(ipair-fold kons knil lis) = (let ((tail (icdr lis)))
                               (ipair-fold kons (kons lis knil) tail))
(ipair-fold kons knil '()) = knil
```

Example:

```scheme
(ipair-fold ipair '() (iq a b c)) => ((c) (b c) (a b c))
```

## `(ipair-fold-right kons knil ilist1 ilist2 ...)`

Holds the same relationship with ifold-right that ipair-fold holds
with ifold. Obeys the recursion

```scheme
    (ipair-fold-right kons knil lis) =
        (kons lis (ipair-fold-right kons knil (icdr lis)))
    (ipair-fold-right kons knil '()) = knil
```

Example:

```scheme
(ipair-fold-right ipair '() (iq a b c)) => ((a b c) (b c) (c))
```

## `(ireduce f ridentity ilist)`

ireduce is a variant of ifold.

ridentity should be a "right identity" of the procedure f — that is,
for any value x acceptable to f,

```scheme
(f x ridentity) = x
```

ireduce has the following definition:

If ilist = (), return ridentity;

Otherwise, return (ifold f (icar ilist) (icdr ilist)).

...in other words, we compute (ifold f ridentity ilist).

Note that ridentity is used only in the empty-list case. You typically
use ireduce when applying f is expensive and you'd like to avoid the
extra application incurred when ifold applies f to the head of ilist
and the identity value, redundantly producing the same value passed in
to f. For example, if f involves searching a file directory or
performing a database query, this can be significant. In general,
however, ifold is useful in many contexts where ireduce is not
(consider the examples given in the ifold definition — only one of the
five folds uses a function with a right identity. The other four may
not be performed with ireduce).

```scheme
;; take the max of an ilist of non-negative integers.
(ireduce max 0 nums) ; i.e., (iapply max 0 nums)
```

## `(ireduce-right f ridentity ilist)`

ireduce-right is the fold-right variant of ireduce. It obeys the following definition:

```scheme
(ireduce-right f ridentity '()) = ridentity
(ireduce-right f ridentity (iq e1)) = (f e1 ridentity) = e1
(ireduce-right f ridentity (iq e1 e2 ...)) =
  (f e1 (ireduce f ridentity (e2 ...)))
```

...in other words, we compute (ifold-right f ridentity ilist).

```scheme
;; Append a bunch of ilists together.
;; I.e., (iapply iappend ilist-of-ilists)
(ireduce-right iappend '() ilist-of-ilists)
```

## `(iunfold p f g seed [tail-gen])`

iunfold is best described by its basic recursion:

```scheme
    (iunfold p f g seed) =
        (if (p seed) (tail-gen seed)
            (ipair (f seed)
                  (iunfold p f g (g seed))))
```

- p, Determines when to stop unfolding.
- f, Maps each seed value to the corresponding ilist element.
- g, Maps each seed value to next seed value.
- seed, The "state" value for the unfold.
- tail-gen, Creates the tail of the ilist; defaults to (lambda (x) '())

In other words, we use g to generate a sequence of seed values

```scheme
seed, g(seed), g2(seed), g3(seed), ...
```

These seed values are mapped to ilist elements by f, producing the
elements of the result ilist in a left-to-right order. P says when to
stop.

iunfold is the fundamental recursive ilist constructor, just as
ifold-right is the fundamental recursive ilist consumer. While iunfold
may seem a bit abstract to novice functional programmers, it can be
used in a number of ways:

```scheme
    ;; Ilist of squares: 1^2 ... 10^2
    (iunfold (lambda (x) (> x 10))
            (lambda (x) (* x x))
    	(lambda (x) (+ x 1))
    	1)

    (iunfold null-ilist? icar icdr lis) ; Copy a proper ilist.

    ;; Read current input port into an ilist of values.
    (iunfold eof-object? values (lambda (x) (read)) (read))

    ;; Copy a possibly non-proper ilist:
    (iunfold not-ipair? icar icdr lis
                  values)

    ;; Append HEAD onto TAIL:
    (iunfold null-ilist? icar icdr head
                  (lambda (x) tail))
```

Interested functional programmers may enjoy noting that ifold-right
and iunfold are in some sense inverses. That is, given operations
knull?, kar, kdr, kons, and knil satisfying

```scheme
(kons (kar x) (kdr x)) = x and (knull? knil) = #t
```

then

```scheme
(ifold-right kons knil (iunfold knull? kar kdr x)) = x
```

and

```scheme
(iunfold knull? kar kdr (ifold-right kons knil x)) = x
```

This combinator sometimes is called an "anamorphism;" when an explicit
tail-gen procedure is supplied, it is called an "apomorphism."

## `(iunfold-right p f g seed [tail])`

iunfold-right constructs an ilist with the following loop:

```scheme
    (let lp ((seed seed) (lis tail))
      (if (p seed) lis
          (lp (g seed)
              (ipair (f seed) lis))))

    p
        Determines when to stop unfolding.
    f
        Maps each seed value to the corresponding ilist element.
    g
        Maps each seed value to next seed value.
    seed
        The "state" value for the unfold.
    tail
        ilist terminator; defaults to '().
```

In other words, we use g to generate a sequence of seed values

```scheme
    seed, g(seed), g2(seed), g3(seed), ...
```

These seed values are mapped to ilist elements by f, producing the
elements of the result ilist in a right-to-left order. P says when to
stop.

iunfold-right is the fundamental iterative ilist constructor, just as
ifold is the fundamental iterative ilist consumer. While iunfold-right
may seem a bit abstract to novice functional programmers, it can be
used in a number of ways:

```scheme
    ;; Ilist of squares: 1^2 ... 10^2
    (iunfold-right zero?
                  (lambda (x) (* x x))
                  (lambda (x) (- x 1))
                  10)

    ;; Reverse a proper ilist.
    (iunfold-right null-ilist? icar icdr lis)

    ;; Read current input port into an ilist of values.
    (iunfold-right eof-object? values (lambda (x) (read)) (read))

    ;; (iappend-reverse rev-head tail)
    (iunfold-right null-ilist? icar icdr rev-head tail)
```

Interested functional programmers may enjoy noting that ifold and iunfold-right are in some sense inverses. That is, given operations knull?, kar, kdr, kons, and knil satisfying

```scheme
(kons (kar x) (kdr x)) = x and (knull? knil) = #t
```

then

```scheme
(ifold kons knil (iunfold-right knull? kar kdr x)) = x
```

and

```scheme
(iunfold-right knull? kar kdr (ifold kons knil x)) = x.
```

This combinator presumably has some pretentious mathematical name;
interested readers are invited to communicate it to the author.

## `(imap proc ilist1 ilist2 ...)`

proc is a procedure taking as many arguments as there are ilist arguments and returning a single value. imap applies proc element-wise to the elements of the ilists and returns an ilist of the results, in order. The dynamic order in which proc is applied to the elements of the ilists is unspecified.

```scheme
    (imap icadr (iq (a b) (d e) (g h))) =>  (b e h)

    (imap (lambda (n) (expt n n))
         (iq 1 2 3 4 5))
        =>  (1 4 27 256 3125)

    (imap + (iq 1 2 3) (iq 4 5 6)) =>  (5 7 9)

    (let ((count 0))
      (imap (lambda (ignored)
             (set! count (+ count 1))
             count)
           (iq a b))) =>  (1 2) or (2 1)
```

## `(ifor-each proc ilist1 ilist2 ...)`

The arguments to ifor-each are like the arguments to imap, but
ifor-each calls proc for its side effects rather than for its
values. Unlike imap, ifor-each is guaranteed to call proc on the
elements of the ilists in order from the first element(s) to the last,
and the value returned by ifor-each is unspecified.

```scheme
    (let ((v (make-vector 5)))
      (ifor-each (lambda (i)
                  (vector-set! v i (* i i)))
                (iq 0 1 2 3 4))
      v)  =>  #(0 1 4 9 16)
```

## `(iappend-map  f ilist1 ilist2 ...)`

Equivalent to

```scheme
(iapply iappend (imap f ilist1 ilist2 ...))
```

and

```scheme
(iapply iappend (imap f ilist1 ilist2 ...))
```

Map f over the elements of the ilists, just as in the imap
function. However, the results of the applications are appended
together (using iappend) to make the final result.

The dynamic order in which the various applications of f are made is
not specified.

Example:

```scheme
(iappend-map (lambda (x) (ilist x (- x))) (iq 1 3 8))
  ;; => (1 -1 3 -3 8 -8)
```

## `(imap-in-order f ilist1 ilist2 ...)`

A variant of the imap procedure that guarantees to apply f across the
elements of the ilisti arguments in a left-to-right order. This is
useful for mapping procedures that both have side effects and return
useful values.

## `(ipair-for-each f ilist1 ilist2 ...)`

Like ifor-each, but f is applied to successive sub-ilists of the
argument ilists. That is, f is applied to the cells of the ilists,
rather than the ilists' elements. These applications occur in
left-to-right order.

```scheme
    (ipair-for-each (lambda (ipair) (display ipair) (newline)) (iq a b c)) ==>
        (a b c)
        (b c)
        (c)
```

## `(ifilter-map f ilist1 ilist2 ...)`

Like imap, but only true values are saved.

```scheme
    (ifilter-map (lambda (x) (and (number? x) (* x x))) (iq a 1 b 3 c 7))
        => (1 9 49)
```

The dynamic order in which the various applications of f are made is not specified.

## `(ifilter pred ilist)`

Return all the elements of ilist that satisfy predicate pred. The
ilist is not disordered — elements that appear in the result ilist
occur in the same order as they occur in the argument ilist. The
returned ilist may share a common tail with the argument ilist. The
dynamic order in which the various applications of pred are made is
not specified.

```scheme
(ifilter even? (iq 0 7 8 8 43 -4)) => (0 8 8 -4)
```

## `(ipartition pred ilist)`

Partitions the elements of ilist with predicate pred, and returns two
values: the ilist of in-elements and the ilist of out-elements. The
ilist is not disordered — elements occur in the result ilists in the
same order as they occur in the argument ilist. The dynamic order in
which the various applications of pred are made is not specified. One
of the returned ilists may share a common tail with the argument
ilist.

```scheme
    (ipartition symbol? (iq one 2 3 four five 6)) =>
        (one four five)
        (2 3 6)
```

## `(iremove pred ilist)`

Returns ilist without the elements that satisfy predicate pred:

```scheme
(lambda (pred ilist) (ifilter (lambda (x) (not (pred x))) ilist))
```

The ilist is not disordered — elements that appear in the result ilist
occur in the same order as they occur in the argument ilist. The
returned ilist may share a common tail with the argument ilist. The
dynamic order in which the various applications of pred are made is
not specified.

```scheme
(iremove even? (iq 0 7 8 8 43 -4)) => (7 43)
```

## `(ifind pred ilist)`

Return the first element of ilist that satisfies predicate pred; false if no element does.

```scheme
(ifind even? (iq 3 1 4 1 5 9)) => 4
```

Note that ifind has an ambiguity in its lookup semantics — if ifind
returns #f, you cannot tell (in general) if it found a #f element that
satisfied pred, or if it did not find any element at all. In many
situations, this ambiguity cannot arise — either the ilist being
searched is known not to contain any #f elements, or the ilist is
guaranteed to have an element satisfying pred. However, in cases where
this ambiguity can arise, you should use ifind-tail instead of ifind —
ifind-tail has no such ambiguity:

```scheme
    (cond ((ifind-tail pred lis) => (lambda (ipair) ...)) ; Handle (icar ipair)
          (else ...)) ; Search failed.
```

## `(ifind-tail pred ilist)`

Return the first ipair of ilist whose icar satisfies pred. If no ipair
does, return false.

ifind-tail can be viewed as a general-predicate variant of the imember
function.

Examples:

```scheme
    (ifind-tail even? (iq 3 1 37 -8 -5 0 0)) => (-8 -5 0 0)
    (ifind-tail even? (iq 3 1 37 -5)) => #f

    ;; IMEMBER X LIS:
    (ifind-tail (lambda (elt) (equal? x elt)) lis)
```

iqfind-tail is essentially idrop-while, where the sense of the
predicate is inverted: Ifind-tail searches until it finds an element
satisfying the predicate; idrop-while searches until it finds an
element that doesn't satisfy the predicate.

## `(itake-while  pred ilist)`

Returns the longest initial prefix of ilist whose elements all satisfy the predicate pred.

```scheme
(itake-while even? (iq 2 18 3 10 22 9)) => (2 18)
```

## `(idrop-while pred ilist)`

idrops the longest initial prefix of ilist whose elements all satisfy
the predicate pred, and returns the rest of the ilist.

```scheme
(idrop-while even? (iq 2 18 3 10 22 9)) => (3 10 22 9)
```

## `(ispan pred ilist)`
## `(ibreak  pred ilist)`

ispan splits the ilist into the longest initial prefix whose elements
all satisfy pred, and the remaining tail. ibreak inverts the sense of
the predicate: the tail commences with the first element of the input
ilist that satisfies the predicate.

In other words: ispan finds the initial span of elements satisfying
pred, and ibreak breaks the ilist at the first element satisfying
pred.

ispan is equivalent to

```scheme
    (values (itake-while pred ilist)
            (idrop-while pred ilist))

    (ispan even? (iq 2 18 3 10 22 9)) =>
      (2 18)
      (3 10 22 9)

    (ibreak even? (iq 3 1 4 1 5 9)) =>
      (3 1)
      (4 1 5 9)
```

## `(iany pred ilist1 ilist2 ...)`

Applies the predicate across the ilists, returning true if the
predicate returns true on any application.

If there are n ilist arguments ilist1 ... ilistn, then pred must be a
procedure taking n arguments and returning a boolean result.

iany applies pred to the first elements of the ilisti parameters. If
this application returns a true value, iany immediately returns that
value. Otherwise, it iterates, applying pred to the second elements of
the ilisti parameters, then the third, and so forth. The iteration
stops when a true value is produced or one of the ilists runs out of
values; in the latter case, iany returns #f. The application of pred
to the last element of the ilists is a tail call.

Note the difference between ifind and iany — ifind returns the element
that satisfied the predicate; iany returns the true value that the
predicate produced.

Like ievery, iany's name does not end with a question mark — this is
to indicate that it does not return a simple boolean (#t or #f), but a
general value.

```scheme
    (iany integer? (iq a 3 b 2.7))   => #t
    (iany integer? (iq a 3.1 b 2.7)) => #f
    (iany < (iq 3 1 4 1 5)
           (iq 2 7 1 8 2)) => #t
```

## `(ievery pred ilist1 ilist2 ...)`

Applies the predicate across the ilists, returning true if the
predicate returns true on every application.

If there are n ilist arguments ilist1 ... ilistn, then pred must be a
procedure taking n arguments and returning a boolean result.

ievery applies pred to the first elements of the ilisti parameters. If
this application returns false, ievery immediately returns
false. Otherwise, it iterates, applying pred to the second elements of
the ilisti parameters, then the third, and so forth. The iteration
stops when a false value is produced or one of the ilists runs out of
values. In the latter case, ievery returns the true value produced by
its final application of pred. The application of pred to the last
element of the ilists is a tail call.

If one of the ilisti has no elements, ievery simply returns #t.

Like iany, ievery's name does not end with a question mark — this is
to indicate that it does not return a simple boolean (#t or #f), but a
general value.

## `(ilist-index pred ilist1 ilist2 ...)`

Return the index of the leftmost element that satisfies pred.

If there are n ilist arguments ilist1 ... ilistn, then pred must be a
function taking n arguments and returning a boolean result.

ilist-index applies pred to the first elements of the ilisti
parameters. If this application returns true, ilist-index immediately
returns zero. Otherwise, it iterates, applying pred to the second
elements of the ilisti parameters, then the third, and so forth. When
it finds a tuple of ilist elements that cause pred to return true, it
stops and returns the zero-based index of that position in the ilists.

The iteration stops when one of the ilists runs out of values; in this
case, ilist-index returns #f.

```scheme
    (ilist-index even? (iq 3 1 4 1 5 9)) => 2
    (ilist-index < (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)) => 1
    (ilist-index = (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)) => #f
```

## `(imember x ilist [=])`
## `(imemq x ilist)`
## `(imemv x ilist)

These procedures return the first sub-ilist of ilist whose icar is x,
where the sub-ilists of ilist are the non-empty ilists returned by
(idrop ilist i) for i less than the length of ilist. If x does not
occur in ilist, then #f is returned. imemq uses eq? to compare x with
the elements of ilist, while imemv uses eqv?, and imember uses equal?.

```
        (imemq 'a (iq a b c))           =>  (a b c)
        (imemq 'b (iq a b c))           =>  (b c)
        (imemq 'a (iq b c d))           =>  #f
        (imemq (list 'a)
                (ilist 'b '(a) 'c))     =>  #f
        (imember (list 'a)
                (ilist 'b '(a) 'c)))    =>  ((a) c)
        (imemq 101 (iq 100 101 102))    =>  *unspecified*
        (imemv 101 (iq 100 101 102))    =>  (101 102)
```

The comparison procedure is used to compare the elements ei of ilist
to the key x in this way:

```scheme
(= x ei) ; ilist is (E1 ... En)
```

That is, the first argument is always x, and the second argument is
one of the ilist elements. Thus one can reliably find the first
element of ilist that is greater than five with (imember 5 ilist <)

Note that fully general ilist searching may be performed with the
ifind-tail and ifind procedures, e.g.

```scheme
(ifind-tail even? ilist) ; Find the first elt with an even key.
```

## `(idelete  x ilist [=])`

idelete uses the comparison procedure =, which defaults to equal?, to
find all elements of ilist that are equal to x, and deletes them from
ilist. The dynamic order in which the various applications of = are
made is not specified.

The ilist is not disordered — elements that appear in the result ilist
occur in the same order as they occur in the argument ilist. The
result may share a common tail with the argument ilist.

Note that fully general element deletion can be performed with the
iremove procedures, e.g.:

```scheme
;; idelete all the even elements from LIS:
(iremove even? lis)
```

The comparison procedure is used in this way: (= x ei). That is, x is
always the first argument, and an ilist element is always the second
argument. The comparison procedure will be used to compare each
element of ilist exactly once; the order in which it is applied to the
various ei is not specified. Thus, one can reliably remove all the
numbers greater than five from an ilist with (idelete 5 ilist <)

## `(idelete-duplicates  ilist [=])`

idelete-duplicates removes duplicate elements from the ilist
argument. If there are multiple equal elements in the argument ilist,
the result ilist only contains the first or leftmost of these elements
in the result. The order of these surviving elements is the same as in
the original ilist — idelete-duplicates does not disorder the ilist
(hence it is useful for "cleaning up" immutable association lists).

The = parameter is used to compare the elements of the ilist; it
defaults to equal?. If x comes before y in ilist, then the comparison
is performed (= x y). The comparison procedure will be used to compare
each pair of elements in ilist no more than once; the order in which
it is applied to the various pairs is not specified.

Implementations of idelete-duplicates are allowed to share common
tails between argument and result ilists — for example, if the ilist
argument contains only unique elements, it may simply return exactly
this ilist.

Be aware that, in general, idelete-duplicates runs in time O(n2) for
n-element ilists. Uniquifying long ilists can be accomplished in O(n
lg n) time by sorting the ilist to bring equal elements together, then
using a linear-time algorithm to remove equal elements. Alternatively,
one can use algorithms based on element-marking, with linear-time
results.

```scheme
(idelete-duplicates (iq a b a c a b c z)) => (a b c z)

;; Clean up an ialist:
(idelete-duplicates (iq (a . 3) (b . 7) (a . 9) (c . 1))
  (lambda (x y) (eq? (icar x) (icar y))))
;; => ((a . 3) (b . 7) (c . 1))
```

## `(ialist-cons key datum ialist)`

```scheme
(lambda (key datum ialist) (ipair (ipair key datum) ialist))
```

Construct a new ialist entry mapping key -> datum onto ialist.

## `(ialist-delete  key ialist [=])`

ialist-delete deletes all associations from ialist with the given key,
using key-comparison procedure =, which defaults to equal?. The
dynamic order in which the various applications of = are made is not
specified.

Return values may share common tails with the ialist argument. The
ialist is not disordered — elements that appear in the result ialist
occur in the same order as they occur in the argument ialist.

The comparison procedure is used to compare the element keys ki of
ialist's entries to the key parameter in this way: (= key ki). Thus,
one can reliably remove all entries of ialist whose key is greater
than five with (ialist-delete 5 ialist <)


## `(replace-icar ipair object)`

This procedure returns an ipair with object in the icar field and the
icdr of ipair in the icdr field.

## `(replace-icdr ipair object)`

This procedure returns an ipair with object in the icdr field and the
icar of ipair in the icar field.

## `(pair->ipair pair)`

## `(ipair->pair ipair)`

These procedures, which are inverses, return an ipair and a pair
respectively that have the same (i)car and (i)cdr fields as the
argument.

## `(list->ilist flist)`

## `(ilist->list dilist)`

These procedures return an ilist and a list respectively that have the
same elements as the argument. The tails of dotted (i)lists are
preserved in the result, which makes the procedures not inverses when
the tail of a dotted ilist is a list or vice versa. The empty list is
converted to itself.

It is an error to apply list->ilist to a circular list.

## `(tree->itree object)`

## `(itree->tree object)`

These procedures walk a tree of pairs or ipairs respectively and make
a deep copy of it, returning an isomorphic tree containing ipairs or
pairs respectively. The result may share structure with the
argument. If the argument is not of the expected type, it is returned.

These procedures are not inverses in the general case. For example, a
pair of ipairs would be converted by tree->itree to an ipair of
ipairs, which if converted by itree->tree would produce a pair of
pairs.

## `(gtree->itree object)`

## `(gtree->tree object)`

These procedures walk a generalized tree consisting of pairs, ipairs,
or a combination of both, and make a deep copy of it, returning an
isomorphic tree containing only ipairs or pairs respectively. The
result may share structure with the argument. If the argument is
neither a pair nor an ipair, it is returned.

## `(iapply procedure object ... ilist)`

The iapply procedure is an analogue of apply whose last argument is an
ilist rather than a list. It is equivalent to (apply procedure object
... (ilist->list ilist)), but may be implemented more efficiently.

## `ipair-comparator`

The ipair-comparator object is a SRFI-114 comparator suitable for
comparing ipairs. Note that it is not a procedure. It compares pairs
using default-comparator on their cars. If the cars are not equal,
that value is returned. If they are equal, default-comparator is used
on their cdrs and that value is returned.

## `ilist-comparator`

The ilist-comparator object is a SRFI-114 comparator suitable for
comparing ilists. Note that it is not a procedure. It compares ilists
lexicographically, as follows:

- The empty ilist compares equal to itself.

- The empty ilist compares less than any non-empty ilist.

- Two non-empty ilists are compared by comparing their icars. If the
  icars are not equal when compared using default-comparator, then the
  result is the result of that comparison. Otherwise, the icdrs are
  compared using ilist-comparator.

## `(make-ilist-comparator comparator)`

The make-ilist-comparator procedure returns a comparator suitable for
comparing ilists using element-comparator to compare the elements.

## `(make-improper-ilist-comparator comparator)`

The make-improper-ilist-comparator procedure returns a comparator that
compares arbitrary objects as follows: the empty list precedes all
ipairs, which precede all other objects. Ipairs are compared as if
with (make-ipair-comparator comparator comparator). All other objects
are compared using comparator.

## `(make-icar-comparator comparator)`

The make-icar-comparator procedure returns a comparator that compares
ipairs on their icars alone using comparator.

## `(make-icdr-comparator comparator)

The make-icdr-comparator procedure returns a comparator that compares
ipairs on their icdrs alone using comparator.
