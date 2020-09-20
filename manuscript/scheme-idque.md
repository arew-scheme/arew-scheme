# `(scheme idque)`

This is based on [SRFI-134](https://srfi.schemers.org/srfi-134/).

This SRFI defines immutable deques. A deque is a double-ended queue, a
sequence which allows elements to be added or removed efficiently from
either end. A structure is immutable when all its operations leave the
structure unchanged. Note that none of the procedures specified here
ends with an exclamation point.

This SRFI describes immutable deques, or ideques. Immutable structures
are sometimes called persistent and are closely related to pure
functional (a.k.a. pure) structures. The availability of immutable
data structures facilitates writing efficient programs in the
pure-functional style. Immutable deques can also be seen as a
bidirectional generalization of immutable lists, and some of the
procedures documented below are most useful in that context. Unlike
the immutable lists of SRFI 116, it is efficient to produce modified
versions of an ideque; unlike the list queues of SRFI 117, it is
possible to efficiently return an updated version of an ideque without
mutating any earlier versions of it.

The specification was designed jointly by Kevin Wortman and John
Cowan. John Cowan is the editor and shepherd. The two-list
implementation was written by John Cowan.

## `(ideque element ...)`

Returns an ideque containing the elements. The first element (if any)
will be at the front of the ideque and the last element (if any) will
be at the back. Takes O(n) time, where n is the number of elements.

## `(ideque-tabulate n proc)`

Invokes the predicate proc on every exact integer from 0 (inclusive)
to n (exclusive). Returns an ideque containing the results in order of
generation. Takes O(n) time.

## `(ideque-unfold stop? mapper successor seed)`

Invokes the predicate stop? on seed. If it returns false, generate the
next result by applying mapper to seed, generate the next seed by
applying successor to seed, and repeat this algorithm with the new
seed. If stop? returns true, return an ideque containing the results
in order of accumulation. Takes O(n) time.

## `(ideque-unfold-right stop? mapper successor seed)`

Invokes the predicate stop? on seed. If it returns false, generate the
next result by applying mapper to seed, generate the next seed by
applying successor to seed, and repeat the algorithm with the new
seed. If stop? returns true, return an ideque containing the results
in reverse order of accumulation. Takes O(n) time.  Predicates

## `(ideque? x)`

Returns #t if x is an ideque, and #f otherwise. Takes O(1) time.

## `(ideque-empty? idaeque)`

Returns #t if ideque contains zero elements, and #f otherwise. Takes
O(1) time.

## `(ideque= elt= ideque ...)`

Determines ideque equality, given an element-equality
procedure. Ideque A equals ideque B if they are of the same length,
and their corresponding elements are equal, as determined by elt=. If
the element-comparison procedure's first argument is from idequei,
then its second argument is from idequei+1, i.e. it is always called
as (elt= a b) for a an element of ideque A, and b an element of ideque
B.

In the n-ary case, every idequei is compared to idequei+1 (as opposed,
for example, to comparing ideque1 to every idequei, for i > 1). If
there are zero or one ideque arguments, ideque= simply returns
true. The name does not end in a question mark for compatibility with
the SRFI-1 procedure list=.

Note that the dynamic order in which the elt= procedure is applied to
pairs of elements is not specified. For example, if ideque= is applied
to three ideques, A, B, and C, it may first completely compare A to B,
then compare B to C, or it may compare the first elements of A and B,
then the first elements of B and C, then the second elements of A and
B, and so forth.

The equality procedure must be consistent with eq?. Note that this
implies that two ideques which are eq? are always ideque=, as well;
implementations may exploit this fact to "short-cut" the
element-by-element comparisons.

## `(ideque-any pred ideque)`

## `(ideque-every pred ideque)`

Invokes pred on the elements of the ideque in order until one call
returns a true/false value, which is then returned. If there are no
elements, returns #f/#t. Takes O(n) time.  Queue operations

## `(ideque-front ideque)`

## `(ideque-back ideque)`

Returns the front/back element of ideque. It is an error for ideque to
be empty. Takes O(1) time.

## `(ideque-remove-front ideque)`

## `(ideque-remove-back ideque)`

Returns an ideque with the front/back element of ideque removed. It is
an error for ideque to be empty. Takes O(1) time.

## `(ideque-add-front ideque obj)`

## `(ideque-add-back ideque obj)`

Returns an ideque with obj pushed to the front/back of ideque. Takes
O(1) time.  Other accessors

## `(ideque-ref ideque n)`

Returns the nth element of ideque. It is an error unless n is less than the length of ideque. Takes O(n) time.

## `(ideque-take ideque n)`

## `(ideque-take-right ideque n)`

Returns an ideque containing the first/last n elements of ideque. It
is an error if n is greater than the length of ideque. Takes O(n)
time.

## `(ideque-drop ideque n)`

## `(ideque-drop-right ideque n)`

Returns an ideque containing all but the first/last n elements of
ideque. It is an error if n is greater than the length of
ideque. Takes O(n) time.

## `(ideque-split-at ideque n)`

Returns two values, the results of (ideque-take ideque n) and
(ideque-drop ideque n) respectively, but may be more efficient. Takes
O(n) time.  The whole ideque

## `(ideque-length ideque)`

Returns the length of ideque as an exact integer. May take O(n) time,
though O(1) is optimal.

## `(ideque-append ideque ...)`

Returns an ideque with the contents of the ideque followed by the
others, or an empty ideque if there are none. Takes O(kn) time, where
k is the number of ideques and n is the number of elements involved,
though O(k log n) is possible.

## `(ideque-reverse ideque)`

Returns an ideque containing the elements of ideque in reverse
order. Takes O(1) time.

## `(ideque-count pred ideque)`

Pred is a procedure taking a single value and returning a single
value. It is applied element-wise to the elements of ideque, and a
count is tallied of the number of elements that produce a true
value. This count is returned. Takes O(n) time. The dynamic order of
calls to pred is unspecified.

## `(ideque-zip ideque1 ideque2 ...)`

Returns an ideque of lists (not ideques) each of which contains the
corresponding elements of ideques in the order specified. Terminates
when all the elements of any of the ideques have been processed. Takes
O(kn) time, where k is the number of ideques and n is the number of
elements in the shortest ideque.

## `(ideque-map proc ideque)`

Applies proc to the elements of ideque and returns an ideque
containing the results in order. The dynamic order of calls to proc is
unspecified. Takes O(n) time.

## `(ideque-filter-map proc ideque)`

Applies proc to the elements of ideque and returns an ideque
containing the true (i.e. non-#f) results in order. The dynamic order
of calls to proc is unspecified. Takes O(n) time.

## `(ideque-for-each proc ideque)`

## `(ideque-for-each-right proc ideque)`

Applies proc to the elements of ideque in forward/reverse order and
returns an unspecified result. Takes O(n) time.

## `(ideque-fold proc nil ideque)`

## `(ideque-fold-right proc nil ideque)`

Invokes proc on the elements of ideque in forward/reverse order,
passing the result of the previous invocation as a second
argument. For the first invocation, nil is used as the second
argument. Returns the result of the last invocation, or nil if there
was no invocation. Takes O(n) time.

## `(ideque-append-map proc ideque)`

Applies proc to the elements of ideque. It is an error if the result
is not a list. Returns an ideque containing the elements of the lists
in order. Takes O(n) time, where n is the number of elements in all
the lists returned.

## `(ideque-filter pred ideque)`

## `(ideque-remove pred ideque)`

Returns an ideque containing the elements of ideque that do/do not
satisfy pred. Takes O(n) time.

## `(ideque-partition proc ideque)`

Returns two values, the results of (ideque-filter pred ideque) and
(ideque-remove pred ideque) respectively, but may be more
efficient. Takes O(n) time.

## `(ideque-find pred ideque [ failure ])`

## `(ideque-find-right pred ideque [ failure ])`

Returns the first/last element of ideque that satisfies pred. If there
is no such element, returns the result of invoking the thunk failure;
the default thunk is (lambda () #f). Takes O(n) time.

## `(ideque-take-while pred ideque)`

## `(ideque-take-while-right pred ideque)`

Returns an ideque containing the longest initial/final prefix of
elements in ideque all of which satisfy pred. Takes O(n) time.

## `(ideque-drop-while pred ideque)`

## `(ideque-drop-while-right pred ideque)`

Returns an ideque which omits the longest initial/final prefix of
elements in ideque all of which satisfy pred, but includes all other
elements of ideque. Takes O(n) time.

## `(ideque-span pred ideque)`

## `(ideque-break pred ideque)`

Returns two values, the initial prefix of the elements of ideque which
do/do not satisfy pred, and the remaining elements. Takes O(n) time.

## `(list->ideque list)`

## `(ideque->list ideque)`

Conversion between ideque and list structures. FIFO order is
preserved, so the front of a list corresponds to the front of an
ideque. Each operation takes O(n) time.

## `(generator->ideque generator)`

## `(ideque->generator ideque)`

Conversion between SRFI 121 generators and ideques. Each operation
takes O(n) time. A generator is a procedure that is called repeatedly
with no arguments to generate consecutive values, and returns an
end-of-file object when it has no more values to return.
