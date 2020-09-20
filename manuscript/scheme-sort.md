# `(scheme sort)`

This is based on [SRFI-132](https://srfi.schemers.org/srfi-132/).

This library describes the API for a full-featured sort toolkit.

## `(list-sorted? < lis)`

## `(vector-sorted? < v [start [ end ] ]`

These procedures return true iff their input list or vector is in
sorted order, as determined by <. Specifically, they return #f iff
there is an adjacent pair ... X Y ... in the input list or vector such
that Y < X in the sense of <. The optional start and end range
arguments restrict vector-sorted? to examining the indicated
subvector.

These procedures are equivalent to the SRFI 95 sorted? procedure when
applied to lists or vectors respectively, except that they do not
accept a key procedure.

## `(list-sort < lis)`

## `(list-stable-sort < lis)`

These procedures do not alter their inputs, but are allowed to return
a value that shares a common tail with a list argument.

The list-stable-sort procedure is equivalent to the R6RS list-sort
procedure. It is also equivalent to the SRFI 95 sort procedure when
applied to lists, except that it does not accept a key procedure.

## `(list-sort! < lis)`

## `(list-stable-sort! < lis)`

These procedures are linear update operators — they are allowed, but
not required, to alter the cons cells of their arguments to produce
their results. They return a sorted list containing the same elements
as lis.

The list-stable-sort! procedure is equivalent to the SRFI 95 sort!
procedure when applied to lists, except that it does not accept a key
procedure.

## `(vector-sort < v [ start [ end ] ])`

## `(vector-stable-sort < v [ start [ end ] ])`

These procedures do not alter their inputs, but allocate a fresh
vector as their result, of length end - start. The vector-stable-sort
procedure with no optional arguments is equivalent to the R6RS
vector-sort procedure. It is also equivalent to the SRFI 95 sort
procedure when applied to vectors, except that it does not accept a
key procedure.

## `(vector-sort! < v [ start [ end ] ])`

## `(vector-stable-sort! < v [ start [ end ] ])`

These procedures sort their data in-place. (But note that
vector-stable-sort! may allocate temporary storage proportional to the
size of the input — there are no known O(n lg n) stable vector sorting
algorithms that run in constant space.) They return an unspecified
value.

The vector-sort! procedure with no optional arguments is equivalent to
the R6RS vector-sort! procedure.

## `(list-merge < lis1 lis2)`

This procedure does not alter its inputs, and is allowed to return a
value that shares a common tail with a list argument.

This procedure is equivalent to the SRFI 95 merge procedure when
applied to lists, except that it does not accept a key procedure.

## `(list-merge! < lis1 lis2)`

This procedure makes only a single, iterative, linear-time pass over
its argument lists, using set-cdr!s to rearrange the cells of the
lists into the list that is returned — it works "in place." Hence, any
cons cell appearing in the result must have originally appeared in an
input. It returns the sorted input.

Additionally, list-merge! is iterative, not recursive — it can operate
on arguments of arbitrary size without requiring an unbounded amount
of stack space. The intent of this iterative-algorithm commitment is
to allow the programmer to be sure that if, for example, list-merge!
is asked to merge two ten-million-element lists, the operation will
complete without performing some extremely (possibly twenty-million)
deep recursion.

This procedure is equivalent to the SRFI 95 merge! procedure when
applied to lists, except that it does not accept a key procedure.

## `(vector-merge < v1 v2 [ start1 [ end1 [ start2 [ end2 ] ] ] ])`

This procedure does not alter its inputs, and returns a newly
allocated vector of length (end1 - start1) + (end2 - start2).

This procedure is equivalent to the SRFI 95 merge procedure when
applied to vectors, except that it does not accept a key procedure.

## `(vector-merge! < to from1 from2 [ start [ start1 [ end1 [ start2 [ end2 ] ] ] ] ])`

This procedure writes its result into vector to, beginning at index
start, for indices less than end, which is defined as start + (end1 -
start1) + (end2 - start2). The target subvector to[start, end) may not
overlap either of the source subvectors from1[start1, end1] and
from2[start2, end2]. It returns an unspecified value.

This procedure is equivalent to the SRFI 95 merge! procedure when
applied to lists, except that it does not accept a key procedure.

## `(list-delete-neighbor-dups = lis)`

This procedure does not alter its input list, but its result may share
storage with the input list.

## `(list-delete-neighbor-dups! = lis)`

This procedure mutates its input list in order to construct its
result. It makes only a single, iterative, linear-time pass over its
argument, using set-cdr!s to rearrange the cells of the list into the
final result — it works "in place." Hence, any cons cell appearing in
the result must have originally appeared in the input.

## `(vector-delete-neighbor-dups = v [ start [ end ] ])`

This procedure does not alter its input vector, but rather newly
allocates and returns a vector to hold the result.

## `(vector-delete-neighbor-dups! = v [ start [ end ] ])`

This procedure reuses its input vector to hold the answer, packing it
into the index range [start, newend), where newend is the non-negative
exact integer that is returned as its value. The vector is not altered
outside the range [start, newend).

Examples:

```scheme
	(list-delete-neighbor-dups = '(1 1 2 7 7 7 0 -2 -2))
               => (1 2 7 0 -2)

	(vector-delete-neighbor-dups = '#(1 1 2 7 7 7 0 -2 -2))
               => #(1 2 7 0 -2)

	(vector-delete-neighbor-dups < '#(1 1 2 7 7 7 0 -2 -2) 3 7))
               => #(7 0 -2)

;; Result left in v[3,9):
(let ((v (vector 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6)))
  (cons (vector-delete-neighbor-dups! < v 3)
        v))
              => (9 . #(0 0 0 1 2 3 4 5 6 4 4 5 5 6 6))
```

## `(vector-find-median < v knil [ mean ])`

This procedure does not alter its input vector, but rather newly
allocates a vector to hold the intermediate result. Runs in O(n) time.

## `(vector-find-median! < v knil [ mean ])`

This procedure reuses its input vector to hold the intermediate
result, leaving it sorted, but is otherwise the same as
vector-find-median. Runs in O(n ln n) time.

## `(vector-select! < v k [ start [ end ] ] )`

This procedure returns the kth smallest element (in the sense of the <
argument) of the region of a vector between start and end. Elements
within the range may be reordered, whereas those outside the range are
left alone. Runs in O(n) time.

## `(vector-separate! < v k [ start [ end ] ] )`

This procedure places the smallest k elements (in the sense of the <
argument) of the region of a vector between start and end into the
first k positions of that range, and the remaining elements into the
remaining positions. Otherwise, the elements are not in any particular
order. Elements outside the range are left alone. Runs in O(n)
time. Returns an unspecified value.
