# `(scheme set)`

This library is based on
[SRFI-113](https://srfi.schemers.org/srfi-113/).

Sets and bags (also known as multisets) are unordered collections that
can contain any Scheme object. Sets enforce the constraint that no two
elements can be the same in the sense of the set's associated equality
predicate; bags do not.

## `(set comparator element ... )`

Returns a newly allocated empty set. The comparator argument is a SRFI
114 comparator, which is used to control and distinguish the elements
of the set. The elements are used to initialize the set.

## `(set-unfold comparator stop? mapper successor seed)`

Create a newly allocated set as if by set using comparator. If the
result of applying the predicate stop? to seed is true, return the
set. Otherwise, apply the procedure mapper to seed. The value that
mapper returns is added to the set. Then get a new seed by applying
the procedure successor to seed, and repeat this algorithm.
Predicates

## `(set? obj)`

Returns #t if obj is a set, and #f otherwise.

## `(set-contains? set element)`

Returns #t if element is a member of set and #f otherwise.

## `(set-empty? set)`

Returns #t if set has no elements and #f otherwise.

## `(set-disjoint? set1 set2)`

Returns #t if set1 and set2 have no elements in common and #f
otherwise.

## `(set-member set element default)`

Returns the element of set that is equal, in the sense of set's
equality predicate, to element. If element is not a member of set,
default is returned.

## `(set-element-comparator set)`

Returns the comparator used to compare the elements of set.

## `(set-adjoin set element ...)`

The set-adjoin procedure returns a newly allocated set that uses the
same comparator as set and contains all the values of set, and in
addition each element unless it is already equal (in the sense of the
comparator) to one of the existing or newly added members. It is an
error to add an element to set that does not return #t when passed to
the type test procedure of the comparator.

## `(set-adjoin! set element ...)`

The set-adjoin! procedure is the same as set-adjoin, except that it is
permitted to mutate and return the set argument rather than allocating
a new set.

## `(set-replace set element)`

The set-replace procedure returns a newly allocated set that uses the
same comparator as set and contains all the values of set except as
follows: If element is equal (in the sense of set's comparator) to an
existing member of set, then that member is omitted and replaced by
element. If there is no such element in set, then set is returned
unchanged.

## `(set-replace! set element)`

The set-replace! procedure is the same asset-replace, except that it
is permitted to mutate and return the set argument rather than
allocating a new set.

## `(set-delete set element ...)`

## `(set-delete! set element ...)`

## `(set-delete-all set element-list)`

## `(set-delete-all! set element-list)`

The set-delete procedure returns a newly allocated set containing all
the values of set except for any that are equal (in the sense of set's
comparator) to one or more of the elements. Any element that is not
equal to some member of the set is ignored.

The set-delete! procedure is the same as set-delete, except that it is
permitted to mutate and return the set argument rather than allocating
a new set.

The set-delete-all and set-delete-all! procedures are the same as
set-delete and set-delete!, except that they accept a single argument
which is a list of elements to be deleted.

## `(set-search! set element failure success)`

The set is searched for element. If it is not found, then the failure
procedure is tail-called with two continuation arguments, insert and
ignore, and is expected to tail-call one of them. If element is found,
then the success procedure is tail-called with the matching element of
set and two continuations, update and remove, and is expected to
tail-call one of them.

The effects of the continuations are as follows (where obj is any
Scheme object):

- Invoking (insert obj) causes element to be inserted into set.

- Invoking (ignore obj) causes set to remain unchanged.

- Invoking (update new-element obj) causes new-element to be inserted
  into set in place of element.

- Invoking (remove obj) causes the matching element of set to be
  removed from it.

In all cases, two values are returned: the possibly updated set and
obj.

## `(set-size set)`

Returns the number of elements in set as an exact integer.

## `(set-find predicate set failure)`

Returns an arbitrarily chosen element of set that satisfies predicate,
or the result of invoking failure with no arguments if there is none.

## `(set-count predicate set)`

Returns the number of elements of set that satisfy predicate as an
exact integer.

## `(set-any? predicate set)`

Returns #t if any element of set satisfies predicate, or #f
otherwise. Note that this differs from the SRFI 1 analogue because it
does not return an element of the set.

## `(set-every? predicate set)`

Returns #t if every element of set satisfies predicate, or #f
otherwise. Note that this differs from the SRFI 1 analogue because it
does not return an element of the set.

## `(set-map comparator proc set)`

Applies proc to each element of set in arbitrary order and returns a
newly allocated set, created as if by (set comparator), which contains
the results of the applications. For example:

```scheme
        (set-map string-ci-comparator symbol->string (set eq? 'foo 'bar 'baz))
             => (set string-ci-comparator "foo" "bar" "baz")
```

Note that, when proc defines a mapping that is not 1:1, some of the
mapped objects may be equivalent in the sense of comparator's equality
predicate, and in this case duplicate elements are omitted as in the
set constructor. For example:

```scheme
(set-map (lambda (x) (quotient x 2))
         integer-comparator
         (set integer-comparator 1 2 3 4 5))
 => (set integer-comparator 0 1 2)
```

If the elements are the same in the sense of eqv?, it is unpredictable
which one will be preserved in the result.

## `(set-for-each proc set)`

Applies proc to set in arbitrary order, discarding the returned
values. Returns an unspecified result.

## `(set-fold proc nil set)`

Invokes proc on each member of set in arbitrary order, passing the
result of the previous invocation as a second argument. For the first
invocation, nil is used as the second argument. Returns the result of
the last invocation, or nil if there was no invocation.

## `(set-filter predicate set)`

Returns a newly allocated set with the same comparator as set,
containing just the elements of set that satisfy predicate.

## `(set-filter! predicate set)`

A linear update procedure that returns a set containing just the
elements of set that satisfy predicate.

## `(set-remove predicate set)`

Returns a newly allocated set with the same comparator as set,
containing just the elements of set that do not satisfy predicate.

## `(set-remove! predicate set)`

A linear update procedure that returns a set containing just the
elements of set that do not satisfy predicate.

## `(set-partition predicate set)`

Returns two values: a newly allocated set with the same comparator as
set that contains just the elements of set that satisfy predicate, and
another newly allocated set, also with the same comparator, that
contains just the elements of set that do not satisfy predicate.

## `(set-partition! predicate set)`

A linear update procedure that returns two sets containing the
elements of set that do and do not, respectively, not satisfy
predicate.

## `(set-copy set)`

Returns a newly allocated set containing the elements of set, and
using the same comparator.

## `(set->list set)`

Returns a newly allocated list containing the members of set in
unspecified order.

## `(list->set comparator list)`

Returns a newly allocated set, created as if by set using comparator,
that contains the elements of list. Duplicate elements (in the sense
of the equality predicate) are omitted.

## `(list->set! set list)`

Returns a set that contains the elements of both set and
list. Duplicate elements (in the sense of the equality predicate) are
omitted.

## `(set=? set1 set2 ...)`

Returns #t if each set contains the same elements.

## `(set<? set1 set2 ...)`

Returns #t if each set other than the last is a proper subset of the
following set, and #f otherwise.

## `(set>? set1 set2 ...)`

Returns #t if each set other than the last is a proper superset of the
following set, and #f otherwise.

## `(set<=? set1 set2 ...)`

Returns #t if each set other than the last is a subset of the
following set, and #f otherwise.

## `(set>=? set1 set2 ...)`

Returns #t if each set other than the last is a superset of the
following set, and #f otherwise.

## `(set-union set1 set2 ...)`

## `(set-intersection set1 set2 ...)`

## `(set-difference set1 set2 ...)`

## `(set-xor set1 set2)`

Return a newly allocated set that is the union, intersection,
asymmetric difference, or symmetric difference of the sets. Asymmetric
difference is extended to more than two sets by taking the difference
between the first set and the union of the others. Symmetric
difference is not extended beyond two sets. Elements in the result set
are drawn from the first set in which they appear.

## `(set-union! set1 set2 ...)`

## `(set-intersection! set1 set2 ...)`

## `(set-difference! set1 set2 ...)`

## `(set-xor! set1 set2)`

Linear update procedures returning a set that is the union,
intersection, asymmetric difference, or symmetric difference of the
sets. Asymmetric difference is extended to more than two sets by
taking the difference between the first set and the union of the
others. Symmetric difference is not extended beyond two sets. Elements
in the result set are drawn from the first set in which they appear.

## `(bag comparator element ...)`

## `(bag-unfold ...)`

## `(bag? obj)`

## `(bag-contains? ...)`

## (bag-empty? obj)`

## (bag-disjoint? ...)

## `(bag-member ...)`

## `(bag-element-comparator ...)

## `(bag-adjoin ...)`

## `(bag-adjoin! ...)`

## `(bag-replace ...)`

## `(bag-replace! ...)`

## `(bag-delete ...)`

## `(bag-delete! ...)`

## `(bag-delete-all ...)`

## `(bag-delete-all! ...)`

## `(bag-search! ...)`

## `(bag-size ...)`

## `(bag-find ...)`

## `(bag-count ...)`

## `(bag-any? ...)`

## `(bag-every? ...)`

## `(bag-map ...)`

## `(bag-for-each ...)`

## `(bag-fold ...)`

## `(bag-filter ...)`

## `(bag-remove ...)`

## `(bag-partition ...)`

## `(bag-filter! ...)`

## `(bag-remove! ...)`

## `(bag-partition! ...)`

## `(bag-copy ...)`

## `(bag->list ...)`

## `(list->bag ...)`

## `(list->bag! ...)`

## `(bag=? ...)`

## `(bag<? ...)`

## `(bag>? ...)`

## `(bag<=? ...)`

## `(bag>=? ...)`

## `(bag-union ...)`

## `(bag-intersection ...)`

## `(bag-difference ...)`

## `(bag-xor ...)`

## `(bag-union! ...)`

## `(bag-intersection! ...)`

## `(bag-difference! ...)`

## `(bag-xor! ...)`

## `(bag-sum set1 set2 ... )`

## `(bag-sum! bag1 bag2 ... )`

The bag-sum procedure returns a newly allocated bag containing all the
unique elements in all the bags, such that the count of each unique
element in the result is equal to the sum of the counts of that
element in the arguments. It differs from bag-union by treating
identical elements as potentially distinct rather than attempting to
match them up.

The bag-sum! procedure is equivalent except that it is linear-update.

## `(bag-product n bag)`

## `(bag-product! n bag)`

The bag-product procedure returns a newly allocated bag containing all
the unique elements in bag, where the count of each unique element in
the bag is equal to the count of that element in bag multiplied by n.

The bag-product! procedure is equivalent except that it is
linear-update.

## `(bag-unique-size bag)`

Returns the number of unique elements of bag.

## `(bag-element-count bag element)`

Returns an exact integer representing the number of times that element
appears in bag.

## `(bag-for-each-unique proc bag)`

Applies proc to each unique element of bag in arbitrary order, passing
the element and the number of times it occurs in bag, and discarding
the returned values. Returns an unspecified result.

## `(bag-fold-unique proc nil bag)`

Invokes proc on each unique element of bag in arbitrary order, passing
the number of occurrences as a second argument and the result of the
previous invocation as a third argument. For the first invocation, nil
is used as the third argument. Returns the result of the last
invocation.

## `(bag-increment! bag element count)`

## `(bag-decrement! bag element count)`

Linear update procedures that return a bag with the same elements as
bag, but with the element count of element in bag increased or
decreased by the exact integer count (but not less than zero).

## `(bag->set bag)`

## `(set->bag set)`

## `(set->bag! bag set)`

The bag->set procedure returns a newly allocated set containing the
unique elements (in the sense of the equality predicate) of bag. The
set->bag procedure returns a newly allocated bag containing the
elements of set. The set->bag! procedure returns a bag containing the
elements of both bag and set. In all cases, the comparator of the
result is the same as the comparator of the argument or arguments.

## `(bag->alist bag)`

## `(alist->bag comparator alist)`

The bag->alist procedure returns a newly allocated alist whose keys
are the unique elements of bag and whose values are the number of
occurrences of each element. The alist->bag returning a newly
allocated bag based on comparator, where the keys of alist specify the
elements and the corresponding values of alist specify how many times
they occur.  Comparators

## `set-comparator`

## `bag-comparator`

Note that these comparators do not provide comparison procedures, as
there is no ordering between sets or bags. It is an error to compare
sets or bags with different element comparators.
