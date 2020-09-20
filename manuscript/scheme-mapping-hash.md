# `(scheme mapping hash)`

## `(hashmap comparator [key value] ...)`

Returns a newly allocated hashmap. The comparator argument is used to
control and distinguish the keys of the hashmap. The args alternate
between keys and values and are used to initialize the hashmap. In
particular, the number of args has to be even. Earlier associations
with equal keys take precedence over later arguments.

## `(hashmap-unfold stop? mapper successor seed comparator)`

Create a newly allocated hashmap as if by hashmap using comparator. If
the result of applying the predicate stop? to seed is true, return the
hashmap. Otherwise, apply the procedure mapper to seed. Mapper returns
two values which are added to the hashmap as the key and the value,
respectively. Then get a new seed by applying the procedure successor
to seed, and repeat this algorithm. Associations earlier in the list
take precedence over those that come later.

## `(hashmap? obj)`

Returns #t if obj is a hashmap, and #f otherwise.

## `(hashmap-contains? hashmap key)`

Returns #t if key is the key of an association of hashmap and #f
otherwise.

## `(hashmap-empty? hashmap)`

Returns #t if hashmap has no associations and #f otherwise.

## `(hashmap-disjoint? hashmap1 hashmap2)`

Returns #t if hashmap1 and hashmap2 have no keys in common and #f
otherwise.

## `(hashmap-ref hashmap key [failure [success]])`

Extracts the value associated to key in the hashmap hashmap, invokes
the procedure success in tail context on it, and returns its result;
if success is not provided, then the value itself is returned. If key
is not contained in hashmap and failure is supplied, then failure is
invoked in tail context on no arguments and its values are
returned. Otherwise, it is an error.

## `(hashmap-ref/default hashmap key default)`

## `(hashmap-key-comparator hashmap)`

Returns the comparator used to compare the keys of the hashmap
hashmap.

## `(hashmap-adjoin hashmap arg ...)`

The hashmap-adjoin procedure returns a newly allocated hashmap that
uses the same comparator as the hashmap hashmap and contains all the
associations of hashmap, and in addition new associations by
processing the arguments from left to right. The args alternate
between keys and values. Whenever there is a previous association for
a key, the previous association prevails and the new association is
skipped. It is an error to add an association to hashmap whose key
that does not return #t when passed to the type test procedure of the
comparator.

## `(hashmap-adjoin! hashmap arg ...)`

The hashmap-adjoin! procedure is the same as hashmap-adjoin, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

## `(hashmap-set hashmap arg ...)`

The hashmap-set procedure returns a newly allocated hashmap that uses
the same comparator as the hashmap hashmap and contains all the
associations of hashmap, and in addition new associations by
processing the arguments from left to right. The args alternate
between keys and values. Whenever there is a previous association for
a key, it is deleted. It is an error to add an association to hashmap
whose key that does not return #t when passed to the type test
procedure of the comparator.

## `(hashmap-set! hashmap arg ...)`

The hashmap-set! procedure is the same as hashmap-set, except that it
is permitted to mutate and return the hashmap argument rather than
allocating a new hashmap.

## `(hashmap-replace hashmap key value)`

The hashmap-replace procedure returns a newly allocated hashmap that
uses the same comparator as the hashmap hashmap and contains all the
associations of hashmap except as follows: If key is equal (in the
sense of hashmap's comparator) to an existing key of hashmap, then the
association for that key is omitted and replaced the association
defined by the pair key and value. If there is no such key in hashmap,
then hashmap is returned unchanged.

## `(hashmap-replace! hashmap key value)`

The hashmap-replace! procedure is the same as hashmap-replace, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

## `(hashmap-delete hashmap key ...)`

## `(hashmap-delete! hashmap key ...)`

## `(hashmap-delete-all hashmap key-list)`

## `(hashmap-delete-all! hashmap key-list)`

The hashmap-delete procedure returns a newly allocated hashmap
containing all the associations of the hashmap hashmap except for any
whose keys are equal (in the sense of hashmap's comparator) to one or
more of the keys. Any key that is not equal to some key of the hashmap
is ignored.

The hashmap-delete! procedure is the same as hashmap-delete, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

The hashmap-delete-all and hashmap-delete-all! procedures are the same
as hashmap-delete and hashmap-delete!, respectively, except that they
accept a single argument which is a list of keys whose associations
are to be deleted.

## `(hashmap-intern hashmap key failure)`

Extracts the value associated to key in the hashmap hashmap, and
returns hashmap and the value as two values. If key is not contained
in hashmap, failure is invoked on no arguments. The procedure then
returns two values, a newly allocated hashmap that uses the same
comparator as the hashmap and contains all the associations of
hashmap, and in addition a new association hashmap key to the result
of invoking failure, and the result of invoking failure.

## `(hashmap-intern! hashmap key failure)`

The hashmap-intern! procedure is the same as hashmap-intern, except
that it is permitted to mutate and return the hashmap argument as its
first value rather than allocating a new hashmap.

## `(hashmap-update hashmap key updater [failure [success]])`

TODO

## `(hashmap-update! hashmap key updater [failure [success]])`

The hashmap-update! procedure is the same as hashmap-update, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

## `(hashmap-update/default hashmap key updater default)`

TODO

## `(hashmap-update!/default hashmap key updater default)`

The hashmap-update!/default procedure is the same as
hashmap-update/default, except that it is permitted to mutate and
return the hashmap argument rather than allocating a new hashmap.

## `(hashmap-pop hashmap [failure])`

The hashmap-pop procedure exported from (srfi 146) chooses the
association with the least key from hashmap and returns three values,
a newly allocated hashmap that uses the same comparator as hashmap and
contains all associations of hashmap except the chosen one, and the
key and the value of the chosen association. If hashmap contains no
association and failure is supplied, then failure is invoked in tail
context on no arguments and its values returned. Otherwise, it is an
error.

## `(hashmap-pop! hashmap [failure])`

The hashmap-pop! procedure is the same as hashmap-pop, except that it
is permitted to mutate and return the hashmap argument rather than
allocating a new hashmap.

## `(hashmap-search hashmap key failure success)`

The hashmap hashmap is searched in order (that is in the order of the
stored keys) for an association with key key. If it is not found, then
the failure procedure is tail-called with two continuation arguments,
insert and ignore, and is expected to tail-call one of them. If an
association with key key is found, then the success procedure is
tail-called with the matching key of hashmap, the associated value,
and two continuations, update and remove, and is expected to tail-call
one of them.

It is an error if the continuation arguments are invoked, but not in
tail position in the failure and success procedures. It is also an
error if the failure and success procedures return to their implicit
continuation without invoking one of their continuation arguments.

The effects of the continuations are as follows (where obj is any
Scheme object):

- Invoking (insert value obj) causes a hashmap to be newly allocated
  that uses the same comparator as the hashmap hashmap and contains
  all the associations of hashmap, and in addition a new association
  hashmap key to value.

- Invoking (ignore obj) has no effects; in particular, no new hashmap
  is allocated (but see below).

- Invoking (update new-key new-value obj) causes a hashmap to be newly
  allocated that uses the same comparator as the hashmap and contains
  all the associations of hashmap, except for the association with key
  key, which is replaced by a new association hashmap new-key to
  new-value.

- Invoking (remove obj) causes a hashmap to be newly allocated that
  uses the same comparator as the hashmap and contains all the
  associations of hashmap, except for the association with key key.

In all cases, two values are returned: the possibly newly allocated
hashmap and obj.

## `(hashmap-search! hashmap key failure success)`

The hashmap-search! procedure is the same as hashmap-search, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

## `(hashmap-size hashmap)`

Returns the number of associations in hashmap as an exact integer.

## `(hashmap-find predicate hashmap failure)`

Returns the association with the least key of the hashmap hashmap
consisting of a key and value as two values such that predicate
returns a true value when invoked with key and value as arguments, or
the result of tail-calling failure with no arguments if there is
none. There are no guarantees how many times and with which keys and
values predicate is invoked.

## `(hashmap-count predicate hashmap)`

Returns the number of associations of the hashmap hashmap that satisfy
predicate (in the sense of hashmap-find) as an exact integer. There
are no guarantees how many times and with which keys and values
predicate is invoked.

## `(hashmap-any? predicate hashmap)`

Returns #t if any association of the hashmap hashmap satisfies
predicate (in the sense of hashmap-find), or #f otherwise. There are
no guarantees how many times and with which keys and values predicate
is invoked.

## `(hashmap-every? predicate hashmap)`

Returns #t if every association of the hashmap hashmap satisfies
predicate (in the sense of hashmap-find), or #f otherwise. There are
no guarantees how many times and with which keys and values predicate
is invoked.

## `(hashmap-keys hashmap)`

Returns a newly allocated list of all the keys in increasing order in
the hashmap hashmap.

## `(hashmap-values hashmap)`

Returns a newly allocated list of all the values in increasing order
of the keys in the hashmap hashmap.

## `(hashmap-entries hashmap)`

Returns two values, a newly allocated list of all the keys in the
hashmap hashmap, and a newly allocated list of all the values in the
hashmap hashmap in increasing order of the keys.

## `(hashmap-map proc comparator hashmap)`

Applies proc, which returns two values, on two arguments, the key and
value of each association of hashmap in increasing order of the keys
and returns a newly allocated hashmap that uses the comparator
comparator, and which contains the results of the applications
inserted as keys and values.

## `(hashmap-map->list proc hashmap)`

Calls proc for every association in increasing order of the keys in
the hashmap hashmap with two arguments: the key of the association and
the value of the association. The values returned by the invocations
of proc are accumulated into a list, which is returned.

## `(hashmap-for-each proc hashmap)`

Invokes proc for every association in the hashmap hashmap in
increasing order of the keys, discarding the returned values, with two
arguments: the key of the association and the value of the
association. Returns an unspecified value.

## `(hashmap-fold proc nil hashmap)`

Invokes proc for each association of the hashmap hashmap in increasing
order of the keys with three arguments: the key of the association,
the value of the association, and an accumulated result of the
previous invocation. For the first invocation, nil is used as the
third argument. Returns the result of the last invocation, or nil if
there was no invocation.

## `(hashmap-filter predicate hashmap)`

Returns a newly allocated hashmap with the same comparator as the
hashmap hashmap, containing just the associations of hashmap that
satisfy predicate (in the sense of hashmap-find).

## `(hashmap-filter! predicate hashmap)`

A linear update procedure that returns a hashmap containing just the
associations of hashmap that satisfy predicate.

## `(hashmap-remove predicate hashmap)`

Returns a newly allocated hashmap with the same comparator as the
hashmap hashmap, containing just the associations of hashmap that do
not satisfy predicate (in the sense of hashmap-find).

## `(hashmap-remove! predicate hashmap)`

A linear update procedure that returns a hashmap containing just the
associations of hashmap that do not satisfy predicate.

## `(hashmap-partition predicate hashmap)`

Returns two values: a newly allocated hashmap with the same comparator
as the hashmap hashmap that contains just the associations of hashmap
that satisfy predicate (in the sense of hashmap-find), and another
newly allocated hashmap, also with the same comparator, that contains
just the associations of hashmap that do not satisfy predicate.

## `(hashmap-partition! predicate hashmap)`

A linear update procedure that returns two hashmaps containing the
associations of hashmap that do and do not, respectively, satisfy
predicate.

## `(hashmap-copy hashmap)`

Returns a newly allocated hashmap containing the associations of the
hashmap hashmap, and using the same comparator.

## `(hashmap->alist hashmap)`

Returns a newly allocated association list containing the associations
of the hashmap in increasing order of the keys. Each association in
the list is a pair whose car is the key and whose cdr is the
associated value.

## `(alist->hashmap comparator alist)`

Returns a newly allocated hashmap, created as if by hashmap using the
comparator comparator, that contains the associations in the list,
which consist of a pair whose car is the key and whose cdr is the
value. Associations earlier in the list take precedence over those
that come later.

## `(alist->hashmap! hashmap alist)`

A linear update procedure that returns a hashmap that contains the
associations of both hashmap and alist. Associations in the hashmap
and those earlier in the list take precedence over those that come
later.

## `(hashmap-union hashmap1 hashmap2 ...)`

## `(hashmap-intersection hashmap1 hashmap2 ...)`

## `(hashmap-difference hashmap1 hashmap2 ...)`

## `(hashmap-xor hashmap1 hashmap2 ...)`

Return a newly allocated hashmap whose set of associations is the
union, intersection, asymmetric difference, or symmetric difference of
the sets of associations of the hashmaps hashmaps. Asymmetric
difference is extended to more than two hashmaps by taking the
difference between the first hashmap and the union of the
others. Symmetric difference is not extended beyond two hashmaps. When
comparing associations, only the keys are compared. In case of
duplicate keys (in the sense of the hashmaps comparators),
associations in the result hashmap are drawn from the first hashmap in
which they appear.

## `(hashmap-union! hashmap1 hashmap2 ...)`

## `(hashmap-intersection! hashmap1 hashmap2 ...)`

## `(hashmap-difference! hashmap1 hashmap2 ...)`

## `(hashmap-xor! hashmap1 hashmap2 ...)`

These procedures are the linear update analogs of the corresponding
pure functional procedures above.

## `(comparator? obj)`

Type predicate for comparators as exported by `(scheme comparator)`.

## `hashmap-comparator`

hashmap-comparator is constructed by invoking make-hashmap-comparator
on (make-default-comparator).

## `(make-hashmap-comparator comparator)`

Returns a comparator for hashmaps that is compatible with the equality
predicate (hashmap=? comparator hashmap1 hashmap2). If
make-hashmap-comparator is imported from (srfi 146), it provides a
(partial) ordering predicate that is applicable to pairs of hashmaps
with the same (key) comparator. If (make-hashmap-comparator) is
imported from (srfi 146 hash), it provides an implementation-dependent
hash function.

If make-hashmap-comparator is imported from (srfi 146), the
lexicographic ordering with respect to the keys (and, in case a
tiebreak is necessary, with respect to the ordering of the values) is
used for hashmaps sharing a comparator.

The existence of comparators returned by make-hashmap-comparator
allows hashmaps whose keys are hashmaps themselves, and it allows to
compare hashmaps whose values are hashmaps.
