# `(scheme ephemeron)`

This library is based on
[SRFI-124](https://srfi.schemers.org/srfi-124/), that is itself based
on the MIT Scheme Reference Manual.

 An ephemeron is an object with two components called its key and its
 datum. It differs from an ordinary pair as follows: if the garbage
 collector (GC) can prove that there are no references to the key
 except from the ephemeron itself and possibly from the datum, then it
 is free to break the ephemeron, dropping its reference to both key
 and datum. In other words, an ephemeron can be broken when nobody
 else cares about its key. Ephemerons can be used to construct weak
 vectors or lists and (possibly in combination with finalizers) weak
 hash tables.

## `(ephemeron? obj)`

Returns #t if object is an ephemeron; otherwise returns #f.

## `(make-ephemeron key datum)`

Returns a newly allocated ephemeron, with components key and
datum. Note that if key and datum are the same in the sense of eq?,
the ephemeron is effectively a weak reference to the object.

## `(ephemeron-broken? ephemeron)`

Returns #t if ephemeron has been broken; otherwise returns #f.

This procedure must be used with care. If it returns #f, that
guarantees only that prior evaluations of ephemeron-key or
ephemeron-datum yielded the key or datum that was stored in
ephemeron. However, it makes no guarantees about subsequent calls to
ephemeron-key or ephemeron-datum, because the GC may run and break the
ephemeron immediately after ephemeron-broken? returns. Thus, the
correct idiom to fetch an ephemeron's key and datum and use them if
the ephemeron is not broken is:

```scheme
     (let ((key (ephemeron-key ephemeron))
           (datum (ephemeron-datum ephemeron)))
       (if (ephemeron-broken? ephemeron)
           ... broken case ...
           ... code using key and datum ...))
```

## `(ephemeron-key ephemeron)`

## `(ephemeron-value ephemeron)`

These return the key or datum component, respectively, of
ephemeron. If ephemeron has been broken, these operations return #f,
but they can also return #f if that is what was stored as the key or
datum.

## ` (reference-barrier key)`

This procedure is optional.

This procedure ensures that the garbage collector does not break an
ephemeron containing an unreferenced key before a certain point in a
program. The program can invoke a reference barrier on the key by
calling this procedure, which guarantees that even if the program does
not use the key, it will be considered strongly reachable until after
reference-barrier returns.
