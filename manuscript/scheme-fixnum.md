# `(scheme fixnum)`

This is based on [SRFI-143](https://srfi.schemers.org/srfi-143/).

This library describes arithmetic procedures applicable to a limited
range of exact integers only. These procedures are semantically
similar to the corresponding generic-arithmetic procedures, but allow
more efficient implementations.

## `fx-width`

Bound to the value w that specifies the implementation-defined
range. (R6RS fixnum-width is a procedure that always returns this
value.)

## `fx-greatest`

Bound to the value 2w-1-1, the largest representable fixnum. (R6RS
greatest-fixnum is a procedure that always returns this value.)

## `fx-least`

Bound to the value -2w-1, the smallest representable fixnum. (R6RS
least-fixnum is a procedure that always returns this value.)

## `(fixnum? obj)`

Returns #t if obj is an exact integer within the fixnum range, and #f
otherwise.

## `(fx=? i ...)`

Semantically equivalent to =.

## `(fx<? i ...)`

Semantically equivalent to <.

## `(fx>? i ...)`

Semantically equivalent to >.

## `(fx<=? i ...)`

Semantically equivalent to <=.

## `(fx>=? i ...)`

Semantically equivalent to >=.

## `(fxzero? i)`

Semantically equivalent to zero?.

## `(fxpositive? i)`

Semantically equivalent to positive?.

## `(fxnegative? i)`

Semantically equivalent to negative?.

## `(fxodd? i)`

Semantically equivalent to odd?.

## `(fxeven? i)`

Semantically equivalent to even?.

## `(fxmax i j ...)`

Semantically equivalent to max.

## `(fxmin i j ...)`

Semantically equivalent to min.

## `(fx+ i j)`

Semantically equivalent to +, but accepts exactly two arguments.

## `(fx- i j)`

Semantically equivalent to -, but accepts exactly two arguments.

## `(fxneg i)`

Semantically equivalent to -, but accepts exactly one argument.

## `(fx* i j)`

Semantically equivalent to *, but accepts exactly two arguments.

## `(fxquotient i j)`

Semantically equivalent to quotient.

## `(fxremainder i j)`

Semantically equivalent to remainder.

## `(fxabs i)`

Semantically equivalent to abs. In accordance with the fixnum rule,
has undefined results when applied to fx-least.

## `(fxsquare i)`

Semantically equivalent to square.

## `(fxsqrt i)`

Semantically equivalent to exact-integer-sqrt (not sqrt).

## `(fx+/carry i j k)`

Returns the two fixnum results of the following computation:

```scheme
(let*-values (((s) (+ i j k))
       ((q r) (balanced/ s (expt 2 fx-width))))
  (values r q))

(fx-/carry i j k)
```

Returns the two fixnum results of the following computation:

```scheme
(let*-values (((d) (- i j k))
       ((q r) (balanced/ d (expt 2 fx-width))))
  (values r q))

(fx*/carry i j k)
```

Returns the two fixnum results of the following computation:

```scheme
(let*-values (((s) (+ (* i j) k))
       ((q r) (balanced/ s (expt 2 fx-width))))
  (values r q))
```

The balanced/ procedure is available in SRFI 141, and also in the R6RS
base library under the name of div0-and-mod0.  Bitwise operations

The following procedures are the fixnum counterparts of certain
bitwise operations from SRFI 151 and the R6RS (rnrs arithmetic
fixnums) library. In case of disagreement, SRFI 151 is preferred. The
prefixes bitwise- and integer- are dropped for brevity and
compatibility.

## `(fxnot i)`

Semantically equivalent to bitwise-not.

## `(fxand i ...)`

Semantically equivalent to bitwise-and.

## `(fxior i ...)`

Semantically equivalent to bitwise-ior.

## `(fxxor i ...)`

Semantically equivalent to bitwise-xor.

## `(fxarithmetic-shift i count)`

Semantically equivalent to arithmetic-shift, except that it is an
error for the absolute value of count to exceed w-1.

## `(fxarithmetic-shift-left i count)`

The same as fxarithmetic-shift except that a negative value of count
is an error. This is provided for additional efficiency.

## `(fxarithmetic-shift-right i count)`

The same as fxarithmetic-shift except that a non-negative value of
count specifies the number of bits to shift right, and a negative
value is an error. This is provided for additional efficiency.

## `(fxbit-count i)`

Semantically equivalent to SRFI 151 bit-count.

## `(fxlength i)`

Semantically equivalent to integer-length.

## `(fxif mask i j)`

Semantically equivalent to bitwise-if. It can be implemented as (fxior
(fxand mask i) (fxand (fxnot mask) j))).

## `(fxbit-set? index i)`

Semantically equivalent to SRFI 151 bit-set?, except that it is an
error for index to be larger than or equal to fx-width.

## `(fxcopy-bit index i boolean)`

Semantically equivalent to SRFI 151 copy-bit, except that it is an
error for index to be larger than or equal to fx-width.

## `(fxfirst-set-bit i)`

Semantically equivalent to first-set-bit.

## `(fxbit-field i start end)`

Semantically equivalent to bit-field.

## `(fxbit-field-rotate i count start end)`

Semantically equivalent to SRFI 151 bit-field-rotate.

## `(fxbit-field-reverse i start end)`

Semantically equivalent to bit-field-reverse.
