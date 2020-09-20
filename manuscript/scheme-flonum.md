
# `(scheme flonum)`

This is based on [SRFI-144](https://srfi.schemers.org/srfi-144/).

This library describes numeric procedures applicable to flonums, a
subset of the inexact real numbers provided by a Scheme
implementation. In most Schemes, the flonums and the inexact reals are
the same. These procedures are semantically equivalent to the
corresponding generic procedures, but allow more efficient
implementations.

## `fl-e`

Bound to the mathematical constant e. (C99 M_E)

## `fl-1/e`

Bound to 1/e. (C99 M_E)

## `fl-e-2`

Bound to e2.

## `fl-e-pi/4`

Bound to eπ/4.

## `fl-log2-e`

Bound to log2 e. (C99 M_LOG2E)

## `fl-log10-e`

Bound to log10 e. (C99 M_LOG10E)

## `fl-log-2`

Bound to loge 2. (C99 M_LN2)

## `fl-1/log-2`

Bound to 1/loge 2. (C99 M_LN2)

## `fl-log-3`

Bound to loge 3.

## `fl-log-pi`

Bound to loge π.

## `fl-log-10`

Bound to loge 10. (C99 M_LN10)

## `fl-1/log-10`

Bound to 1/loge 10. (C99 M_LN10)

## `fl-pi`

Bound to the mathematical constant π. (C99 M_PI)

## `fl-1/pi`

Bound to 1/π. (C99 M_1_PI)

## `fl-2pi`

Bound to 2π.

## `fl-pi/2`

Bound to π/2. (C99 M_PI_2)

## `fl-pi/4`

Bound to π/4. (C99 M_PI_4)

## `fl-pi-squared`

Bound to π2.

## `fl-degree`

Bound to π/180, the number of radians in a degree.

## `fl-2/pi`

Bound to 2/π. (C99 M_2_PI)

## `fl-2/sqrt-pi`

Bound to 2/√π. (C99 M_2_SQRTPI)

## `fl-sqrt-2`

Bound to √2. (C99 M_SQRT2)

## `fl-sqrt-3`

Bound to √3.

## `fl-sqrt-5`

Bound to √5.

## `fl-sqrt-10`

Bound to √10.

## `fl-1/sqrt-2`

Bound to 1/√2. (C99 M_SQRT1_2)

## `fl-cbrt-2`

Bound to ∛2.

## `fl-cbrt-3`

Bound to ∛3.

## `fl-4thrt-2`

Bound to ∜2.

## fl-phi`

Bound to the mathematical constant φ.

## `fl-log-phi`

Bound to log(φ).

## `fl-1/log-phi`

Bound to 1/log(φ).

## `fl-euler`

Bound to the mathematical constant γ (Euler's constant).

## `fl-e-euler`

Bound to eγ.

## `fl-sin-1`

Bound to sin 1.

## `fl-cos-1`

Bound to cos 1.

## `fl-gamma-1/2`

Bound to Γ(1/2).

## `fl-gamma-1/3`

Bound to Γ(1/3).

## `fl-gamma-2/3`

Bound to Γ(2/3).

## `fl-greatest`

## `fl-least`

Bound to the largest/smallest positive finite flonum. (e.g. C99
DBL_MAX and C11 DBL_TRUE_MIN)

## `fl-epsilon`

Bound to the appropriate machine epsilon for the hardware
representation of flonums. (C99 DBL_EPSILON in <float.h>)

## `fl-fast-fl+*`

Bound to #t if (fl+* x y z) executes about as fast as, or faster than,
(fl+ (fl* x y) z); bound to #f otherwise. (C99 FP_FAST_FMA)

So that the value of this variable can be determined at compile time,
R7RS implementations and other implementations that provide a features
function should provide the feature fl-fast-fl+* if this variable is
true, and not if it is false or the value is unknown at compile time.

## `fl-integer-exponent-zero`

Bound to whatever exact integer is returned by (flinteger-exponent
0.0). (C99 FP_ILOGB0)

## `fl-integer-exponent-nan`

Bound to whatever exact integer is returned by (flinteger-exponent
+nan.0). (C99 FP_ILOGBNAN)

## `(flonum number)`

If number is an inexact real number and there exists a flonum that is
the same (in the sense of =) to number, returns that flonum. If number
is a negative zero, an infinity, or a NaN, return its flonum
equivalent. If such a flonum does not exist, returns the nearest
flonum, where "nearest" is implementation-dependent. If number is not
a real number, it is an error. If number is exact, applies inexact or
exact->inexact to number first.

## `(fladjacent x y)`

Returns a flonum adjacent to x in the direction of y. Specifically: if
x < y, returns the smallest flonum larger than x; if x > y, returns
the largest flonum smaller than x; if x = y, returns x. (C99
nextafter)

## `(flcopysign x y)`

Returns a flonum whose magnitude is the magnitude of x and whose sign
is the sign of y. (C99 copysign)

## `(make-flonum x n)`

Returns x × 2n, where n is an integer with an implementation-dependent
range. (C99 ldexp)

## `(flinteger-fraction x)`

Returns two values, the integral part of x as a flonum and the
fractional part of x as a flonum. (C99 modf)

## `(flexponent x)`

Returns the exponent of x. (C99 logb)

## `(flinteger-exponent x)`

Returns the same as flexponent truncated to an exact integer. If x is
zero, returns fl-integer-exponent-zero; if x is a NaN, returns
fl-integer-exponent-nan; if x is infinite, returns a large
implementation-dependent exact integer. (C99 ilogb)

## `(flnormalized-fraction-exponent x)`

Returns two values, a correctly signed fraction y whose absolute value
is between 0.5 (inclusive) and 1.0 (exclusive), and an exact integer
exponent n such that x = y(2n). (C99 frexp)

## `(flsign-bit x)`

Returns 0 for positive flonums and 1 for negative flonums and
-0.0. The value of (flsign-bit +nan.0) is implementation-dependent,
reflecting the sign bit of the underlying representation of NaNs. (C99
signbit)

## `(flonum? obj)`

Returns #t if obj is a flonum and #f otherwise.

## `(fl=? x y z ...)`

## `(fl<? x y z ...)`

## `(fl>? x y z ...)`

## `(fl<=? x y z ...)`

## `(fl>=? x y z ...)`

These procedures return #t if their arguments are (respectively):
equal, monotonically increasing, monotonically decreasing,
monotonically nondecreasing, or monotonically nonincreasing; they
return #f otherwise. These predicates must be transitive. (C99 =, <, >
<=, >= operators respectively)

## `(flunordered? x y)`

Returns #t if x and y are unordered according to IEEE rules. This
means that one of them is a NaN.

These numerical predicates test a flonum for a particular property,
returning #t or #f.

## `(flinteger? x)`

Tests whether x is an integral flonum.

## `(flzero? x)`

Tests whether x is zero. Beware of roundoff errors.

## `(flpositive? x)`

Tests whether x is positive.

## `(flnegative? x)`

Tests whether x is negative. Note that (flnegative? -0.0) must return
#f; otherwise it would lose the correspondence with (fl<? -0.0 0.0),
which is #f according to IEEE 754.

## `(flodd? x)`

Tests whether the flonum x is odd. It is an error if x is not an
integer.

## `(fleven? x)`

Tests whether the flonum x is even. It is an error if x is not an
integer.

## `(flfinite? x)`

Tests whether the flonum x is finite. (C99 isfinite)

## `(flinfinite? x)`

Tests whether the flonum x is infinite. (C99 isinf)

## `(flnan? x)`

Tests whether the flonum x is NaN. (C99 isnan)

## `(flnormalized? x)`

Tests whether the flonum x is normalized. (C11 isnormal; in C99, use
fpclassify(x) == FP_NORMAL)

## `(fldenormalized? x)`

Tests whether the flonum x is denormalized. (C11 issubnormal; in C99,
use fpclassify(x) == FP_SUBNORMAL)

## `(flmax x ...)`

## `(flmin x ...)`

Return the maximum/minimum argument. If there are no arguments, these
procedures return -inf.0 or +inf.0 if the implementation provides
these numbers, and (fl- fl-greatest) or fl-greatest otherwise. (C99
fmax fmin)

## `(fl+ x ...)`

## `(fl* x ...)`

Return the flonum sum or product of their flonum arguments. (C99 + *
operators respectively)

## `(fl+* x y z)`

Returns xy + z as if to infinite precision and rounded only once. The
boolean constant fl-fast-fl+* indicates whether this procedure
executes about as fast as, or faster than, a multiply and an add of
flonums. (C99 fma)

## `(fl- x y ...)`

## `(fl/ x y ...)`

With two or more arguments, these procedures return the difference or
quotient of their arguments, associating to the left. With one
argument, however, they return the additive or multiplicative inverse
of their argument. (C99 - / operators respectively)

## `(flabs x)`

Returns the absolute value of x. (C99 fabs)

## `(flabsdiff x y)`

Returns |x - y|.

## `(flposdiff x y)`

Returns the difference of x and y if it is non-negative, or zero if
the difference is negative. (C99 fdim)

## `(flsgn x)`

Returns (flcopysign 1.0 x).

## `(flnumerator x)`

## `(fldenominator x)`

Returns the numerator/denominator of x as a flonum; the result is
computed as if x was represented as a fraction in lowest terms. The
denominator is always positive. The numerator of an infinite flonum is
itself. The denominator of an infinite or zero flonum is 1.0. The
numerator and denominator of a NaN is a NaN.

## `(flfloor x)`

Returns the largest integral flonum not larger than x. (C99 floor)

## `(flceiling x)`

Returns the smallest integral flonum not smaller than x. (C99 ceil)

## `(flround x)`

Returns the closest integral flonum to x, rounding to even when x
represents a number halfway between two integers. (Not the same as C99
round, which rounds away from zero)

## `(fltruncate x)`

Returns the closest integral flonum to x whose absolute value is not
larger than the absolute value of x (C99 trunc) Exponents and
logarithms

## `(flexp x)`

Returns ex. (C99 exp)

## `(flexp2 x)`

Returns 2x. (C99 exp2)

## `(flexp-1 x)`

Returns ex - 1, but is much more accurate than flexp for very small
values of x. It is recommended for use in algorithms where accuracy is
important. (C99 expm1)

## `(flsquare x)`

Returns x2.

## `(flsqrt x)`

Returns √x. For -0.0, flsqrt should return -0.0. (C99 sqrt)

## `(flcbrt x)`

Returns ∛x. (C99 cbrt)

## `(flhypot x y)`

Returns the length of the hypotenuse of a right triangle whose sides
are of length |x| and |y|. (C99 hypot)

## `(flexpt x y)`

Returns xy. If x is zero, then the result is zero. (C99 pow)

## `(fllog x)`

Returns loge x. (C99 log)

## `(fllog1+ x)`

Returns loge (x+ 1), but is much more accurate than fllog for values
of x near 0. It is recommended for use in algorithms where accuracy is
important. (C99 log1p)

## `(fllog2 x)`

Returns log2 x. (C99 log2)

## `(fllog10 x)`

Returns log10 x. (C99 log10)

## `(make-fllog-base x)`

Returns a procedure that calculates the base-x logarithm of its
argument. If x is 1.0 or less than 1.0, it is an error.

## `(flsin x)`

Returns sin x. (C99 sin)

## `(flcos x)`

Returns cos x. (C99 cos)

## `(fltan x)`

Returns tan x. (C99 tan)

## `(flasin x)`

Returns arcsin x. (C99 asin)

## `(flacos x)`

Returns arccos x. (C99 acos)

## `(flatan [y] x)`

Returns arctan x. (C99 atan)

With two arguments, returns arctan(y/x). in the range [-π,π], using
the signs of x and y to choose the correct quadrant for the
result. (C99 atan2)

## `(flsinh x)`

Returns sinh x. (C99 sinh)

## `(flcosh x)`

Returns cosh x. (C99 cosh)

## `(fltanh x)`

Returns tanh x. (C99 tanh)

## `(flasinh x)`

Returns arcsinh x. (C99 asinh)

## `(flacosh x)`

Returns arccosh x. (C99 acosh)

## `(flatanh x)`

Returns arctanh x. (C99 atanh)

## `(flquotient x y)`

Returns the quotient of x/y as an integral flonum, truncated towards
zero.

## `(flremainder x y)`

Returns the truncating remainder of x/y as an integral flonum.

## `(flremquo x y)`
` Returns two values, the rounded remainder of x/y and the low-order n
bits (as a correctly signed exact integer) of the rounded
quotient. The value of n is implementation-dependent but at
least 3. This procedure can be used to reduce the argument of the
inverse trigonometric functions, while preserving the correct quadrant
or octant. (C99 remquo)

## `(flgamma x)`

Returns Γ(x), the gamma function applied to x. This is equal to (x-1)!
for integers. (C99 tgamma)

## `(flloggamma x)`

Returns two values, log |Γ(x)| without internal overflow, and the sign
of Γ(x) as 1.0 if it is positive and -1.0 if it is negative. (C99
lgamma)

## `(flfirst-bessel n x)`

Returns the nth order Bessel function of the first kind applied to x,
Jn(x). (jn, which is an XSI Extension of C99)

## `(flsecond-bessel n x)`

Returns the nth order Bessel function of the second kind applied to x,
Yn(x). (yn, which is an XSI Extension of C99)

## `(flerf x)`

Returns the error function erf(x). (C99 erf)

## `(flerfc x)`

Returns the complementary error function, 1 - erf(x). (C99 erfc)
