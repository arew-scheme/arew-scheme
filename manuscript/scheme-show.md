# `(scheme show)`

This library is based on
[SRFI-159](https://srfi.schemers.org/srfi-159/).

A library of procedures for formatting Scheme objects to text in
various ways, and for easily concatenating, composing and extending
these formatters efficiently without resorting to capturing and
manipulating intermediate strings.


## `(show output-dest fmt ...)`

The entry point for all formatting. Applies the fmt formatters in
sequence, accumulating the output to output-dest. As with SRFI 28
format, output-dest can be an output port, #t to indicate the current
output port, or #f to accumulate the output into a string and return
that as the result of show.

Each fmt should be a formatter as discussed below. As a convenience,
non-formatter arguments are also allowed and are formatted as if
wrapped with displayed, described below, so that

```scheme
(show #f "π = " (with ((precision 2)) (acos -1)) nl)
```

would return the string "π = 3.14\n".

As mentioned, formatters are an opaque type and cannot directly be
applied outside of show. Custom formatters are built on the existing
formatters, and as first class objects may be named or computed
dynamically, so that:

```scheme
      (let ((~.2f (lambda (x) (with ((precision 2)) x))))
        (show #f "π = " (~.2f (acos -1)) nl))
```

produces the same result. For typical uses you only need to combine
the existing high level formatters described in the succeeding
sections, but see the section Higher Order Formatters and State for
control flow and state manipulation primitives.

The return value of show is the accumulated string if output-dest is
#f and unspecified otherwise.

## `(displayed obj)`

If obj is a formatter, returns obj as is. Otherwise, outputs obj using
display semantics. Specifically, strings are output as if by
write-string and characters are written as if by write-char. Other
objects are output as with written (including nested strings and chars
inside obj). This is the default behavior for top-level formats in
show, each and most other high-level formatters.

## `(written obj)`

Outputs obj using write semantics. Uses the current numeric formatting
settings to the extent that the written result can still be passed to
read, possibly with loss of precision. Specifically, the current radix
is used if set to any of 2, 8, 10 or 16, and the fixed point precision
is used if specified and the radix is 10.

```scheme
(show #f (written (cons 0 1)))
=> "(0 . 1)"

(show #f 1.5 " " (with ((precision 0)) 1.5))
=> "1.5 2"

(show #f 1/7 " " (with ((precision 3)) 1/7)
      " " (with ((precision 20)) 1/7))
=> "1/7 0.143 0.14285714285714285714"
```

Implementations should allow arbitrary precision for exact rational
numbers, for example, using string-segment from SRFI 152, the
following code returns the first 100 Fibonacci numbers:

```scheme
    (map string->number
         (string-segment
          (show #f (with ((precision 2500))
                     (/ 1000 (- #e1e50 #e1e25 1))))
          25))
```

## `(written-simply obj)`

As above, but doesn't handle shared structures. Infinite loops can
still be avoided if used inside a formatter that truncates data (see
trimmed and fitted below).

## `(pretty obj)`

Pretty-prints obj. The result should be identical to written except
possibly for differences in whitespace to make the output resemble
formatted source code. Implementations should print vectors and data
lists (lists that don't begin with a (nested) symbol) in a tabular
format when possible to reduce vertical space.

## `(pretty-simply obj)`

As above but without sharing.

## `(escaped str [quote-ch esc-ch renamer])`

Outputs the string str, escaping any quote or escape characters. If
esc-ch, which defaults to #\\, is #f, escapes only the quote-ch, which
defaults to #\", by doubling it, as in SQL strings and CSV values. If
renamer is provided, it should be a procedure of one character which
maps that character to its escape value, e.g. #\newline => #\n, or #f
if there is no escape value.

```scheme
    (show #f (escaped "hi, bob!"))
    => "hi, bob!"

    (show #f (escaped "hi, \"bob!\""))
    => "hi, \"bob!\""
```

## `(maybe-escaped str pred [quote-ch esc-ch renamer])`

Like escaped, but first checks if any quoting is required (by the
existence of either any quote or escape characters, or any character
matching pred), and if so outputs the string in quotes and with
escapes. Otherwise outputs the string as is. This is useful for
quoting symbols and CSV output, etc.

```scheme
(show #f (maybe-escaped "foo" char-whitespace? #\"))
=> "foo"

(show #f (maybe-escaped "foo bar" char-whitespace? #\"))
=> "\"foo bar\""

(show #f (maybe-escaped "foo\"bar\"baz" char-whitespace? #\"))
=> "\"foo\"bar\"baz\""
```

## `(numeric num [radix precision sign comma comma-sep decimal-sep])`

Formats a single number num. You can optionally specify any radix from
2 to 36 (even if num isn't an integer). precision forces a fixed-point
format.

A sign of #t indicates to output a plus sign (+) for positive
integers. However, if sign is a pair of two strings, it means to wrap
negative numbers with the two strings. For example, ("(" . ")") prints
negative numbers in parentheses, financial style: -1.99 => (1.99).

- comma is an integer specifying the number of digits between commas.

- comma-sep is the character to use for commas, defaulting to #\,.

- decimal-sep is the character to use for decimals, defaulting to #\.,
  or to #\, (European style) if comma-sep is already #\..

These parameters may seem unwieldy, but they can also take their
defaults from state variables, described below.

## `(numeric/comma num [base precision sign])`

Shortcut for numeric to print with commas.

```scheme
(show #f (numeric/comma 1234567))
=> "1,234,567"
```

## `(numeric/si num [base separator])`

Abbreviates num with an SI suffix as in the -h or --si option to many
GNU commands. The base defaults to 1024, using suffix names like Ki,
Mi, Gi, etc. Other bases (e.g. the standard 1000) have the suffixes k,
M, G, etc. If separator is provided, it is inserted after the number,
before any suffix.

```scheme
(show #f (numeric/si 608))
=> "608"

(show #f (numeric/si 608) "B")
=> "608B"

(show #f (numeric/si 608 1000 " ") "B")
=> "608 B"

(show #f (numeric/si 3986))
=> "3.9Ki"

(show #f (numeric/si 3986 1000) "B")
=> "4kB"

(show #f (numeric/si 1.23e-6 1000) "m")
=> "1.2µm"

(show #f (numeric/si 1.23e-6 1000 " ") "m")
=> "1.2 µm"

See https://en.wikipedia.org/wiki/Metric_prefix for the complete list of abbreviations.

    (numeric/fitted width n . args)

Like numeric, but if the result doesn't fit in width using the current precision, output instead a string of hashes rather than showing an incorrectly truncated number. For example

(show #f (with ((precision 2)) (numeric/fitted 4 1.25)))
=> "1.25"

(show #f (with ((precision 2)) (numeric/fitted 4 12.345)))
=> "#.##"
```

## `nl`

Outputs a newline.

```scheme
(show #f nl)
=> "\n"
```

## `fl`

Short for "fresh line," outputs a newline only if we're not already at
the start of a line.

```scheme
(show #f fl)
=> ""

(show #f "hi" fl)
=> "hi\n"

(show #f "hi" nl fl)
=> "hi\n"
```

## `(space-to column)`

Outputs spaces up to the given column. If the current column is
already >= column, does nothing. The character used for spacing is the
current value of pad-char, described below, which defaults to
space. Columns are zero-based.

```scheme
(show #f "a" (space-to 5) "b")
=> "a    b"

(show #f "a" (space-to 0) "b")
=> "ab"
```

## `(tab-to [tab-width])`

Outputs spaces up to the next tab stop, using tab stops of width
tab-width, which defaults to 8. If already on a tab stop, does
nothing. If you want to ensure you always tab at least one space, you
can use (each " " (tab-to width)). Columns are zero-based.

```
(show #f (tab-to 5) "b")
=> "b"

(show #f "a" (tab-to 5) "b")
=> "a    b"

(show #f "abcdefghi" (tab-to 5) "b")
=> "abcdefghi b"
```

## `nothing`

Outputs nothing (useful in combinators and as a default noop in
conditionals).

```
(show #f "a" nothing "b")
=> "ab"
```

## `(each fmt ...)`

Applies each fmt in sequence, as in the top-level of show.

```scheme
(show #f (each "a" "b"))
=> "ab"
```

## `(each-in-list list-of-fmts)`

Equivalent to (apply each list-of-fmts) but may be more efficient.

## `(joined mapper list [sep])`

Formats each element elt of list with (mapper elt), inserting sep in
between. sep defaults to the empty string, but can be any format or
string.

```scheme
(show #f (joined displayed '(a b c) ", "))
=> "a, b, c"
```

## `(joined/prefix mapper list [sep])`

## `(joined/suffix mapper list [sep])`

```scheme
(show #f (joined/prefix displayed '(usr local bin) "/"))
=> "/usr/local/bin"

(show #f (joined/suffix displayed '(1 2 3) nl))
=> "1\n2\n3\n"
```

As joined, but inserts sep before/after every element.

## `(joined/last mapper last-mapper list [sep])`

As joined, but the last element of the list is formatted with
last-mapper instead.

```
    (show #f (joined/last displayed
                          (lambda (last) (each "and " last))
                          '(lions tigers bears)
                          ", "))
    => "lions, tigers, and bears"
```

## `(joined/dot mapper dot-mapper list [sep])`

As joined, but if the list is a dotted list, then formats the dotted
value with dot-mapper instead.

```
    (show #f
          "("
          (joined/dot displayed
    		  (lambda (dot) (each ". " dot))
    		  '(1 2 . 3)
    		  " ")
          ")")
    => "(1 2 . 3)"
```

## `(joined/range mapper start [end sep])`

As joined, but counts from start (inclusive) to end (exclusive),
formatting each integer in the range with mapper. If end is #f or
unspecified, produces an infinite stream of output.

```scheme
    (show #f (joined/range displayed 0 5 " "))
    => "0 1 2 3 4"
```

## `(padded width fmt ...)`

## `(padded/right width fmt ...)`

## `(padded/both width fmt ...)`

Analogs of SRFI 13 string-pad, these add extra space to the left,
right or both sides of the output generated by the fmts to pad it to
width. If width is exceeded, has no effect. padded/both will include
one more extra space on the right side of the output if the difference
is odd.

padded/right is guaranteed not to accumulate any intermediate data.

Note these are column-oriented padders, so won't necessarily work with
multi-line output (padding doesn't seem a likely operation for
multi-line output).

```scheme
(show #f (padded 5 "abc"))
=> "  abc"

(show #f (padded/right 5 "abc"))
=> "abc  "

(show #f (padded/both 5 "abc"))
=> " abc "
```

## `(trimmed width fmt ...)`

## `(trimmed/right width fmt ...)`

## `(trimmed/both width fmt ...)`

Analogs of SRFI 13 string-trim, these truncate the output of the fmts
to force it in under width columns. As soon as any of the fmts exceeds
width, stop formatting and truncate the result, returning control to
whoever called trimmed. If width is not exceeded, is equivalent to
each.

If a truncation ellipsis is set, then when any truncation occurs
trimmed and trimmed/right will prepend and append the ellipsis,
respectively. trimmed/both will both prepend and append. The length of
the ellipsis will be considered when truncating the original string,
so that the total width will never be longer than width. It is an
error if width is less than the length of ellipsis, or double the
length for /both.

```scheme
(show #f (with ((ellipsis "...")) (trimmed 5 "abcde")))
=>  "abcde"

(show #f (with ((ellipses "...")) (trimmed 5 "abcdef")))
=>  "ab..."
```

It is an error if width is shorter than the width of the ellipsis.

## `(trimmed/lazy width fmt ...)`

A variant of trimmed which generates each fmt in left to right order,
and truncates and terminates immediately if more than width characters
are generated. Thus this is safe to use with an infinite amount of
output, e.g. from written-simply on an infinite list.

## `(fitted width fmt ...)`

## `(fitted/right width fmt ...)`

## `(fitted/both width fmt ...)`

A combination of padded and trimmed that ensures that the output width
is exactly width, truncating if it goes over and padding if it goes
under.

## `(columnar column ...)`

Formats each column side-by-side, i.e. as though each were formatted
separately and then the individual lines concatenated together. The
current line width (from the width state variable) is divided evenly
among the columns, and all but the last column are right-padded. For
example

```scheme
    (show #t (columnar (displayed "abc\ndef\n")
                       (displayed "123\n456\n")))
```

outputs

```scheme
    abc     123
    def     456
```

 assuming a 16-char width (the left side gets half the width, or 8
 spaces, and is left aligned). Note that we explicitly use displayed
 instead of the strings directly. This is because columnar treats raw
 strings as literals inserted into the given location on every line,
 to be used as borders, for example:

```scheme
    (show #t (columnar "/* " (displayed "abc\ndef\n")
                       " | " (displayed "123\n456\n")
                       " */"))
```

would output

```
    /* abc | 123 */
    /* def | 456 */
```

You may also prefix any column with any of the symbols 'left, 'right
or 'center to control the justification. The symbol 'infinite can be
used to indicate the column generates an infinite stream of output.

You can further prefix any column with a width modifier. Any positive
integer is treated as a fixed width, ignoring the available width. Any
real number between 0 and 1 indicates a fraction of the available
width (after subtracting out any fixed widths). Columns with
unspecified width divide up the remaining width evenly. If the extra
space does not divide evenly, it is allocated column-wise left to
right, e.g. if the width of 78 is divided among 5 columns, the column
widths become 16, 16, 16, 15, 15 in order.

Note that columnar builds its output incrementally, interleaving calls
to the generators until each has produced a line, then concatenating
that line together and outputting it. This is important because as
noted above, some columns may produce an infinite stream of output,
and in general you may want to format data larger than can fit into
memory. Thus columnar would be suitable for line numbering a file of
arbitrary size, or implementing the Unix yes(1) command, etc.

## `(tabular column ...)`

Equivalent to columnar except that each column is padded at least to
the minimum width required on any of its lines. Thus

```scheme
    (show #t (tabular "|" (each "a\nbc\ndef\n") "|"
                          (each "123\n45\n6\n") "|"))
```

outputs

```
    |a  |123|
    |bc |45 |
    |def|6  |
```

This makes it easier to generate tables without knowing widths in
advance. However, because it requires generating the entire output in
advance to determine the correct column widths, tabular cannot format
a table larger than would fit in memory.  (wrapped fmt ...)

Behaves like each, except text is accumulated and lines are wrapped to
fit in the current width as in the Unix fmt(1) command. Specifically,
words are tokenized by splitting on all characters which satisfy the
predicate in the parameter word-separator?, which defaults to
char-whitespace?. Words are grouped into lines separating them by
space, and line breaks are introduced to minimize the sum of the cube
of trailing whitespace on every line.

## `(wrapped/list list-of-strings)`

Like wrapped, but taking a pre-tokenized list of strings.

## `(wrapped/char fmt ...)`

As wrapped, but splits simply on individual characters exactly as the
current width is reached on each line. Thus there is nothing to
optimize and this formatter doesn't buffer output.

## `(justified <format> ...)`

Like wrapped except the lines are full-justified.

```scheme
    (define func
      '(define (fold kons knil ls)
         (let lp ((ls ls) (acc knil))
           (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))

    (define doc
      (string-append
        "The fundamental list iterator.  Applies KONS to each "
        "element of LS and the result of the previous application, "
        "beginning with KNIL.  With KONS as CONS and KNIL as '(), "
        "equivalent to REVERSE."))

    (show #t (columnar (pretty func) " ; " (justified doc)))
```

outputs

```scheme
    (define (fold kons knil ls)          ; The   fundamental   list   iterator.
      (let lp ((ls ls) (acc knil))       ; Applies  KONS  to  each  element  of
        (if (null? ls)                   ; LS  and  the  result of the previous
            acc                          ; application,  beginning  with  KNIL.
            (lp (cdr ls)                 ; With  KONS  as CONS and KNIL as '(),
                (kons (car ls) acc)))))  ; equivalent to REVERSE.
```

## `(from-file pathname)`

Displays the contents of the file pathname one line at a time, so that
in typical formatters such as columnar only constant memory is
consumed, making this suitable for formatting files of arbitrary size.

## `(line-numbers [start])`

A convenience utility, just formats an infinite stream of numbers (in
the current radix) beginning with start, which defaults to 1.

The Unix nl(1) utility could be implemented as:

```scheme
    (show #t (columnar 4 'right 'infinite (line-numbers)
                       " " (from-file "read-line.scm")))
```

which might output:

```scheme
       1
       2 (define (read-line . o)
       3   (let ((port (if (pair? o) (car o) (current-input-port))))
       4     (let lp ((res '()))
       5       (let ((c (read-char port)))
       6         (if (or (eof-object? c) (eqv? c #\newline))
       7             (list->string (reverse res))
       8             (lp (cons c res)))))))
```

## `(as-red fmt ...)`

## `(as-blue fmt ...)`

## `(as-green fmt ...)`

## `(as-cyan fmt ...)`

## `(as-yellow fmt ...)`

## `(as-magenta fmt ...)`

## `(as-white fmt ...)`

## `(as-black fmt ...)`

## `(as-bold fmt ...)`

## `(as-underline fmt ...)`

Outputs the formatters colored or (boldened or underline) with ANSI
escapes, for use when formatting to a terminal.

## `(as-unicode fmt ...)`

Equivalent to

```scheme
(with ((string-width unicode-terminal-width)) fmt ...)
```

Padding, trimming and tabbing, etc. will generally not do the right
thing in the presence of zero-width and double-width Unicode
characters. This formatter overrides the string-width state var used
in column tracking to do the right thing in such cases, considering
Unicode double or full width characters as 2 characters wide (as they
typically are in fixed-width terminals), while treating combining and
non-spacing characters as 0 characters wide.

```scheme
;; 3 characters padded to 5
(show #f (with ((pad-char #\〜)) (padded/both 5 "日本語")))
=> "〜日本語〜"

;; the 3 characters have a terminal width of 6 so are not padded
(show #f (as-unicode (with ((pad-char #\〜)) (padded/both 5 "日本語"))))
=> "日本語"
```

## `(unicode-terminal-width str)`

A utility function which returns the integer number of columns str
would require in a terminal, according to the following rules:

- non-spacing characters (format control characters with the property
  Cf, or non-spacing marks with the property Mn) count as 0 columns

- characters with the East Asian Wide (W) or East Asian Fullwidth (F)
  properties, according to Unicode TR #11, count as 2 columns

- characters with the Halfwidth (H) or Narrow (Na) should count as 1
  column

- characters with the Neutral (N) non-East Asian also count as 1
  column

- characters with the Ambiguous (A) property are implementation
  defined

- ANSI terminal escapes, as output by the color formatters above,
  count as 0 columns

- the tab character is implementation defined

- Implementations should support the properties from at least the
current Unicode specification at time of writing this SRFI, 10.0.0.
Higher Order Formatters and State

Formatters up to this point have been simple accumulators of output,
with no control flow or handling of state. Both of these are provided
by fn and with for getting and setting state, respectively.

A formatter is essentially an environment monad, although the
underlying implementation is unspecified.

## `(fn ((id state-var) ...) expr ... fmt)`

Short for "function," this is the analog to lambda. Returns a
formatter which on application evaluates each expr and fmt in
left-to-right order, in a lexical environment extended with each
identifier id bound to the current value of the state variable named
by the symbol state-var. The result of the fmt is then applied as a
formatter.

As a convenience, any (id state-var) list may be abbreviated as simply
id, indicating id is bound to the state variable of the same (symbol)
name.

```scheme
    (show #f "column: " (fn (col) col))
    => "column: 8"

    (show #f "column: " (fn ((col1 col))
                         (each col1 ", " (fn ((col2 col)) col2))))
    => "column: 8, 11"
```

The trivial case of no state variables is often useful to allow for
lazy applications of formatters, needed for conditional formatting and
loops. For example:

```scheme
    (show #t (let lp ((ls ls))
               (if (pair? ls)
                   (each (car ls) (lp (cdr ls)))
                   nothing)))
```

would eagerly create a formatter concatenating every element of ls
before starting to accumulate any output, whereas

```scheme
    (show #t (let lp ((ls ls))
               (if (pair? ls)
                   (each (car ls) (fn () (lp (cdr ls))))
                   nothing)))
```

would lazily apply the formatters one at a time.

## `(with ((state-var value) ...) fmt ...)`

Conceptually the formatting equivalent of parameterize, temporarily
altering state variables. Applies each of the formatters fmt with each
state-var bound to the corresponding value. The resulting state is
then updated to restore each state-var to its original value.

```scheme
(with! (state-var value) ...)
```

Similar to with but does not restore the original values, changing the
value of each state-var for any remaining formatters in a sequence.

## `(forked fmt1 fmt2)`

Calls fmt1 on (a conceptual copy of) the current state, then fmt2 on
the same original state as though fmt1 had not been called.

## `(call-with-output formatter mapper)`

A utility, calls formatter on a copy of the current state (as with
forked), accumulating the results into a string. Then calls the
formatter resulting from (mapper result-string) on the original state.

## `port`

The textual port output is written to, this can be overridden to
capture intermediate output.

## `row`

The current row of output.

## `col`

The current column of output, used for padding and spacing, etc.

## `width`

The current line width, used for wrapping, pretty-printing, and
columnar formatting. The default is implementation-defined.

## `output`

The underlying standard formatter for writing a single string. The
default value outputs the string while tracking the current row and
col. This can be overridden both to capture intermediate output and
perform transformations on strings before outputting, but should
generally wrap the existing output to preserve expected behavior.

## `writer`

The mapper for automatic formatting of non-string/char values in
top-level show, each and other formatters. Default value is
implementation-defined.

## `string-width`

A function of a single string, it returns the length in columns of
that string, used by the default output.

## `pad-char`

The character used by space-to, tab-to and other padding formatters.

```scheme
    (define (print-table-of-contents alist)
      (define (print-line x)
        (each (car x) (space-to 72) (padded 3 (cdr x))))
      (show #t (with ((pad-char #\.))
                 (joined/suffix print-line alist nl))))

    (print-table-of-contents
     '(("An Unexpected Party" . 29)
       ("Roast Mutton" . 60)
       ("A Short Rest" . 87)
       ("Over Hill and Under Hill" . 100)
       ("Riddles in the Dark" . 115)))
```

would output

```
    An Unexpected Party.....................................................29
    Roast Mutton............................................................60
    A Short Rest............................................................87
    Over Hill and Under Hill...............................................100
    Riddles in the Dark....................................................115
```

## `eellipsis`

The string used when truncating as described in trimmed.

## `radix`

The radix for numeric output, defaulting to 10, as used in numeric and
written.

## `precision`

The precision for numeric output, as described in numeric and
written. The precision specifies the number of digits written after
the decimal point. If the numeric value to be written out requires
more digits to represent it than precision, the written representation
is chosen which is closest to the numeric value and representable with
the specified precision. If the numeric value falls on the midpoint of
two such representations, it is implementation dependent which
representation is chosen.

When the numeric value is an inexact floating-point number, there is
more than one interpretation of this "rounding". One is to take the
effective value the floating-point number represents (e.g. if we use
binary floating-point numbers, we take the value of (* sign mantissa
(expt 2 exponent))), and compare it to the two closest numeric
representations of the given precision. Another way is to obtain the
default notation of the floating-point number and apply rounding to
it. The former (we call it effective rounding) is consistent with most
floating-point number operations, but may lead to a more non-intuitive
result than the latter (we call it notational rounding). For example,
5.015 can't be represented exactly in binary floating-point
numbers. With IEEE754 floating-point numbers, the floating point
number closest to 5.015 is smaller than exact 5.015, i.e. (< 5.015
5015/1000) => #t. With effective rounding with precision 2, it should
result in "5.01". However, users who look at the notation may be
confused by "5.015" not being rounded up as they usually expect. With
notational rounding the implementation chooses "5.02" (if it also
adopts round-half-to-infinity or round-half-up rule). It is up to the
implementation to choose which interpretation to adopt.

## `decimal-sep`

The decimal separator for floating point output, default ".".

## `decimal-align`

Specifies an alignment for the decimal place when formatting numbers,
and is useful for outputting tables of numbers.

```scheme
    (define (print-angles x)
      (joined numeric (list x (sin x) (cos x) (tan x)) " "))

    (show #t (with ((decimal-align 5) (precision 3))
               (joined/suffix print-angles (iota 5) nl)))
```

would output

```scheme
     0.000    0.000    1.000    0.000
     1.000    0.842    0.540    1.557
     2.000    0.909   -0.416   -2.185
     3.000    0.141   -0.990   -0.142
     4.000   -0.757   -0.654    1.158
```

## `word-separator?`

A character predicate used to tokenize words for wrapped and
justify. Defaults to char-whitespace?. More flexibility is available
with wrapped/list.
