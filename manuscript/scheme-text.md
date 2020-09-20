# `(scheme text)`

This is based on [SRFI-135](https://srfi.schemers.org/srfi-135/).

 In Scheme, strings are a mutable data type. Although it "is an error"
 (R5RS and R7RS) to use string-set! on literal strings or on strings
 returned by symbol->string, and any attempt to do so "should raise an
 exception" (R6RS), all other strings are mutable.

Although many mutable strings are never actually mutated, the mere
possibility of mutation complicates specifications of libraries that
use strings, encourages precautionary copying of strings, and
precludes structure sharing that could otherwise be used to make
procedures such as substring and string-append faster and more
space-efficient.

This SRFI specifies a new data type of immutable texts. It comes with
efficient and portable sample implementations that guarantee O(1)
indexing for both sequential and random access, even in systems whose
string-ref procedure takes linear time.

The operations of this new data type include analogues for all of the
non-mutating operations on strings specified by the R7RS and most of
those specified by SRFI 130, but the immutability of texts and
uniformity of character-based indexing simplify the specification of
those operations while avoiding several inefficiencies associated with
the mutability of Scheme's strings.

## `(text? obj)`

Is obj an immutable text? In particular, (text? obj) returns false if
(string? obj) returns true, which implies string? returns false if
text? returns true. Must execute in O(1) time.

## `(textual? obj)`

Returns true if and only obj is an immutable text or a string. Must
execute in O(1) time.

## `(textual-null? text)`

Is text the empty text? Must execute in O(1) time.

## `(textual-every pred textual [start end])`

## `(textual-any   pred textual [start end])`

Checks to see if every/any character in textual satisfies pred,
proceeding from left (index start) to right (index end). textual-every
These procedures are short-circuiting: if pred returns false,
textual-every does not call pred on subsequent characters; if pred
returns true, textual-any does not call pred on subsequent characters;
Both procedures are "witness-generating":

- If textual-every is given an empty interval (with start = end), it
  returns #t.

- If textual-every returns true for a non-empty interval (with start <
  end), the returned true value is the one returned by the final call
  to the predicate on (text-ref (textual-copy text) (- end 1)).

- If textual-any returns true, the returned true value is the one
  returned by the predicate.

Note: The names of these procedures do not end with a question
mark. This indicates a general value is returned instead of a simple
boolean (#t or #f).

## `(make-text len char)`

Returns a text of the given length filled with the given character.

## `(text char ...)`

Returns a text consisting of the given characters.

## `(text-tabulate proc len)`

Proc is a procedure that accepts an exact integer as its argument and
returns a character. Constructs a text of size len by calling proc on
each value from 0 (inclusive) to len (exclusive) to produce the
corresponding element of the text. The order in which proc is called
on those indexes is not specified.

Rationale: Although text-unfold is more general, text-tabulate is
likely to run faster for the common special case it implements.

## `(text-unfold stop? mapper successor seed [base make-final])`

This is a fundamental constructor for texts.

- successor is used to generate a series of "seed" values from the
  initial seed:

- seed, (successor seed), (successor2 seed), (successor3 seed), ...

- stop? tells us when to stop — when it returns true when applied to
  one of these seed values.

- mapper maps each seed value to the corresponding character(s) in the
  result text, which are assembled into that text in left-to-right
  order. It is an error for mapper to return anything other than a
  character, string, or text.

- base is the optional initial/leftmost portion of the constructed
  text, which defaults to the empty text (text). It is an error if
  base is anything other than a character, string, or text.

- make-final is applied to the terminal seed value (on which stop?
  returns true) to produce the final/rightmost portion of the
  constructed text. It defaults to (lambda (x) (text)). It is an error
  for make-final to return anything other than a character, string, or
  text.

text-unfold is a fairly powerful text constructor. You can use it to
convert a list to a text, read a port into a text, reverse a text,
copy a text, and so forth. Examples:

```scheme
    (port->text p) = (text-unfold eof-object?
                               values
                               (lambda (x) (read-char p))
                               (read-char p))

    (list->text lis) = (text-unfold null? car cdr lis)

    (text-tabulate f size) = (text-unfold (lambda (i) (= i size)) f add1 0)

    To map f over a list lis, producing a text:

    (text-unfold null? (compose f car) cdr lis)
```

Interested functional programmers may enjoy noting that
textual-fold-right and text-unfold are in some sense inverses. That
is, given operations knull?, kar, kdr, kons, and knil satisfying

```scheme
(kons (kar x) (kdr x)) = x  and  (knull? knil) = #t
```
then

```scheme
(textual-fold-right kons knil (text-unfold knull? kar kdr x)) = x
```

and

```scheme
(text-unfold knull? kar kdr (textual-fold-right kons knil text)) = text.
```

This combinator pattern is sometimes called an "anamorphism."

Note: Implementations should not allow the size of texts created by
text-unfold to be limited by limits on stack size.

## `(text-unfold-right stop? mapper successor seed [base make-final])`

This is a fundamental constructor for texts. It is the same as
text-unfold except the results of mapper are assembled into the text
in right-to-left order, base is the optional rightmost portion of the
constructed text, and make-final produces the leftmost portion of the
constructed text.

```scheme
    (text-unfold-right (lambda (n) (< n (char->integer #\A)))
                       (lambda (n) (char-downcase (integer->char n)))
                       (lambda (n) (- n 1))
                       (char->integer #\Z)
                       #\space
                       (lambda (n) " The English alphabet: "))
        => « The English alphabet: abcdefghijklmnopqrstuvwxyz »
```

## `(textual->text textual)`

When given a text, textual->text just returns that text. When given a
string, textual->text returns the result of calling string->text on
that string. Signals an error when its argument is neither string nor
text.

## `(textual->string textual [start end])`

## `(textual->vector textual [start end])`

## `(textual->list   textual [start end])`

textual->string, textual->vector, and textual->list return a newly
allocated (unless empty) mutable string, vector, or list of the
characters that make up the given subtext or substring.

## `(string->text string [start end])`

## `(vector->text char-vector [start end])`

## `(list->text   char-list [start end])`

These procedures return a text containing the characters of the given
substring, subvector, or sublist. The behavior of the text will not be
affected by subsequent mutation of the given string, vector, or list.

## `(reverse-list->text char-list)`

An efficient implementation of (compose list->text reverse):

```scheme
(reverse-list->text '(#\a #\B #\c)) → «cBa»
```

This is a common idiom in the epilogue of text-processing loops that
accumulate their result using a list in reverse order. (See also
textual-concatenate-reverse for the "chunked" variant.)


## `(textual->utf8    textual [start end])`

## `(textual->utf16   textual [start end])`

## `(textual->utf16be textual [start end])`

## `(textual->utf16le textual [start end])`

These procedures return a newly allocated (unless empty) bytevector
containing a UTF-8 or UTF-16 encoding of the given subtext or
substring.

The bytevectors returned by textual->utf8, textual->utf16be, and
textual->utf16le do not contain a byte-order mark
(BOM). textual->utf16be returns a big-endian encoding, while
textual->utf16le returns a little-endian encoding.

The bytevectors returned by textual->utf16 begin with a BOM that
declares an implementation-dependent endianness, and the bytevector
elements following that BOM encode the given subtext or substring
using that endianness.

Rationale: These procedures are consistent with the Unicode
standard. Unicode suggests UTF-16 should default to big-endian, but
Microsoft prefers little-endian.

## `(utf8->text    bytevector [start end])`

## `(utf16->text   bytevector [start end])`

## `(utf16be->text bytevector [start end])`

## `(utf16le->text bytevector [start end])`

These procedures interpret their bytevector argument as a UTF-8 or
UTF-16 encoding of a sequence of characters, and return a text
containing that sequence.

The bytevector subrange given to utf16->text may begin with a byte
order mark (BOM); if so, that BOM determines whether the rest of the
subrange is to be interpreted as big-endian or little-endian; in
either case, the BOM will not become a character in the returned
text. If the subrange does not begin with a BOM, it is decoded using
the same implementation-dependent endianness used by textual->utf16.

The utf16be->text and utf16le->text procedures interpret their inputs
as big-endian or little-endian, respectively. If a BOM is present, it
is treated as a normal character and will become part of the result.

It is an error if the bytevector subrange given to utf8->text contains
invalid UTF-8 byte sequences. For the other three procedures, it is an
error if start or end are odd, or if the bytevector subrange contains
invalid UTF-16 byte sequences.

## `(text-length text)`

Returns the number of characters within the given text. Must execute
in O(1) time.

## `(text-ref text idx)`

Returns character text[idx], using 0-origin indexing. Must execute in
O(1) time.

## `(textual-length textual)`

## `(textual-ref textual idx)`

textual-length returns the number of characters in textual, and
textual-ref returns the character at character index idx, using
0-origin indexing. These procedures are the generalizations of
text-length and text-ref to accept strings as well as texts. If
textual is a text, they must execute in O(1) time, but there is no
such requirement if textual is a string.

Rationale: These procedures may be more convenient than the text-only
versions, but compilers may generate faster code for calls to the
text-only versions.

## `(subtext    text start end)`

## `(subtextual textual start end)`

These procedures return a text containing the characters of text or
textual beginning with index start (inclusive) and ending with index
end (exclusive).

If textual is a string, then that string does not share any storage
with the result, so subsequent mutation of that string will not affect
the text returned by subtextual. When the first argument is a text, as
is required by subtext, implementations are encouraged to return a
result that shares storage with that text, to whatever extent sharing
is possible while maintaining some small fixed bound on the ratio of
storage used by the shared representation divided by the storage that
would be used by an unshared representation. In particular, these
procedures should just return their first argument when that argument
is a text, start is 0, and end is the length of that text.

## `(textual-copy textual [start end])`

Returns a text containing the characters of textual beginning with
index start (inclusive) and ending with index end (exclusive).

Unlike subtext and subtextual, the result of textual-copy never shares
substructures that would retain characters or sequences of characters
that are substructures of its first argument or previously allocated
objects.

If textual-copy returns an empty text, that empty text may be eq? or
eqv? to the text returned by (text). If the text returned by
textual-copy is non-empty, then it is not eqv? to any previously
extant object.

## `(textual-take       textual nchars)`

## `(textual-drop       textual nchars)`

## `(textual-take-right textual nchars)`

## `(textual-drop-right textual nchars)`

textual-take returns a text containing the first nchars of textual;
textual-drop returns a text containing all but the first nchars of
textual. textual-take-right returns a text containing the last nchars
of textual; textual-drop-right returns a text containing all but the
last nchars of textual.

If textual is a string, then that string does not share any storage
with the result, so subsequent mutation of that string will not affect
the text returned by these procedures. If textual is a text,
implementations are encouraged to return a result that shares storage
with that text (which is easily accomplished by using subtext to
create the result).

```scheme
(textual-take "Pete Szilagyi" 6) => «Pete S»
(textual-drop "Pete Szilagyi" 6) => «zilagyi»

(textual-take-right "Beta rules" 5) => «rules»
(textual-drop-right "Beta rules" 5) => «Beta »
```

It is an error to take or drop more characters than are in the text:

```scheme
(textual-take "foo" 37) => error
```

## `(textual-pad       textual len [char start end])`

## `(textual-pad-right textual len [char start end])`

Returns a text of length len comprised of the characters drawn from
the given subrange of textual, padded on the left (right) by as many
occurrences of the character char as needed. If textual has more than
len chars, it is truncated on the left (right) to length len. char
defaults to #\space.

If textual is a string, then that string does not share any storage
with the result, so subsequent mutation of that string will not affect
the text returned by these procedures. If textual is a text,
implementations are encouraged to return a result that shares storage
with that text whenever sharing would be space-efficient.

```scheme
(textual-pad     "325" 5) => «  325»
(textual-pad   "71325" 5) => «71325»
(textual-pad "8871325" 5) => «71325»
```

## `(textual-trim       textual [pred start end])`

## `(textual-trim-right textual [pred start end])`

## `(textual-trim-both  textual [pred start end])`

Returns a text obtained from the given subrange of textual by skipping
over all characters on the left / on the right / on both sides that
satisfy the second argument pred: pred defaults to char-whitespace?.

If textual is a string, then that string does not share any storage
with the result, so subsequent mutation of that string will not affect
the text returned by these procedures. If textual is a text,
implementations are encouraged to return a result that shares storage
with that text whenever sharing would be space-efficient.

```scheme
(textual-trim-both "  The outlook wasn't brilliant,  \n\r")
=> «The outlook wasn't brilliant,»
```

## `(textual-replace textual1 textual2 start1 end1 [start2 end2])`

Returns

```scheme
    (textual-append (subtextual textual1 0 start1)
                    (subtextual textual2 start2 end2)
                    (subtextual textual1 end1 (textual-length textual1)))
```

That is, the segment of characters in textual1 from start1 to end1 is
replaced by the segment of characters in textual2 from start2 to
end2. If start1=end1, this simply splices the characters drawn from
textual2 into textual1 at that position.

Examples:

```scheme
    (textual-replace "The TCL programmer endured daily ridicule."
                     "another miserable perl drone" 4 7 8 22)
        => «The miserable perl programmer endured daily ridicule.»

    (textual-replace "It's easy to code it up in Scheme." "lots of fun" 5 9)
        => «It's lots of fun to code it up in Scheme.»

    (define (textual-insert s i t) (textual-replace s t i i))

    (textual-insert "It's easy to code it up in Scheme." 5 "really ")
        => «It's really easy to code it up in Scheme.»

    (define (textual-set s i c) (textual-replace s (text c) i (+ i 1)))

    (textual-set "Text-ref runs in O(n) time." 19 #\1)
        => «Text-ref runs in O(1) time.»
```

## `(textual=? textual1 textual2 textual3 ...)`

Returns #t if all the texts have the same length and contain exactly
the same characters in the same positions; otherwise returns #f.

## `(textual<?  textual1 textual2 textual3 ...)`

## `(textual>?  textual1 textual2 textual3 ...)`

## `(textual<=? textual1 textual2 textual3 ...)`

## `(textual>=? textual1 textual2 textual3 ...)`

These procedures return #t if their arguments are (respectively):
monotonically increasing, monotonically decreasing, monotonically
non-decreasing, or monotonically non-increasing.

These comparison predicates are required to be transitive.

These procedures compare texts in an implementation-defined way. One
approach is to make them the lexicographic extensions to texts of the
corresponding orderings on characters. In that case, text<? would be
the lexicographic ordering on texts induced by the ordering char<? on
characters, and if two texts differ in length but are the same up to
the length of the shorter text, the shorter text would be considered
to be lexicographically less than the longer string. However,
implementations are also allowed to use more sophisticated
locale-specific orderings.

In all cases, a pair of texts must satisfy exactly one of textual<?,
textual=?, and textual>?, must satisfy textual<=? if and only if they
do not satisfy textual>?, and must satisfy textual>=? if and only if
they do not satisfy textual<?.

Note: Implementations are encouraged to use the same orderings for
texts as are used by the corresponding comparisons on strings, but are
allowed to use different orderings.

Rationale: The only portable way to ensure these comparison predicates
use the same orderings used by the corresponding comparisons on
strings is to convert all texts to strings, which would be
unacceptably inefficient.

## `(textual-ci=? textual1 textual2 textual3 ...)`

Returns #t if, after calling textual-foldcase on each of the
arguments, all of the case-folded texts would have the same length and
contain the same characters in the same positions; otherwise returns
#f.

## `(textual-ci<?  textual1 textual2 textual3 ...)`

## `(textual-ci>?  textual1 textual2 textual3 ...)`

## `(textual-ci<=? textual1 textual2 textual3 ...)`

## `(textual-ci>=? textual1 textual2 textual3 ...)`

These procedures behave as though they had called textual-foldcase on
their arguments before applying the corresponding procedures without
"-ci".

## `(textual-prefix-length textual1 textual2 [start1 end1 start2 end2])`

## `(textual-suffix-length textual1 textual2 [start1 end1 start2 end2])`

Return the length of the longest common prefix/suffix of textual1 and
textual2. For prefixes, this is equivalent to their "mismatch index"
(relative to the start indexes).

The optional start/end indexes restrict the comparison to the
indicated subtexts of textual1 and textual2.

## `(textual-prefix? textual1 textual2 [start1 end1 start2 end2])`

## `(textual-suffix? textual1 textual2 [start1 end1 start2 end2])`

Is textual1 a prefix/suffix of textual2?

The optional start/end indexes restrict the comparison to the
indicated subtexts of textual1 and textual2.

## `(textual-index       textual pred [start end])`

## `(textual-index-right textual pred [start end])`

## `(textual-skip        textual pred [start end])`

## `(textual-skip-right  textual pred [start end])`

textual-index searches through the given subtext or substring from the
left, returning the index of the leftmost character satisfying the
predicate pred. textual-index-right searches from the right, returning
the index of the rightmost character satisfying the predicate pred. If
no match is found, these procedures return #f.

Rationale: The SRFI 130 analogues of these procedures return cursors,
even when no match is found, and SRFI 130's string-index-right returns
the successor of the cursor for the first character that satisfies the
predicate. As there are no cursors in this SRFI, it seems best to
follow the more intuitive and long-standing precedent set by SRFI 13.

The start and end arguments specify the beginning and end of the
search; the valid indexes relevant to the search include start but
exclude end. Beware of "fencepost" errors: when searching
right-to-left, the first index considered is (- end 1), whereas when
searching left-to-right, the first index considered is start. That is,
the start/end indexes describe the same half-open interval [start,end)
in these procedures that they do in all other procedures specified by
this SRFI.

The skip functions are similar, but use the complement of the
criterion: they search for the first char that doesn't satisfy
pred. To skip over initial whitespace, for example, say

```scheme
    (subtextual text
                (or (textual-skip text char-whitespace?)
                    (textual-length text))
                (textual-length text))
```

These functions can be trivially composed with textual-take and
textual-drop to produce take-while, drop-while, span, and break
procedures without loss of efficiency.

## `(textual-contains       textual1 textual2 [start1 end1 start2 end2])`

## `(textual-contains-right textual1 textual2 [start1 end1 start2 end2])`

Does the subtext of textual1 specified by start1 and end1 contain the
sequence of characters given by the subtext of textual2 specified by
start2 and end2?

Returns #f if there is no match. If start2 = end2, textual-contains
returns start1 but textual-contains-right returns end1. Otherwise
returns the index in textual1 for the first character of the
first/last match; that index lies within the half-open interval
[start1,end1), and the match lies entirely within the [start1,end1)
range of textual1.

```scheme
(textual-contains "eek -- what a geek." "ee" 12 18) ; Searches "a geek"
=> 15
```

Note: The names of these procedures do not end with a question
mark. This indicates a useful value is returned when there is a match.

## `(textual-upcase   textual)`

## `(textual-downcase textual)`

## `(textual-foldcase textual)`

## `(textual-titlecase textual)`

These procedures return the text obtained by applying Unicode's full
uppercasing, lowercasing, case-folding, or title-casing algorithms to
their argument. In some cases, the length of the result may be
different from the length of the argument. Note that
language-sensitive mappings and foldings are not used.

## `(textual-append textual ...)`

Returns a text whose sequence of characters is the concatenation of
the sequences of characters in the given arguments.

## `(textual-concatenate textual-list)`

Concatenates the elements of textual-list together into a single text.

If any elements of textual-list are strings, then those strings do not
share any storage with the result, so subsequent mutation of those
string will not affect the text returned by this
procedure. Implementations are encouraged to return a result that
shares storage with some of the texts in the list if that sharing
would be space-efficient.

Rationale: Some implementations of Scheme limit the number of
arguments that may be passed to an n-ary procedure, so the (apply
textual-append textual-list) idiom, which is otherwise equivalent to
using this procedure, is not as portable.

## `(textual-concatenate-reverse textual-list [final-textual end])`

With no optional arguments, calling this procedure is equivalent to

```scheme
(textual-concatenate (reverse textual-list))
```

If the optional argument final-textual is specified, it is effectively
consed onto the beginning of textual-list before performing the
list-reverse and textual-concatenate operations.

If the optional argument end is given, only the characters up to but
not including end in final-textual are added to the result, thus
producing

```scheme
    (textual-concatenate
      (reverse (cons (subtext final-textual 0 end)
                     textual-list)))
```

For example:

```scheme
(textual-concatenate-reverse '(" must be" "Hello, I") "
    going.XXXX" 7) => «Hello, I must be going.» `` Rationale: This
    procedure is useful when constructing procedures that accumulate
    character data into lists of textual buffers, and wish to convert
    the accumulated data into a single text when done. The optional
    end argument accommodates that use case when final-textual is a
    mutable string, and is allowed (for uniformity) when final-textual
    is an immutable text.
```

## `(textual-join textual-list [delimiter grammar])`

This procedure is a simple unparser; it pastes texts together using the delimiter text.

textual-list is a list of texts and/or strings. delimiter is a text or a string. The grammar argument is a symbol that determines how the delimiter is used, and defaults to 'infix. It is an error for grammar to be any symbol other than these four:

- 'infix means an infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty text.

- 'strict-infix means the same as 'infix if the textual-list is non-empty, but will signal an error if given an empty list. (This avoids an ambiguity shown in the examples below.)

- 'suffix means a suffix or terminator grammar: insert the delimiter after every list element.

- 'prefix means a prefix grammar: insert the delimiter before every list element.

The delimiter is the text used to delimit elements; it defaults to a single space " ".

```scheme
    (textual-join '("foo" "bar" "baz"))
             => «foo bar baz»
    (textual-join '("foo" "bar" "baz") "")
             => «foobarbaz»
    (textual-join '("foo" "bar" "baz") «:»)
             => «foo:bar:baz»
    (textual-join '("foo" "bar" "baz") ":" 'suffix)
             => «foo:bar:baz:»

    ;; Infix grammar is ambiguous wrt empty list vs. empty text:
    (textual-join '()   ":") => «»
    (textual-join '("") ":") => «»

    ;; Suffix and prefix grammars are not:
    (textual-join '()   ":" 'suffix)) => «»
    (textual-join '("") ":" 'suffix)) => «:»
```

## `(textual-fold       kons knil textual [start end])`

## `(textual-fold-right kons knil textual [start end])`

These are the fundamental iterators for texts.

The textual-fold procedure maps the kons procedure across the given
text or string from left to right:

```scheme
(... (kons textual[2] (kons textual[1] (kons textual[0] knil))))
```

In other words, textual-fold obeys the (tail) recursion

```scheme
(textual-fold kons knil textual start end)
= (textual-fold kons (kons textual[start] knil) start+1 end)
```

The textual-fold-right procedure maps kons across the given text or
string from right to left:

```scheme
    (kons textual[0]
          (... (kons textual[end-3]
                     (kons textual[end-2]
                           (kons textual[end-1]
                                 knil)))))
```

obeying the (tail) recursion

```scheme
      (textual-fold-right kons knil textual start end)
    = (textual-fold-right kons (kons textual[end-1] knil) start end-1)
```

Examples:

```scheme
    ;;; Convert a text or string to a list of chars.
    (textual-fold-right cons '() textual)

    ;;; Count the number of lower-case characters in a text or string.
    (textual-fold (lambda (c count)
                    (if (char-lower-case? c)
                        (+ count 1)
                        count))
                  0
                  textual)
```

The textual-fold-right combinator is sometimes called a
"catamorphism."

## `(textual-map proc textual1 textual2 ...)`

It is an error if proc does not accept as many arguments as the number
of textual arguments passed to textual-map, does not accept characters
as arguments, or returns a value that is not a character, string, or
text.

The textual-map procedure applies proc element-wise to the characters
of the textual arguments, converts each value returned by proc to a
text, and returns the concatenation of those texts. If more than one
textual argument is given and not all have the same length, then
textual-map terminates when the shortest textual argument runs
out. The dynamic order in which proc is called on the characters of
the textual arguments is unspecified, as is the dynamic order in which
the coercions are performed. If any strings returned by proc are
mutated after they have been returned and before the call to
textual-map has returned, then textual-map returns a text with
unspecified contents; the textual-map procedure itself does not mutate
those strings.

Example:

```scheme
    (textual-map (lambda (c0 c1 c2)
                   (case c0
                    ((#\1) c1)
                    ((#\2) (string c2))
                    ((#\-) (text #\- c1))))
                 (string->text "1222-1111-2222")
                 (string->text "Hi There!")
                 (string->text "Dear John"))
         => «Hear-here!»
```

## `(textual-for-each proc textual1 textual2 ...)`

It is an error if proc does not accept as many arguments as the number
of textual arguments passed to textual-map or does not accept
characters as arguments.

The textual-for-each procedure applies proc element-wise to the
characters of the textual arguments, going from left to right. If more
than one textual argument is given and not all have the same length,
then textual-for-each terminates when the shortest textual argument
runs out.

## `(textual-map-index proc textual [start end])`

Calls proc on each valid index of the specified subtext or substring,
converts the results of those calls into texts, and returns the
concatenation of those texts. It is an error for proc to return
anything other than a character, string, or text. The dynamic order in
which proc is called on the indexes is unspecified, as is the dynamic
order in which the coercions are performed. If any strings returned by
proc are mutated after they have been returned and before the call to
textual-map-index has returned, then textual-map-index returns a text
with unspecified contents; the textual-map-index procedure itself does
not mutate those strings.

## `(textual-for-each-index proc textual [start end])`

Calls proc on each valid index of the specified subtext or substring,
in increasing order, discarding the results of those calls. This is
simply a safe and correct way to loop over a subtext or substring.

Example:

```scheme
    (let ((txt (string->text "abcde"))
          (v '()))
      (textual-for-each-index
        (lambda (cur) (set! v (cons (char->integer (text-ref txt cur)) v)))
        txt)
      v) => (101 100 99 98 97)
```

## `(textual-count textual pred [start end])`

Returns a count of the number of characters in the specified subtext
of textual that satisfy the given predicate.

## `(textual-filter pred textual [start end])`

## `(textual-remove pred textual [start end])`

Filter the given subtext of textual, retaining only those characters
that satisfy / do not satisfy pred.

If textual is a string, then that string does not share any storage
with the result, so subsequent mutation of that string will not affect
the text returned by these procedures. If textual is a text,
implementations are encouraged to return a result that shares storage
with that text whenever sharing would be space-efficient.

## `(textual-replicate textual from to [start end])`

This is an "extended subtext" procedure that implements replicated
copying of a subtext or substring.

textual is a text or string; start and end are optional arguments that
specify a subtext of textual, defaulting to 0 and the length of
textual. This subtext is conceptually replicated both up and down the
index space, in both the positive and negative directions. For
example, if textual is "abcdefg", start is 3, and end is6, then we
have the conceptual bidirectionally-infinite text

```scheme
        ...  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d ...
            -9 -8 -7 -6 -5 -4 -3 -2 -1  0 +1 +2 +3 +4 +5 +6 +7 +8 +9
```

textual-replicate returns the subtext of this text beginning at index
from, and ending at to. It is an error if from is greater than to.

You can use textual-replicate to perform a variety of tasks:

- To rotate a text left: (textual-replicate "abcdef" 2 8) => «cdefab»

- To rotate a text right: (textual-replicate "abcdef" -2 4) =>
  «efabcd»

- To replicate a text: (textual-replicate "abc" 0 7) => «abcabca»

Note that

- The from/to arguments give a half-open range containing the
  characters from index from up to, but not including, index to.

- The from/to indexes are not expressed in the index space of
  textual. They refer instead to the replicated index space of the
  subtext defined by textual, start, and end.

It is an error if start=end, unless from=to, which is allowed as a
special case.

## `(textual-split textual delimiter [grammar limit start end])`

Returns a list of texts representing the words contained in the
subtext of textual from start (inclusive) to end (exclusive). The
delimiter is a text or string to be used as the word separator. This
will often be a single character, but multiple characters are allowed
for use cases such as splitting on "\r\n". The returned list will have
one more item than the number of non-overlapping occurrences of the
delimiter in the text. If delimiter is an empty text, then the
returned list contains a list of texts, each of which contains a
single character.

The grammar is a symbol with the same meaning as in the textual-join
procedure. If it is infix, which is the default, processing is done as
described above, except an empty textual produces the empty list; if
grammar is strict-infix, then an empty textual signals an error. The
values prefix and suffix cause a leading/trailing empty text in the
result to be suppressed.

If limit is a non-negative exact integer, at most that many splits
occur, and the remainder of textual is returned as the final element
of the list (so the result will have at most limit+1 elements). If
limit is not specified or is #f, then as many splits as possible are
made. It is an error if limit is any other value.

To split on a regular expression re, use SRFI 115's regexp-split
procedure:

```scheme
(map string->text (regexp-split re (textual->string txt)))
```

Rationale: Although it would be more efficient to have a version of
regexp-split that operates on texts directly, the scope of this SRFI
is limited to specifying operations on texts analogous to those
specified for strings by R7RS and SRFI 130.
