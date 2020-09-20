# Tutorial

## Basics

### Continuation

After reading this section you will be able to write basic Scheme
programs. In particular, you will study:

- How to comment code

- How to write literals for builtin types

- How to call a procedure

- How to define a variable

- How to compare objects

- How to define a procedure

### How to comment code

You can comment code with the semi-colon, that is `;`.  Idiomatic code
use two semi-colons:

```scheme
;; Everything after one semi-colon is a comment.
```

The following sections will use two semi-colons with followed by an
arrow `=>` to describe the return value.

### How to write literals for builtin types

#### number

- Integers can be written as usual `42`
- Inexact reals can be written as usual `3.1415`
- There is more number types.  It is called the [Numerical
  tower](https://en.wikipedia.org/wiki/Numerical_tower)

#### boolean

- false: `#f`
- true: `#t`

#### characters

Characters can be written with their natural representation prefixed
with `#\\`, for instance the character `x` is represented in Scheme
code as follow:

```scheme
#\x
```

#### string

A string is written with double quotes, that is `"`, for instance:

```scheme
"hello world"
```

#### symbol

A symbol is most of the time written with a simple quote prefix, that
is `'`. For instance:

```scheme
'unique
```

#### pair

A pair of the symbol `'pi` and the value `3.1415` can be written as:

```scheme
'(pi . 3.1415)
```

#### list

A list can be written as literals separated by one space and enclosed
by parenthesis. For instance, the following list has three items:

```scheme
'(unique "hello world" (pi . 3.1415))`
```

The first item is the symbol `'unique`, the second item is a string,
the third item is a pair. The empty list is written `'()`.

#### vector

A vector looks somewhat like a list but without the explicit simple
quote. It use a hash prefix. For instance, the following vector has
three items:

```scheme
#(unique "hello world" 42)
```

The first item is the symbol `'unique`, the second item is a string,
the third item is a number.

#### bytevector

A bytevector is like vector but can contain only bytes.  It looks like
a list of integers, prefixed with `#vu8`. For instance, the following
bytevector has three bytes:

```scheme
#vu8(0 42 255)
```

### How to call a procedure

A procedure call looks like a list without the simple quote prefix.
The following describe the addition 21 and 21:

```scheme
(+ 21 21) ;; => 42
```

It returns `42`. So does the following multiplication:

```scheme
(* 21 2) ;; => 42
```

The first item is a procedure object.  Most of the time, procedure
names are made of letters separated with dashes.  That usually called
`kebab-case`.

Here is another procedure call:

```scheme
(string-append "hello" " " "world")
```

It will return a string `"hello world"`.

### How to define a variable

The first kind of variables that you encountered are procedures things
like `+`, `*` or `string-append`.

Variables can also contain constants. You can use `define`:

```scheme
(define %thruth 42)
```

The above code will create a variable called `%thruth` that contains
`42`.

Look at this very complicated computation:

```scheme
(+ %thruth 1 (* 2 647)) ;; => 1337
```

### How to compare objects

#### Identity equivalence

To compare by identity, in pratice, whether two object represent the
same memory location, you can use the procedure `eq?`.

In the case where you are comparing symbols you can use the procedure
`eq?`:

```scheme
(eq? 'unique 'unique) ;; => #t
(eq? 'unique 'singleton) ;; => #f
```

### Equivalence

If you do not know the type of the compared objects, or the objects
can be of different types, you can use the procedure `equal?`:

```scheme
(equal? #t "true") ;; => #f
```

The string `"true"` is not equivalent to the boolean `#t`.

It is rare to use `equal?`, because, usually, you know the type of the
compared objects and the compared object have the same type.

### Equivalence predicates

The astute reader might have recognized a pattern in the naming of the
equivalence procedures `eq?` and `equal?`: both end with a question
mark.  That is a convention that all procedures that can only return a
boolean should end with a question mark.  Those are called
*predicates*.

They are predicates for every builtin types.  For instance string type
has a string equivalence predicate written `string=?`:

```scheme
(string=? "hello" "hello world" "hello, world!") ;; => #f
```

The predicate procedure `string=?` will return `#t` if all arguments
are the same string, in the sense they contain the same characters.

### How to define a procedure

The simplest procedure ever, is the procedure that takes no argument
and returns itself:

```scheme
(define (ruse)
  ruse)
```

A procedure that takes not argument is called a *thunk*.  Indentation
and the newline are cosmetic conventions.  If you call the procedure
`ruse`, it will return `ruse`:

```scheme
(eq? ruse (ruse))
```

One can define a procedure that adds one as follow:

```scheme
(define (add1 number)
  (+ number 1))
```

The predicate to compare numbers is `=`.  Hence, the following:

```scheme
(= 2006 (add1 2005)) ;; => #t
```

Mind the fact that it returns a new number.  It does not mutate the
value even if it is passed as a variable.

Let's imagine a procedure that appends a name to the string `"Hello
"`. For instance, given `"Aziz"` or a variable containing `"Aziz"`, it
will return `"Hello Aziz"`.

```scheme
(define name "Aziz")

(define (say-hello name)
  (string-append "Hello " name))

(string=? "Hello Aziz" (say-hello name)) ;; => #t

;; XXX: the variable name still contains "Aziz"

(string=? name "Aziz")) ;; => #t
```

It does not matter for the callee whether the arguments are passed as
variables or literals:

```scheme
(string=? "Hello John"  (say-hello "John")) ;; => #t
```

### Backtrack

In this section you learned:

- How to comment code using a semi-colon character `;`

- How to write literals for builtin types

    - integer: `42`
    - float: `3.1415`
    - symbol: `'unique`
    - string: `"hello world"`
    - pair: `(pi . 3.1415)`
    - list: `'(42 "hello world" (pi . 3.1415))`
    - vector: `#(42 "hello world" (pi . 3.1415))`
    - bytevector: `#vu8(1 42 255)`

- How to call a procedure `(string-append "hello " "Aziz")`

- How to define a variable `(define %thruth 42)`

- How to compare objects using their type specific predicates. For
  instance: `(string=? "hello" "hello")`

- How to define a procedure again using `define` with slightly
  different syntax `(define (add1 number) (+ number 1))`


## Forward

### Continuation

After reading this section you will be able to write more complex
Scheme code.  In particular you will study:

- How to create lexical bindings

- How to set a variable

- How to do a `if`

- How to create a new type

- How to write a named-let

### How to create lexical bindings

Lexical bindings can be created with `let`, `let*`, `letrec` and
`letrec*`.  They have slightly different behaviors, but the same
syntax:

```scheme
(let (<binding> ...) <expression> ...)
```

Where `<binding>` looks like an association of a variable name with
the initial value it is holding.  For instance:

```scheme
(let ((a 1)
      (b 2))
  (+ a b 3)) ;; => 6
```

The above `let` form will bind `a` to `1`, `b` to `2` and return the
output of `(+ a b 3)` that is `6`.

### How to set a variable

To change what a variable holds without overriding it or mutating the
object contained in the varialbe, you can use `set!`.  Mind the
exclamation mark, it is a convention that forms that have a
side-effect ends with a exclamation mark. For instance:

```scheme
(define %thruth 42)

(display %truth)
(newline)

(set! %thruth 101)

(display %truth)
(newline)
```

### How to do a `if`

Scheme `if` will consider false, only the object `#f`. Hence, one can
do the following:

```scheme
(if #t
  (display "true")
  (display "never executed"))
```

Similarly:

```scheme
(if #f
  (display "never executed")
  (display "false"))
```

In particular, the number zero is true according to `if`:

```scheme
(if 0
  (display "zero is true")
  (display "never executed"))
```

If you want to check whether a value is zero you can use the predicate
`zero?` like so:

```scheme
(if (zero? %thruth)
   (display "%thruth is zero")
   (display "%thruth is not zero"))
```

Or the less idiomatic predicate `=`:

```scheme
(if (= %truth 0)
  (display "%thruth is zero")
  (display "%thruth is not zero"))
```

### How to create a new type

To create a new type you can use the macro `define-record-type`. For
instance, in a todo list application, we will need an `<item>` type
that can be defined as:

```scheme
(define-record-type <item>
  (make-item title body status)
  item?
  (title item-title item-title!)
  (body item-body item-body!)
  (status item-status item-status!))
```

Where:

- `<item>` is the record name,
- `make-item` is the constructor,
- `item?` is the predicate that allows to tell whether an object is a
  `<item>` type,
- `title`, `body` and `status` are fields with their associated
  getters and setters. Setters ends with an exclamation mark.  They
  will mutate the object.  Setters are optional.

Here is an example use of the above `<item>` definition:

```scheme
(define item (make-item "Learn Scheme" "The Scheme programming language is awesome, I should learn it" 'todo))

;; To change the status, one can do the following:

(item-status! item 'wip)

;; to get the title, one can do the following:

(display (item-title item))
(newline)
```

### How to write a named-let

A named-let allows to do recursion without going through the ceremony
of defining a separate procedure.  In pratice, it used in similar
contexts such as `for` or `while` loop in other languages.  Given the
procedure `(cons item lst)` that will return a new list with `LST` as
tail and `ITEM` as first item, study the following code:

```scheme
(let loop ((index 0)
           (out '())
  (if (= index 10)
      (display out)
      (loop (+ index 1) (cons index out))))
```

It is equivalent to the following:

```scheme
(define (loop index out)
  (if (= index 10)
      (display out)
      (loop (+ index 1) (cons index out))))

(loop 0 '())
```

A named-let, look like a `let` form that can be used to bind variables
prefixed with a name.  Here is some pseudo-code that describe the
syntax of the named-let form:

```scheme
(let <name> (<binding> ...) expression ...))
```

So `<binding>` and `<expression>` are very similar to a `let`.
`<name>` will be bound to a procedure that takes as many argument as
there is `<binding>` and its body will be `<expression> ...`.  It will
be called with the associated objects in `<binding> ...`. `expression`
can call `<name>` most likely in tail call position but not
necessarly.  If the named-let is not tail-recursive it is to be a
*grow the stack recursive call*.  Another way to see the named-let is
pseudo-code:

```scheme
(define <name> (lambda <formals> <expression> ...))

(<name> <arguments> ...)
```

Where:

- `<formals>` are the variable names from `<binding> ...`
- `<arguments>` are the initial object bound in `<binding> ...`

### Backtrack

- How to create lexical bindings with `let`, `let*`, `letrec` and
  `letrec*`,

- How to set a variable using `(set! %thruth 42)`,

- How to do a `if` with `(if %thruth (display "That is true") (display
  "That is false"))`,

- How to create a new type using `define-record-type` that can look
  like:

```scheme
(define-record-type <record-name>
  (make-record-name field0 ...)
  record-name?
  (field0 record-name-field0 record-name-field0!))
```

- How to write a named-let, for instance an infinite loop will look
  like:

```scheme
(let loop ((index 0))
  (display index)
  (loop (+ index 1)))
```

## Beyond

### Continuation

After reading this section you will be able to create libraries.

### Backtrack

## Elements of Style

<!--
- See scheme style guides
- See common lisp style guides
- See clojure style guides
- Better to avoid `eqv?` -->
