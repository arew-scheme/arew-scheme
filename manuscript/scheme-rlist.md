# `(scheme rlist)`

This library is based on
[SRFI-101](https://srfi.schemers.org/srfi-101/).

Random-access lists [1] are a purely functional data structure for
representing lists of values. A random-access list may act as a drop
in replacement for the usual linear-access pair and list data
structures (pair?, cons, car, cdr), which additionally supports fast
index-based addressing and updating (list-ref, list-set). The impact
is a whole class of purely-functional algorithms expressed in terms of
index-based list addressing become feasible compared with their
linear-access list counterparts.

This document proposes a library API for purely functional
random-access lists consistent with the R6RS [2] base library and list
utility standard library [3].
