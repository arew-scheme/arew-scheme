
;;> Simple generic function interface.

(library (generic)
  (export define-generic #;define-method make-generic generic-add!)
  (import (chezscheme))

  (include "generic.scm"))
