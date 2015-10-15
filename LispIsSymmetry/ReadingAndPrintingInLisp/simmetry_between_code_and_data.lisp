;; Lisp has a deeper symmetry.
;; It can treat program code and data interchangeably.
;; A programming language that uses the same data structures
;; ti store data and program code is called homoiconic.

'(+ 1 2) ; data mode (because of the quote)
; (+ 1 2)
(+ 1 2) ; code mode
; 3

(defparameter *foo* '(* 3 2))
; *FOO*
(eval *foo*)
; 6
