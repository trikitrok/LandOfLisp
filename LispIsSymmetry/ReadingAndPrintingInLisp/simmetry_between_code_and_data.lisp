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

;; Despite its power and simplicity,
;; an experienced Lisper will only rarely use eval.
;; The bottom line is that the symmetry of data and code
;; in Lisp pretty much makes it the poster child of homoiconicity.
;; Quoting, quasiquoting, eval and macros allow you to take
;; advantage of this property in your code.

;; Warning: Inexperienced use of eval can pose a security risk.

;; Creating a repl

(defun a-repl ()
  (loop (print (eval (read)))))
; A-REPL

(a-repl)
