;; One part of Lisp that is not so beautiful,
;; involves the functions for comparing things.
;; There are many functions for doing it.
;; Of these equal, eql, eq, =, string-equal and equalp
;; are the most commonly used.

;; Conrad's rule of thumb for comparing stuff:
;; 1. Use eq to compare symbols.
;; 2. Use equal for everything else.

;; Symbols should always be compared to other symbols using eq
;; eq is the simplest of all the Lisp comparison functions, and
;; it's also very fast. It doesn't really work for comparing
;; items other than symbols.

(defparameter *fruit* 'apple)
; *FRUIT*

(cond ((eq *fruit* 'apple) 'it-is-an-apple)
      ((eq *fruit* 'orange) 'it-is-an-orange))
; IT-IS-AN-APPLE

;; If you're not using symbols just use equal.
;; equal works for the whole suit of Lisp datatypes:

;; Symbols (better use eq here)
(equal 'apple 'apple)
; T
;; Lists
(equal (list 2 3 4) (list 2 3 4))
; T
;; Identical lists created in different ways
(equal '(1 2 3) (cons 1 (cons 2 (cons 3 ()))))
; T
;; Integers
(equal 2 2)
; T
;; Floating point numbers
(equal 2.5 2.5)
; T
;; Strings
(equal "koko" "koko")
; T
;; Characters
(equal #\a #\a)
; T

;; eql is similar to eq, but unlike eq, it also
;; handles comparisons of numbers and characters
(eql 'foo 'foo)
; T
(eql 3.5 3.5)
; T
(eql #\v #\v)
; T

;; equalp is essentially the same as equal,
;; except that it can handle some difficult comparison
;; cases with a bit of extra sophistication.
;; For example, tt can compare strings with different
;; capitalizations and can compare integers against
;; floating-point numbers
(equalp "koko" "KoKo")
; T
(equalp 2 2.0)
; T

;; The remaining comparison operators are just specializations
;; for specific datatypes. Otherwise they are similar to equal.
;; = -> numbers
;; string-equal -> for strings
;; char-equal -> for characters
;; ...