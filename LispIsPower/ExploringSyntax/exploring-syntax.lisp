;; Symbols are case insensitive
(eq 'fooo 'Fooo)
; T

;; Numbers
; Supports both floating-point numbers and integers.
(+ 1 1.0)
; 2.0

; Huge numbers
(expt 53 53)
; 24356848165022712132477606520104725518533453128685640844505130879576720609150223301256150373

; Rational numbers
(/ 4 6)
; 2/3

; But
(/ 4.0 6)
; 0.6666667

;; Strings, use double quotes
(princ "Tutti")
; Tutti
; "Tutti" <- This is because princ prints the string and then returns it

; Scape characters using backslash
(princ "He yelled \"Stop that thief!\"")
; He yelled "Stop that thief!"
; "He yelled \"Stop that thief!\""

; Common Lisp uses two modes when it reads your code: a code mode and a data mode.

; Code mode.
; Whenever you write something into the Lisp REPL, the compiler assumes that
; you're entering a command you want to execute, e.g., Lisp always assumes that
; you're writing code and defaults to code mode.
; Lisp expects code to be entered as a special type of list: a form.
; The first element of the list is treated as a function and applied on the
; rest of the elements of the list which are treated as the function parameters.
(expt 3 2)
; 9

; Data mode
; You need to single quote a list for it to be treated as data.
'(expt 3 2)
; (EXPT 3 2)

; Cons Cells and Lists
(cons 'chicken 'cow)
; (CHICKEN . COW) <- a cons cell, not a proper list

(cons 'chicken nil)
; (CHICKEN) <- a proper one element list

(cons 'chicken ()) ; () is the empty list
; (CHICKEN) <- idem

(cons 'chicken (cons 'cow nil))
; (CHICKEN COW) <- a proper two elements list

(list 'chicken 'cow)
; (CHICKEN COW)

; In Lisp a chain of cons cells and a list are exactly the same.

; car and cdr

(car '('proper 'pork 'meat)) ; <- car is "get first element"
; 'PROPER

(cdr '('proper 'pork 'meat)) ; <- cdr is get the list with all elements except the first one
; ('PORK 'MEAT)

; cadr
(car (cdr '('proper 'pork 'meat)))
; 'PORK

(cadr '('proper 'pork 'meat)) ; <- cadr is "get second element"
; 'PORK

; Nested lists
'(cat (duck bat) ant)
; (CAT (DUCK BAT) ANT)

; cdar
(cdar '((koko cat)(duck bat) ant))
; (CAT)

; cddr
(cddr '((peas carrots tomatoes) (pork beef chicken) ducks))
; (DUCKS)

; cddar
(cddar '((peas carrots tomatoes) (pork beef chicken) ducks))
; (TOMATOES)

; cadadr
(cadadr '((peas carrots tomatoes) (pork beef chicken) ducks))
; BEEF

; You can use any function with the name c*r out of the box up to 4 levels deep.

