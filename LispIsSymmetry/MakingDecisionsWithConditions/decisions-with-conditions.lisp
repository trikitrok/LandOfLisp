;; Empty equals false when used in a boolean context

(if '()
    'i-am-true
    'i-am-false)
; I-AM-FALSE

(if '(1)
    'i-am-true
    'i-am-false)
; I-AM-TRUE

; Because we can easily detect an empty list,
; we can process lists using recursion.
; Many Lisp functions are "list eaters"
(defun my-length (ls)
  (if ls
      (1+ (my-length (cdr ls)))
      0))
; MY-LENGTH
(my-length '(list with four symbols))
; 4

;; The four disguises of ()
(eq '() nil)
; T
(eq '() ())
; T
(eq '() 'nil)
; T
; Any value not equivalent to an empty list would be considered a true value.

;; The conditionals: if and beyond
; if

(if (= (+ 1 2) 3)
    'yup
    'nope)
; YUP

(if (= (+ 3 2) 3)
    'yup
    'nope)
; NOPE

; It can also be used to check if a list is empty

(if '(1)
    'the-list-has-stuff
    'the-list-is-empty)
; THE-LIST-HAS-STUFF

(if ()
    'the-list-has-stuff
    'the-list-is-empty)
; THE-LIST-IS-EMPTY

(if (oddp 5) ; <- the p in oddp is for predicate
    'odd-number
    'even-number)
; ODD-NUMBER

(if (oddp 5)
    'odd-number
    (/ 1 0)) ; <- (/ 1 0) is never evaluated
; ODD-NUMBER

; if it's not a just a function,
; it's a special form, meaning it's evaluated
; in a particular way
(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t) ; <- progn allows you to do more than one thing in a branch
           'odd-number)              ; only the last evaluation is returned from progn
    'even-number')
; ODD-NUMBER
*number-was-odd*
; T

; Alternatives to if

; when
; Includes an implicit progn
; all the enclosed expressions are evaluated when
; the condition is true
(defvar *number-was-odd* nil)
(when (oddp 5)
  (setf *number-was-odd* t)
  'odd-number)
; ODD_NUMBER

; unless
; Includes an implicit progn
; all the enclosed expressions are evaluated when
; the condition is false
(unless (oddp 4)
  (setf *number-was-odd* nil)
  'even-number)
; EVEN_NUMBER

; Both return nil when the condition evaluates in the opposite way,
; they just return nil and do nothing.

;; cond
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry)
         (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien'- you ate my pudding))
        ((eq person 'johny)
         (setf *arch-enemy* 'useless-old-johny)
         '(i hope you choked on my pudding johny))
        (t ; <- default case
          '(why you eat my pudding stranger?))))
; PUDDING-EATER
(pudding-eater 'koko)
; (WHY YOU EAT MY PUDDING STRANGER?)
*arch-enemy*
; NIL
(pudding-eater 'johny)
; (I HOPE YOU CHOKED ON MY PUDDING JOHNY)
*arch-enemy*
; USELESS-OLD-JOHNY
(pudding-eater 'henry)
; (CURSE YOU LISP ALIEN '- YOU ATE MY PUDDING)
*arch-enemy*
; STUPID-LISP-ALIEN

; case
; When you're using eq to compare with the same symbol
; in all cond branches it is a lot easier to use case
(defun pudding-eater (person)
  (case person
        ((henry)
         (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien'- you ate my pudding))
        (('johny)
         (setf *arch-enemy* 'useless-old-johny)
         '(i hope you choked on my pudding johny))
        (otherwise ; <- default case
          '(why you eat my pudding stranger?))))
(pudding-eater 'koko)
; (WHY YOU EAT MY PUDDING STRANGER?)
(pudding-eater 'henry)
; (CURSE YOU LISP ALIEN '- YOU ATE MY PUDDING)
(pudding-eater 'johny)
; (WHY YOU EAT MY PUDDING STRANGER?)

; Warning: because the case command uses eq for comparisons,
; it is usually used only for branching on symbol values.
; It can't be used to branch on string values, among other things.
