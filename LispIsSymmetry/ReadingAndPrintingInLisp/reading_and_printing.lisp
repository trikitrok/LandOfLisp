;; print
(print "foo")
; "foo"
; "foo"

(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))
; "this"
; "is"
; "a"
; "test"
; "test"

;; prin1
(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))
; "this""is""a""test"
; "test"

;; read
(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))
; SAY-HELLO
(say-hello)
; "Please type your name:" "koko"
; "Nice to meet you, "
; "koko"
; "koko"

;; When using print and read every value displayed and entered is surrounded by quotation marks.

;; Whenever you have an I/O task in Lisp you should ask yourself, "Can print or read do the job?"
;; You will save youtself a lot of trouble if you always use these two functions as your starting point.

;; Warning -> The read command can be dangerous if used in the wrong way,

;; Almost any conveivable type of data in Lisp can be printed and read using these commands,
;; without the slightest bit of loss along the way.

(defun add-five ()
  (print "please enter a number: ")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5)))) ; <- print used for numbers
; ADD-FIVE
(add-five)
; "please enter a number: " 8
; "When I add five I get"
; 13
; 13

(print '3)
; 3
; 3
(print '3.4)
; 3.4
; 3.4
(print 'foo)
; FOO
; FOO
(print '"foo")
; "foo"
; "foo"
(print '#\a)
; #\a
; #\a

;; Common Lisp symbols are not case-sensitive for most of strings.
;; However it's possible to create a case-sensitive symbols by
;; surrounding the symbol with a vertical pipe.
;; So the symbol |CaseSensitiveSymbol| will retain its case.
;; Symbols surrounded by vertical pipes can even contain puctutation.

;; princ -> print data in a way appealing to humans
(princ '3)
; 3
; 3
(princ '3.4)
; 3.4
; 3.4
(princ 'foo)
; FOO
; FOO
(princ '"foo")
; foo
; "foo"
(princ '#\a)
; a
; #\a

(progn (princ "This sentence will be interrupted")
       (princ #\newline)
       (princ "by an annoying newline character"))
; This sentence will be interrupted
; by an annoying newline character
; "by an annoying newline character"

;; By its nature, princ could be used to print any arbitrary output
;; of characters you want. This is fundamentally different from print.
;; print prints objects in such a way that they can always be read back
;; into their internal presentation. However this means that print can't
;; be used to generate an arbitrary bit of text.
;; On the other hand. princ can be used to print anything you want.
;; Therefor, although princ can print stuff in a way a human prefer,
;; it's a one-way street. Once wa have printed sth with princ, only a
;; human-like intelligence could decipher how to change it back into a
;; meaningful, appropiate Lisp data structure.

; read-line
(defun say-hello ()
  (princ "Please type your name: ")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))
; SAY-HELLO
(say-hello)
; Please type your name: Koko
; Nice to meet you, Koko
; "Koko"
