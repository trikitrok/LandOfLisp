;; or & and are logical operators
(and (oddp 5) (oddp 7) (oddp 9)
; T

(or (oddp 5) (oddp 4) (oddp 6))
; T

;; but, they can also be used for conditional behavior
(defparameter *is-it-even* nil)
; *IS-IT-EVEN*
*is-it-even*
; NIL
(or (oddp 4) (setf *is-it-even* t)) ; <- changes *is-it-even*
; T
*is-it-even*
; T
(defparameter *is-it-even* nil)
; *IS-IT-EVEN*
*is-it-even*
; NIL
(or (oddp 5) (setf *is-it-even* t)) ; <- does not change *is-it-even*
; T
*is-it-even*
; NIL

;; Lisp uses shortcut Boolean evaluation in and & or.
;; This means that
(if *file-modified*
    (if (ask-user-about-saving)
        (save-file)))
;; is equivalent to
(and *file-modified* (ask-user-about-saving) (save-file))

;; "Using this cleaner style for evaluating conditional code
;; is possible only if you think beyond the typical use of
;; boolean operators as simply logical operators.
;; This form has an elegant symmetry between the three expressions,
;; which some lispers may like. However, others would argue that
;; a reader of your code may easily miss the fact that save-file
;; does something beyond returning a boolean value."
;; A third version would be
(if (and *file-modified*
         (ask-user-about-saving)) ; <- a bit clearer
    (save-file))


