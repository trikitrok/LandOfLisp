;; Another benefit of Lisp's simple way
;; of thinking about true and false

;; Any value in CommonLisp is true
;; except for false and the different
;; variations of nil: (), '(), 'nil, nil

;; This means that functions that are commonly
;; used in conditions have the option of returning
;; more than just the truth

(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list)
; ONE-IS-IN-THE-LIST

;; ok, but
(member 1 '(3 4 1 5))
; (1 5)

;; One function that really benefits from rich return values is find-if
(find-if #'oddp '(2 4 5 6)) ; <- HOF that finds the first element
; 5                         ;    in the list that satisfies the oddp predicate

(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number)
; THERE-IS-AN-ODD-NUMBER

;; find-if can fill dual roles: either as a retriever of values matching some constraint
;; or as a true/false value inside a condition

;; But in this case there's a use that breaks the symmetry
(find-if #'null '(5 nil 8 2)) ; <- null predicate returns true only for any of the nil values
; NIL
(if (find-if #'null '(5 nil 8 2))
    'there-is-a-nil
    'there-is-no-nil)
; THERE-IS-NO-NIL ; <- Not what we wanted!!