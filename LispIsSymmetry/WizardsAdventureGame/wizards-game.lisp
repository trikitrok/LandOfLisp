;; By keeping your source data structures free from assumptions
;; regarding output format from the start, your coding can take
;; full advantage of your programming language.
;; Since the easiest things to manipulate in Lisp are symbols
;; and lists, most experienced Lisp programmers will try to
;; focus on these datatypes.

;; Association lists or alists.

(defparameter
  *nodes*
  '((living-room
      (you are in the living room.
           a wizard is snoring loudly on the couch.))
    (garden
      (you are in a beautiful garden.
           there is a well in front of you.))
    (attic
      (you are in the attic.
           there is a giant welding torch in the corner.))))

;; Functional programming style.
;; In this style, a function will reference only parameters
;; or variables declared in the function itself, and it will
;; do nothing besides return a value.
;; By writing functions that don't reference variables in the
;; "outside world" directly and that don't perform any actions
;; other than returning a value, you can write code that can
;; easily be tested in isolation.

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter
  *edges*
  '((living-room
      (garden west door)
      (attic upstairs ladder))
    (garden
      (living-room east door))
    (attic
      (living-room downstairs ladder))))

;; Quasiquoting -> `,
;; It's a Lisp feature that allows to create chunks
;; of data that have small pieces of Lisp code
;; embedded in them.
;; To enable quasiquoting, you must use a backquote -> `
;; to switch from code mode to data mode.
;; Both the single quote and the backquote in Lisp flip
;; a piece of code into data mode, but only a backquote
;; can also be unquotedusing the comma character -> ,
;; to flip back into code mode.

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; mapcar
;; This function is used frequently by Lispers. It takes
;; another function and a list, and then applies the function
;; to every member of the list.
;; Like Clojure's map.
;; It's a HOF.

(defun describe-paths (location edges)
  (apply #'append
         (mapcar #'describe-path
                 (cdr (assoc location edges)))))

;; #'
;; This symbol sequence is a shothand for the function
;; operator. The Lisp reader will convert
;; (mapcar #'describe-path (cdr (assoc location edges)))
;; into
;; (mapcar (function describe-path) (cdr (assoc location edges)))
;; Common Lisp requires you to use the function operator when referring
;; to a function as a value directly like this, because the name
;; of a function may conflict with other named items in a program,
;; causing unpredictable errors.
;; Common Lisp tracks functions names differently from variable names.
;; It has multiple namespaces, including one for variables and another
;; for functions.
;; Scheme the other popular dialect does not force you to mark functions
;; with a function operator when using them as values. Neither does Clojure.
;; Scheme and Clojure have only one namespace for both functions and variables.
;; This is why Scheme is called a Lisp-1, whereas Common Lisp is sometimes referred
;; to as a Lisp-2.

;; append.
;; This function joins several lists into one big list.

;; apply.
;; You pass it a function and a list of objects, and it pretends
;; that the items in the list are separate objects and passes them
;; to the given function as such.

;; assoc.
;; It's used to look up the correct locationfrom the edge alist.
;; It retrieves both the key and the value.
;; Watch out: its semantic is completely different from Clojur's assoc one

(defparameter
  *objects*
  '(whiskey bucket frog chain))

(defparameter
  *object-locations*
  '((whiskey living-room)
    (bucket living-room)
    (chain garden)
    (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ; <- allows you to define function locally, like at-loc-p
    ((at-loc-p (obj) ; <- it's a predicate (ending with p is the convention for predicates)
               (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels
    ((describe-obj (obj)
                   `(you see a ,obj on the floor.)))
    (apply #'append
           (mapcar #'describe-obj
                   (objects-at loc objs obj-locs)))))

(defparameter
  *location*
  'living-room)

;; look function is not in the functional programming style.
;; because it uses global variables

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;; It happens the same with the walk function

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr))) ; <- passing in a keyword parameter
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

;; A keyword parameter has two parts:
;; * The first is the name, which begins with a colon.
;; * The second is the value

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

;; member.
;; It is used to check if the object is indeed on the list returned
;; by objects-at

;; push.
;; It adds a new item to the front of a list.
;; It's basically a convenient function written on top of setf.
;; Only the most recent value will be seen by the member function.
;; Using the push and assoc functions together in this way allows us
;; to pretend that values in an alist are changing, while still
;; preserving old values.

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))