;; Functions are first-class values in Lisp
(defun half (n)
  (/ n 2))
; HALF

#'half
; #<FUNCTION HALF (N) (DECLARE (SYSTEM::IN-DEFUN HALF)) (BLOCK HALF (/ N 2))>

;; lambda
(lambda    ; <- Creates an anonymous function
  (n)      ; lambda's first parameter is a parameters list
  (/ n 2)) ; lambda's second parameter is the body of the function
; #<FUNCTION :LAMBDA (N) (/ N 2)>

;; Many functions in Lisp accept functions as parameters.
;; If you use these functions, you are using a tachnique called
;; higher-order functional programming.
(mapcar (lambda (n) (/ n 2)) '(2 4 6 8))
; (1 2 3 4)

;; All parameters to a Lisp function are evaluated before the function
;; itself is evaluated.
;; Because not all its parameters are evaluated, lambda is not an actual function.
;; It's something called a macro.
;; The actual value that lambda returns is a regular Lisp function.
;; When Lispers talk about lambda function they are talking about the functions
;; created using lambda, not about lambda itself.
;; The lambda form allows your code to take a conceptual leap: using functions as values.

;; The ability to pass around functions opens up all kind of conceptual possibilities
;; in the design of your programs. This style of programming that relies heavily
;; on passing functions as values is called higher-order functional programming.

;; In a purely mathematical sense , lambda is actually the only Lisp command there is.

;; Lisp was derived directly from a mathematical concept called lambda calculus. In short,
;; the lambda calculus is a theoretical programming language that contains only one command:
;; the lambda command.

;; The take command home is that the lambda special form is the most fundamental
;; command in a Lisp system, and the fundamental concept from which other functions
;; in Lisp derive. In fact, is the central concept from which the very idea of Lisp itself
;; originated.
