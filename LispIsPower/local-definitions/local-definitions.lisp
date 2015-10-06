; Local variables
; (let (variable declarations)
;   ...body...)

(let
  ((a 5)
   (b 8))
  (+ a b))

; Local functions
; (flet ((function_name (arguments)
;          ...function body...))
;   ...body...)
; with flet is not possible to use
; a previouly defined local function
M from another local function

(flet
  ((f (n)
      (+ n 10))
   (g (n)
      (- n 3)))
  (g (f 5)))

; Altenative to use previously defined local functions from
; another local function -> labels
; It also allows recursively calling local functions.

(labels
  ((a (n)
      (+ n 5))
   (b (n)
      (+ (a n) 6)))
  (b 10))
