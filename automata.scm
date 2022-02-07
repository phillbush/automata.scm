; Left folding is a high-order function that recursively applies a
; left-associative procedure on a list of elements, beginning with
; an initial value.  We'll use folding to run a DFA.
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

; A Deterministic Finite Automaton (DFA) is made of three elements:
; - An initial state.
; - A procedure that determines whether a given state is a final state.
; - A procedure that, given a state and a symbol, gets the next state.
(define (make-dfa initstate isfinal nextstate)
  (list initstate isfinal nextstate))

; The usual selectors
(define (initstate dfa) (car dfa))
(define (isfinal dfa) (cadr dfa))
(define (nextstate dfa) (caddr dfa))

; To run a dfa on a string, just check if the result of left-folding the
; nextstate procedure on that string from its initial state results in a
; final state.
(define (dfa-run dfa string)
  ((isfinal dfa) (fold-left (nextstate dfa) (initstate dfa) string)))
