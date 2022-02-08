(import (srfi 1))               ; for fold, any, filter, etc

; srfi 1 should have a flatmap...
(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

; selectors
(define (automaton-type automaton) (car automaton))
(define (initstate automaton) (cadr automaton))
(define (isfinal automaton) (caddr automaton))
(define (nextstate automaton) (cadddr automaton))
(define (nextstates automaton) (cadddr automaton))

; A Deterministic Finite Automaton (DFA) is made of three elements:
; - An initial state.
; - A procedure that determines whether a given state is a final state.
; - A procedure that, given a state and a symbol, gets the next state.
(define (make-dfa initstate isfinal nextstate)
  (list 'dfa initstate isfinal nextstate))

; A Deterministic Finite Automaton (DFA) is made of three elements:
; - An initial state.
; - A procedure that determines whether a given state is a final state.
; - A procedure that, given a state and a symbol, gets a list of next states.
(define (make-nfa initstate isfinal nextstates)
  (list 'nfa initstate isfinal nextstates))

; run automaton
(define (automaton-run automaton string)

  (define (dfa-run automaton string)
    ((isfinal automaton)
     (fold (nextstate automaton) (initstate automaton) (string->list string))))

  (define (nfa-step symbol states)
    (filter
      (lambda (x) x)
      (flatmap
        (lambda (state) ((nextstates automaton) symbol state))
        states)))

  (define (nfa-run automaton string)
    (any
      (isfinal automaton)
      (fold
        (lambda (symbol states)
          (let ((states (lset-union eq? states (nfa-step '() states))))
            (nfa-step symbol states)))
        (list (initstate automaton))
        (string->list string))))

  (cond ((eq? (automaton-type automaton) 'dfa) (dfa-run automaton string))
        ((eq? (automaton-type automaton) 'nfa) (nfa-run automaton string))))
