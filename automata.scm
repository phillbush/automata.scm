(import (srfi 1))               ; for fold, any, filter, etc

; selectors
(define (automaton-type automaton) (car automaton))
(define (initstate automaton) (cadr automaton))
(define (isfinal automaton) (caddr automaton))
(define (nextstate automaton) (cadddr automaton))
(define (nextstates automaton) (cadddr automaton))

; A Deterministic Finite Automaton (DFA) is constructed from three
; elements:
; - An initial state.
; - A procedure that determines whether a given state is a final state.
; - A procedure that, given a state and a symbol, gets the next state.
(define (make-dfa initstate isfinal nextstate)
  (list 'dfa initstate isfinal nextstate))

; A Nondeterministic Finite Automaton (NFA) is just like a DFA, but the
; third element (nextstates) returns a list of next states given a state
; and either a symbol or nil.
(define (make-nfa initstate isfinal nextstates)
  (list 'nfa initstate isfinal nextstates))

; run automaton
(define (automaton-run automaton string)

  (define (dfa-run)
    ((isfinal automaton)
     (fold (nextstate automaton) (initstate automaton) (string->list string))))

  (define (nfa-step symbol states)
    (reduce
      append
      '()
      (map
        (lambda (state) ((nextstates automaton) symbol state))
        states)))

  (define (nfa-run)
    (any
      (isfinal automaton)
      (fold
        (lambda (symbol states)
          (let ((states (reduce append '() (list states (nfa-step '() states)))))
            (nfa-step symbol states)))
        (list (initstate automaton))
        (string->list string))))

  (cond ((eq? (automaton-type automaton) 'dfa) (dfa-run))
        ((eq? (automaton-type automaton) 'nfa) (nfa-run))))
