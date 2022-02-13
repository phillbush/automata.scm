(import (srfi 1)                ; for fold
        (srfi 113))             ; for set, set-union, and set-any

(define (make-set . list) (apply set eq? list))
(define empty-string '())
(define (is-empty-string? s) (null? s))

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

; regular expression
(define (make-union left right)  (list 'union left right))
(define (make-cat left right)  (list 'cat left right))
(define (make-star arg)  (list 'star arg))
(define (regexp-op regexp) (car regexp))
(define (regexp-arg regexp) (cadr regexp))
(define (regexp-left regexp) (cadr regexp))
(define (regexp-right regexp) (caddr regexp))

; run automaton
(define (automaton-run automaton string)

  (define (dfa-run)
    ((isfinal automaton)
     (fold (nextstate automaton) (initstate automaton) (string->list string))))

  (define (nfa-step symbol states)
    (set-fold
      set-union
      (make-set)
      (set-map
        eq?
        (lambda (state) ((nextstates automaton) symbol state))
        states)))

  (define (nfa-path symbol states)
    (if (not (is-empty-string? symbol))
        (nfa-step symbol (set-union states (nfa-path empty-string states)))
        (let ((newstates (nfa-step symbol states)))
          (if (set-empty? newstates)
              states
              (set-union states (nfa-path symbol newstates))))))

  (define (nfa-run)
    (set-any?
      (isfinal automaton)
      (nfa-path empty-string
        (fold
          nfa-path
          (make-set (initstate automaton))
          (string->list string)))))

  (cond ((eq? (automaton-type automaton) 'dfa) (dfa-run))
        ((eq? (automaton-type automaton) 'nfa) (nfa-run))))

(define (regexp->nfa regexp)
  (define (make-prototype initial final transitions) (list initial final transitions))
  (define (prototype-initial prototype) (car prototype))
  (define (prototype-final prototype) (cadr prototype))
  (define (prototype-transitions prototype) (caddr prototype))

  (define (make-transition state symbol final) (list state symbol final))
  (define (transition-state transition) (car transition))
  (define (transition-symbol transition) (cadr transition))
  (define (transition-final transition) (caddr transition))

  (let ((state-count 0))
    (define (rec expr)
      (cond ((null? expr)
             (make-prototype state-count empty-string (make-set)))
            ((not (pair? expr))
             (let ((initial state-count)
                   (final (+ state-count 1)))
               (set! state-count (+ state-count 2))
               (make-prototype
                 initial
                 final
                 (list (make-transition initial expr (make-set final))))))
            ((eq? (regexp-op expr) 'star)
             (let* ((arg (rec (regexp-arg expr)))
                    (initial state-count)
                    (final (+ state-count 1)))
               (set! state-count (+ state-count 2))
               (make-prototype
                 initial
                 final
                 (append
                   (prototype-transitions arg)
                   (list
                     (make-transition initial
                                      empty-string
                                      (make-set (prototype-initial arg) final))
                     (make-transition (prototype-final arg)
                                      empty-string
                                      (make-set (prototype-initial arg) final)))))))
            (else
             (let ((op (regexp-op expr))
                   (left (rec (regexp-left expr)))
                   (right (rec (regexp-right expr))))
               (cond
                 ((eq? 'cat op)
                  (make-prototype
                    (prototype-initial left)
                    (prototype-final right)
                    (append
                      (prototype-transitions left)
                      (prototype-transitions right)
                      (list
                        (make-transition
                          (prototype-final left)
                          empty-string
                          (make-set (prototype-initial right)))))))
                 ((eq? 'union op)
                  (let ((initial state-count)
                        (final (+ state-count 1)))
                    (set! state-count (+ state-count 2))
                    (make-prototype
                      initial
                      final
                      (append
                        (prototype-transitions left)
                        (prototype-transitions right)
                        (list
                          (make-transition
                            initial
                            empty-string
                            (make-set
                              (prototype-initial left)
                              (prototype-initial right)))
                          (make-transition
                            (prototype-final left)
                            empty-string
                            (make-set final))
                          (make-transition
                            (prototype-final right)
                            empty-string
                            (make-set final))))))))))))
    (define (next list)
      (lambda (symbol state)
        (if (null? list)
            (make-set)
            (let ((trans (car list))
                  (rest (cdr list)))
              (if (and (eq? (transition-symbol trans) symbol)
                       (eq? (transition-state trans) state))
                  (transition-final trans)
                  ((next rest) symbol state))))))
    (let ((prototype (rec regexp)))
      (make-nfa
        (prototype-initial prototype)
        (lambda (state) (eq? state (prototype-final prototype)))
        (next (prototype-transitions prototype))))))
