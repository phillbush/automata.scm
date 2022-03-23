(import (srfi 1)                ; for fold
        (srfi 113))             ; for set, set-union, and set-any

(define (make-set . list) (apply set equal? list))
(define empty-string '())
(define (empty-string? s) (null? s))
(define end-symbol #f)
(define (end-symbol? s) (not s))

; selectors
(define (automaton-type automaton) (car automaton))
(define (automaton-initstate automaton) (cadr automaton))
(define (automaton-isfinal automaton) (caddr automaton))
(define (automaton-transition automaton) (cadddr automaton))

; A Deterministic Finite Automaton (DFA) is constructed from three
; elements:
; - An initial state.
; - A procedure that determines whether a given state is a final state.
; - A procedure that, given a state and a symbol, gets the next state.
(define (make-dfa automaton-initstate automaton-isfinal automaton-transition)
  (list 'dfa automaton-initstate automaton-isfinal automaton-transition))

; A Nondeterministic Finite Automaton (NFA) is just like a DFA, but the
; third element (automaton-transition) returns a list of next states given a state
; and either a symbol or nil.
(define (make-nfa automaton-initstate automaton-isfinal automaton-transition)
  (list 'nfa automaton-initstate automaton-isfinal automaton-transition))

; A Pushdown Automaton (PDA) is like a NFA, but the third element
; (automaton-transition) also manages a stack.
(define (make-pda automaton-initstate automaton-isfinal automaton-transition)
  (list 'pda automaton-initstate automaton-isfinal automaton-transition))

; regular expression constructors and selectors
(define (make-union left right) (list 'union left right))
(define (make-cat left right) (list 'cat left right))
(define (make-star arg) (list 'star arg))
(define (regexp-op regexp) (car regexp))
(define (regexp-arg regexp) (cadr regexp))
(define (regexp-left regexp) (cadr regexp))
(define (regexp-right regexp) (caddr regexp))

; This procedure call a automaton-runner subprocedure depending on the
; type of the automaton.  Run an automaton on a string means to test
; whether the automaton recognizes the string.
(define (automaton-run automaton string)

  (define (dfa-run)
    ((automaton-isfinal automaton)
     (fold (automaton-transition automaton)
           (automaton-initstate automaton)
           (string->list string))))

  (define (nfa-map symbol states)
    (set-fold
      set-union
      (make-set)
      (set-map
        equal?
        (lambda (state) ((automaton-transition automaton) symbol state))
        states)))

  (define (nfa-step symbol states)
    (let ((nextstates (nfa-map symbol states)))
      (if (set-empty? nextstates)
          states
          (set-union nextstates (nfa-step empty-string nextstates)))))

  (define (nfa-run)
    (set-any?
      (automaton-isfinal automaton)
      (fold
        nfa-step
        (make-set (automaton-initstate automaton))
        (cons empty-string (string->list string)))))

  (define (pda-map symbol transitions)
    (set-fold
      set-union
      (make-set)
      (set-map
        equal?
        (lambda (transition)
          ((automaton-transition automaton) symbol (car transition) (cdr transition)))
        transitions)))

  (define (pda-step symbol transitions)
    (let ((nexttransitions (pda-map symbol transitions)))
      (if (set-empty? nexttransitions)
          (make-set)
          (set-union nexttransitions (pda-step empty-string nexttransitions)))))

  (define (pda-run)
    (set-any?
      (lambda (transition)
        ((automaton-isfinal automaton) (car transition)))
      (fold
        pda-step
        (make-set (cons (automaton-initstate automaton) (list '())))
        (cons empty-string (string->list string)))))

  (cond ((eq? (automaton-type automaton) 'dfa) (dfa-run))
        ((eq? (automaton-type automaton) 'nfa) (nfa-run))
        ((eq? (automaton-type automaton) 'pda) (pda-run))))

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
      (cond ((or (empty-string? expr) (not (pair? expr)))
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
            ((eq? (regexp-op expr) 'cat)
             (let ((left (rec (regexp-left expr)))
                   (right (rec (regexp-right expr))))
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
                       (make-set (prototype-initial right))))))))
            ((eq? (regexp-op expr) 'union)
             (let ((left (rec (regexp-left expr)))
                   (right (rec (regexp-right expr))))
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
                         (make-set final))))))))))

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
