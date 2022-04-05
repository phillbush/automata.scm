(import (srfi 1)                ; for fold
        (srfi 113))             ; for set, set-union, and set-any

; a pd-pair is a pair of (state . stack), it is used, along with the
; current index in the input string, to describe the current state in
; a pushdown automata.  We compare the state (ie, the cars) of pd-pairs.
; We use sets to agregate alphabetic symbols (characters), machine
; states (symbols) and pd-pairs (pairs); therefore, our comparator for
; make-set should check if the elements are pd-pairs, and compare the
; cars of both elements, or compare both elements themselves otherwise.
(define (comparator a b)
   (if (and (pair? a) (pair? b))
       (eq? (car a) (car b))
       (eq? a b)))
(define (make-set . list) (apply set comparator list))
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
; (automaton-transition) also manages a stack.  We also add some
; constructors and selectors to handle a transition (a pair of state
; and stack).
;
; Note that the PDA does not need to push the end-of-stack symbol onto
; the stack because it is already initialized with it.
(define (make-pda automaton-initstate automaton-isfinal automaton-transition)
  (list 'pda automaton-initstate automaton-isfinal automaton-transition))
(define (pd-pair? pd-pair) (pair? pd-pair))
(define (make-pd-pair state stack) (cons state stack))
(define (pd-state pd-pair) (car pd-pair))
(define (pd-stack pd-pair) (cdr pd-pair))

; regular expression constructors and selectors
(define (make-union left right) (list 'union left right))
(define (make-cat left right) (list 'cat left right))
(define (make-star arg) (list 'star arg))
(define (regexp-op regexp) (car regexp))
(define (regexp-arg regexp) (cadr regexp))
(define (regexp-left regexp) (cadr regexp))
(define (regexp-right regexp) (caddr regexp))

; context-free grammar constructors and selectors
(define (make-rule sym str) (cons sym str))
(define (rule-sym rule) (car rule))
(define (rule-str rule) (cdr rule))
(define (make-cfg start . rules) (cons start rules))
(define (cfg-start cfg) (car cfg))
(define (cfg-rules cfg) (cdr cfg))

; This procedure call a automaton-runner subprocedure depending on the
; type of the automaton.  Running an automaton on a string means to test
; whether the automaton recognizes the string.
(define (automaton-run automaton string)

  (define (dfa-run)
    ((automaton-isfinal automaton)
     (fold (automaton-transition automaton)
           (automaton-initstate automaton)
           (string->list string))))

  ; a description is either a state or a pd-pair
  (define (nfa-map symbol descriptions)
    (set-fold
      set-union
      (make-set)
      (set-map
        equal?
        (lambda (description) ((automaton-transition automaton) symbol description))
        descriptions)))

  (define (nfa-step symbol descriptions)
    (if (not (empty-string? symbol))
        (nfa-map symbol (set-union descriptions (nfa-step empty-string descriptions)))
        (let ((newdescriptions (nfa-map symbol descriptions)))
          (if (set-empty? newdescriptions)
              descriptions
              (set-union
                descriptions
                (nfa-step
                  symbol
                  (set-difference newdescriptions descriptions)))))))

  (define (nfa-run)
    (set-any?
      (automaton-isfinal automaton)
      (nfa-step
        empty-string
        (fold
          nfa-step
          (make-set (automaton-initstate automaton))
          (string->list string)))))

  ; PDAs are non-deterministic automata, and use the nfa-step and
  ; nfa-map procedures.
  (define (pda-run)
    (set-any?
      (lambda (pd-pair)
        ((automaton-isfinal automaton) (pd-state pd-pair)))
      (nfa-step
        empty-string
        (fold
          nfa-step
          (make-set (make-pd-pair (automaton-initstate automaton) (list end-symbol)))
          (string->list string)))))

  (cond ((eq? (automaton-type automaton) 'dfa) (dfa-run))
        ((eq? (automaton-type automaton) 'nfa) (nfa-run))
        ((eq? (automaton-type automaton) 'pda) (pda-run))))

; regular grammar to nondeterministic finite automaton conversion procedure
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

; context-free grammar to pushdown automaton conversion procedure
(define (cfg->pda cfg)

  (define (transitions rules stack)
    (if (null? rules)
        (make-set)
        (let ((sym (rule-sym (car rules)))
              (str (rule-str (car rules)))
              (rest (cdr rules)))
          (if (eq? sym (car stack))
              (set-adjoin
                (transitions rest stack)
                (make-pd-pair 1 (append (string->list str) (cdr stack))))
              (transitions rest stack)))))

  (make-pda
    0
    (lambda (state) (eq? state 2))
    (lambda (symbol pd-pair)
      (let ((state (pd-state pd-pair))
            (stack (pd-stack pd-pair)))
        (cond ((and (eq? state 0) (empty-string? symbol))
               (make-set (make-pd-pair 1 (cons (cfg-start cfg) stack))))
              ((and (eq? state 1) (empty-string? symbol) (end-symbol? (car stack)))
               (make-set (make-pd-pair 2 (cdr stack))))
              ((and (eq? state 1) (not (empty-string? symbol)) (eq? symbol (car stack)))
               (make-set (make-pd-pair 1 (cdr stack))))
              ((and (eq? state 1) (empty-string? symbol))
               (transitions (cfg-rules cfg) stack))
              (else (make-set)))))))
