(require math) ; to have mean

;; this is the format of a machine
;; #(h (h h h h h h h h h h))
;; this machine dont have states,
;; it has 2 part: head and body

;; the body part of it has 10 slots
;; 1 slot for an initial strategy
;; and 9 slots for switching rules.

;; the switching rules read:
;; if previous outcome is HH -> this round i play H for sure
;; and so on for previous outcome of HM, HL, MH, MM, ML, LH, LM, LL
;; each slot can have 3 possible strategy: H, M or L
;; with probability 1 (so we call this deterministic machine)
;; -> there are possible 3^10 machines

;; the head of the machine is a slot
;; for the current/previous strategy.
;; in round 1, i'll set the current strategy
;; into that slot and match automata.
;; in round 2, i can recall
;; the previous strategies to set new strategy.

;; if we code strategy with letters, we'll have to convert
;; h -> 8 anyway. so i define strategy by number:
;; h = 2
;; m = 1
;; l = 0

;; now the mass production of machines looks like this
;; #(0 (0 0 0 0 0 0 0 0 0 0))
;; #(0 (0 0 0 0 0 0 0 0 0 1))
;; #(0 (0 0 0 0 0 0 0 0 0 2))
;; #(0 (0 0 0 0 0 0 0 0 1 0))
;; ...

;; if we just look at the body (main) part of the machines
;; they form base3 numbers.
;; when i convert these base3 into base10,
;; i have 0 1 2 3 4 5...until 3^10.

;; this means that if i generate a list of 3^10 machines
;; the position of each machine can be its name.
;; AND if i translate its name (position) from base10
;; into base3, i have its DNA!!!

;; here are some famous and sophisticated machines
(define (h) (vector 2 (list 2 2 2 2 2 2 2 2 2 2)))
(define (m) (vector 1 (list 1 1 1 1 1 1 1 1 1 1)))
(define (l) (vector 0 (list 0 0 0 0 0 0 0 0 0 0)))
(define (a) (vector 1 (list 1 0 1 2 0 1 2 0 1 2))) ; accommodator

(define M 10) ; how many machines to mass produce?

(define (create-body n)
  (~r n #:base 3 #:min-width 10 #:pad-string "0"))

;; this creates:
;; "0000000000"
;; "0000000001"
;; "0000000002"
;; ...
;; the first number is initial strategy,
;; the rest is switching rules

(define (separate-string a-string)
  (map string->number (map ~a (string->list a-string))))

;; R is only a list of all body parts of machines
;; i define them just to know
(define R
  (for/list ([n M])
    (separate-string (create-body n))))

;; we append into each body  a place-holder (head)
;; which holds  current (or previous) strategy

(define (mass-produce p)
  (for/list ([n p])
    (vector 0 (separate-string (create-body n)))))

`(define A (mass-produce M))

;; in this world, do they need name?
;; their name = their position in A (litterally)
;; their dna is base3 of their name (base10)

;; MATCH
;; we translate outcome 0, 1, 2 (h, m, l) into payoff 2, 5, 8
(define (pay? s)
  (cond [(zero? s) 2]
        [(equal? s 1) 5]
        [(equal? s 2) 8]
        [else 0]))

(define (match-strat s1 s2)
  (if (<= (+ s1 s2) 2)
      (map pay? (list s1 s2))
      (map pay? (list -1 -1))))
;; because we are matching 0, 1, 2 so the outcome is 0, 1, 2
;; (not 2 5 8 or h m l)
;; for example, s1=0, s2=2
;; s1+s2=0+2=2<=2 -> we list (0,2)
;; if s1+s2>2, i let the outcome be -1, -1
;; so that we can distinguish it with the outcome
;; s1=s2=0 (low)
;; after having the outcome 0 1 2, we need to translate them into payoff 2 5 8

(define (vector-first a-vector)
  (vector-ref a-vector 0))
(define (vector-second a-vector)
  (vector-ref a-vector 1))

(define (pre-strat auto)
  (vector-first auto))
(define (body auto)
  (vector-second auto))
(define (init-strat auto)
  (first (body auto)))
(define (switching-rules auto)
  (rest (body auto)))

(define (hh? a-list)
  (equal? '(2 2) a-list))
(define (hm? a-list)
  (equal? '(2 1) a-list))
(define (hl? a-list)
  (equal? '(2 0) a-list))
(define (mh? a-list)
  (equal? '(1 2) a-list))
(define (mm? a-list)
  (equal? '(1 1) a-list))
(define (ml? a-list)
  (equal? '(1 0) a-list))
(define (lh? a-list)
  (equal? '(0 2) a-list))
(define (lm? a-list)
  (equal? '(0 1) a-list))
(define (ll? a-list)
  (equal? '(0 0) a-list))

;; this is to set the next move into the place of current move (head of machine)
;; according to its rules in the body about each previous outcome
(define (set-cond-move! a1 a2)
  (let ([pre-list (list (pre-strat a1) (pre-strat a2))]
        [rule-1 (switching-rules a1)]
        [rule-2 (switching-rules a2)])
    (cond [(hh? pre-list)
           (begin
             (vector-set! a1 0 (first rule-1))
             (vector-set! a2 0 (first rule-2)))]
          [(hm? pre-list)
           (begin
             (vector-set! a1 0 (second rule-1))
             (vector-set! a2 0 (fourth rule-2)))]
          [(hl? pre-list)
           (begin
             (vector-set! a1 0 (third rule-1))
             (vector-set! a2 0 (seventh rule-2)))]
          [(mh? pre-list)
           (begin
             (vector-set! a1 0 (fourth rule-1))
             (vector-set! a2 0 (second rule-2)))]
          [(mm? pre-list)
           (begin
             (vector-set! a1 0 (fifth rule-1))
             (vector-set! a2 0 (fifth rule-1)))]
          [(ml? pre-list)
           (begin
             (vector-set! a1 0 (sixth rule-1))
             (vector-set! a2 0 (eighth rule-2)))]
          [(lh? pre-list)
           (begin
             (vector-set! a1 0 (seventh rule-1))
             (vector-set! a2 0 (third rule-2)))]
          [(lm? pre-list)
           (begin
             (vector-set! a1 0 (eighth rule-1))
             (vector-set! a2 0 (sixth rule-2)))]
          [(ll? pre-list)
           (begin
             (vector-set! a1 0 (ninth rule-1))
             (vector-set! a2 0 (ninth rule-2)))])))

(define (match-auto a1 a2 r)
  (let ([init-1 (init-strat a1)]
        [init-2 (init-strat a2)]
        [rule-1 (switching-rules a1)]
        [rule-2 (switching-rules a2)])
    (begin
      (reset-auto! a1)
      (reset-auto! a2)
      (append
       (list (match-strat (vector-first a1)
                          (vector-first a2)))
       (for/list ([n (sub1 r)])
         (begin
           (set-cond-move! a1 a2)
           (match-strat (vector-first a1)
                        (vector-first a2)))))
      )))

(define (reset-auto! auto)
  (vector-set! auto 0 (init-strat auto)))

(define (mean-pay posn pay-list)
  (mean (map posn pay-list)))

(define (mean-pay-pair pay-list)
  (list (mean-pay first pay-list)
        (mean-pay last pay-list)))

;; mass matching
(define (mass-match auto-list rounds)
  (for*/list ([i M]
              [j M])
    (mean-pay first (match-auto
                     (list-ref auto-list i)
                     (list-ref auto-list j)
                     rounds))))

`(define P (mass-match A 50))

(define (pack-pay pay-list)
  (for/list ([n M])
    (take
     (drop pay-list (* n M))
     M)))

(define (export-data path txt)
  (call-with-output-file path
    (lambda (output-port)
      (write txt output-port))
    #:exists 'append))
