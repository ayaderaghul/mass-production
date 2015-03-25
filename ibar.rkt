(require racket/gui/base) ; to have TV
(require plot/no-gui) ; to have plot/dc
(require racket/draw) ; to draw
(require racket/math) ; to draw arc
(require math) ; to have mean
(plot-new-window? #t)

(define (vector-first a-vector)
  (vector-ref a-vector 0))
(define (vector-second a-vector)
  (vector-ref a-vector 1))
(define (vector-third a-vector)
  (vector-ref a-vector 2))
(define (vector-fourth a-vector)
  (vector-ref a-vector 3))
(define (vector-last a-vector)
  (vector-ref a-vector (- (vector-length a-vector) 1)))
(define (vector-rest a-vector)
  (vector-drop a-vector 1))

;; outcome HH HM HL MH MM ML LH LM LL
(define (h) (vector 8 8 (list 8 8 8 8 8 8 8 8 8))) ; the second is the previous strat
(define (m) (vector 5 5 (list 5 5 5 5 5 5 5 5 5)))
(define (l) (vector 2 2 (list 2 2 2 2 2 2 2 2 2)))
(define (a) (vector 5 5 (list 2 5 8 2 5 8 2 5 8))) ; accommodator

(define 4-type-list (list (h) (m) (l) (a)))

;; support function for automata
(define (morph an-auto)
  (let ([listed (vector->list an-auto)])
    (append (take listed 2)
            (drop listed 2))))
(define (clone an-auto)
  (let* ([morphed (morph an-auto)]
	 [vectored (list->vector morphed)])
    (vector-append (vector-take vectored 2)
		   (vector-drop vectored 2))))

(define (h? s)
  (equal? s 8))

(define (m? s)
  (equal? s 5))

(define (l? s)
  (equal? s 2))

(define (hh? a-list)
  (equal? '(8 8) a-list))

(define (hm? a-list)
  (equal? '(8 5) a-list))

(define (hl? a-list)
  (equal? '(8 2) a-list))

(define (mh? a-list)
  (equal? '(5 8) a-list))

(define (mm? a-list)
  (equal? '(5 5) a-list))

(define (ml? a-list)
  (equal? '(5 2) a-list))

(define (lh? a-list)
  (equal? '(2 8) a-list))

(define (lm? a-list)
  (equal? '(2 5) a-list))

(define (ll? a-list)
  (equal? '(2 2) a-list))

(define (match-strat s1 s2)
  (if (<= (+ s1 s2) 10)
      (list s1 s2)
      (list 0 0)))

(define (init-strat a)
  (vector-first a))

(define (pre-strat a)
  (vector-second a))

(define (move-rules a)
  (vector-third a))

;; HH HM HL  first second third
;; MH MM ML  fourth fifth sixth
;; LH LM LL  seventh eighth ninth

(define (set-cond-moves! a1 a2)
  (let ([pre-list (list (pre-strat a1) (pre-strat a2))]
        [rule-1 (move-rules a1)]
        [rule-2 (move-rules a2)])
    (cond [(hh? pre-list)
           (begin
             (vector-set! a1 1 (first rule-1))
             (vector-set! a2 1 (first rule-2)))]
          [(hm? pre-list)
           (begin
             (vector-set! a1 1 (second rule-1))
             (vector-set! a2 1 (fourth rule-2)))]
          [(hl? pre-list)
           (begin
             (vector-set! a1 1 (third rule-1))
             (vector-set! a2 1 (seventh rule-2)))]
          [(mh? pre-list)
           (begin
             (vector-set! a1 1 (fourth rule-1))
             (vector-set! a2 1 (second rule-2)))]
          [(mm? pre-list)
           (begin
             (vector-set! a1 1 (fifth rule-1))
             (vector-set! a2 1 (fifth rule-1)))]
          [(ml? pre-list)
           (begin
             (vector-set! a1 1 (sixth rule-1))
             (vector-set! a2 1 (eighth rule-2)))]
          [(lh? pre-list)
           (begin
             (vector-set! a1 1 (seventh rule-1))
             (vector-set! a2 1 (third rule-2)))]
          [(lm? pre-list)
           (begin
             (vector-set! a1 1 (eighth rule-1))
             (vector-set! a2 1 (sixth rule-2)))]
          [(ll? pre-list)
           (begin
             (vector-set! a1 1 (ninth rule-1))
             (vector-set! a2 1 (ninth rule-2)))])))

(define (match-auto a1 a2 r)
  (let ([init-1 (init-strat a1)]
        [init-2 (init-strat a2)]
        [rule-1 (move-rules a1)]
        [rule-2 (move-rules a2)])
    (begin
      (vector-set! a1 1 init-1)
      (vector-set! a2 1 init-2)
      (append
       (list (match-strat (vector-second a1) (vector-second a2)))
       (for/list ([n (sub1 r)])
         (begin
           (set-cond-moves! a1 a2)
           (match-strat (vector-second a1)
                        (vector-second a2))))))))

(define (reset-auto! a)
  (vector-set! a 1 (init-strat a)))

;; create world

;; support function

(define (shuffle a-vector)
  (do
      ([n (vector-length a-vector) (- n 1)])
      ((zero? n) a-vector)
    (let* ([r (random n)]
	   [t (vector-ref a-vector r)])
      (vector-set! a-vector r (vector-ref a-vector (- n 1)))
      (vector-set! a-vector (- n 1) t))))

;; create a world of p slots
(define (create-world p)
  (for/vector ([i p]) 0))
(define N 1000)
(define A (create-world N)) ; A is population
(define B (create-world N)) ; B is payoff book
(define B+ (create-world N)) ; B+ is the positive payoff book
(define T (create-world 4)) ; C is the type counting
(define F (create-world 4)) ; D is the type fitness
(define S (vector 0)) ; S is the payoff total
(define series (list (vector 0 0)))

(define h-series (list (vector 0 0)))
(define m-series (list (vector 0 0)))
(define l-series (list (vector 0 0)))
(define a-series (list (vector 0 0)))

;; create population to fill in the world (all positive numbers)
(define (create-population world high medium low accom)
                                        ;; (80 10 1 9)
  (let ([auto-population
         (shuffle
          (list->vector
           (append
            (for/list
                ([i high])
              (clone (h)))
            (for/list
                ([j medium])
              (clone (m)))
            (for/list
                ([k low])
              (clone (l)))
            (for/list
                ([m accom])
              (clone (a))))))])
    (begin
      (set! series (list (vector high medium)))
      (for ([n (vector-length world)])
        (vector-set! world n (vector-ref auto-population n))))))


(define (mean-pay posn pay-list)
  (mean (map posn pay-list)))

(define (accum a-list)
  (for/list ([n (length a-list)])
    (sum (take a-list (+ n 1)))))

;; payoff book
(define (set-payoff! population i1 i2 r pay-book)
  (let ([payoff (match-auto (vector-ref population i1)
                             (vector-ref population i2)
                             r)])
    (vector-set! pay-book i1  (mean-pay first payoff))
    (vector-set! pay-book i2  (mean-pay second payoff))))

(define (match-population! population r pay-book)
  (begin
    (for ([n (/ (vector-length population) 2)])
      (set-payoff! population (* 2 n) (add1 (* 2 n)) r pay-book))
    (vector-map reset-auto! population)))



(define (add1! pay-book posi-book)
  (for ([i (vector-length pay-book)])
    (vector-set! posi-book i
                 (add1
                  (vector-ref
                   pay-book i)))))

(define (v-sum! posi-book sum-book)
  (vector-set! sum-book 0
	       (sum (vector->list posi-book))))

(define (reset-book! book)
  (for ([n (vector-length book)])
    (vector-set! book n 0)))


;; outcome HH HM HL MH MM ML LH LM LL
;(define (h) (vector 8 8 (list 8 8 8 8 8 8 8 8 8))) ; the second is the previous strat
;(define (m) (vector 5 5 (list 5 5 5 5 5 5 5 5 5)))
;(define (l) (vector 2 2 (list 2 2 2 2 2 2 2 2 2)))
;(define (a) (vector 5 5 (list 2 5 8 2 5 8 2 5 8))) ; accommodator


(define (identify-auto auto)
  (cond [(equal? auto #(8 8 (8 8 8 8 8 8 8 8 8))) 0] ; all-high
        [(equal? auto #(5 5 (5 5 5 5 5 5 5 5 5))) 1] ; all-medium
        [(equal? auto #(2 2 (2 2 2 2 2 2 2 2 2))) 2] ; all-low
        [else 3])) ; the accommodator

(define (high? auto)
  (equal? auto #(8 8 (8 8 8 8 8 8 8 8 8))))
(define (medium? auto)
  (equal? auto #(5 5 (5 5 5 5 5 5 5 5 5))))
(define (low? auto)
  (equal? auto #(2 2 (2 2 2 2 2 2 2 2 2))))
(define (accom? auto)
  (equal? auto #(5 5 (2 5 8 2 5 8 2 5 8))))

(define (count-types population type-book)
  (begin
    (vector-set! type-book 0 (vector-count high? population))
    (vector-set! type-book 1 (vector-count medium? population))
    (vector-set! type-book 2 (vector-count low? population))
    (vector-set! type-book 3 (vector-count accom? population))))

(define (extract-payoff type? population posi-book)
  (for/list ([i (vector-length population)])
    (and
     (type? (vector-ref population i))
     (vector-ref posi-book i))))

(define (true? x)
  (not (false? x)))

(define (extract-fit type? population posi-book sum-book)
    (/ (sum (filter true? (extract-payoff type? population posi-book)))
       (vector-first sum-book)))

(define (calculate-type-fitness population posi-book fitness-book sum-book)
  (begin
    (vector-set! fitness-book 0 (extract-fit high? population posi-book sum-book))
    (vector-set! fitness-book 1 (extract-fit medium? population posi-book sum-book))
    (vector-set! fitness-book 2 (extract-fit low? population posi-book sum-book))
    (vector-set! fitness-book 3 (extract-fit accom? population posi-book sum-book))))

(define (extract-average type? population posi-book)
  (if (zero? (vector-count type? population))
      0
      (/ (extract-fit type? population posi-book)
         (vector-count type? population))))

(define (type-average population posi-book)
  (vector (extract-average high? population posi-book)
          (extract-average medium? population posi-book)
          (extract-average low? population posi-book)
          (extract-average accom? population posi-book)))

(define (do-cal! population pay-book posi-book sum-book type-book fitness-book)
  (begin
    (add1! pay-book posi-book)
    (v-sum! posi-book sum-book)
    (count-types population type-book)
    (calculate-type-fitness population posi-book fitness-book sum-book)))

(define (abridged-report type-book fitness-book)
  (list
   (vector->list type-book)
   (accum (vector->list fitness-book))))

(define (regenerate population speed type-book fitness-book)
  (let ([accum-fitness (second (abridged-report type-book fitness-book))])
    (for ([i speed])
     (vector-set! population i
                  (let ([r (random)])
                    (cond [(< r (first accum-fitness)) (clone (h))]
                          [(and (>= r (first accum-fitness))
                                (< r (second accum-fitness))) (clone (m))]
                          [(and (>= r (second accum-fitness))
                                (< r (third accum-fitness))) (clone (l))]
                          [else (clone (a))]))))))

(define (fitness-test speed type-book fitness-book)
  (let ([accum-fitness (second (abridged-report type-book fitness-book))])
    (for/vector ([i speed])
      (let ([r (random)])
        (cond [(< r (first accum-fitness)) (clone (h))]
              [(and (>= r (first accum-fitness))
                    (< r (second accum-fitness))) (clone (m))]
              [(and (>= r (second accum-fitness))
                    (< r (third accum-fitness))) (clone (l))]
              [else (clone (a))])))))

(define type-test (vector 0 0 0 0))

(define (export-data path txt)
  (call-with-output-file path
    (lambda (output-port)
      (write txt output-port))
    #:exists 'append))


(define (shuffle! population)
  (let ([new-popu (shuffle population)])
    (for ([i (vector-length population)])
      (vector-set! population i (vector-ref new-popu i)))))


;; TV
(define dynamic-frame (new frame% [label "replicator dynamic"]
                           [width 400]
                           [height 400]))
(define dyna-canvas (new canvas% [parent dynamic-frame]))
(define dc-dyna (send dyna-canvas get-dc))

(define (add-pair! type-book)
  (begin
    (set! series (append series (list
                                 (vector-take
                                  type-book 2))))))


(define (plot-dynamic type-book)
  (begin
    (add-pair! type-book)
    (plot/dc
     (lines series
            #:x-min 0 #:x-max N
            #:y-min 0 #:y-max N)
     dc-dyna
     0 0
     400 400)))


;;create population A at ratio...
(define (evolve-population cycles speed pause)
  (for/and ([n cycles])
    (match-population! A 100 B)
    (sleep pause)
    (do-cal! A B B+ S T F)
    (plot-dynamic T)
    (sleep pause)
    (regenerate A speed T F)
    (shuffle! A)
           ; (export-data "report.rkt"
           ;              (vector-take (count-types A T) 2))
    ))

(send dynamic-frame show #t)
