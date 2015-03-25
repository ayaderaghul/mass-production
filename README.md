#mass-production

this mass-produces conditional-strategy machines in iterated bargaining game

#how to run

open racket

```
(load "mass.rkt")

(define M 100) ; mass-produce 100 first machines

(define A (mass-produce M))

(define P (mass-match A 50)) ; 50 rounds per match

(export-data "report.txt" (pack-pay P)) ; produce list-based payoff matrix, not csv yet
```
