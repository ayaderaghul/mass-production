#mass-production

this mass-produces conditional-strategy machines in iterated bargaining game

file ibar.rkt plots dynamic of the bargaining game, regarding 4 types (High, Medium, Low, Accommodator).

#how to run

open racket

```
(load "mass.rkt")

(define M 100) ; mass-produce 100 first machines

(define A (mass-produce M))

(define P (mass-match A 50)) ; 50 rounds per match

(export-data "report.txt" (pack-pay P)) ; produce list-based payoff matrix, not csv yet
```

```
(load "ibar.rkt")

(create-population A 800 150 1 49) ; the ratio in the population High - Medium - Low - Accommodator

(evolve-population 150 50 0) ; 150 cycles - speed 50 - pause 0
```
