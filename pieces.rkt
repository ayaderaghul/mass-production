(define (n->an n)
  (string->symbol
   (string-append "a" (number->string n))))

; (for/list [(n (in-range 1 101))]
  (n->an n))

(define B (for/list ([n (in-range 1 10)])
            (list 'define (list (n->an n) 'x) (list '* n 'x))))

(map eval B)

(define D
  (for/list ([n (in-range 2 11)])
    `(define ,(x->lx (sub1 n))
      (flatten (for/list ([z ,(in-range 1 n)]) z)))))

(~r 17 #:base 3 #:min-width 9 #:pad-string "0")

(string->list a-string) ; list of char

(string->number "123" 3)

(~a a-char) ; a-char -> string
