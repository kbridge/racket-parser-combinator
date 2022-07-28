#lang racket

;; our parser combinators:
;; - accept a list of characters
;; - return two values [parse-result, remaining-characters] if succeed
;;   return no value if fail

;; superseded by `any`, see below

;(define item
;  (lambda (chars)
;    (if (null? chars)
;        (values)
;        (values (car chars) (cdr chars)))))

;; examples:
;; (item (string->list ""))    => void
;; (item (string->list "a"))   => #\a ()
;; (item (string->list "abc")) => #\a (#\b #\c)

(define (satisfy pred)
  (lambda (chars)
    (if (null? chars)
        (values)
        (if (pred (car chars))
            (values (car chars) (cdr chars))
            (values)))))

(define (char c)
  (satisfy (curry char=? c)))

(define any
  (satisfy (const #t)))

;; note:
;; `item` itself is a parser
;; `satisfy` and `char` are not, they are parser creators

;; examples:
;; (any (string->list ""))                        => void
;; (any (string->list "a"))                       => #\a ()
;; (any (string->list "abc"))                     => #\a (#\b #\c)
;; ((char #\a) (string->list "abc"))              => #\a (#\b #\c)
;; ((char #\x) (string->list "abc"))              => void
;; ((satisfy char-numeric?) (string->list "123")) => #\1 (#\2 #\3)
;; ((satisfy char-numeric?) (string->list "xyz")) => void

(define (call-with-parse-result parser chars on-ok [on-error values])
  (call-with-values
   (lambda () (parser chars))
   (lambda result
     (if (null? result)
         (on-error)
         (apply on-ok result)))))

(define (sequence . parsers)
  (lambda (chars)
    (if (null? parsers)
        (values '() chars)
        (call-with-parse-result
         (car parsers)
         chars
         (lambda (first-item chars)
           (call-with-parse-result
            (apply sequence (cdr parsers))
            chars
            (lambda (rest-items chars)
              (values (cons first-item rest-items)
                      chars))))))))

;; note:
;; `sequence` is effectively a multi-argument `and` operator
;; except it returns all the results as a list instead of the last one

;; examples:
;; ((sequence (char #\a) (char #\b) (char #\c)) (string->list "abcd")) => (#\a #\b #\c) (#\d)
;; ((sequence (char #\a) (char #\b) (char #\c)) (string->list "ab"))   => void
;; ((sequence) (string->list "a"))                                     => () (#\a)
;; ((sequence) (string->list ""))                                      => () ()
;; ((sequence (satisfy char-lower-case?)) (string->list "a"))          => (#\a) ()

(define (alternative . parsers)
  (lambda (chars)
    (if (null? parsers)
        (values)
        (call-with-parse-result
         (car parsers)
         chars
         values
         (lambda () ((apply alternative (cdr parsers)) chars))))))

;; note:
;; `alternative` is effectively a multi-argument `or` operator

;; examples:
;; ((alternative (char #\a) (char #\b) (char #\c)) (string->list "axxx")) => #\a (#\x #\x #x)
;; ((alternative (char #\a) (char #\b) (char #\c)) (string->list "bxxx")) => #\b (#\x #\x #x)
;; ((alternative (char #\a) (char #\b) (char #\c)) (string->list "cxxx")) => #\c (#\x #\x #x)
;; ((alternative (char #\a) (char #\b) (char #\c)) (string->list "dxxx")) => void
;; ((alternative) (string->list "xxx"))                                   => void
;; ((alternative (satisfy char-alphabetic?)) (string->list "xxx"))        => #\x (#\x #\x)

(define (repeat n parser)
  (apply sequence (make-list n parser)))

;; examples:
;; ((repeat 3 (char #\a)) (string->list "aaaa"))                          => (#\a #\a #\a) (#\a)
;; ((repeat 3 (char #\a)) (string->list "aa"))                            => void
;; ((repeat 0 (char #\a)) (string->list "aaa))                            => () (#\a #\a #\a)
;; ((repeat 0 (char #\a)) (string->list ""))                              => () ()
;; ((repeat 3 (sequence (char #\a) (char #\b))) (string->list "abababc")) => ((#\a #\b) (#\a #\b) (#\a #\b)) (#\c)
;; ((repeat 3 (sequence (char #\a) (char #\b))) (string->list "ababac"))  => void
;; ((repeat 3 (sequence (char #\a) (char #\b))) (string->list "xxx"))     => void

(define none
  (lambda (chars)
    (values #f chars)))

(define (optional parser)
  (alternative parser none))

;; examples:
;; (none (string->list "abc"))                                            => #f (#\a #\b #\c)
;; ((optional (repeat 2 (satisfy char-alphabetic?))) (string->list "ab")) => (#\a #\b) ()
;; ((optional (repeat 2 (satisfy char-alphabetic?))) (string->list "a"))  => #f ()

(define (many parser)
  (lambda (chars)
    (call-with-parse-result
     parser
     chars
     (lambda (first-item chars)
       (call-with-parse-result
        (many parser)
        chars
        (lambda (rest-items chars)
          (values (cons first-item rest-items)
                  chars))))
     (lambda () (values '() chars)))))

;; examples:
;; ((many (satisfy char-numeric?)) (string->list "123a")) => (#\1 #\2 #\3) (#\a)
;; ((many (satisfy char-numeric?)) (string->list "1a"))   => (#\1) (#\a)
;; ((many (satisfy char-numeric?)) (string->list "1"))    => (#\1) ()
;; ((many (satisfy char-numeric?)) (string->list ""))     => () ()
;; ((many (satisfy char-numeric?)) (string->list "aaa"))  => () (#\a #\a #\a)

(define (many1 parser)
  (lambda (chars)
    (call-with-parse-result
     parser
     chars
     (lambda (first-item chars)
       (call-with-parse-result
        (many parser)
        chars
        (lambda (rest-items chars)
          (values (cons first-item rest-items)
                  chars))
        (lambda () (values '() chars)))))))

;; note:
;; you can see that the differences between `many` and `many1` are subtle

;; examples:
;; ;; same test cases as above
;; ((many1 (satisfy char-numeric?)) (string->list "123a")) => (#\1 #\2 #\3) (#\a)
;; ((many1 (satisfy char-numeric?)) (string->list "1a"))   => (#\1) (#\a)
;; ((many1 (satisfy char-numeric?)) (string->list "1"))    => (#\1) ()
;; ((many1 (satisfy char-numeric?)) (string->list ""))     => void                ; changed 
;; ((many1 (satisfy char-numeric?)) (string->list "aaa"))  => void                ; changed

(define (fmap f parser)
  (lambda (chars)
    (call-with-parse-result
     parser
     chars
     (lambda (item chars)
       (values (f item)
               chars)))))

;; examples:
;; ((fmap void (char #\a)) (string->list "abc"))                                     => (#\b #\c)
;; ((fmap void (char #\a)) (string->list "xxx"))                                     => void
;; (call-with-values (lambda () ((fmap void (char #\a)) (string->list "abc"))) list) => (#<void> (#\b #\c)) ; not the same `void` above
;; (call-with-values (lambda () ((fmap void (char #\a)) (string->list "xxx"))) list) => ()

(define (return item)
  (lambda (chars)
    (values item chars)))

;; examples:
;; ((return 'foo) (string->list "abc")) => foo (#\a #\b #\c)

(define (bind parser f)
  (lambda (chars)
    (call-with-parse-result
     parser
     chars
     (lambda (item chars)
       ((f item) chars)))))

;; examples:
;; (define twin (bind any (lambda (c) (char c))))
;; (twin (string->list ""))     => void
;; (twin (string->list "a"))    => void
;; (twin (string->list "aa"))   => #\a ()
;; (twin (string->list "aaa"))  => #\a (#\a)
;; (twin (string->list "aaaa")) => #\a (#\a #\a)
;; (twin (string->list "ab"))   => void
;; (twin (string->list "aab"))  => #\a (#\b)

(define (applicative parser . parsers)
  (lambda (chars)
    (call-with-parse-result
     parser
     chars
     (lambda (f chars)
       (call-with-parse-result
        (apply sequence parsers)
        chars
        (lambda (items chars)
          (values (apply f items)
                  chars)))))))

(define (lift-applicative f . parsers)
  (apply applicative (cons (return f) parsers)))

;; examples:
;;
;; (define single-digit-integer (fmap (compose (curryr - 48) char->integer) (satisfy char-numeric?)))
;;
;; (single-digit-integer (string->list ""))   => void
;; (single-digit-integer (string->list "x"))  => void
;; (single-digit-integer (string->list "1"))  => 1 ()
;; (single-digit-integer (string->list "1x")) => 1 (#\x)
;;
;; ((lift-applicative + single-digit-integer single-digit-integer) (string->list "1"))   => void
;; ((lift-applicative + single-digit-integer single-digit-integer) (string->list "12"))  => 3 ()
;; ((lift-applicative + single-digit-integer single-digit-integer) (string->list "123")) => 3 (#\3)
;;
;; (define single-digit-integer (fmap (compose (curryr - 48) char->integer) (satisfy char-numeric?)))
;; (define operator (alternative (fmap (const +) (char #\+)) (fmap (const *) (char #\*))))
;;
;; (operator (string->list ""))   => void
;; (operator (string->list "+"))  => #<procedure:+> ()
;; (operator (string->list "+1")) => #<procedure:+> (#\1)
;; (operator (string->list "*1")) => #<procedure:*> (#\1)
;; (operator (string->list "?1")) => void
;;
;; ((applicative operator single-digit-integer single-digit-integer) (string->list ""))     => void
;; ((applicative operator single-digit-integer single-digit-integer) (string->list "+"))    => void
;; ((applicative operator single-digit-integer single-digit-integer) (string->list "+3"))   => void
;; ((applicative operator single-digit-integer single-digit-integer) (string->list "+35"))  => 8 ()
;; ((applicative operator single-digit-integer single-digit-integer) (string->list "*35"))  => 15 ()
;; ((applicative operator single-digit-integer single-digit-integer) (string->list "*35?")) => 15 (#\?)

;; note:
;; to understand the 'evaluation order' of Applicative, try this in GHCi:
;; (putStrLn ".f" >> return (*)) <*> (putStrLn ".1" >> return 3) <*> (putStrLn ".2" >> (return 5))
;; expected:
;; .f
;; .1
;; .2
;; 15
