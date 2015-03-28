;; A recursive decent parser for a simple expression language
;; parsing grammar (PG)
;; Additive       <- Multiplicative AdditiveSuffix
;; AdditiveSuffix <- '+' Multiplicative AdditiveSuffix
;;                 / '-' Multiplicative AdditiveSuffix
;;                 / ()
;; Multiplicative <- Primary MultiSuffix
;; MultiSuffix    <- '*' Primary MultiSuffix
;;                 / '/' Primary MultiSuffix
;;                 / ()
;; Primary        <- '(' Additive ')'
;;                 / Decimal
;; Decimal        <- '0' | ... | '9'
(use-modules ((ice-9 match))
             (srfi srfi-9)
             (srfi srfi-26))

(define-record-type <parsed>
  (make-parsed value rest)
  parsed?
  (value parsed-value)
  (rest  parsed-rest))
;; Parsed is (make-parsed Number (listof Character))
;; interp. the current value and the rest of characters

(define-record-type <success>
  (make-success value)
  success?
  (value success-value))
;; Success is (make-success 'a)
;; interp. a successful result of type 'a

(define-record-type <failure>
  (make-failure value)
  failure?
  (value failure-value))
;; Failure is (make-failure 'a)
;; interp. a failed result of type 'a

;; Result is one of:
;;  - (Success 'a)
;;  - (Failure 'b)
;; interp. Success is a successful result with value 'a and
;;         Failure is a failed result with value 'b

#;
(define (fn-for-result r)
  (cond [(success? r) (... (success-value r))]
        [else         (... (failure-value r))]))

;; Result<'a> ('a -> Result<'b>) -> Result<'b>
(define (>>= r fn)
  (cond [(success? r) (fn (success-value r))]
        [else         r]))



(define (successful-parsed v rest)
  (make-success (make-parsed v rest)))

  ;; Decimal <- '0' | ... | '9'
  ;; decimal : (listof Character) -> Result

(define (decimal cs)
  (match cs
    ((#\0 . rest) (successful-parsed 0 rest))
    ((#\1 . rest) (successful-parsed 1 rest))
    ((#\2 . rest) (successful-parsed 2 rest))
    ((#\3 . rest) (successful-parsed 3 rest))
    ((#\4 . rest) (successful-parsed 4 rest))
    ((#\5 . rest) (successful-parsed 5 rest))
    ((#\6 . rest) (successful-parsed 6 rest))
    ((#\7 . rest) (successful-parsed 7 rest))
    ((#\8 . rest) (successful-parsed 8 rest))
    ((#\9 . rest) (successful-parsed 9 rest))
    (_ (make-failure "expecting a decimal")))) ; not a valid decimal character


;; (listof Character) -> Result
;; Primary <- '(' Additive ')'
;;          / Decimal

(define (primary loc)
  (define (match-end p)
    (match (parsed-rest p)
      [(#\) . loc') (successful-parsed (parsed-value p)
                                       (cdr (parsed-rest p)))]
      [_  (make-failure "Failed to match )")]))
  (define (match-rest loc)
    (>>= (additive loc)
         match-end))
  (case (car loc)
    [(#\() (match-rest (cdr loc))]
    [else  (decimal loc)]))  


;; (listof Character) -> Result
;; MultiSuffix <- '*' Primary MultiSuffix
;;              / '/' Primary MultiSuffix
;;              / ()
(define (multi-suffix loc)
  (define (match-primary op loc)
    (>>= (primary loc)
         (lambda (right)
           (>>= (multi-suffix (parsed-rest right))
                (lambda (rest)
                  (successful-parsed 
                   (lambda (left)
                     ((parsed-value rest) (op left 
                                              (parsed-value right))))
                   (parsed-rest rest)))))))
  (match loc
    [(#\* . loc') (match-primary * loc')]
    [(#\/ . loc') (match-primary / loc')]
    [_            (successful-parsed identity loc)]))

;; (listof Character) -> Result
;; Multiplicative <- Primary MultiSuffix
(define (multi loc)
  (>>= (primary loc)
       (lambda (p)
         (>>= (multi-suffix (parsed-rest p))
              (lambda (ms)
                (successful-parsed ((parsed-value ms)
                                    (parsed-value p))
                                   (parsed-rest ms)))))))


;; (listof Character) -> Result
;; AdditiveSuffix <- '+' Multiplicative AdditiveSuffix
;;                 / '-' Multiplicative AdditiveSuffix
;;                 / ()
(define (add-suffix loc)
  (define (match-primary op loc)
    (>>= (multi loc)
         (lambda (right)
           (>>= (add-suffix (parsed-rest right))
                (lambda (rest)
                  (successful-parsed 
                   (lambda (left)
                     ((parsed-value rest) (op left 
                                              (parsed-value right))))
                   (parsed-rest rest)))))))
  (match loc
    [(#\+ . loc') (match-primary + loc')]
    [(#\- . loc') (match-primary - loc')]
    [_            (successful-parsed identity loc)]))


;; (listof Character) -> Result
;; combines a Multiplicative and AdditiveSuffix
;; Additive <- Multiplicative AdditiveSuffix
(define (additive loc)
  (>>= (multi loc)
       (lambda (m)
         (>>= (add-suffix (parsed-rest m))
              (lambda (as)
                (successful-parsed ((parsed-value as)
                                    (parsed-value m))
                                   (parsed-rest as)))))))

(define (parse-expr input)
  (let ((result (additive (string->list input))))
    (cond [(success? result) (display (parsed-value (success-value result)))]
          [else (display (failure-value result))]))
  (newline))
