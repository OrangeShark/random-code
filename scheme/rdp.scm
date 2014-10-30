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

;; Type representing a parsed input
;; value is a number and rest-of-characters is a list of characters
(define (make-parsed value rest-of-characters)
  (vector 'parsed value rest-of-characters))

(define (parsed? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'parsed)))

(define (parsed-value obj)
  (if (parsed? obj)
      (vector-ref obj 1)
      (error "not a parsed object")))

(define (parsed-rest obj)
  (if (parsed? obj)
      (vector-ref obj 2)
      (error "not a parsed object")))

;; Type representing no parsed input
(define (make-no-parse)
  (vector 'no-parse))

(define (no-parse? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'no-parse)))

;; result type is either a parsed or no-parse
(define (result? obj)
  (or (parsed? obj)
      (no-parse? obj)))

;; Decimal <- '0' | ... | '9'
;; decimal : list-of-characters -> result
(define (decimal cs)
  (let ((rest (cdr cs)))
    (case (car cs)
      ((#\0) (make-parsed 0 rest))
      ((#\1) (make-parsed 1 rest))
      ((#\2) (make-parsed 2 rest))
      ((#\3) (make-parsed 3 rest))
      ((#\4) (make-parsed 4 rest))
      ((#\5) (make-parsed 5 rest))
      ((#\6) (make-parsed 6 rest))
      ((#\7) (make-parsed 7 rest))
      ((#\8) (make-parsed 8 rest))
      ((#\9) (make-parsed 9 rest))
      (else (make-no-parse))))) ; not a valid decimal character

;; Primary <- '(' Additive ')'
;;          / Decimal
;; primary : list-of-characters -> result
(define (primary cs)
  (case (car cs)
    ;; match '(' and call additive
    ((#\() (let ((result (additive (cdr cs))))
             ;; Additive match?
             (cond ((parsed? result)
                    (let ((rest (parsed-rest result)))
                      (case (car rest)
                        ;; match ')'
                        ((#\)) (make-parsed (parsed-value result)
                                            (cdr rest)))
                        ;; failed to match ')'
                        (else (make-no-parse)))))
                   ;; failed to match Additive
                   (else (make-no-parse)))))
    ;; match decimal
    (else (decimal cs))))

;; MultiSuffix <- '*' Primary MultiSuffix
;;              / '/' Primary MultiSuffix
;; multi-suffix : list-of-characters -> result
(define (multi-suffix cs)
  (case (car cs)
    ;; match '*' and call primary
    ((#\*) (let ((result-right (primary (cdr cs))))
             ;; Primary match? and call multi-suffix
             (cond ((parsed? result-right)
                    (let ((result-stuff (multi-suffix (parsed-rest result-right))))
                      ;; MultiSuffix match?
                      (cond ((parsed? result-stuff)
                             (let ((vright (parsed-value result-right))
                                   (vstuff (parsed-value result-stuff)))
                               ;; return a lambda of results
                               (make-parsed (lambda (vleft)
                                              (vstuff (* vleft vright)))
                                            (parsed-rest result-stuff))))
                            ;; failed to match MultiSuffix
                            (else (make-no-parse)))))
                   ;; failed to match Primary
                   (else (make-no-parse)))))
    ;; match '/' and call primary
    ((#\/) (let ((result-right (primary (cdr cs))))
             ;; Primary matched?
             (cond ((parsed? result-right)
                    (let ((result-stuff (multi-suffix (parsed-rest result-right))))
                      ;; MultiSuffix matched?
                      (cond ((parsed? result-stuff)
                             (let ((vright (parsed-value result-right))
                                   (vstuff (parsed-value result-stuff)))
                               ;; return a lambda of results
                               (make-parsed (lambda (vleft)
                                              (vstuff (/ vleft vright)))
                                            (parsed-rest result-stuff))))
                            ;; failed to match MultiSuffix
                            (else (make-no-parse)))))
                   ;; failed to match Primary
                   (else (make-no-parse)))))
     ;; identity function v -> v
    (else (make-parsed identity cs))))
