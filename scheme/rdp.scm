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
(use-modules ((ice-9 match)))

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
  (match cs
         ((#\0 . rest) (make-parsed 0 rest))
         ((#\1 . rest) (make-parsed 1 rest))
         ((#\2 . rest) (make-parsed 2 rest))
         ((#\3 . rest) (make-parsed 3 rest))
         ((#\4 . rest) (make-parsed 4 rest))
         ((#\5 . rest) (make-parsed 5 rest))
         ((#\6 . rest) (make-parsed 6 rest))
         ((#\7 . rest) (make-parsed 7 rest))
         ((#\8 . rest) (make-parsed 8 rest))
         ((#\9 . rest) (make-parsed 9 rest))
         (_ (make-no-parse)))) ; not a valid decimal character

;; Primary <- '(' Additive ')'
;;          / Decimal
;; primary : list-of-characters -> result
(define (primary cs)
  (match cs
         ;; match '(' and call additive
         ((#\( . cs')
          (let ((result (additive cs')))
            ;; Additive match?
            (if (parsed? result)
                (let ((rest (parsed-rest result)))
                  (match rest
                         ;; match ')'
                         ((#\) . cs'') 
                          (make-parsed (parsed-value result)
                                       cs''))
                         ;; failed to match ')'
                         (_ (make-no-parse))))
                ;; failed to match Additive
                (make-no-parse))))
          ;; match decimal
          (_ (decimal cs))))

;; MultiSuffix <- '*' Primary MultiSuffix
;;              / '/' Primary MultiSuffix
;;              / ()
;; multi-suffix : list-of-characters -> result
(define (multi-suffix cs)
  (match cs
         ;; match '*' and call primary
         ((#\* . cs')
          (let ((result-right (primary cs')))
            ;; Primary match? and call multi-suffix
            (if (parsed? result-right)
                (let ((result-stuff (multi-suffix (parsed-rest result-right))))
                  ;; MultiSuffix match?
                  (if (parsed? result-stuff)
                      (let ((vright (parsed-value result-right))
                            (vstuff (parsed-value result-stuff)))
                        ;; return a lambda of results
                        (make-parsed (lambda (vleft)
                                       (vstuff (* vleft vright)))
                                     (parsed-rest result-stuff)))
                      ;; failed to match MultiSuffix
                      (make-no-parse)))
                ;; failed to match Primary
                (make-no-parse))))
         ;; match '/' and call primary
         ((#\/ . cs') 
          (let ((result-right (primary cs')))
            ;; Primary matched?
            (if (parsed? result-right)
                (let ((result-stuff (multi-suffix (parsed-rest result-right))))
                  ;; MultiSuffix matched?
                  (if (parsed? result-stuff)
                      (let ((vright (parsed-value result-right))
                            (vstuff (parsed-value result-stuff)))
                        ;; return a lambda of results
                        (make-parsed (lambda (vleft)
                                       (vstuff (/ vleft vright)))
                                     (parsed-rest result-stuff)))
                      ;; failed to match MultiSuffix
                      (make-no-parse)))
                ;; failed to match Primary
                (make-no-parse))))
         ;; identity function v -> v
         (_ (make-parsed identity cs))))

;; Multiplicative <- Primary MultiSuffix
(define (multi cs)
  ;; call primary
  (let ((result (primary cs)))
    ;; Primary match?
    (cond ((parsed? result)
           ;; call multi-suffix
           (let ((result' (multi-suffix (parsed-rest result))))
             ;; multi-suffix match?
             (cond ((parsed? result')
                    (let ((vleft (parsed-value result))
                          (vstuff (parsed-value result'))
                          (vrest (parsed-rest result')))
                      ;; apply multi-suffix value to primary value
                      (make-parsed (vstuff vleft) vrest)))
                   ;; failed to match multi-suffix
                   (else (make-no-parse)))))
          ;; failed to match primary
          (else (make-no-parse)))))

;; AdditiveSuffix <- '+' Multiplicative AdditiveSuffix
;;                 / '-' Multiplicative AdditiveSuffix
;;                 / ()
(define (add-suffix cs)
  (match cs
         ;; match '+' and call multi
         ((#\+ . cs') 
          (let ((result-right (multi cs')))
            ;; Multiplicative match? and call add-suffix
            (cond ((parsed? result-right)
                   (let ((result-stuff (add-suffix (parsed-rest result-right))))
                     ;; AdditiveSuffix match?
                     (cond ((parsed? result-stuff)
                            (let ((vright (parsed-value result-right))
                                  (vstuff (parsed-value result-stuff)))
                              ;; return a lambda of results
                              (make-parsed (lambda (vleft)
                                             (vstuff (+ vleft vright)))
                                           (parsed-rest result-stuff))))
                           ;; failed to match AdditiveSuffix
                           (else (make-no-parse)))))
                  ;; failed to match Multiplicative
                  (else (make-no-parse)))))
         ;; match '-' and call multi
         ((#\- . cs') 
          (let ((result-right (multi cs')))
            ;; Multiplicative matched?
            (cond ((parsed? result-right)
                   (let ((result-stuff (add-suffix (parsed-rest result-right))))
                     ;; AdditiveSuffix matched?
                     (cond ((parsed? result-stuff)
                            (let ((vright (parsed-value result-right))
                                  (vstuff (parsed-value result-stuff)))
                              ;; return a lambda of results
                              (make-parsed (lambda (vleft)
                                             (vstuff (- vleft vright)))
                                           (parsed-rest result-stuff))))
                           ;; failed to match AdditiveSuffix
                           (else (make-no-parse)))))
                  ;; failed to match Multiplicative
                  (else (make-no-parse)))))
         ;; identity function v -> v
         (_ (make-parsed identity cs))))

;; Additive       <- Multiplicative AdditiveSuffix
(define (additive cs)
  ;; call multi
  (let ((result (multi cs)))
    ;; Multiplicative match?
    (cond ((parsed? result)
           ;; call add-suffix
           (let ((result' (add-suffix (parsed-rest result))))
             ;; AdditivieSuffix match?
             (cond ((parsed? result')
                    (let ((vleft (parsed-value result))
                          (vstuff (parsed-value result'))
                          (vrest (parsed-rest result')))
                      ;; apply add-suffix value to primary value
                      (make-parsed (vstuff vleft) vrest)))
                   ;; failed to match AdditiveSuffix
                   (else (make-no-parse)))))
          ;; failed to match Multiplicative
          (else (make-no-parse)))))

(define (parse-expr input)
  (let ((result (additive (string->list input))))
    (if (parsed? result)
        (display (parsed-value result))
        (display "Bad input")))
  (newline))
