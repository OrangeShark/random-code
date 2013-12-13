;;; tutorial1.lisp
;;; http://www.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html

;;
;; Triple the value of a number
;;

(defun triple (X)
  "Compute three times X."
  (* 3 X))

;;
;; Negate the sign of a number
;;

(defun negate (X)
  "Negate the value of X."
  (- X))

;;
;; Compute the factorial of a number
;;

(defun factorial (N)
  "Compute the factorial of N."
  (if (= N 1)
    1
    (* N (factorial (- N 1)))))

;;
;; The N'th trianglular number
;; T(n) = 1           if n = 1
;; T(n) = n + T(n-1)  if n > 1
;;

(defun triangular (N)
  "Compute the triangular of N."
  (if (= N 1)
    1
    (+ N (triangular (- N 1)))))

;;
;; Computer the power of a number
;; P(b,e) = b             if e = 0
;; P(b,e) = b * P(b,e-1)  if e >= 1
;;

(defun power (B E)
  "Compute the power of B to the E."
  (if (= E 0)
    1
    (* B (power B (- E 1)))))
