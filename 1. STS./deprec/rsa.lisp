(defpackage #:rsa
  (:use :cl))

(in-package #:rsa)


(defparameter e 65537)

(defparameter *base-primes*
  (remove-if-not #'(lambda (prime?) (aux::miller-rabin prime? 12))
                 (loop for prime? from (1+ (ash 1 15)) to (1- (ash 1 16)) by 2
                       collect prime?)))


(defun generate-even (target-len)
  (apply #'+ (ash 1 (1- target-len))
         (mapcar #'(lambda (bit pow) (* bit (ash 1 pow)))
                 (append (loop for bit from 0 to (- target-len 3)
                               collect (random 2)) '(0))
                 (loop for pow from (- target-len 2) downto 0 collect pow))))


(defun generate-prime (target-len)
  (when (not (aux::is-pow-of-2? target-len))
    (return-from generate-prime))
  (let ((prime) (s) (prime?) (req-len (- target-len 16)))
    (tagbody pick-prime
       (setq prime (nth (random (length *base-primes*)) *base-primes*))
       (when (not (aux::miller-rabin prime)) (go pick-prime)))
    (tagbody try-again
       (setq s (generate-even req-len)
             prime? (1+ (* prime s)))
       (if (and (= 1 (aux::mod-expt 2 (1- prime?) prime?))
                (/= 1 (aux::mod-expt 2 s prime?))
                (zerop (logxor (length (write-to-string prime? :base 2))
                               target-len)))
           (return-from generate-prime prime?)
           (go try-again)))))


(defun rsa (key-length)
  (when (not (aux::is-pow-of-2? key-length))
    (return-from rsa))
  (let ((p) (q) (n) (phi) (d))
    (tagbody try-again
       (setq p (generate-prime (ash key-length -1))
             q (generate-prime (ash key-length -1))
             n (* p q))
       (when (not (zerop (logxor (length (write-to-string n :base 2))
                                 key-length)))
         (go try-again)))
    (setq phi (* (1- p) (1- q))
          d (mod (cadr (aux::ext-gcd e phi)) phi))
    (list e d n)))
