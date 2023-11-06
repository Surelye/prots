(defpackage #:sig
  (:use #:cl)
  (:export #:gen-keys
           #:sign-message
           #:verify-message))


(in-package #:sig)


(defun get-k ()
  (format t "~%Введите значение параметра безопасности k (по умолчанию 8): ")
  (let (k)
    (setq k (read-line))
    (when (or (zerop (length k))
              (null (setq k (parse-integer k :junk-allowed t)))
              (< k 4))
      (setq k 8)) k))


(defun gen-keys (bit-len)
  (let* ((p (aux:generate-prime bit-len))
         (q (aux:generate-prime bit-len))
         (n (* p p q)) (k (get-k)))
    (when (uiop:directory-exists-p "sig")
      (uiop:run-program "rm -r sig"))
    (uiop:run-program "mkdir sig")
    (aux:write-to-file (list p) "sig/p")
    (aux:write-to-file (list q) "sig/q")
    (aux:write-to-file (list n) "sig/n")
    (aux:write-to-file (list k) "sig/k") (list p q n k)))


(defun sign-message (message p q n k)
  (let* ((digest (crypt:hash message n))
         (pq (* p q)) (x (random pq))
         (chi (ceiling (/ (- digest (aux:mod-expt x k n))
                          pq)))
         (h (mod (* chi (cadr (aux:ext-gcd (* k (aux:mod-expt x (1- k) p)) p))) p)))
    (+ x (* h pq))))


(defun verify-message (message s n k)
  (let ((digest (crypt:hash message n))
        (ksi (ceiling (* 2/3 (integer-length n)))))
    (<= digest (aux:mod-expt s k n) (+ digest (ash 1 ksi)))))
