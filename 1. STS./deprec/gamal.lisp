(defpackage #:gamal
  (:use :cl))

(in-package #:gamal)


(defun gamal-gen-keys (key-length)
  (when (not (zerop (logand key-length (1- key-length))))
    (return-from gamal-gen-keys))
  (let ((p) (g) (x) (y))
    (tagbody try-again
       (setq p (rsa::generate-prime key-length))
       (when (not (aux::miller-rabin p)) (go try-again))
       (setq g (find-g p))
       (when (null g) (go try-again)))
    (setq x (+ 2 (random (- p 3)))
          y (aux::mod-expt g x p))
    (list y g p x)))
