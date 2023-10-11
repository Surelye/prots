(defpackage #:rsa
  (:use :cl))

(in-package #:rsa)


; (defparameter e 65537)


(defun rsa-machinerie (key-length &optional (num-users 1))
  (when (not (aux::is-pow-of-2? key-length))
    (return-from rsa-machinerie))
  (let ((p) (q) (n) (phi) (e) (d) (res))
    (tagbody try-again
       (setq p (aux::generate-prime (ash key-length -1))
             q (aux::generate-prime (ash key-length -1))
             n (* p q))
       (when (not (zerop (logxor (length (write-to-string n :base 2))
                                 key-length)))
         (go try-again)))
    (do ((i 0 (1+ i))) ((= num-users i) res)
      (setq phi (* (1- p) (1- q)))
      (aux::while (/= 1 (gcd (setq e (random phi)) phi)))
      (setq d (mod (cadr (aux::ext-gcd e phi)) phi)
            res (cons (list e d n) res)))))


(defun rsa-generate-keys (key-length prefixes)
  (let ((keys (rsa-machinerie key-length (length prefixes)))
        (prefix) (name) (pub-filename) (priv-filename))
    (do ((i 0 (1+ i))) ((= (length prefixes) i))
      (destructuring-bind (e d n) (nth i keys)
        (setq prefix (nth i prefixes)
              name (string-upcase (nth i prefixes))
              pub-filename (concatenate 'string prefix  "-pub-key")
              priv-filename (concatenate 'string prefix "-priv-key"))
        (aux::write-to-file (list name e n)  pub-filename)
        (format t "Публичный RSA-ключ пользователя ~s был записан в файл ~s.~%"
                name pub-filename)
        (aux::write-to-file (list name d n) priv-filename)
        (format t "Приватный RSA-ключ пользователя ~s был записан в файл ~s.~%"
                name priv-filename)
        (format t "    Публичный ключ: 0x~x;
    Приватный ключ: 0x~x.~%" e d) (terpri)))))
