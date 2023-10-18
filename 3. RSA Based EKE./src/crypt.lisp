(defpackage :crypt
  (:use :common-lisp)
  (:export :aes-encrypt :aes-decrypt
           :random-string :gen-session-key))


(in-package :crypt)


(defun ripemd128 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :ripemd-128
    (ironclad:ascii-string-to-byte-array str))))


(defun get-cipher (key)
  (ironclad:make-cipher :aes
    :mode :ecb
    :key (ironclad:ascii-string-to-byte-array (ripemd128 key))))


(defun aes-encrypt (plaintext key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:ascii-string-to-byte-array plaintext)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))


(defun aes-decrypt (ciphertext-int key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:integer-to-octets ciphertext-int)))
    (ironclad:decrypt-in-place cipher msg)
    (coerce (mapcar #'code-char (coerce msg 'list)) 'string)))


(defun random-string (&optional (len 32))
  (ironclad:byte-array-to-hex-string
   (ironclad:random-data len)))


(defun gen-session-key (len-mod)
  (reduce #'+ (mapcar #'* (loop for digit from 1 to (floor len-mod 4)
                                collect (1+ (random 9)))
                      (loop for pow from 1 to (floor len-mod 4)
                            collect (expt 10 pow)))))
