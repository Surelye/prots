(defpackage :crypt
  (:use #:cl)
  (:export #:aes-encrypt
           #:aes-decrypt
           #:hash))


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


(defun skein1024 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :skein1024
    (ironclad:ascii-string-to-byte-array str))))


(defun hash (str modulo)
  (let ((digest (skein1024 str)))
    (mod (ironclad:octets-to-integer
          (ironclad:ascii-string-to-byte-array digest)) modulo)))
