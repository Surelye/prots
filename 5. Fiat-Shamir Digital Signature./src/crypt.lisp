(defpackage :crypt
  (:use :common-lisp)
  (:export :f :list-all-digests
           :hash-setter *hash*))


(in-package :crypt)


(defvar *hash*)

(defvar *digests* '(:blake2/256 :groestl/256 :jh/256       :keccak/256
                    :md5        :ripemd-160  :sha256       :sha384
                    :sha512     :skein1024   :streebog/256 :tiger))


(defun hash-file-setter (filename)
  (let ((hash-of-choice))
    (setq hash-of-choice (read-from-string (uiop:read-file-line filename)))
    (if (member hash-of-choice *digests*)
        (setf *hash* hash-of-choice)
        (return-from hash-file-setter))))


(defun hash-setter (c)
  (let ((hash-of-choice))
    (setq hash-of-choice (nth (1- c) *digests*))
    (aux::write-to-file (list (write-to-string hash-of-choice))
                        "hash-of-choice")
    (setf *hash* hash-of-choice) hash-of-choice))


(defun list-all-digests ()
  (format t
"~%~4t[1]: BLAKE2/256; [ 2]: GROESTL/256; [ 3]:       JH/256; [ 4]: KECCAK/256;
    [5]:        MD5; [ 6]:  RIPEMD-160; [ 7]:       SHA256; [ 8]:     SHA384;
    [9]:     SHA512; [10]:   SKEIN1024; [11]: STREEBOG/256; [12]:      TIGER.~%"))


(defun hash (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    *hash*
    (ironclad:ascii-string-to-byte-array str))))


(defun f (str modulo)
  (let ((digest (hash str)))
    (mod (ironclad:octets-to-integer
          (ironclad:ascii-string-to-byte-array digest)) modulo)))
