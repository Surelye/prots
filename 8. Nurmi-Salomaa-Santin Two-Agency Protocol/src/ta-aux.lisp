(defpackage #:ta-aux
  (:use #:cl)
  (:export #:*candidates*
           #:get-n
           #:get-bit-len
           #:gen-voters&ts
           #:form-folders
           #:send-ciphertexts
           #:compare-tags
           #:collect-keys
           #:collect-Bs
           #:decrypt-received
           #:count-voices))


(in-package #:ta-aux)


(defparameter *num-bytes* 16)

(defparameter *candidates* '("Кандидат №1" "Кандидат №2" "Кандидат №3"
                             "Кандидат №4" "Кандидат №5" "Кандидат №6"
                             "Кандидат №7" "Кандидат №8" "Кандидат №9"))


(defun get-n ()
  "Шаг 1. Определение количества избирателей."
  (format t "~%Введите количество избирателей (по умолчанию 9): ")
  (let (n)
    (setq n (read-line))
    (when (or (zerop (length n))
              (null (setq n (parse-integer n :junk-allowed t)))
              (< n 1))
      (setq n 9))
    (aux:write-to-file (list n) "n") n))


(defun get-bit-len ()
  (format t "~%Введите битовую длину числа l (l > 15, l = 2^m, по умолчанию l = 1024): ")
  (let ((bit-len (read-line)))
    (when (or (zerop (length bit-len))
              (null (setq bit-len (parse-integer bit-len :junk-allowed t)))
              (< bit-len 16)
              (plusp (logand bit-len (1- bit-len))))
      (setq bit-len 1024)) bit-len))


(defun gen-voters&ts ()
  "Шаг 1. Функция генерирует список избирателей и для каждого избирателя
   опознавательную метку t_i."
  (let ((n (aux:read-parse "n")) voters ts)
    (loop for j from n downto 1
          do (setq voters (cons (concatenate 'string "Голосующий №"
                                             (write-to-string j)) voters)
                   ts (cons (crypt:random-string *num-bytes*) ts)))
    (aux:write-to-file voters "voters")
    (aux:write-to-file     ts     "ts") (list voters ts)))


(defun gen-secret-keys (n)
  (loop for j from 0 below n
        collect (crypt:random-string *num-bytes*)))


(defun form-votes (n)
  (let ((up-to-choice (1+ (length *candidates*))))
    (loop for j from 0 below n
          collect (random up-to-choice))))


(defun form-messages-to-sign (votes ts)
  (setq votes (mapcar #'write-to-string votes))
  (mapcar #'(lambda (vote t-val)
              (concatenate 'string vote " " t-val)) votes ts))


(defun form-signatures (to-sign p q n k)
  (mapcar #'(lambda (vote)
              (sig::sign-message vote p q n k)) to-sign))


(defun form-ciphertexts (ts sigs keys)
  (let (ciphertexts)
    (setq sigs (mapcar #'write-to-string sigs)
          ciphertexts (mapcar #'(lambda (t-val sig)
                                  (concatenate 'string t-val " " sig))
                              ts sigs)
          ciphertexts (mapcar #'(lambda (plaintext key)
                                  (crypt:aes-encrypt plaintext key))
                              ciphertexts keys))))


(defun form-folders-routine (keys Bs sigs ts ciphertexts n)
  (let ((general-path "voting-data/voter-~a") path)
    (dotimes (j n)
      (setq path (format nil general-path j))
      (uiop:run-program (format nil "mkdir voting-data/voter-~a" j))
      (aux:write-to-file (list (nth j keys))
                         (concatenate 'string path "/e-secret"))
      (aux:write-to-file (list (nth j Bs))
                         (concatenate 'string path "/B"))
      (aux:write-to-file (list (nth j sigs))
                         (concatenate 'string path "/s"))
      (aux:write-to-file (list (nth j ts))
                         (concatenate 'string path "/t"))
      (aux:write-to-file (list (nth j ciphertexts))
                         (concatenate 'string path "/ciphertext")))))


(defun form-folders (num-users)
  (let* ((p (aux:read-parse "sig/p")) (q (aux:read-parse "sig/q"))
         (n (aux:read-parse "sig/n")) (k (aux:read-parse "sig/k"))
         (secret-keys (gen-secret-keys num-users)) (votes (form-votes num-users))
         (ts (uiop:read-file-lines "ts"))
         (to-sign (form-messages-to-sign votes ts))
         (sigs (form-signatures to-sign p q n k))
         (ciphertexts (form-ciphertexts ts sigs secret-keys)))
    (when (uiop:directory-exists-p "voting-data")
      (uiop:run-program "rm -r voting-data"))
    (uiop:run-program "mkdir voting-data")
    (form-folders-routine secret-keys to-sign sigs ts ciphertexts num-users)
    (list secret-keys to-sign sigs ciphertexts)))


(defun collect-tag (num-voter)
  (uiop:read-file-line
   (format nil "voting-data/voter-~a/t" num-voter)))


(defun collect-tags (num-users)
  (loop for j from 0 below num-users
        collect (collect-tag j)))


(defun collect-ciphertext (num-voter)
  (uiop:read-file-line
   (format nil "voting-data/voter-~a/ciphertext" num-voter)))


(defun collect-ciphertexts (num-users)
  (loop for j from 0 below num-users
        collect (collect-ciphertext j)))


(defun send-ciphertexts (num-users)
  (let ((ts (collect-tags num-users))
        (ciphertexts (collect-ciphertexts num-users)))
    (aux:write-to-file (mapcar #'(lambda (t-val ciphertext)
                                   (concatenate 'string t-val " " ciphertext))
                               ts ciphertexts)
                       "ciphertexts") t))


(defun compare-tags ()
  (let (tags received)
    (unless (and (uiop:file-exists-p "ts")
                 (uiop:file-exists-p "ciphertexts"))
      (return-from compare-tags nil))
    (setq tags (uiop:read-file-lines "ts")
          received (uiop:read-file-lines "ciphertexts")
          received (mapcar #'(lambda (ciphertext)
                               (uiop:split-string ciphertext :separator " "))
                           received))
    (null (remove-if #'(lambda (tag)
                         (member tag received :key #'car :test #'equal)) tags))))


(defun collect-key (num-voter)
  (uiop:read-file-line
   (format nil "voting-data/voter-~a/e-secret" num-voter)))


(defun collect-keys (num-users)
  (loop for j from 0 below num-users
        collect (collect-key j)))


(defun decrypt-received (num-users)
  (let ((keys (collect-keys num-users)) ciphertexts)
    (setq ciphertexts (uiop:read-file-lines "ciphertexts")
          ciphertexts (mapcar #'(lambda (ciphertext)
                                  (uiop:split-string ciphertext :separator " "))
                              ciphertexts)
          ciphertexts (mapcar #'(lambda (ciphertext)
                                  (parse-integer (cadr ciphertext))) ciphertexts))
    (mapcar #'(lambda (ciphertext key)
                (crypt:aes-decrypt ciphertext key))
            ciphertexts keys)))


(defun collect-B (num-voter)
  (uiop:read-file-line
   (format nil "voting-data/voter-~a/B" num-voter)))


(defun collect-Bs (num-users)
  (loop for j from 0 below num-users
        collect (collect-B j)))


(defun verify-sigs (num-users)
  (let ((sigs (decrypt-received num-users))
        (Bs (collect-Bs num-users))
        (n (aux:read-parse "sig/n"))
        (k (aux:read-parse "sig/k")))
    (setq sigs (mapcar #'(lambda (decrypted)
                           (uiop:split-string decrypted :separator " ")) sigs)
          sigs (mapcar #'(lambda (lst-decrypted)
                           (parse-integer (cadr lst-decrypted))) sigs))
    (every #'(lambda (B sig)
               (sig::verify-message B sig n k)) Bs sigs)))


(defun make-keyword (num)
  (intern (concatenate 'string ":" (write-to-string num))))


(defun count-voices (num-users)
  (unless (verify-sigs num-users)
    (return-from count-voices nil))
  (let ((voices (collect-Bs num-users))
        (election-results (loop for j from 0 below (* 2 (1+ (length *candidates*)))
                                if (evenp j)
                                  collect (make-keyword (ash j -1))
                                else collect 0)) voice-keyword)
    (setq voices (mapcar #'(lambda (B)
                             (uiop:split-string B :separator " ")) voices)
          voices (mapcar #'(lambda (B-splitted)
                             (parse-integer (car B-splitted))) voices))
    (dolist (voice voices election-results)
      (setq voice-keyword (make-keyword voice))
      (when (getf election-results voice-keyword)
        (incf (getf election-results voice-keyword))))))
