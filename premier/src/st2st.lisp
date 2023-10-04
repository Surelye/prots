(defpackage #:st2st
  (:use :cl))

(in-package #:st2st)


(defun stop () (read-line))


(defun get-bit-len ()
  (let ((bit-len))
    (tagbody try-again
       (setq bit-len (read))
       (when (and (integerp bit-len) (aux::is-pow-of-2? bit-len) (> bit-len 16))
         (return-from get-bit-len bit-len))
       (format t "Некорректный ввод! Попробуйте снова: ")
       (go try-again))))


(defun gen-p&g (bit-len)
  (let ((p) (g))
    (tagbody gen-prime
       (setq p (aux::generate-prime bit-len)
             g (aux::find-g p))
       (when (null g) (go gen-prime)))
    (list p g)))


(defun step-1 (p g)
  (let ((x) (g^x))
    (format t "~%[1] -- Алиса генерирует случайное число x (1 < x < p - 1):
      [x] = 0x~x, вычисляет g^x (mod p) и отправляет результат Бобу:
    [g^x] = 0x~x.~%" (setq x (+ 2 (random (- p 3)))) (setq g^x (aux::mod-expt g x p)))
    (aux::write-to-file (list x g^x) "alice-dh-keys")
    (list x g^x)))


(defun step-2 (p g)
  (let ((y) (g^y))
    (format t "~%[2] -- Боб генерирует случайное число y (1 < y < p - 1):
      [y] = 0x~x, и вычисляет g^y (mod p):
    [g^y] = 0x~x.~%" (setq y (+ 2 (random (- p 3)))) (setq g^y (aux::mod-expt g y p)))
    (aux::write-to-file (list y g^y) "bob-dh-keys")
    (list y g^y)))


(defun step-3 (p)
  (let* ((g^x (parse-integer (uiop:read-file-line "alice-dh-keys" :at 1)))
         (y (parse-integer (uiop:read-file-line "bob-dh-keys" :at 0)))
         (shared-key (aux::mod-expt g^x y p)))
    (format t "~%[3] -- Боб вычисляет общий секретный ключ:
    [K] = (g^x)^y (mod p) = 0x~x.~%" shared-key)
    (aux::write-to-file (list shared-key) "bob-shared-key")))


(defun encrypt (prefix fn-plaintext fn-key)
  (let* ((plaintext (mapcar #'parse-integer (uiop:read-file-lines fn-plaintext)))
         (key (parse-integer (uiop:read-file-line fn-key :at 0)))
         (ciphered (mapcar #'(lambda (plain) (logxor plain key)) plaintext)))
    (aux::write-to-file ciphered (concatenate 'string prefix "-sig-ciphered"))))


(defun step-4 ()
  (let ((g^y (parse-integer (uiop:read-file-line "bob-dh-keys" :at 1)))
        (g^x (parse-integer (uiop:read-file-line "alice-dh-keys" :at 1))))
    (format t "~%[4] -- Боб подписывает g^y, g^x и шифрует их при помощи K. Шифртекст и g^y отправляются Алисе.~%")
    (aux::write-to-file (list g^y g^x) "transient")
    (ec-schnorr::schnorr-sign-message "bob" "transient")
    (encrypt "bob" "bob-sig" "bob-shared-key")
    (uiop:run-program "rm transient bob-sig")))


(defun step-5 (p)
  (let* ((g^y (parse-integer (uiop:read-file-line "bob-dh-keys" :at 1)))
         (x (parse-integer (uiop:read-file-line "alice-dh-keys" :at 0)))
         (shared-key (aux::mod-expt g^y x p)))
    (format t "~%[5] -- Алиса вычисляет общий секретный ключ:
    [K] = (g^y)^x (mod p) = 0x~x.~%" shared-key)
    (aux::write-to-file (list shared-key) "alice-shared-key")))


(defun decrypt (fn-cipher fn-key)
  (let ((ciphertext (mapcar #'parse-integer (uiop:read-file-lines fn-cipher)))
        (key (parse-integer (uiop:read-file-line fn-key :at 0))))
    (aux::write-to-file (mapcar #'(lambda (cipher) (logxor cipher key))
                                ciphertext) "transient")))


(defun step-6 ()
  (format t "~%[6] -- Алиса дешифрует криптограмму и проверяет подпись Боба.~%")
  (decrypt "bob-sig-ciphered" "alice-shared-key")
  (ec-schnorr::schnorr-verify-signature "bob" "transient")
  (uiop:run-program "rm transient"))


(defun step-7 ()
  (let ((g^x (parse-integer (uiop:read-file-line "alice-dh-keys" :at 1)))
        (g^y (parse-integer (uiop:read-file-line "bob-dh-keys" :at 1))))
    (format t "~%[7] -- Алиса подписывает g^x, g^y и шифрует их при помощи K. Шифртекст отправляется Алисе.~%")
    (aux::write-to-file (list g^x g^y) "transient")
    (ec-schnorr::schnorr-sign-message "alice" "transient")
    (encrypt "alice" "alice-sig" "alice-shared-key")
    (uiop:run-program "rm transient alice-sig")))


(defun step-8 ()
  (format t "~%[8] -- Боб дешифрует криптограмму и проверяет подпись Алисы.~%")
  (decrypt "alice-sig-ciphered" "bob-shared-key")
  (ec-schnorr::schnorr-verify-signature "alice" "transient")
  (uiop:run-program "rm transient"))


(defun st2st ()
  (let ((bit-len-sig) (bit-len-sts))
    (format t "~%Введите битовую длину подписи l (l = 2^k, l > 16): ")
    (setq bit-len-sig (get-bit-len))
    (ec-schnorr::schnorr-generate-keys bit-len-sig "alice")
    (ec-schnorr::schnorr-generate-keys bit-len-sig "bob")
    (format t "~%Введите битовую длину модуля p (STS): ")
    (setq bit-len-sts (get-bit-len))
    (destructuring-bind (p g) (gen-p&g bit-len-sts)
      (format t "~%Были сгенерированы параметры:
    [p] = 0x~x;~%    [g] = 0x~x.~%" p g)
      (step-1 p g) (stop) (step-2 p g) (stop)
      (step-3 p  ) (stop) (step-4    ) (stop)
      (step-5 p  ) (stop) (step-6    ) (stop)
      (step-7    ) (stop) (step-8    ) (stop))))
