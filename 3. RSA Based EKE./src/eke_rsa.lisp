(defpackage :eke
  (:use :common-lisp))

(in-package :eke)


(defun stop () (read-line))


(defun read-parse (filename &optional (at 0))
  (parse-integer (uiop:read-file-line filename :at at)))


(defun step-1-aux ()
  (let ((key-len))
    (format t "~%Введите длину модуля RSA l (l > 16, l = 2^m, по умолчанию l = 1024): ")
    (tagbody try-again
       (setq key-len (parse-integer (read-line) :junk-allowed t))
       (when (not (integerp key-len))
         (setq key-len 1024))
       (when (or (null key-len) (not (and (zerop (logand key-len (1- key-len)))
                                          (> key-len 16))))
         (format t "~%Некорректное значение l! Введите l снова: ")
         (go try-again))) key-len))


(defun step-1 ()
  (let ((key-len (step-1-aux)) (shared-password) (encrypted) (pub-key))
    (format t "~%[1.1] -- Алиса случайным образом генерирует пару открытый ключ / закрытый ключ.~2%")
    (rsa::rsa-generate-keys key-len '("alice"))
    (setq pub-key (cdr (uiop:read-file-lines "alice-pub-key"))
          shared-password (uiop:read-file-line "shared-password"))
    (format t "[1.2] -- Она шифрует открытый ключ K' с помощью симметричного алгоритма, используя P
         в качестве ключа: E_P(K'). Она посылает Бобу: A, E_P(K'): ")
    (setq encrypted (cons (crypt:aes-encrypt (car pub-key) shared-password)
                          (cdr pub-key)))
    (aux::write-to-file encrypted "step-1-message")
    (format t "~2%~4tE_P(K') = 0x~x,
              0x~x.~%" (car encrypted) (parse-integer (cadr encrypted)))))


(defun step-2 ()
  (let ((shared-password) (decrypted) (modulo) (session-key) (encrypted))
    (format t "~%[2.1] -- Боб знает P. Он расшифровывает сообщение и получает K'.")
    (setq shared-password (uiop:read-file-line "shared-password")
          decrypted (crypt:aes-decrypt (read-parse "step-1-message")
                                       shared-password)
          modulo (uiop:read-file-line "step-1-message" :at 1))
    (format t "~2%~4tK' = 0x~x;
         0x~x." (setq decrypted (parse-integer decrypted)) (parse-integer modulo))
    (format t "~2%[2.2] -- Затем он генерирует случайный сеансовый ключ K, шифрует его открытым ключом,
         который он получил от Алисы, а затем используя P в качестве ключа.")
    (setq session-key (crypt:gen-session-key (length modulo))
          encrypted (aux::mod-expt session-key decrypted (parse-integer modulo)))
    (aux::write-to-file (list session-key) "session-key")
    (format t "~2%~4tСессионный ключ K = 0x~x;
~4tE_K'(K)           = 0x~x;" session-key encrypted)
    (setq encrypted (crypt:aes-encrypt (write-to-string encrypted) shared-password))
    (format t "~%~4tE_P(E_K'(K))      = 0x~x.~%" encrypted)
    (aux::write-to-file (list encrypted) "step-2-message")))


(defun step-3 ()
  (let ((shared-password) (priv-key) (modulo)
        (decrypted) (random-string) (encrypted))
    (format t "~%[3.1] -- Алиса расшифровывает сообщение, получая K.")
    (setq shared-password (uiop:read-file-line "shared-password")
          priv-key (read-parse "alice-priv-key" 1)
          modulo (read-parse "alice-priv-key" 2)
          decrypted (aux::mod-expt
                     (parse-integer (crypt:aes-decrypt (read-parse "step-2-message")
                                                       shared-password))
                     priv-key modulo))
    (format t "~2%~4tK = 0x~x." decrypted)
    (setq random-string (crypt:random-string))
    (aux::write-to-file (list random-string) "alice-random-string")
    (setq encrypted (crypt:aes-encrypt random-string (write-to-string decrypted)))
    (format t "~2%[3.2] -- Она генерирует случайную строку R_A, шифрует её с помощью K и посылает Бобу.
~%~4tR_A      = ~a;~%~4tE_K(R_A) = 0x~x.~%" random-string encrypted)
    (aux::write-to-file (list encrypted) "step-3-message")))


(defun step-4 ()
  (let ((session-key) (decrypted) (random-string) (encrypted))
    (format t "~%[4.1] -- Боб расшифровывает сообщение, получая R_A.")
    (setq session-key (uiop:read-file-line "session-key")
          decrypted (read-parse "step-3-message")
          decrypted (crypt:aes-decrypt decrypted session-key))
    (format t "~2%~4tR_A = ~a." decrypted)
    (setq random-string (crypt:random-string)
          encrypted (crypt:aes-encrypt (concatenate 'string decrypted " " random-string)
                                       session-key))
    (aux::write-to-file (list random-string) "bob-random-string")
    (aux::write-to-file (list encrypted) "step-4-message")
    (format t "~2%[4.2] -- Он генерирует другую случайную строку R_B, шифрует обе строки ключом
         K и посылает Алисе результат.~2%~4tR_B           = ~a;~%~4tE_K(R_A, R_B) = 0x~x.~%"
            random-string encrypted)))


(defun step-5 ()
  (let ((session-key) (alice-str) (decrypted) (encrypted))
    (format t "~%[5.1] -- Алиса расшифровывает сообщение, получая R_A и R_B.")
    (setq session-key (uiop:read-file-line "session-key")
          alice-str (uiop:read-file-line "alice-random-string")
          decrypted (uiop:split-string (crypt:aes-decrypt (read-parse "step-4-message")
                                                          session-key)
                                       :separator " "))
    (format t "~2%~4tR_A = ~a;~%~4tR_B = ~a." (car decrypted) (cadr decrypted))
    (when (not (equal alice-str (car decrypted)))
      (format t "~2%Строка R_A, полученная от Боба, не совпадает со строкой, посланной на 3-м этапе! Экстренное завершение протокола.~%")
      (return-from step-5))
    (format t "~2%[5.2] -- Если строка R_A, полученная от Боба, -- это та самая строка, которую она посылала
         Бобу на этапе (3), она, используя K, шифрует R_B и посылает её Бобу.")
    (setq encrypted (crypt:aes-encrypt (cadr decrypted) session-key))
    (aux::write-to-file (list encrypted) "step-5-message")
    (format t "~2%~4tE_K(R_B) = 0x~x.~%" encrypted) t))


(defun step-6 ()
  (let ((session-key) (bob-str) (decrypted))
    (format t "~%[6.1] -- Боб расшифровывает сообщение, получая R_B.")
    (setq session-key (uiop:read-file-line "session-key")
          bob-str (uiop:read-file-line "bob-random-string")
          decrypted (crypt:aes-decrypt (read-parse "step-5-message") session-key))
    (format t "~2%~4tR_B = ~a." decrypted)
    (when (not (equal bob-str decrypted))
      (format t "~2%Строка R_B, полученная от Алисы, не совпадает со строкой, посланной на 4-ом этапе! Экстренное завершение протокола.~%")
      (return-from step-6))
    (format t "~2%[6.2] -- Если строка R_B, полученная от Алисы, -- это та самая строка, которую он послал
         ей на этапе (4), то протокол завершён. ") t))


(defun eke-rsa ()
  (let ((shared-password))
    (format t "~%Введите общий пароль Алисы и Боба (по умолчанию 1q3wae123): ")
    (setq shared-password (read-line))
    (when (zerop (length shared-password))
      (setq shared-password "1q3wae123"))
    (aux::write-to-file (list shared-password) "shared-password"))
  (step-1) (stop) (step-2) (stop) (step-3) (stop)
  (step-4) (stop)
  (when (not (step-5)) (return-from eke-rsa)) (stop)
  (when (not (step-6)) (return-from eke-rsa)) (stop))
