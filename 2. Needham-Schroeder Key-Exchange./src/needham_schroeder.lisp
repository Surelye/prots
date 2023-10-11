(defpackage #:nske
  (:use :cl))

(in-package #:nske)


(defun stop () (read-line))


(defun read-parse (filename &optional (at 0))
  (parse-integer (uiop:read-file-line filename :at at)))


(defun step-1 ()
  (let ((name-alice) (name-bob) (random-num) (bound))
    (format t "~%[1] -- Алиса посылает Тренту сообщение, содержащее её имя (A), имя Боба (B) и случайное число (R_A): ") (stop)
    (setq name-alice (uiop:read-file-line "alice-pub-key")
          name-bob   (uiop:read-file-line   "bob-pub-key")
          bound      (read-parse "alice-pub-key" 2)
          random-num (random bound))
    (format t "~%    Имя Алисы A: ~a;
    Имя Боба  B: ~a;
    Число   R_A: 0x~x.~%" name-alice name-bob random-num)
    (aux::write-to-file (list name-alice name-bob random-num) "step-1-message")
    (format t "~%Сообщение было сохранено в файл step-1-message.~%")))


(defun step-2 ()
  (let ((bound) (k) (name-alice) (key-bob) (ciphertext)
        (random-alice) (name-bob) (key-alice))
    (format t "~%[2] -- Трент генерирует случайный сеансовый ключ (K). Он шифрует сообщение, содержащее случайный
       сеансовый ключ (K) и имя Алисы (A), секретным ключом, общим для него и Боба (E_B(K, A)). Затем он шифрует
       случайное число Алисы (R_A), имя Боба (B), ключ (K), и шифрованное сообщение секретным ключом, общим
       для него и Алисы (E_A(R_A, B, K, E_B(K, A))). Наконец, он отправляет шифрованное сообщение Алисе: ") (stop)
    (setq bound (read-parse "alice-pub-key" 2) k (random bound)
          name-alice (uiop:read-file-line "step-1-message")
          key-bob (read-parse "bob-pub-key" 1)
          ciphertext (mapcar #'(lambda (num) (aux::mod-expt num key-bob bound))
                             (list k (mod (sxhash name-alice) bound)))
          random-alice (read-parse "step-1-message" 2)
          name-bob (uiop:read-file-line "step-1-message" :at 1)
          key-alice (read-parse "alice-pub-key" 1)
          ciphertext (mapcar #'(lambda (num) (aux::mod-expt num key-alice bound))
                             (append (list random-alice (mod (sxhash name-bob) bound) k)
                                     ciphertext)))
    (format t "~%    Публичный ключ Алисы: 0x~x;
    Публичный ключ Боба:  0x~x;
    Сеансовый ключ K:     0x~x;
    Имя Алисы A:          ~a;
    Число R_A:            0x~x;
    Имя Боба B:           ~a.~%"
            key-alice key-bob k name-alice random-alice name-bob)
    (aux::write-to-file ciphertext "step-2-ciphertext")
    (format t "~%Зашифрованное сообщение было сохранено в файле step-2-ciphertext.~%")))


(defun step-3 ()
  (let ((bound) (priv-alice) (plaintext) (random-alice)
        (random-alice-step-1) (hash-bob) (k) (name-bob))
    (format t "~%[3] -- Алиса расшифровывает сообщение и извлекает K. Она убеждается, что R_A совпадает со
       значением, отправленным Тренту на этапе (1). Затем она посылает Бобу сообщение, зашифрованное Трентом
       ключом Боба: ") (stop)
    (setq bound (read-parse "alice-pub-key" 2)
          priv-alice (read-parse "alice-priv-key" 1)
          plaintext (mapcar #'(lambda (num) (aux::mod-expt num priv-alice bound))
                            (mapcar #'parse-integer (uiop:read-file-lines "step-2-ciphertext")))
          random-alice (nth 0 plaintext) hash-bob (nth 1 plaintext)
          random-alice-step-1 (read-parse "step-1-message" 2)
          k (nth 2 plaintext) name-bob (uiop:read-file-line "bob-pub-key"))
    (when (/= random-alice random-alice-step-1)
      (format t "~%Расшифрованное значение R_A не совпадает с отправленным на первом шаге! Экстренное завершение протокола.~%")
      (return-from step-3 t))
    (format t "~%    Число R_A:              0x~x;~%" random-alice)
    (when (/= (mod (sxhash name-bob) bound) hash-bob)
      (format t "~%Расшифрованное имя Боба не совпадает с отправленным на первом шаге! Экстренное завершение протокола.~%")
      (return-from step-3 t))
    (format t "    Имя Боба B:             ~a;
    Сеансовый ключ K Алисы: 0x~x.~%" name-bob k)
    (aux::write-to-file (list k) "sess-alice")
    (aux::write-to-file (list (nth 3 plaintext) (nth 4 plaintext)) "step-3-ciphertext")
    (format t "~%Сообщение, зашифрованное Трентом ключом Боба, было сохранено в файле step-3-ciphertext.~%")))


(defun step-4 ()
  (let ((bound) (priv-bob) (name-alice) (hash-alice) (plaintext) (k) (random-bob))
    (format t "~%[4] -- Боб расшифровывает сообщение и извлекает K. Затем он генерирует другое случайное число, R_B.
       Он шифрует это число ключом K и отправляет его Алисе: ") (stop)
    (setq bound (read-parse "bob-pub-key" 2)
          priv-bob (read-parse "bob-priv-key" 1)
          name-alice (uiop:read-file-line "alice-pub-key")
          plaintext (mapcar #'(lambda (num) (aux::mod-expt num priv-bob bound))
                            (mapcar #'parse-integer (uiop:read-file-lines "step-3-ciphertext")))
          k (nth 0 plaintext) hash-alice (nth 1 plaintext)
          random-bob (random bound))
    (format t "~%    Сеансовый ключ K Боба: 0x~x;" k)
    (when (/= (mod (sxhash name-alice) bound) hash-alice)
      (format t "~2%Полученное имя Алисы не совпадает с действительным! Экстренное завершение протокола.~%")
      (return-from step-4 t))
    (format t "~%    Имя Алисы A:           ~a;
    Число R_B:             0x~x.~%" name-alice random-bob)
    (aux::write-to-file (list k) "sess-bob")
    (aux::write-to-file (list (logxor random-bob k)) "step-4-encrypted"))
    (format t "~%Зашифрованное Бобом число было сохранено в файле step-4-encrypted.~%"))


(defun step-5 ()
  (format t "~%[5] -- Алиса расшифровывает сообщение с помощью ключа K. Она создает число R_B - 1 и шифрует это число
       ключом K. Затем посылает это сообщение Бобу: ") (stop)
  (let ((sess) (encrypted) (decrypted))
    (setq sess (read-parse "sess-alice")
          encrypted (read-parse "step-4-encrypted")
          decrypted (logxor sess encrypted))
    (format t "~%    Сеансовый ключ K Алисы:        0x~x;
    Расшифрованное число Боба R_B: 0x~x.~%" sess decrypted)
    (aux::write-to-file (list (logxor (1- decrypted) sess)) "step-5-encrypted")
    (format t "~%Зашифрованное Алисой число было сохранено в файле step-5-encrypted.~%")))


(defun step-6 ()
  (format t "~%[6] -- Боб расшифровывает сообщение с помощью ключа K и проверяет значение R_B - 1: ") (stop)
  (let ((sess) (random-bob) (random-bob-pred?))
    (setq sess (read-parse "sess-bob")
          random-bob (logxor (read-parse "step-4-encrypted") sess)
          random-bob-pred? (logxor (read-parse "step-5-encrypted") sess))
    (format t "~%    Сеансовый ключ K Боба:    0x~x;
    Случайное число Боба R_B: 0x~x;
    Число Алисы R_B - 1:      0x~x.~%" sess random-bob random-bob-pred?)
    (if (zerop (logxor random-bob (1+ random-bob-pred?)))
        (format t "~%Обмен ключами прошёл успешно.~%")
        (format t "~%Обмен ключами не удался.~%"))))


(defun nske ()
  (let ((bit-len))
    (format t "~%Введите битовую длину l модуля RSA (l = 2^k, l > 16): ")
    (setq bit-len (aux::get-bit-len)) (terpri)
    (rsa::rsa-generate-keys bit-len '("alice" "bob"))
          (step-1)                           (step-2)
    (when (step-3) (return-from nske)) (when (step-4) (return-from nske))
          (step-5)                           (step-6)))
