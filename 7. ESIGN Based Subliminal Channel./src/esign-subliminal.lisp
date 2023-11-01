(defpackage #:esign-sc
  (:use #:cl)
  (:export #:gen-keys
           #:sign-message-usual
           #:verify-message-usual
           #:sign-message-subliminal
           #:verify-message-subliminal
           #:recover-subliminal-message))


(in-package #:esign-sc)


(defun stop () (read-line))


(defun gen-keys ()
  (format t "~%~20t[ЭТАП ГЕНЕРАЦИИ КЛЮЧЕЙ]")
  "Функция генерации открытого (n = ppqr) и закрытого ключей (p, q и r).
   После генерации каждое из чисел по отдельности лежит в соответствующих
   именных файлах."
  (format t "~2%Введите битовую длину l открытого ключа n (l = 2^m, l > 63): ")
  (let* ((bit-len (sc-aux:get-bit-len)) (n) (p) (q) (r))
    (setq n (sc-aux:gen-n bit-len) p (aux:read-parse "p")
          q (aux:read-parse "q")   r (aux:read-parse "r"))
    (format t "~%Были сгенерированы значения: ")
    (format t "~2%~4tp = 0x~x;~%~4tq = 0x~x;~%~4tr = 0x~x;~%~4tn = 0x~x." p q r n) t))


(defun sign-message-usual ()
  "Функция подписания обычного сообщения. Результатом работы функции является
   значение подписи s."
  (format t "~%~20t[ПОДПИСЬ ОБЫЧНОГО СООБЩЕНИЯ]~%")
  (format t "~%[1] -- Определение подписываемого сообщения:~%")
  (let ((message (sc-aux:get-message)) (x) (w) (s))
    (format t "~2%[2] -- Определение параметра безопасности k:~%")
    (sc-aux:get-k)
    (format t "~2%[3] -- Чтобы подписать обычное сообщение, Алиса сначала выбирает случайное число x,
       меньшее pqr: ")
    (setq x (sc-aux:pick-x))
    (format t "~2%~4tx = 0x~x;~%" x) (stop)
    (format t "~%[4] -- Далее она вычисляет w -- наименьшее целое, которое больше или равно
       (H(m) - x^k mod n) / (pqr): ")
    (setq w (sc-aux:compute-w message))
    (format t "~2%~4tw = 0x~x;~%" w) (stop)
    (format t "~%[5] -- Затем она вычисляет значение s = x + ((w / (kx^{k-1})) mod p) * pqr подписи: ")
    (setq s (sc-aux:compute-s))
    (format t "~2%~4ts = 0x~x.~%" s) t))


(defun step-3-verify-message-usual (s)
  (let ((s^k))
    (format t "~2%[3] -- Для проверки подписи Боб вычисляет s^k (mod n): ")
    (setq s^k (sc-aux:compute-s^k s))
    (format t "~2%~4ts^k (mod n) = 0x~x;~%" s^k) t))


(defun step-4-5-verify-message-usual (message)
  (let* ((n (aux:read-parse "n")) (digest (crypt:hash message n))
         (s^k (aux:read-parse "s^k")) (a) (2^a) (digest+) (max-len))
     (format t "~%[4] -- Кроме того, он вычисляет a, наименьшее целое, которое больше или равно
       утроенному числу битов n, делённому на четыре: ")
    (setq a (sc-aux:compute-a) 2^a (ash 1 a) digest+ (+ digest 2^a)
          max-len (apply #'max (mapcar #'(lambda (num)
                                           (length (write-to-string num :base 16)))
                                       (list digest s^k digest+))))
    (format t "~2%~4ta           = ~d;~%" a) (stop)
    (format t "~%[5] -- Если H(m) меньше или равна s^k mod n, и если s^k mod n меньше H(m) + 2^a,
       то подпись считается правильной: ")
    (format t "~2%~4tH(m)        = 0x~v,'0x;~%~4ts^k (mod n) = 0x~v,'0x;~%~4tH(m) + 2^a  = 0x~v,'0x;~%~4t2^a         = 0x~v,'0x."
            max-len digest max-len s^k max-len digest+ max-len 2^a)
    (format t (if (< digest (1+ s^k) (1+ digest+))
                  "~2%Подпись корректна.~%"
                  "~2%Значение подписи некорректно.~%")) t))


(defun verify-message-usual ()
  (format t "~%~20t[ПРОВЕРКА ПОДПИСИ ОБЫЧНОГО СООБЩЕНИЯ]~%")
  (format t "~%[1] -- Определение проверяемого сообщения:~%")
  (let* ((message (sc-aux:get-message)) (s))
    (format t "~2%[2] -- Определение проверяемой подписи:~%")
    (setq s (sc-aux:get-signature))
    (step-3-verify-message-usual s) (stop)
    (step-4-5-verify-message-usual (sc-aux:concat message)) t))


(defun step-4-sign-message-subliminal (subliminal-message)
  (let ((x-sub) (encrypted))
    (format t "~2%[4] -- Чтобы скрыть сообщение M, используя сообщение M', Алиса вычисляет
       x' = M + ur (u из [1, pq - 1]): ")
    (setq x-sub (sc-aux:pick-x-subliminal subliminal-message))
    (when (null x-sub)
      (return-from step-4-sign-message-subliminal nil))
    (setq encrypted (aux:read-parse "subliminal-message-encrypted"))
    (format t "~2%~4tE(M) = 0x~x;~%~4tx'   = 0x~x;~%" encrypted x-sub) t))


(defun step-5-sign-message-subliminal (innocuous-message)
  (let ((w-sub))
    (format t "~%[5] -- Далее она вычисляет w как наибольшее целое выражения
       (H(M') - x'^k (mod n)) / (pqr): ")
    (setq w-sub (sc-aux:compute-w-subliminal innocuous-message))
    (format t "~2%~4tw' = 0x~x;~%" w-sub) t))


(defun step-6-sign-message-subliminal ()
  (let ((s-sub))
    (format t "~%[6] -- Затем Алиса аналогично вычисляет s с использованием модифицированных x' и w': ")
    (setq s-sub (sc-aux:compute-s-subliminal))
    (format t "~2%~4ts  = 0x~x.~%" s-sub)))


(defun sign-message-subliminal ()
  (format t "~%~20t[ПОДПИСЬ СООБЩЕНИЯ С ВНЕДРЕНИЕМ СКРЫТОГО]~%")
  (let ((innocuous-message) (subliminal-message))
    (format t "~%[1] -- Ввод \"безобидного\" сообщения: ~%")
    (setq innocuous-message (sc-aux:concat (sc-aux:get-message)))
    (format t "~2%[2] -- Ввод скрываемого сообщения: ~%")
    (setq subliminal-message (sc-aux:concat (sc-aux:get-message)))
    (format t "~2%[3] -- Определение параметра безопасности k:~%")
    (sc-aux:get-k)
    (when (null (step-4-sign-message-subliminal subliminal-message))
      (return-from sign-message-subliminal nil)) (stop)
    (step-5-sign-message-subliminal innocuous-message) (stop)
    (step-6-sign-message-subliminal) t))


(defun step-3-verify-message-subliminal (s)
  (let ((s^k))
    (format t "~2%[3] -- Уолтер вычисляет s^k (mod n):")
    (setq s^k (sc-aux:compute-s^k-subliminal s))
    (format t "~2%~4ts^k (mod n) = 0x~x.~%" s^k) t))


(defun step-4-verify-message-subliminal (message)
  (format t "~%[4] -- Он убеждается, что подпись корректна путём проверки равенства
       H(m) <= s^k (mod n) < H(m) + 2^a:~%")
  (let* ((n (aux:read-parse "n")) (digest (crypt:hash message n))
         (s^k (aux:read-parse "s^k-sublim")) (a (sc-aux:compute-a))
         (2^a (ash 1 a)) (digest+ (+ digest 2^a))
         (max-len (apply #'max (mapcar #'(lambda (num)
                                           (length (write-to-string num :base 16)))
                                       (list digest s^k digest+)))))
    (format t "~%~4tH(M)        = 0x~v,'0x;~%~4ts^k (mod n) = 0x~v,'0x;
   ~4tH(M) + 2^a  = 0x~v,'0x;~%~4t2^a         = 0x~v,'0x.~%"
            max-len digest max-len s^k max-len digest+ max-len 2^a)
    (format t (if (< digest (1+ s^k) (1+ digest+))
                  "~%Подпись корректна.~%"
                  "~%Значение подписи некорректно.~%")) t))


(defun verify-message-subliminal ()
  (format t "~%~20t[ПРОВЕРКА ПОДПИСИ СО СКРЫТЫМ СООБЩЕНИЕМ]~%")
  (format t "~%[1] -- Определение проверяемого сообщения:~%")
  (let ((message (sc-aux:concat (sc-aux:get-message))) (s))
    (format t "~2%[2] -- Определение проверяемой подписи:~%")
    (setq s (sc-aux:get-signature))
    (step-3-verify-message-subliminal s) (stop)
    (step-4-verify-message-subliminal message) t))


(defun recover-subliminal-message ()
  (format t "~%~20t[ИЗВЛЕЧЕНИЕ СКРЫТОГО СООБЩЕНИЯ ИЗ ПОДПИСИ]~%")
  (format t "~%[1] -- Определение файла с подписью:~%")
  (let* ((r (aux:read-parse "r")) (s (sc-aux:get-signature))
         (session-key (uiop:read-file-line "subliminal-key"))
         (encrypted (mod s r)) (decrypted))
    (format t "~2%[2] -- Для восстановления скрытого сообщения достаточно вычислить
       s = x' + ypqr = M + ur + ypqr = M (mod r): ")
    (format t "~2%~4tE(M) = 0x~x;~%" encrypted) (stop)
    (setq decrypted (crypt:aes-decrypt encrypted session-key))
    (format t "~%[3] -- Расшифруем E(M) и получим:~%")
    (format t "~%~4tM = ~s.~%" decrypted) t))
