(defpackage #:nss
  (:use #:cl)
  (:export #:nss))


(in-package #:nss)


(defun stop () (read-line))


(defun step-1-substep-1 ()
  (format t "~2%[1.1] -- Создаёт набор опознавательных меток t_i и утверждает список возможных избирателей.~2%")
  (destructuring-bind (voters ts) (ta-aux:gen-voters&ts)
    (let ((len (length voters)))
      (do ((j 0 (1+ j))) ((= len j))
        (format t "~4tДля избирателя ~s была сгенерирована опознавательная метка ~a;~%"
                (nth j voters) (nth j ts))))))


(defun step-1-substep-2 ()
  (format t "~%[1.2] -- Отправляет по защищённому каналу по одной метке каждому голосующему.~2%")
  (format t "~4tВ дальнейшем для каждого избирателя будет создан индивидуальный каталог, содержащий всю необходимую информацию.~%"))


(defun step-1-substep-3 ()
  (format t "~%[1.3] -- Отправляет A весь набор меток без информации о том, какая метка кому принадлежит.~%")
  (format t "~%~4tНабор меток был сохранён в файле ts.~%"))


(defun step-1 ()
  (format t "~%~4tШаг [1]. V (регистратор)~2%[1.0.1] -- Определение числа избирателей.~%")
  (ta-aux:get-n)
  (step-1-substep-1) (stop)
  (step-1-substep-2) (stop)
  (step-1-substep-3) (stop) t)


(defun step-2-substep-1 (p q n k keys)
  (format t "~%В подписи ESIGN пара чисел (n, k) является открытым ключом, а (p, q) -- закрытым.~2%")
  (format t "~4tn = 0x~x;~%~4tk = ~a;~%~4tp = 0x~x;~%~4tq = 0x~x.~%"
          n k p q)
  (format t "~%Эти значения являются общими для всех избирателей.~%")
  (format t "~%Сгенерированные значения e-secret каждого избирателя:~%")
  (let* ((len-keys (length keys)) (len (length (write-to-string len-keys))))
    (dotimes (j len-keys (terpri))
      (format t "~%~4te-sec_{~v,'0d} = 0x~x;" len (1+ j) (nth j keys)))))


(defun step-2-substep-2 (Bs)
  (format t "~%[2.2] -- Для каждого избирателя было сформировано сообщение B:~%")
  (let* ((len-Bs (length Bs)) (len (length (write-to-string len-Bs))))
    (dotimes (j len-Bs (terpri))
      (format t "~%~4tB_{~v,'0d} = ~s;" len (1+ j) (nth j Bs)))))


(defun step-2-substep-3 (sigs)
  (format t "~%[2.3] -- Для каждого сообщения B была сформирована подпись s:~%")
  (let* ((len-sigs (length sigs)) (len (length (write-to-string len-sigs))))
    (dotimes (j len-sigs (terpri))
      (format t "~%~4ts_{~v,'0d} = 0x~x;" len (1+ j) (nth j sigs)))))


(defun step-2-substep-4 (ciphers)
  (format t "~%[2.4] -- К каждой подписи была приложена метка t и всё было зашифровано:~%")
  (let* ((len-ciphers (length ciphers)) (len (length (write-to-string len-ciphers))))
    (dotimes (j len-ciphers (terpri))
      (format t "~%~4tencr_{~v,'0d} = 0x~x;" len (1+ j) (nth j ciphers)))))


(defun step-2-substep-5 (num-users)
  (ta-aux:send-ciphertexts num-users)
  (format t "~%[2.5] -- Шифртексты с приложенной меткой t были отправлены на рассмотрение в A.~%")
  (format t "~%~4tЭти строки были сохранены в файле ciphertexts.~%"))


(defun step-2 ()
  (format t "~%~4tШаг [2]. E (избиратель)")
  (format t "~2%[2.0.1] -- Определение битовой длины чисел p и q.~%")
  (let ((bit-len (ta-aux:get-bit-len)) (num-users (aux:read-parse "n")))
    (format t "~2%[2.1] -- Генерирует e-pub, e-priv (для цифровой подписи) и e-secret (чтобы до нужного времени нельзя было узнать содержимое бюллетеня).~%")
    (format t "~%[2.1.1] -- Определение параметра безопасности k:~%")
    (destructuring-bind (p q n k) (sig:gen-keys bit-len)
      (destructuring-bind (keys Bs sigs ciphers) (ta-aux:form-folders num-users)
        (step-2-substep-1 p q n k keys) (stop)
        (step-2-substep-2 Bs          ) (stop)
        (step-2-substep-3 sigs        ) (stop)
        (step-2-substep-4 ciphers     ) (stop)
        (step-2-substep-5 num-users   ) t))))


(defun step-3-substep-1 ()
  (format t "~2%[3.1] -- Агенство получает шифртексты с тегами. По ним оно определяет, что сообщение пришло
         от легитимного пользователя,но не может определить, ни от какого, ни как он проголосовал.")
  (let ((correct-tags? (ta-aux:compare-tags)))
    (format t "~2%~4tРезультат сравнения отправленных стороной V меток и меток пользователей: ~a.~%"
            correct-tags?) correct-tags?))


(defun step-3-substep-2 ()
  (format t "~%[3.2] -- Выкладывает в открытый доступ полученную пару p тег-шифр:~%")
  (let* ((ciphertexts (uiop:read-file-lines "ciphertexts"))
         (len-ciph (length ciphertexts))
         (len (length (write-to-string len-ciph))) pair)
    (setq ciphertexts (mapcar #'(lambda (ciphertext)
                                  (uiop:split-string ciphertext :separator " "))
                              ciphertexts))
    (dotimes (j len-ciph (terpri))
      (setq pair (nth j ciphertexts))
      (format t "~%~4tp_{~v,'0d} = ~a, 0x~x;"
              len (1+ j) (car pair) (cadr pair)))))


(defun step-3 ()
  (format t "~2%~4tШаг [3]. A (агенство)")
  (when (not (step-3-substep-1))
    (format t "~%Не все значения меток совпали! Завершение протокола.")
    (return-from step-3 nil)) (stop)
  (step-3-substep-2) (stop) t)


(defun step-4 ()
  (format t "~%~4tШаг [4]. Опубликованный файл служит сигналом E отправить секретный ключ e-secret.~%")
  (stop) t)


(defun step-5-substep-1 (num-users)
  (format t "~2%[5.1] -- Собирает ключи.~%")
  (let* ((keys (ta-aux:collect-keys num-users))
         (len-keys (length keys))
         (len (length (write-to-string len-keys))))
    (dotimes (j len-keys (terpri))
      (format t "~%~4te-sec_{~v,'0d} = ~a;" len (1+ j) (nth j keys)))))


(defun step-5-substep-2 (num-users)
  (format t "~%[5.2] -- Расшифровывает сообщения.~%")
  (let* ((decrypted (ta-aux:decrypt-received num-users))
         (len-decr (length decrypted))
         (len (length (write-to-string len-decr))) pair)
    (setq decrypted (mapcar #'(lambda (cipher)
                                (uiop:split-string cipher :separator " "))
                            decrypted))
    (dotimes (j len-decr (terpri))
      (setq pair (nth j decrypted))
      (format t "~%~4tp_{~v,'0d} = ~a, 0x~x;"
              len (1+ j) (car pair) (cadr pair)))))


(defun step-5-substep-3 (num-users)
  (format t "~%[5.3] -- Производит подсчёт голосов.")
  (let* ((election-results (ta-aux:count-voices num-users))
         (list-voices (loop for j from 0 below (length election-results)
                            when (oddp j)
                              collect (nth j election-results))))
    (format t "~2%Результаты выборов:~%")
    (dotimes (j (length ta-aux:*candidates*) (terpri))
      (format t "~%~4tКандидат ~a набрал ~d голос(а/ов);"
              (nth j ta-aux:*candidates*) (nth j list-voices)))
    (format t "~4tКоличество избирателей, воздержавшихся от голосования: ~d.~%"
            (car (last list-voices)))))


(defun step-5-substep-4 (num-users)
  (format t "~%[5.4] -- Присоединяет к опубликованному шифртексту бюллетень без опознавательного тега, на чём голосование заканчивается.~%")
  (let* ((votes (ta-aux:collect-Bs num-users))
         (ciphers (uiop:read-file-lines "ciphertexts"))
         (len (length (write-to-string num-users)))
         (num-cands (length ta-aux:*candidates*)) vote)
    (setq votes (mapcar #'(lambda (vote&tag)
                            (car (uiop:split-string vote&tag :separator " "))) votes)
          votes (mapcar #'parse-integer votes)
          ciphers (mapcar #'(lambda (tag&cipher)
                              (cadr (uiop:split-string tag&cipher :separator " "))) ciphers)
          ciphers (mapcar #'parse-integer ciphers))
    (dotimes (j num-users (terpri))
      (setq vote (1+ (nth j votes)))
      (when (= (1+ num-cands) vote)
        (setq vote 'X))
      (format t "~%~4tv&c_{~v,'0d} = ~a, 0x~x;"
              len (1+ j) vote (nth j ciphers)))))


(defun step-5 ()
  (let ((num-users (aux:read-parse "n")))
    (format t "~%~4tШаг [5]. A (агенство)")
    (step-5-substep-1 num-users) (stop)
    (step-5-substep-2 num-users) (stop)
    (step-5-substep-3 num-users) (stop)
    (step-5-substep-4 num-users) t))


(defun nss ()
  (format t "~%~25t[ПРОТОКОЛ ДВУХ АГЕНСТВ]~%")
  (step-1) (step-2)
  (step-3) (step-4)
  (step-5) t)
