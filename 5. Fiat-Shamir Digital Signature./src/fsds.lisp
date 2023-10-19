(defpackage :fsds
  (:use :cl))


(in-package :fsds)


(defun stop () (read-line))


(defun setup ()
  (let ((len) (p) (q) (n) (c))
    (format t "~%[0] -- Перед выпуском смарт-карт центр выбирает значение модуля n и псевдослучайную
       функцию f, отображающую строку произвольной длины в целое число из диапазона [0; n).")
    (format t "~2%Введите битовую длину l модуля (l = 2^m, l > 16) (по умолчанию 1024): ")
    (setq len (fsds-aux:get-bit-len)
          p (aux::generate-prime (ash len -1)) q (aux::generate-prime (ash len -1))
          n (* p q))
    (aux::write-to-file (list p q) "factors")
    (aux::write-to-file (list n) "modulo")
    (format t "~%~4tp = 0x~x;~%~4tq = 0x~x;~%~4tn = 0x~x.~%" p q n)
    (format t "~%Введите номер хеш-функции, используемой в псевдослучайном отображении f (по умолчанию 7 -- SHA256):~%")
    (crypt:list-all-digests) (format t "~%Ваш выбор: ")
    (setq c (fsds-aux:get-c))
    (format t "~%Псевдослучайной функцией f назначена функция: ~a(string) (mod n)."
            (crypt:hash-setter c))))


(defun issue-a-smartcard ()
  (let* ((I (fsds-aux:form-I)) (j (fsds-aux:get-j))
         (p (aux::read-parse "factors")) (q (aux::read-parse "factors" 1))
         (n (aux::read-parse "modulo")) (sjs (fsds-aux::compute-sjs I j p q n)))
    (format t "~2%[1] -- Вычислить значения v_j = f(I, j).")
    (format t "~2%[2] -- Выбрать k разных значений j, для которых v_j является квадратичным вычетом по
       модулю n, и вычислить наименьший квадратный корень s_j элемента (v_j)^-1 (mod n): ")
    (when (null sjs) (return-from issue-a-smartcard))
    (format t "~%~{~{~%~4ts_~a = 0x~x~^;~}~}." sjs)
    (format t "~2%~4t[3] -- Выпустить смарт-карту, содержащую I, k значений s_j и их индексы.")
    (fsds-aux::write-smartcard (append (list I) sjs) "smartcard")
    (format t "~2%~4tДанные смарткарты записаны в файл smartcard.") t))


(defun step-1-sign (rounds)
  (let ((n (aux::read-parse "modulo")) (r) (x) (xs))
    (format t "~2%[1] -- Алиса выбирает случайные r_1, ..., r_t из [0, n) и вычисляет x_i = (r_i)^2 (mod n):~%")
    (do ((i 0 (1+ i))) ((= rounds i))
      (setq r (random n)
            x (mod (* r r) n)
            xs (cons x xs))
      (format t "~%~4tx_~a = 0x~x;" (1+ i) x)) (terpri)
    (aux::write-to-file xs "xs")))


(defun step-2-sign (message)
  (let* ((n (aux::read-parse "modulo")) (xs (uiop:read-file-lines "xs")) (image)
         (max-j (parse-integer (car (uiop:split-string (car (last (uiop:read-file-lines "smartcard"))) :separator " "))))
         (first-bits) (rounds (aux::read-parse "num-rounds")))
    (format t "~%[2] -- Алиса вычисляет f(m, x_1, ..., x_t) и использует первые max(js) * t битов в качестве значений e_{ij}.")
    (setq image (reduce #'(lambda (f s) (concatenate 'string f s)) (append message xs))
          image (crypt:f image n))
    (format t "~2%~4tf(m, x_1, ..., x_t) = 0x~x;~%~4tПервые max(js) * t битов: 0b~a."
            image (setq first-bits (subseq (write-to-string image :base 2) 0 (* max-j rounds))))))



(defun sign-message ()
  (let ((rounds (fsds-aux::get-t)) (message (fsds-aux::get-message)))
    (step-1-sign  rounds) (stop)
    (step-2-sign message) (stop)))
