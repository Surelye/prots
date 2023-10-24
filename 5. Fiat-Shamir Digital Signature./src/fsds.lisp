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
            (crypt:hash-setter c)) t))


(defun step-1-issue ()
  (let ((I (fsds-aux:form-I)) (j (fsds-aux:get-j)) (vjs)
        (n (aux::read-parse "modulo")))
    (format t "~%[1] -- Вычислить значения v_j = f(I, j) для небольших значений j.")
    (setq vjs (mapcar #'(lambda (k)
                          (crypt:f (concatenate 'string I (write-to-string k)) n))
                      (loop for k from 1 to j collect k)))
    (aux::write-to-file vjs "vjs-issue") (terpri)
    (do ((k 0 (1+ k))) ((= (length vjs) k))
      (format t "~%~4tv_~2d = 0x~x;" (1+ k) (nth k vjs)))
    (terpri) t))


(defun step-2-issue ()
  (format t "~%[2] -- Выбрать k разных значений j, для которых v_j является квадратичным вычетом по
       модулю n, и вычислить наименьший квадратный корень s_j элемента (v_j)^-1 (mod n): ") (terpri)
  (let* ((vjs (mapcar #'parse-integer (uiop:read-file-lines "vjs-issue")))
         (j (length vjs)) (n (aux::read-parse "modulo"))
         (p (aux::read-parse "factors")) (q (aux::read-parse "factors" 1))
         (sjs) (val) (roots) (indices))
    (do ((k 0 (1+ k))) ((= j k))
      (setq val (nth k vjs))
      (when (= 1 (aux::compute-jacobi val n))
        (setq val (mod (cadr (aux::ext-gcd val n)) n)
              roots (aux::sqrt-Zn val p q n))
        (when roots
          (setq sjs (cons (apply #'min roots) sjs)
                indices (cons (1+ k) indices)))))
    (if indices
      (progn (setq sjs (reverse sjs) indices (reverse indices))
             (aux::write-to-file sjs "sjs")
             (aux::write-to-file indices "indices"))
      (progn (format t "~%Ни один из v_j не подошёл! Повторная генерация модуля...")
             (return-from step-2-issue)))
    (do ((k 0 (1+ k))) ((= (length sjs) k))
      (format t "~%~4ts_~2d = 0x~x;" (nth k indices) (nth k sjs))) (terpri) t))


(defun step-3-issue ()
  (format t "~%[3] -- Выпустить смарт-карту, содержащую I, k значений s_j и их индексы.")
  (uiop:run-program "mkdir smartcard/")
  (uiop:run-program "mv I sjs indices smartcard/") t)


(defun issue-a-smartcard ()
             (step-1-issue)                                   (stop)
  (when (not (step-2-issue)) (return-from issue-a-smartcard)) (stop)
             (step-3-issue) t)


(defun step-1-sign (rounds)
  (let ((n (aux::read-parse "modulo")) (r) (rs) (x) (xs))
    (format t "~%[1] -- Алиса выбирает случайные r_1, ..., r_t из [0, n) и вычисляет x_i = (r_i)^2 (mod n):~%")
    (do ((i 0 (1+ i))) ((= rounds i))
      (setq r (random n)
            rs (cons r rs)
            x (mod (* r r) n)
            xs (cons x xs))
      (format t "~%~4tr_~2d = 0x~x;~%~4tx_~2d = 0x~x;~%" (1+ i) r (1+ i) x)) (terpri)
    (aux::write-to-file (reverse rs) "rs")
    (aux::write-to-file (reverse xs) "xs") t))


(defun step-2-sign (message)
  (let* ((n (aux::read-parse "modulo")) (xs (uiop:read-file-lines "xs")) (image)
         (max-j (parse-integer (car (last (uiop:read-file-lines "smartcard/indices")))))
         (first-bits) (rounds (aux::read-parse "num-rounds"))
         (image-bin-len))
    (format t "[2] -- Алиса вычисляет f(m, x_1, ..., x_t) и использует первые max(js) * t битов в качестве значений e_{ij}.")
    (setq image (reduce #'(lambda (f s) (concatenate 'string f s)) (append message xs))
          image (crypt:f image n)
          image-bin-len (length (write-to-string image :base 2)))
    (when (< image-bin-len (* max-j rounds))
      (format t "~2%В образе меньше, чем max(js) * t битов! Попробуйте подписывать по другому модулю,
выбрать другую хеш-функцию или заново сгенерировать модуль.")
      (return-from step-2-sign))
    (format t "~2%~4tf(m, x_1, ..., x_t) = 0x~x;~%~4tПервые max(js) * t битов: 0b~a."
            image (setq first-bits (subseq (write-to-string image :base 2) 0 (* max-j rounds))))
    (terpri) (aux::write-to-file (list first-bits) "first-bits") t))


(defun step-3-sign ()
  (let* ((sjs (fsds-aux:read-parse-l "smartcard/sjs"))
         (indices (fsds-aux:read-parse-l "smartcard/indices"))
         (rs (fsds-aux:read-parse-l "rs")) (ys)
         (first-bits (uiop:read-file-line "first-bits")) (n (aux::read-parse "modulo")))
    (format t "~%[3] -- Алиса вычисляет y_i = r_i П_{e_ij = 1} s_j (mod n) для i = 1, ..., t и
       отправляет I, m, строку с e_ij и элементы y_i Бобу.")
    (setq ys (fsds-aux:compute-ys rs sjs first-bits indices n))
    (format t "~2%~4tВычисленные значения y_i:~%")
    (do ((i 0 (1+ i))) ((= (length ys) i))
      (format t "~%~4ty_~2d = 0x~x;" (1+ i) (nth i ys))) (terpri)
    (aux::write-to-file ys "ys") t))


(defun sign-message ()
  (let ((rounds (fsds-aux:get-t)) (message (fsds-aux:get-message)))
               (step-1-sign  rounds)                              (stop)
    (when (not (step-2-sign message)) (return-from sign-message)) (stop)
               (step-3-sign        ) t))


(defun step-1-verify ()
  (format t "~%[1] -- Боб вычисляет v_j = f(i, j) для полученных значений j.")
  (let ((indices (uiop:read-file-lines "smartcard/indices"))
        (I (uiop:read-file-line "smartcard/I"))
        (n (aux::read-parse "modulo")) (vjs))
    (setq vjs (mapcar #'(lambda (j)
                          (crypt:f (concatenate 'string I j) n))
                      indices))
    (aux::write-to-file vjs "vjs-verify") (terpri)
    (do ((i 0 (1+ i))) ((= (length indices) i))
      (format t "~%~4tv_~2d = 0x~x;" (1+ i) (nth i vjs))) (terpri) t))


(defun step-2-verify ()
  (format t "~%[2] -- Боб вычисляет z_i = (y_i)^2 П_{e_ij = 1} v_j (mod n) для i = 1, ..., t.~%")
  (let* ((ys (fsds-aux:read-parse-l "ys"))
         (vjs (fsds-aux:read-parse-l "vjs-verify"))
         (indices (fsds-aux:read-parse-l "smartcard/indices"))
         (n (aux::read-parse "modulo")) (t-val (length ys)) (zis)
         (first-bits (uiop:read-file-line "first-bits")) (prod 1)
         (step (floor (length first-bits) t-val)) (cur-bits) (y))
    (do ((k 0 (1+ k))) ((= t-val k))
      (setq cur-bits (subseq first-bits (* k step) (* (1+ k) step)))
      (do ((j 0 (1+ j))) ((= (length vjs) j))
        (when (not (zerop (digit-char-p (char cur-bits (1- (nth j indices))))))
          (setq prod (mod (* prod (nth j vjs)) n))))
      (setq y (nth k ys) zis (cons (mod (* y y prod) n) zis) prod 1))
    (setq zis (reverse zis))
    (do ((j 0 (1+ j))) ((= (length zis) j))
      (format t "~%~4tz_~2d = 0x~x;" (1+ j) (nth j zis)))
    (aux::write-to-file zis "zis") (terpri) t))


(defun step-3-verify (message)
  (format t "~%[3] -- Боб убеждается, что первые max(js) * t битов f(m, z_1, ..., z_t) совпадают с e_{ij}.")
  (let ((max-j (parse-integer (car (last (uiop:read-file-lines "smartcard/indices")))))
        (t-val (aux::read-parse "num-rounds")) (n (aux::read-parse "modulo"))
        (first-bits (uiop:read-file-line "first-bits"))
        (zis (uiop:read-file-lines "zis")) (digest) (digest-bits))
    (setq digest (crypt:f (reduce #'(lambda (f s) (concatenate 'string f s))
                                  (append message zis)) n)
          digest-bits (subseq (write-to-string digest :base 2) 0 (* max-j t-val)))
    (format t "~2%~4tПервые max(js) * t битов:
~%~4tf(m, z_1, ..., z_t): 0b~a;
~4t             e_{ij}: 0b~a." digest-bits first-bits)
    (format t (if (string-equal first-bits digest-bits)
                  "~2%Подпись сообщения m подтверждена."
                  "~2%Подпись сообщения m некорректна.")) t))


(defun verify-signature ()
  (format t "~%Введите имя файла, в котором содержится сообщение, подпись которого проверяем (по умолчанию message): ")
  (let ((filename) (message))
    (tagbody try-again
       (setq filename (read-line))
       (when (zerop (length filename))
         (setq filename "message"))
       (when (not (uiop:file-exists-p filename))
         (format t "Файла с указанным именем не существует! Попробуйте ввести имя файла снова: ")
         (go try-again)))
    (setq message (uiop:read-file-lines filename))
    (step-1-verify        ) (stop)
    (step-2-verify        ) (stop)
    (step-3-verify message) t))
