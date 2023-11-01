(defpackage #:sc-aux
  (:use #:cl)
  (:export #:get-bit-len
           #:gen-n
           #:concat
           #:get-message
           #:get-signature
           #:get-k
           #:pick-x
           #:compute-w
           #:compute-s
           #:compute-s^k
           #:compute-a
           #:pick-x-subliminal
           #:compute-w-subliminal
           #:compute-s-subliminal
           #:compute-s^k-subliminal))


(in-package #:sc-aux)


(defun get-bit-len ()
  "Функция считывания битовой длины числа."
  (let ((bit-len))
    (tagbody try-again
       (setq bit-len (read))
       (unless (and (integerp bit-len) (> bit-len 63)
                    (zerop (logand bit-len (1- bit-len))))
         (format t "~%Некорректное значение битовой длины l! Попробуйте ввести l снова: ")
         (go try-again))) bit-len))


(defun gen-n (target-len)
  "Функция генерации открытого ключа n."
  (let* ((1/4-len (ash target-len -2)) (p? (aux:generate-prime 1/4-len))
         (q? (aux:generate-prime 1/4-len)) (r? (aux:generate-prime 1/4-len))
         (n))
    (destructuring-bind (p q r) (sort (list p? q? r?) #'<)
      (aux:write-to-file (list p) "p")
      (aux:write-to-file (list q) "q")
      (aux:write-to-file (list r) "r")
      (setq n (* p p q r)))
    (aux:write-to-file (list n) "n") n))


(defun concat (message)
  (reduce #'(lambda (f s) (concatenate 'string f s)) message))


(defun get-message ()
  "Функция считывания сообщения из файла."
  (format t "~%Введите имя файла, в котором содержится сообщение (по умолчанию message): ")
  (let ((filename))
    (tagbody try-again
       (setq filename (read-line))
       (when (zerop (length filename))
         (setq filename "message"))
       (when (not (uiop:file-exists-p filename))
         (format t "~%Файла с указанным именем не существует! Попробуйте ввести имя файла снова: ")
         (go try-again)))
    (uiop:read-file-lines filename)))


(defun get-signature ()
  (format t "~%Введите имя файла, в котором содержится подпись (по умолчанию s): ")
  (let ((filename))
    (tagbody try-again
       (setq filename (read-line))
       (when (zerop (length filename))
         (setq filename "s"))
       (when (not (uiop:file-exists-p filename))
         (format t "~%Файла с указанным именем не существует! Попробуйте ввести имя файла снова: ")
         (go try-again)))
    (aux:read-parse filename)))


(defun get-k ()
  "Функция считывания параметра безопасности k."
  (let ((k) (default 4))
    (format t "~%Введите значение параметра безопасности k (k >= 4, по умолчанию ~d): "
            default)
    (tagbody try-again
       (setq k (read-line))
       (when (zerop (length k))
         (aux:write-to-file (list default) "k")
         (return-from get-k default))
       (setq k (parse-integer k :junk-allowed t))
       (when (or (null k) (< k 4))
         (format t "~%Некорректное значение k! Попробуйте ввести k снова: ")
         (go try-again)))
    (aux:write-to-file (list k) "k") k))


(defun pick-x ()
  "Подпись обычного сообщения.
   Функция выбора случайного числа x, меньшего pqr."
  (let* ((p (aux:read-parse "p")) (q (aux:read-parse "q"))
         (r (aux:read-parse "r")) (x))
    (setq x (+ 2 (random (- (* p q r) 2))))
    (aux:write-to-file (list x) "x") x))


(defun compute-w (message)
  "Подпись обычного сообщения.
   Функция вычисления w -- наименьшего целого, которое больше или равно
   (H(m) - x^k mod n) / (pqr)."
  (setq message (reduce #'(lambda (f s) (concatenate 'string f s)) message))
  (let ((x (aux:read-parse "x")) (k (aux:read-parse "k"))
        (n (aux:read-parse "n")) (p (aux:read-parse "p"))
        (q (aux:read-parse "q")) (r (aux:read-parse "r")) (w))
    (setq w (ceiling (/ (- (crypt:hash message n) (aux:mod-expt x k n))
                        (* p q r))))
    (aux:write-to-file (list w) "w") w))


(defun compute-s ()
  "Подпись обычного сообщения.
   Функция вычисления s = x + (w / (kx^{k-1}) mod p) * pqr -- подписи
   сообщения m."
  (let ((x (aux:read-parse "x")) (w (aux:read-parse "w")) (k (aux:read-parse "k"))
        (p (aux:read-parse "p")) (q (aux:read-parse "q")) (r (aux:read-parse "r"))
        (inv) (s))
    (setq inv (mod (cadr (aux:ext-gcd (* k (aux:mod-expt x (1- k) p)) p)) p)
          s (+ x (* (mod (* w inv) p) p q r)))
    (aux:write-to-file (list s) "s") s))


(defun compute-s^k (s)
  "Проверка подписи.
   Функция вычисления s^k (mod n)."
  (let ((k (aux:read-parse "k")) (n (aux:read-parse "n")) (s^k))
    (setq s^k (aux:mod-expt s k n))
    (aux:write-to-file (list s^k) "s^k") s^k))


(defun compute-a ()
  "Проверка подписи.
   Функция вычисления a -- наименьшего целого, которое больше или равно
   утроенному числу битов n, делённому на четыре."
  (let ((n-bits (length (write-to-string (aux:read-parse "n") :base 2))) (a))
    (setq a (ceiling (* 3/4 n-bits)))
    (aux:write-to-file (list a) "a") a))


(defun pick-x-subliminal (subliminal-message)
  (let* ((r (aux:read-parse "r")) (n (aux:read-parse "n")) (p) (q)
         (session-key (crypt::ripemd128 (write-to-string (random n))))
         (encrypted (crypt:aes-encrypt subliminal-message session-key))
         (x-sublim))
    (when (> encrypted r)
      (format t "~2%Скрытое сообщение должно быть меньше r! Завершение протокола.")
      (return-from pick-x-subliminal nil))
    (setq p (aux:read-parse "p") q (aux:read-parse "q"))
    (setq x-sublim (+ encrypted (* (1+ (random (1- (* p q)))) r)))
    (aux:write-to-file (list session-key) "subliminal-key")
    (aux:write-to-file (list encrypted) "subliminal-message-encrypted")
    (aux:write-to-file (list x-sublim) "x-sublim") x-sublim))


(defun compute-w-subliminal (innocuous-message)
  (let* ((x (aux:read-parse "x-sublim")) (k (aux:read-parse "k"))
         (n (aux:read-parse "n")) (p (aux:read-parse "p"))
         (q (aux:read-parse "q")) (r (aux:read-parse "r"))
         (w-sublim))
    (setq w-sublim (ceiling (/ (- (crypt:hash innocuous-message n)
                                  (aux:mod-expt x k n))
                               (* p q r))))
    (aux:write-to-file (list w-sublim) "w-sublim") w-sublim))


(defun compute-s-subliminal ()
  (let ((x (aux:read-parse "x-sublim")) (w (aux:read-parse "w-sublim"))
        (k (aux:read-parse "k")) (p (aux:read-parse "p"))
        (q (aux:read-parse "q")) (r (aux:read-parse "r")) (inv) (s))
    (setq inv (mod (cadr (aux:ext-gcd (* k (aux:mod-expt x (1- k) p)) p)) p)
          s (+ x (* (mod (* w inv) p) p q r)))
    (aux:write-to-file (list s) "s-sublim") s))


(defun compute-s^k-subliminal (s)
  (let ((k (aux:read-parse "k")) (n (aux:read-parse "n")) (s^k-sublim))
    (setq s^k-sublim (aux:mod-expt s k n))
    (aux:write-to-file (list s^k-sublim) "s^k-sublim") s^k-sublim))
