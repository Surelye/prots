(defpackage :fsds-aux
  (:use :cl)
  (:export :get-bit-len :get-c
           :form-I :get-j))

(in-package :fsds-aux)


(defun get-bit-len ()
  (let ((bit-len))
    (tagbody try-again
       (setq bit-len (read-line))
       (when (zerop (length bit-len)) (return-from get-bit-len 1024))
       (setq bit-len (parse-integer bit-len :junk-allowed t))
       (when (or (null bit-len) (not (and (zerop (logand bit-len (1- bit-len)))
                                          (> bit-len 16))))
         (format t "~%Некорректное значение длины l модуля! Попробуйте ввести l снова: ")
         (go try-again))) bit-len))


(defun get-c ()
  (let ((c))
    (tagbody try-again
       (setq c (read-line))
       (when (zerop (length c)) (return-from get-c 7))
       (setq c (parse-integer c :junk-allowed t))
       (when (not (member c '(1 2 3 4 5 6 7 8 9 10 11 12)))
         (format t "~%Некорректный номер хеш-функции! Попробуйте ввести номер снова: ")
         (go try-again))) c))


(defun form-I ()
  (let ((name))
    (format t "~%Введите имя пользователя (I), для которого выпускается смарт-карта (по умолчанию Alice): ")
    (setq name (read-line))
    (when (zerop (length name))
      (setq name "Alice"))
    (aux::write-to-file (list name) "I") name))


(defun get-j ()
  (let ((j))
    (format t "~%Введите количество значений v_j (9 < j < 51, по умолчанию 20): ")
    (tagbody try-again
       (setq j (read-line))
       (when (zerop (length j)) (return-from get-j 20))
       (setq j (parse-integer j :junk-allowed t))
       (when (or (null j) (not (< 9 j 51)))
         (format t "~%Некорректное значение j! Попробуйте ввести его снова: ")
         (go try-again))) j))


(defun compute-sjs (I j p q n)
  (let ((sjs) (val) (roots) (indices))
    (do ((k j (1- k))) ((zerop k))
      (setq val (crypt:f (concatenate 'string I (write-to-string k)) n))
      (when (= 1 (aux::compute-jacobi val n))
        (setq val (mod (cadr (aux::ext-gcd val n)) n)
              roots (aux::sqrt-Zn val p q n))
        (when roots
          (setq sjs (cons (list k (apply #'min roots)) sjs)
                indices (cons k indices)))))
    (when sjs
      (aux::write-to-file (list (caar (last sjs))) "max-j")
      (aux::write-to-file indices "indices")) sjs))


(defun write-smartcard (smartcard &optional (filename "smartcard"))
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (dolist (datum smartcard)
      (if (atom datum)
          (format out "~a~%" datum)
          (format out "~a ~a~%" (car datum) (cadr datum))))))


(defun get-t ()
  (let ((t-val))
    (format t "~%Введите количество раундов подписи t (t > 4, по умолчанию 5): ")
    (tagbody try-again
       (setq t-val (read-line))
       (when (zerop (length t-val)) (return-from get-t 5))
       (setq t-val (parse-integer t-val :junk-allowed t))
       (when (or (null t-val) (> 5 t-val))
         (format t "~%Некорректное значение количества раундов подписи! Попробуйте ввести t снова: ")
         (go try-again)))
    (aux::write-to-file (list t-val) "num-rounds") t-val))


(defun get-message ()
  (let ((filename))
    (format t "~%Введите имя файла, в котором содержится сообщение (по умолчанию message): ")
    (tagbody try-again
       (setq filename (read-line))
       (when (zerop (length filename)) (setq filename "message"))
       (when (not (uiop:file-exists-p filename))
         (format t "~%Файла с заданным именем не существует! Попробуйте ввести имя файла с сообщением снова: ")
         (go try-again)))
    (uiop:read-file-lines filename)))
