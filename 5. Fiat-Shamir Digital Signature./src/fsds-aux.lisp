(defpackage :fsds-aux
  (:use :cl)
  (:export :get-bit-len :get-c
           :form-I :get-j
           :get-t :get-message
           :compute-ys :read-parse-l))


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


(defun get-t ()
  (let ((t-val))
    (format t "~%Введите количество раундов подписи t (t > 4, по умолчанию 5): ")
    (tagbody try-again
       (setq t-val (read-line))
       (when (zerop (length t-val))
         (aux::write-to-file (list 5) "num-rounds")
         (return-from get-t 5))
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


(defun compute-ys (rs sjs first-bits indices n)
  (let* ((t-val (length rs)) (ys) (cur-bits) (prod 1)
         (j (floor (length first-bits) t-val)))
    (do ((k 0 (1+ k))) ((= t-val k) (reverse ys))
      (setq cur-bits (subseq first-bits (* j k) (* j (1+ k))))
      (do ((i 0 (1+ i))) ((= (length indices) i))
        (when (not (zerop (digit-char-p (char cur-bits (1- (nth i indices))))))
          (setq prod (mod (* prod (nth i sjs)) n))))
      (setq ys (cons (mod (* (nth k rs) prod) n) ys)
            prod 1))))


(defun read-parse-l (filename)
  (mapcar #'parse-integer (uiop:read-file-lines filename)))
