(defpackage #:bl-aux
  (:use :cl)
  (:export #:get-n-k   #:get-M
           #:gen-p     #:gen-coords
           #:gen-cfs   #:k-linear-independent?
           #:get-ks    #:recover-secret
           #:zerop-row #:array-slice))
   

(in-package #:bl-aux)


(defun get-n-k ()
  (format t "~%[0.1] -- Определение параметров n и к.~%")
  (format t "~%~4tВведите значение n и k через пробел: ")
  (let ((n-k))
    (tagbody try-again
       (setq n-k (uiop:split-string (read-line) :separator " ")
             n-k (mapcar #'(lambda (int?) (parse-integer int? :junk-allowed t)) n-k))
       (destructuring-bind (n k) n-k
         (when (or (null n) (null k) (< k 2) (< n k))
           (format t "~%~4tНекорректное значение параметров n и / или k! Попробуйте ввести их снова: ")
           (go try-again))
         (aux:write-to-file (list n) "n")
         (aux:write-to-file (list k) "k"))) n-k))


(defun get-M ()
  (format t "~%[0.2] -- Определение значения разделяемого секрета.~%")
  (format t "~%~4tВведите значение разделяемого секрета M: ")
  (let ((M))
    (tagbody try-again
       (setq M (parse-integer (read-line) :junk-allowed t))
       (when (or (null M) (< M 0))
         (format t "~%~4tНекорректное значение M! Попробуйте ввести его снова: ")
         (go try-again)))
    (aux:write-to-file (list M) "M") M))


(defun gen-p ()
  (format t "~%[0.3] -- Генерация простого числа p.~%")
  (let* ((M (aux:read-parse "M"))
         (bit-len-M (length (write-to-string M :base 2)))
         (bit-len-p) (p))
    (format t "~%~4tБитовая длина M: ~a.~%" bit-len-M)
    (format t "~%~4tВведите битовую длину l числа p (l = 2^s, l > 15, l > битовая длина(M)): ")
    (tagbody try-again
       (setq bit-len-p (parse-integer (read-line) :junk-allowed t))
       (when (or (null bit-len-p) (>= bit-len-M bit-len-p)
                 (null (setq p (aux:generate-prime bit-len-p))))
         (format t "~%~4tНекорректное значение битовой длины l! Попробуйте ввести l снова: ")
         (go try-again)))
    (format t "~%~4tБыло сгенерировано число p: 0x~x.~%" p)
    (aux:write-to-file (list p) "p") p))


(defun gen-coords ()
  (format t "~%[0.4] -- Случайным образом выбираются числа b_2, ..., b_k \in GF(p). Таким образом задаётся точка
         (M, b_2, ..., b_k) в k-мерном пространстве, первая координата которой является секретом.")
  (format t "~2%~4tБыли выбраны координаты:~%")
  (let ((k (aux:read-parse "k")) (p (aux:read-parse "p")) (coords))
    (do ((j 2 (1+ j))) ((> j k) (setq coords (reverse coords)))
      (setq coords (cons (random p) coords)))
    (do ((j 2 (1+ j))) ((> j k))
      (format t "~%~8tb_~2d = 0x~x;" j (nth (- j 2) coords)))
    (aux:write-to-file coords "coords")))


(defun gen-cfs ()
  (format t "~%[1.1] -- Для каждой стороны P_i, i = (1, ..., n) случайным образом выбираются коэффициенты
         a_{1i}, a_{2i}, ..., 2_{ki}, равномерно распределённые в GF(p). Также для каждой
         стороны необходимо вычислить коэффициенты d_i = -(a_{1i} * M + a_{2i} * b_2 + ...
         + a_{ki} * b_k) (mod p)")
  (let* ((n (aux:read-parse "n")) (k (aux:read-parse "k"))
         (p (aux:read-parse "p")) (M (aux:read-parse "M"))
         (coords (cons M (mapcar #'parse-integer (uiop:read-file-lines "coords"))))
         (cur-cfs) (cfs) (d) (ds)) (terpri)
    (do ((j 0 (1+ j))) ((= n j) (setq cfs (reverse cfs) ds (reverse ds)))
      (setq cur-cfs (loop for i from 1 to k collect (random p)))
      (format t "~%~4tДля стороны P_~2d были выбраны коэффициенты: ~2%" (1+ j))
      (do ((s 0 (1+ s))) ((= k s))
        (format t "~8ta_{~2d~2d} = 0x~x;~%" (1+ s) (1+ j) (nth s cur-cfs)))
      (setq cfs (cons cur-cfs cfs)
            d (mod (- (reduce #'+ (mapcar #'* cur-cfs coords))) p)
            ds (cons d ds))
      (format t "~8td_~2d     = 0x~x;~%" (1+ j) d))
    (aux:write-to-file cfs "cfs") (aux:write-to-file  ds  "ds")))


(defun load-cfs (filename)
  (let ((cfs))
    (with-open-file (in filename)
      (with-standard-io-syntax
        (setq cfs (mapcar #'read-from-string
                          (uiop:read-file-lines in)))))))


(defun array-slice (arr row)
  "Возвращает строку с номером row из двумерного массива arr."
  (let ((arr-dim (gauss:column-dimension arr)))
    (make-array arr-dim
                :displaced-to arr
                :displaced-index-offset (* row arr-dim))))


(defun zerop-row (arr row)
  (every #'zerop (array-slice arr row)))


(defun k-linear-independent? ()
  (format t "~%[1.2] -- При этом необходимо проверить, чтобы любые k уравнений были линейно независимы.")
  (let* ((cfs (load-cfs "cfs")) (p (aux:read-parse "p")) (k (aux:read-parse "k"))
         (ds (mapcar #'parse-integer (uiop:read-file-lines "ds")))
         (equations (mapcar #'(lambda (cfsi di) (append cfsi (list di))) cfs ds))
         (gauss-solution (gauss:reduced-row-echelon (make-array (list (length equations)
                                                                      (length (car equations)))
                                                                :initial-contents equations) p))
         (k-linear-independent?))

    (setq k-linear-independent?
          (= k (length (remove-if #'(lambda (row-index)
                                      (bl-aux:zerop-row gauss-solution row-index))
                                  (loop for idx from 0 to k collect idx)))))
    (format t "~2%~4tРезультат проверки: ~a.~%" k-linear-independent?)
    k-linear-independent?))


(defun get-ks ()
  (let ((n (aux:read-parse "n")) (k (aux:read-parse "k")) (ks))
    (format t "~2%~4tВведите номера ~a сторон k_i (1 <= k_i <= ~a), принимающих участие в восстановлении секрета, через пробел: "
            k n)
    (tagbody try-again
       (setq ks (remove-duplicates (uiop:split-string (read-line) :separator " ")))
       (when (/= (length ks) k)
         (format t "~%Было введено некорректное количество сторон! Попробуйте снова: ")
         (go try-again))
       (setq ks (mapcar #'(lambda (num?) (parse-integer num? :junk-allowed t)) ks))
       (when (some #'(lambda (num?) (or (null num?) (not (<= 1 num? n)))) ks)
         (format t "~%Было введено некорректное значение номера стороны! Попробуйте ввести их снова: ")
         (go try-again))) (mapcar #'1- ks)))


(defun print-equations (cfs)
  (let ((k (aux:read-parse "k"))
        (len (length
              (write-to-string
               (apply #'max
                      (mapcar #'(lambda (cfsi)
                                  (apply #'max cfsi)) cfs)) :base 16))))
    (format t "~%Соответствующая система линейных уравнений для введённых значений k_i:~2%")
    (do ((j 1 (1+ j))) ((< k j))
      (format t "~4t")
      (do ((s 1 (1+ s))) ((< k s))
        (format t (format nil "0x~v,'0x * x_~d + "
                          len (nth (1- s) (nth (1- j) cfs)) s)))
      (format t (format nil "0x~v,'0x (mod p) = 0;~%" len (nth k (nth (1- j) cfs)))))))


(defun print-solution (solution)
  (let ((k (aux:read-parse "k")) (xs) (len))
    (format t "~%Решение приведённой системы уравнений:~%")
    (do ((j 0 (1+ j))) ((= k j) (setq xs (reverse xs)))
      (setq xs (cons (aref (bl-aux:array-slice solution j) k) xs)))
    (setq len (apply #'max (mapcar #'(lambda (num)
                                       (length (write-to-string num :base 16))) xs)))
    (do ((j 0 (1+ j))) ((= k j))
      (format t (format nil "~%~4tx_~d = 0x~v,'0x;" (1+ j) len (nth j xs))))))


(defun recover-secret (ks)
  (let ((cfs (load-cfs "cfs")) (p (aux:read-parse "p"))
        (ds (mapcar #'parse-integer (uiop:read-file-lines "ds")))
        (solution) (secret))
    (setq cfs (mapcar #'(lambda (ki) (nth ki cfs)) ks)
          ds (mapcar #'(lambda (ki) (mod (- (nth ki ds)) p)) ks)
          cfs (mapcar #'(lambda (cfsi di) (append cfsi (list di))) cfs ds)
          solution (gauss:reduced-row-echelon (make-array (list (length cfs)
                                                                (length (car cfs)))
                                                          :initial-contents cfs) p)
          secret (aref (bl-aux:array-slice solution 0) (length ks)))
    (print-equations cfs)
    (print-solution solution)
    (format t "~2%Значение восстановленного секрета: 0x~x = ~d.~%"
            (write-to-string secret :base 16) secret)))
