(defpackage #:gauss
  (:use #:cl)
  (:export #:row-echelon
           #:reduced-row-echelon
           #:row-dimension
           #:column-dimension
           #:switch-rows
           #:multiply-row
           #:add-row))

(in-package #:gauss)


(declaim (inline row-dimension column-dimension))


(defun row-dimension (a)
  "Возвращает количество строк матрицы A."
  (array-dimension a 0))


(defun column-dimension (a)
  "Возвращает количество столбцов матрицы A."
  (array-dimension a 1))


(defun switch-rows (a i j)
  "Деструктивно меняет строки i и j матрицы A, возвращает A."
  (dotimes (k (column-dimension a) a)
    (psetf (aref a i k) (aref a j k)
           (aref a j k) (aref a i k))))


(defun multiply-row (a i alpha p)
  "Деструктивно умножает i-ую строку матрицы A на alpha по модулю p, возвращает A."
  (dotimes (k (column-dimension a) a)
    (setf (aref a i k) (mod (* (aref a i k) alpha) p))))


(defun add-row (a i j alpha p)
  "Деструктивно добавить к i-ой строке матрицы A её j-ую строку, умноженную на alpha,
   по модулю p, возвращает A."
  (dotimes (k (column-dimension a) a)
    (setf (aref a i k) (mod (+ (aref a i k) (* (aref a j k) alpha)) p))))


(defun eliminate-column-below (a i j p)
  "Предполагая, что a[i, j] не равен нулю, деструктивно обнуляет ненулевые коэффициенты
   под a[i, j]. Возвращает A."
  (loop with inv = (cadr (aux:ext-gcd (aref a i j) p))
        for k from (+ i 1) below (array-dimension a 0)
        do (add-row a k i (- (* (aref a k j) inv)) p)
        finally (return a)))


(defun eliminate-column-above (a i j p)
  "Предполагая, что a[i, j] не равен нулю, деструктивно обнуляет ненулевые коэффициенты
   над a[i, j]. Возвращает A."
  (loop with inv = (cadr (aux:ext-gcd (aref a i j) p))
        for k below i
        do (add-row a k i (- (* (aref a k j) inv)) p)
        finally (return a)))


(defun find-pivot-row (a i j p)
  "Возвращает первый встреченный индекс строки, начиная с i, имеющей ненулевой коэффициент
   в j-ом столбце, или nil, в случае, если такой индекс не найден."
  (loop for k from i below (row-dimension a)
        unless (zerop (mod (aref a k j) p))
          do (return k)
        finally (return nil)))


(defun find-pivot-column (a i j p)
   "Возвращает первый встреченный индекс столбца, начиная с i, имеющей ненулевой коэффициент
   в j-ой строке, или nil, в случае, если такой индекс не найден."
  (loop for k from j below (column-dimension a)
        unless (zerop (mod (aref a i k) p))
          do (return k)
        finally (return nil)))


(defun row-echelon (a p)
  "Приведение матрицы A к ступенчатому виду."
  (loop with row-dimension = (row-dimension a)
        with column-dimension = (column-dimension a)
        with current-row = 0
        with current-col = 0
        while (and (< current-row row-dimension)
                   (< current-col column-dimension))
        for pivot-row = (find-pivot-row a current-row current-col p)
        do (when pivot-row
             (unless (= pivot-row current-row)
               (switch-rows a pivot-row current-row))
             (eliminate-column-below a current-row current-col p)
             (incf current-row))
        do (incf current-col)
        finally (return a)))


(defun reduce-row-echelon (a p)
  "Функция выполняет обратный ход метода Гаусса в предположении, что матрица A
   уже в ступенчатом виде."
  (loop for i below (row-dimension a)
        for j = (find-pivot-column a i 0 p) then (find-pivot-column a i j p)
        while j
        unless (= 1 (aref a i j))
          do (multiply-row a i (cadr (aux:ext-gcd (aref a i j) p)) p)
        do (eliminate-column-above a i j p)
        finally (return a)))


(defun reduced-row-echelon (a p)
  "Функция возвращает приведённую ступенчатую форму матрицы A."
  (reduce-row-echelon (row-echelon a p) p))
