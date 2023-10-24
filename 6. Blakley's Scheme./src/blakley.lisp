(defpackage #:blakley
  (:use #:cl)
  (:export #:generate-point
           #:share-secret
           #:recover-secret))


(in-package #:blakley)


(defun stop () (read-line))


(defun generate-point ()
  (format t "~%Реализация (k, n)-пороговой схемы. Секрет M разделяется между n сторонами так, чтобы
любые k из них могли восстановить секрет.~%")
  (bl-aux:get-n-k) (bl-aux:get-M)
    (bl-aux:gen-p) (bl-aux:gen-coords) t)


(defun share-secret ()
  (tagbody try-again
     (bl-aux:gen-cfs) (stop)
     (when (not (bl-aux:k-linear-independent?))
       (format t "~%~4tПолучившаяся система уравнений не является k-линейно независимой. Коэффициенты будут
    перегенерированы.~2%")
       (go try-again))) t)


(defun recover-secret ()
  (format t "~%[2.1] -- Для восстановления секрета любым k сторонам необходимо собраться вместе и из имеющихся долей
         секрета составить уравнения для отыскания точки пересечения гиперплоскостей.")
  (let ((ks (bl-aux:get-ks)))
    (bl-aux:recover-secret ks)) t)
