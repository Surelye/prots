(defpackage #:macro
  (:use :cl))

(in-package #:macro)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))
