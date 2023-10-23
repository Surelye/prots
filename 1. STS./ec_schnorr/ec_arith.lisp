(defpackage #:ec-arith
  (:use #:cl)
  (:export #:add-points
           #:scalar-product))

(in-package #:ec-arith)


(defun add-points (P Q modulo)
  (when (eql P 'INF)
    (return-from add-points Q))
  (when (eql Q 'INF)
    (return-from add-points P))
  (let ((Px (car P)) (Py (cadr P))
        (Qx (car Q)) (Qy (cadr Q))
        (Rx nil) (Ry nil)
        (frac nil))
    (cond
      ((/= Px Qx) (setq frac (* (- Qy Py)
                                (cadr (aux:ext-gcd (mod (- Qx Px) modulo) modulo)))
                        Rx (- (* frac frac) (+ Px Qx))
                        Ry (+ (- Py) (* frac (- Px Rx)))))
      (t (cond
           ((= Py Qy) (setq frac (* (* 3 Px Px)
                                    (cadr (aux:ext-gcd (mod (* 2 Py) modulo)
                                                        modulo)))
                            Rx (- (* frac frac) (* 2 Px))
                            Ry (- (* frac (- Px Rx)) Py)))
           (t (return-from add-points 'INF)))))
    (list (mod Rx modulo) (mod Ry modulo))))


(defun scalar-product (n P modulo)
  (let ((result 'INF) (addend P))
    (aux:while (not (zerop n))
      (when (= 1 (mod n 2))
        (setq result (add-points addend result modulo)))
      (setq addend (add-points addend addend modulo)
            n (ash n -1)))
    result))


(defun generate-subgroup (generator modulo)
  (let* ((accum generator) (subgroup (list accum)))
    (aux:while (not (eql 'INF accum))
      (setq accum (add-points accum generator modulo)
            subgroup (cons accum subgroup))) subgroup))
