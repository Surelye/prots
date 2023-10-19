(defpackage #:aux
  (:use :cl))

(in-package #:aux)


(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))


(defun write-to-file (data filename)
  (with-open-file (out filename :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
    (dolist (param data)
      (format out "~a~%" param))))


(defun read-parse (filename &optional (at 0))
  (parse-integer (uiop:read-file-line filename :at at)))


(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))


(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (1+ n) (cdr lst))
            (cons (n-elts elt n) (compr next 1 (cdr lst)))))))


(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))


(defun is-pow-of-2? (num)
  (zerop (logand num (1- num))))


(defun mod-expt (base power modulo)
  (setq base (mod base modulo))
  (do ((product 1)) ((zerop power) product)
    (do () ((oddp power))
      (setq base (mod (* base base) modulo)
            power (ash power -1)))
    (setq product (mod (* product base) modulo)
          power (1- power))))


(defun miller-rabin (n &optional (k 10))
  (when (or (= 2 n) (= 3 n)) (return-from miller-rabin t))
  (when (or (< n 2) (= 0 (logand n 1))) (return-from miller-rabin))
  (let* ((n-pred (1- n)) (bound (- n-pred 2)) (t-val n-pred) (s 0) (round 0) (x))
    (while (= 0 (logand t-val 1)) (setq s (1+ s) t-val (ash t-val -1)))
    (do () (nil)
      (tagbody next-iteration
         (when (= k round) (return-from miller-rabin t))
         (setq x (mod-expt (+ 2 (random bound)) t-val n))
         (when (or (= 1 x) (= n-pred x))
           (incf round) (go next-iteration))
         (do ((iter 0 (1+ iter))) ((= iter (1- s)) (return-from miller-rabin))
           (setq x (mod (* x x) n))
           (when (= 1 x) (return-from miller-rabin))
           (when (= n-pred x)
             (incf round) (go next-iteration)))))))


(defparameter *base-primes*
  (remove-if-not #'(lambda (prime?) (miller-rabin prime? 12))
                 (loop for prime? from (1+ (ash 1 15)) to (1- (ash 1 16)) by 2
                       collect prime?)))


(defun ext-gcd (a b)
  (let ((s 0) (old-s 1) (r b) (old-r a)
        (quotient) (bezout-t))
    (while (not (zerop r))
      (setq quotient (floor old-r r))
      (psetq old-r r r (- old-r (* quotient r))
             old-s s s (- old-s (* quotient s))))
    (if (zerop b) (setq bezout-t 0)
        (setq bezout-t (floor (- old-r (* old-s a)) b)))
    (list old-r old-s bezout-t)))


(defun rho-pollard-machinerie (n x-0 &optional (c 1) (rounds 1000))
  (when (miller-rabin n) (return-from rho-pollard-machinerie 'PRIME))
  (let ((mapping (lambda (x) (mod (+ c (* x x)) n)))
        (a x-0) (b x-0) (round 0) (q))
    (tagbody map
       (incf round)
       (when (> round rounds) (return-from rho-pollard-machinerie 'GEN-NEW))
       (setq a (funcall mapping a)
             b (funcall mapping (funcall mapping b))
             q (gcd (- a b) n))
       (cond ((< 1 q n) (return-from rho-pollard-machinerie
                          (list q (miller-rabin q))))
             ((= n q) (return-from rho-pollard-machinerie))
             (t (go map))))))


(defun rho-pollard-wrapper (n x-0)
  (let ((c 1) (head) (factor) (factors))
    (while (zerop (logand n 1))
      (setq factors (cons 2 factors) n (ash n -1)))
    (setq x-0 (mod x-0 n))
    (while (/= 1 n)
      (setq factor (rho-pollard-machinerie n x-0 c))
      (cond ((eql 'PRIME factor) (setq factors (cons n factors) n 1))
            ((eql 'GEN-NEW factor) (return))
            ((cadr factor) (setq factors (cons (setq head (car factor)) factors)
                                 n (/ n head)))
            ((null factor) (while (= (- n 2) (setq c (1+ (random (1- n)))))))
            (t (setq n (/ n (setq head (car factor)))
                     factors (append factors
                                     (rho-pollard-wrapper head (random head)))))))
    factors))


(defun rho-pollard (n x-0)
  (let* ((factors (rho-pollard-wrapper n x-0)))
    (when (null factors) (return-from rho-pollard))
    (when (= n (apply #'* factors))
      (compress (sort (rho-pollard-wrapper n x-0) #'<)))))


(defun get-bit-len ()
  (let ((bit-len))
    (tagbody try-again
       (setq bit-len (read))
       (when (and (integerp bit-len) (is-pow-of-2? bit-len) (> bit-len 16))
         (return-from get-bit-len bit-len))
       (format t "Некорректный ввод! Попробуйте снова: ")
       (go try-again))))


(defun find-g (p)
  (when (not (miller-rabin p)) (return-from find-g))
  (let ((phi (1- p)) (factors) (g?) (bound (- p 2)))
    (setq factors (rho-pollard phi (random p)))
    (when (null factors) (return-from find-g))
    (setq factors (mapcar #'(lambda (factor) (cond ((atom factor) factor)
                                                   (t (cadr factor)))) factors)
          factors (mapcar #'(lambda (factor) (floor phi factor)) factors))
    (tagbody try-again
       (setq g? (+ 2 (random bound)))
       (when (= 1 (mod-expt g? (car factors) p)) (go try-again))
       (when (remove-if-not #'(lambda (pow) (= 1 (mod-expt g? pow p)))
                            factors) (go try-again))) g?))


(defun generate-even (target-len)
  (apply #'+ (ash 1 (1- target-len))
         (mapcar #'(lambda (bit pow) (* bit (ash 1 pow)))
                 (append (loop for bit from 0 to (- target-len 3)
                               collect (random 2)) '(0))
                 (loop for pow from (- target-len 2) downto 0 collect pow))))


(defun generate-prime (target-len)
  (when (not (is-pow-of-2? target-len))
    (return-from generate-prime))
  (when (= 16 target-len)
    (return-from generate-prime (nth (random (length *base-primes*))
                                     *base-primes*)))
  (let ((prime) (s) (prime?) (req-len (- target-len 16)))
    (tagbody pick-prime
       (setq prime (nth (random (length *base-primes*)) *base-primes*))
       (when (not (miller-rabin prime)) (go pick-prime)))
    (tagbody try-again
       (setq s (generate-even req-len)
             prime? (1+ (* prime s)))
       (if (and (= 1 (mod-expt 2 (1- prime?) prime?))
                (/= 1 (mod-expt 2 s prime?))
                (zerop (logxor (length (write-to-string prime? :base 2))
                               target-len)))
           (return-from generate-prime prime?)
           (go try-again)))))


(defun gen-p&g (bit-len)
  (let ((p) (g))
    (tagbody gen-prime
       (setq p (generate-prime bit-len)
             g (find-g p))
       (when (null g) (go gen-prime)))
    (list p g)))


(defun compute-jacobi-machinerie (a b)
  (let ((r 1) (t-val) (c))
    (when (< a 0) (setq a (- a))
      (when (= 3 (mod b 4)) (setq r (- r))))
    (tagbody eliminate-evenness
       (setq t-val 0)
       (while (zerop (logand a 1))
         (setq t-val (1+ t-val) a (ash a -1)))
       (when (= 1 (logand t-val 1))
         (when (or (= 3 (mod b 8)) (= 5 (mod b 8)))
           (setq r (- r))))
       (when (and (= 3 (mod a 4)) (= 3 (mod b 4)))
         (setq r (- r)))
       (setq c a a (mod b c) b c)
       (if (not (zerop a))
           (go eliminate-evenness)
           (return-from compute-jacobi-machinerie r)))))


(defun compute-jacobi (a b)
  (when (and (integerp a) (integerp b) (= 1 (logand b 1)) (> b 1))
    (if (= 1 (gcd a b))
        (compute-jacobi-machinerie a b) 0)))


(defun is-square-residue (a p)
  (= 1 (mod-expt a (ash (1- p) -1) p)))


(defun compute-legendre-machinerie (a p)
  (cond ((zerop (mod a p)) 0)
        ((is-square-residue a p) 1)
        (t -1)))


(defun compute-legendre (a p)
  (when (and (integerp a) (miller-rabin p))
    (compute-legendre-machinerie a p)))


(defun find-k-i (a-i q p)
  (do ((k 0 (1+ k))) ((= 1 (mod-expt a-i (* (expt 2 k) q) p)) k)))


(defun get-inv (a p)
  (cadr (ext-gcd a p)))


(defun seq-sqrt-Zp (a p)
  (when (/= 1 (compute-legendre a p)) (return-from seq-sqrt-Zp))
  (let ((b) (k-i -1) (k-is) (r-i) (m 0) (q (1- p))
        (a-prev a) (a-cur a) (pow))
    (while (zerop (logand q 1)) (setq m (1+ m) q (ash q -1)))
    (while (/= -1 (compute-legendre (setq b (random p)) p)))
    (while (not (zerop k-i))
      (setq  k-i    (find-k-i a-cur q p) k-is (cons k-i k-is))
      (psetq a-cur  (mod (* a-cur (mod-expt b (ash 1 (- m k-i)) p)) p)
             a-prev a-cur))
    (setq k-is (cdr k-is) r-i (mod-expt a-prev (ash (1+ q) -1) p))
    (do ((i (length k-is) (1- i))) ((= 0 i) r-i)
      (setq pow (ash 1 (- m (car k-is) 1))
            r-i (mod (* r-i (get-inv (mod-expt b pow p) p)) p)
            k-is (cdr k-is)))))


(defun sqrt-Zn (a p q n)
  (let ((r) (s) (x) (y) (rdq) (scp))
    (when (= 1 (compute-jacobi a p))
      (setq r (seq-sqrt-Zp a p)))
    (when (= 1 (compute-jacobi a q))
      (setq s (seq-sqrt-Zp a q)))
    (when (or (null r) (null s))
      (return-from sqrt-Zn))
    (destructuring-bind (c d) (cdr (ext-gcd p q))
      (setq rdq (mod (* r d q) n) scp (mod (* s c p) n)
            x (mod (+ rdq scp) n)
            y (mod (- rdq scp) n))
      (list x (mod (- x) n) y (mod (- y) n)))))
