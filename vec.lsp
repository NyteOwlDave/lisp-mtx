
;;; Vector Functions

; [DONE]
; *vec-epsilon* vec vecp vec-x vec-y vec-z
; vec-fmt vec-print vec-to-alist vec-from-alist
; vec-to-list vec-from-list vec-zero vec-unit
; vec-copy vec-negate vec-add vec-sub vec-mul
; vec-dot vec-len vec-len-sqr vec-cross vec-manhatten
; vec-scale vec-scale-add vec-combine vec-normal
; vec-midpoint

; some tiny value
(defconstant *vec-epsilon* 1.0e-6)

(defun vec (x y z)
  "create vector"
  (let ((v (make-array '(3))))
    (setf (aref v 0) x)
    (setf (aref v 1) y)
    (setf (aref v 2) z)
    v))

(defun vecp (v)
  "vector type predicate"
  (equal (type-of v) '(SIMPLE-VECTOR 3)))

(defun vec-x (v)
  "extract vector x element"
  (aref  v 0))

(defun vec-y (v) 
  "extract vector y element"
  (aref  v 1))

(defun vec-z (v) 
  "extract vector z element"
  (aref  v 2))

(defun vec-fmt (v)
  "format vector as string"
  (format nil "(x ~d y ~d z ~d)" (vec-x v) (vec-y v) (vec-z v)))

(defun vec-print (v)
  "pretty print a vector"
  (format t "~&~a" (vec-fmt v)))

(defun vec-to-alist (v)
  "convert vector to alist"
  (list 
    (cons 'x (vec-x v))
    (cons 'y (vec-y v))
    (cons 'z (vec-z v))))

(defun vec-from-alist (a) 
  "create vector from alist"
  (let ((x (cdr (assoc 'x a)))
        (y (cdr (assoc 'y a)))
        (z (cdr (assoc 'z a))))
    (vec x y z)))

(defun vec-to-list (v)
  "convert vector to list"
  (list (vec-x v) (vec-y v) (vec-z v)))

(defun vec-from-list (o)
  "create vector from list"
  (vec (car o) (cadr o) (caddr o)))

(defun vec-zero () 
  "create null vector"
  (vec 0 0 0))

(defun vec-unit () 
  "create unit vector"
  (vec 1 0 0))

(defun vec-copy (v) 
  "copy vector"
  (vec (vec-x v) (vec-y v) (vec-z v)))

(defun vec-negate (v) 
  "negate vector"
  (vec (- (vec-x v)) (- (vec-y v)) (- (vec-z v))))

(defun vec-add (va vb)
  "add two vectors"
  (vec
    (+ (vec-x va) (vec-x vb))
    (+ (vec-y va) (vec-y vb))
    (+ (vec-z va) (vec-z vb))))

(defun vec-sub (va vb)
  "subtract one vector from another"
  (vec
    (- (vec-x va) (vec-x vb))
    (- (vec-y va) (vec-y vb))
    (- (vec-z va) (vec-z vb))))

(defun vec-mul (va vb)
  "elementwise multiply"
  (vec
    (* (vec-x va) (vec-x vb))
    (* (vec-y va) (vec-y vb))
    (* (vec-z va) (vec-z vb))))

(defun vec-dot (va vb)
  "dot product"
  (+
    (* (vec-x va) (vec-x vb))
    (* (vec-y va) (vec-y vb))
    (* (vec-z va) (vec-z vb))))

(defun vec-len-sqr (v) 
  "vector magnitude squared"
  (vec-dot v v))

(defun vec-len (v)
  "vector magnitude"
  (sqrt (vec-len-sqr v)))

(defun vec-cross (va vb)
  "cross product"
  (let ((vax (vec-x va))
        (vay (vec-y va))
        (vaz (vec-z va))
        (vbx (vec-x vb))
        (vby (vec-y vb))
        (vbz (vec-z vb)))
    (vec 
      (- (* vay vbz) (* vaz vby))
      (- (* vaz vbx) (* vax vbz))
      (- (* vax vby) (* vay vbx)))))

(defun vec-manhatten (v)
  "vector manhatten sum"
  (+ (vec-x v) (vec-y v) (vec-z v)))

(defun vec-scale (v k)
  "scale vector"
  (vec (* k (vec-x v))
       (* k (vec-y v))
       (* k (vec-z v))))

(defun vec-scale-add (va k vb)
  "scale vector and add another vector"
  (vec (+ (* k (vec-x va)) (vec-x vb))
       (+ (* k (vec-y va)) (vec-y vb))
       (+ (* k (vec-z va)) (vec-z vb))))

(defun vec-combine (va ka vb kb)
  "scale two vectors and sum the results"
  (vec (+ (* ka (vec-x va)) (* kb (vec-x vb)))
       (+ (* ka (vec-y va)) (* kb (vec-y vb)))
       (+ (* ka (vec-z va)) (* kb (vec-z vb)))))

(defun vec-normal (v)
  "compute normal vector"
  (let ((ls (vec-len-sqr v)))
    (if (< ls *vec-epsilon*)
      (values vec-unit ls)
      (values (vec-scale v (/ 1.0 (sqrt ls))) ls))))

(defun vec-midpoint (arr)
  "midpoint of line/polyhedron"
  (let ((count (length arr))
        (x 0) (y 0) (z 0))
    (if (plusp count)
      (loop for v in arr do 
        (incf x (vec-x v))
        (incf y (vec-y v))
        (incf z (vec-z v))
        finally (return (vec (/ x count) (/ y count) (/ z count))))
      (vec-zero))))

