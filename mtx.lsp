
;;; Matrix Functions

(load "vec.lsp")

; [DONE]
; mtx mtxp mtx-zero mtx-identity mtx-cell
; mtx-cell-read mtx-cell-write mtx-randomize
; mtx-rotate mtx-rotate-x mtx-rotate-y mtx-rotate-z
; mtx-scale mtx-translate mtx-transform mtx-quat
; mtx-lookat mtx-copy mtx-transpose 
; mtx-cat mtx-trans-vector mtx-trans-normal
; mtx-inverse mtx-det mtx-adjunct mtx-scale-by

(define-condition on-determinant-too-small (error)
   ((message :initarg :message :reader message)))

(defun mtxp (m)
  "matrix predicate"
  (equal (type-of m) '(SIMPLE-ARRAY T (4 4))))

(defun mtx (a b c d e f g h i j k l m n o p)
  "create matrix"
  (make-array '(4 4)
    :initial-contents `((,a ,b ,c ,d) (,e ,f ,g ,h) (,i ,j ,k ,l) (,m ,n ,o ,p))))

(defun mtx-zero ()
  "create null matrix"
  (mtx 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(defun mtx-identity ()
  "create identity matrix"
  (mtx 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))

(defmacro mtx-cell (m row col)
    "matrix cell accessor"
    `(aref ,m ,row ,col))

(defun mtx-cell-read (m row col)
  "read matrix cell"
  (mtx-cell m row col))

(defun mtx-cell-write (m row col value)
  "write matrix cell"
  (setf (mtx-cell m row col) value))

(defun mtx-randomize (m maxv)
  "create random matrix"
  (loop for row from 0 to 3 do
    (loop for col from 0 to 3 do
      (mtx-cell-write m row col (random maxv)))))

(defun mtx-rotate (ax ay az)
  "create rotation matrix (x, y, z)"
  (let* (
    (sx (sin ax))
    (cx (cos ax))
    (sy (sin ay))
    (cy (cos ay))
    (sz (sin az))
    (cz (cos az))
    (a (cy * cz))
    (b (- (* sx sy cz) (* cx sz)))
    (c (+ (* cx sy cz) (* sx sz))
    ;(d 0)
    (e (* cy sz))
    (f (+ (* sx sy sz) (* cx cz)))
    (g (- (* cx sy sz) (* sx cz)))
    ;(h 0)
    (i (- sy))
    (j (* sx cy))
    (k (* cx cy))))
    ;(lmnop 0)
    (mtx a b c 0 e f g 0 i j k 0 0 0 0 1)))

(defun mtx-rotate-x (angle) 
  "create rotation matrix (x)"
  (let* (
    (c (cos angle))
    (s (sin angle)))
  (mtx
    1 0 0     0
    0 c (- s) 0
    0 s c     0
    0 0 0     1)))

(defun mtx-rotate-y (angle) 
  "create rotation matrix (y)"
  (let* (
    (c (cos angle))
    (s (sin angle)))  
  (mtx
    c     0 s 0
    0     1 0 0
    (- s) 0 c 0
    0     0 0 1)))

(defun mtx-rotate-z (angle) 
  "create rotation matrix (z)"
  (let* (
    (c (cos angle))
    (s (sin angle)))  
  (mtx
    c (- s) 0 0
    s c     0 0
    0 0     1 0
    0 0     0 1)))

(defun mtx-scale (sx sy sz) 
  "create scaling matrix (x, y, z)"
  (mtx
    sx 0 0 0
    0 sy 0 0
    0 0 sz 0
    0 0 0  1))

(defun mtx-translate (tx ty tz) 
  "create translation matrix (x, y, z)"
  (mtx
    1 0 0 tx
    0 1 0 ty
    0 0 1 tz
    0 0 0 1))

(defun mtx-transform (ax ay az mx my mz tx ty tz) 
  "create transformation matrix (rotate, scale, translate)"
  (let* (
    (sx sin(ax))
    (cx cos(ax))
    (sy sin(ay))
    (cy cos(ay))
    (sz sin(az))
    (cz cos(az))
    (a (* mx cy cz))
    (b (* my (- (* sx sy cz) (* cx sz))))
    (c (* mz (+ (* cx sy cz) (* sx sz))))
    (d tx)
    (e (* mx cy sz))
    (f (* my (+ (* sx sy sz) (* cx cz))))
    (g (* mz (- (* cx sy sz) (* sx cz))))
    (h ty)
    (i (* mx (- sy)))
    (j (* my sx cy))
    (k (* mz cx cy))
    (l tz)
    ; (mnop 0 0 0 1)
  )
  (mtx a b c d e f g h i j k l 0 0 0 1)))

(defun mtx-quat (x y z angle)
  "create matrix from quaternion"
  (let* (
    (c (cos angle))
    (s (sin angle))
    (cc (- 1 c))
    (v (vec-normal (vec x y z)))
    (x (vec-x v))
    (y (vec-y v))
    (z (vec-z v))
    (a (+ (* cc x x) c))
    (b (+ (* cc x y) (* z s)))
    (c (- (* cc x z) (* y s)))
    ;(d 0)
    (e (- (* c *x y) (* z s)))
    (f (+ (* c y y) c))
    (g (+ (* cc z y) (* x s)))
    ;(h 0)
    (i (+ (* cc x z) (* y s)))
    (j (- (* cc y z) (* x s)))
    (k (+ (* cc z z) c))
    ;(lmnop 0 0 0 0 1)
  )
  (mtx a b c 0 e f g 0 i j k 0 0 0 0 1)))

(defun mtx-lookat (eye target up)
  "creat camera matrix"
  (let (
      (vx (vec-zero))
      (vy (vec-zero))
      (vz (vec-sub eye target))
    )
    ; eye and target are in the same position
    (if (< (vec-z vz) vec.epsilon) (setf (aref vz 0) 1))
    (setf vz (vec-normal vz))
    (setf vx (vec-cross up vz))
    ; eye and target are in the same vertical
    (if (< (vec-len-sqr vx) vec-epsilon) 
      (progn 
        (setf (aref vz 2) (+ (vec-z vz) 0.0001))
        (setf vx (vec-cross up vz))
      ))
    (setf vx (vec-normal vx))
    (setf vy (vec-cross vx vz))
    (mtx 
      (vec-x vx) (vec-y vx) (vec-z vx) 0
      (vec-x vy) (vec-y vy) (vec-z vy) 0
      (vec-x vz) (vec-y vz) (vec-z vz) 0
      0 0 0 1)))

(defun mtx-copy (m)
  "copy a matrix" 
  (mtx 
    (mtx-cell m 0 0) (mtx-cell m 0 1) (mtx-cell m 0 2) (mtx-cell m 0 3)
    (mtx-cell m 1 0) (mtx-cell m 1 1) (mtx-cell m 1 2) (mtx-cell m 1 3)
    (mtx-cell m 2 0) (mtx-cell m 2 1) (mtx-cell m 2 2) (mtx-cell m 2 3)
    (mtx-cell m 3 0) (mtx-cell m 3 1) (mtx-cell m 3 2) (mtx-cell m 3 3)))

(defun mtx-transpose (m)
  "transpose a matrix"
  (mtx 
    (mtx-cell m 0 0) (mtx-cell m 1 0) (mtx-cell m 2 0) (mtx-cell m 3 0)
    (mtx-cell m 0 1) (mtx-cell m 1 1) (mtx-cell m 2 1) (mtx-cell m 3 1)
    (mtx-cell m 0 2) (mtx-cell m 1 2) (mtx-cell m 2 2) (mtx-cell m 3 2)
    (mtx-cell m 0 3) (mtx-cell m 1 3) (mtx-cell m 2 3) (mtx-cell m 3 3)))

(defun mtx-cat (a b)
  "matrix concatenation"
  (let ((m (mtx-zero)))
    (loop for i from 0 to 3 do
      (loop for j from 0 to 3 do
        (loop for k from 0 to 3 do
          (incf (mtc-cell m i j) (* (mtx-cell a i k) (mtx-cell b k j))))))
    m))

(defun mtx-trans-vector (m v) 
  "transform vector"
  (let (
    (vx (vec-x v))
    (vy (vec-y v))
    (vz (vec-z v))
    (x (+ (* (mtx-cell m 0 0) vx)
          (* (mtx-cell m 0 1) vy)
          (* (mtx-cell m 0 2) vz)
          (mtx-cell m 0 3)))
    (y (+ (* (mtx-cell m 1 0) vx)
          (* (mtx-cell m 1 1) vy)
          (* (mtx-cell m 1 2) vz)
          (mtx-cell m 1 3)))
    (z (+ (* (mtx-cell m 2 0) vx)
          (* (mtx-cell m 2 1) vy)
          (* (mtx-cell m 2 2) vz)
          (mtx-cell m 2 3)))
    (u (vec x y z))
    (k (/ 1.0 (mtx-cell m 3 3)))
  )
  (vec-scale u k)))

(defun mtx-trans-normal (m v) 
  "transform a normal vector"
  (let (
    (orig (vec-zero))
    (t1 (vec-unit)))
    (setf v (vec-normal v))
    ; Createc a t1 vector not aligned with v
    (setf dot (vec-dot t1 v))
    (if (> (abs dot) 0.8)
      (setf t1 (vec 0.0 1.0 0.0)))
    (setf t2 (vec-cross v t1))
    (setf t1 (vec-cross t2 v))
    ; transform tangents 1 and 2
    (setf t1 (mtx-trans-vec m t1))
    (setf t2 (mtx-trans-vec m t2))
    ; transform origin
    (setf orig (mtx-trans-vec m orig))
    ; calc relative tangent vectors
    (setf t1 (vec-sub t1 orig))
    (setf t2 (vec-sub t2 orig))
    ; calc final normal
    (setf out (vec-cross t1 t2))
  out))

(defun mtx-inverse (m)
  "create inverse matrix"
  (let* ((det (mtx_det m))
         (absdet (abs det)))
        (if (< absdet *vec-epsilon*) 
          (error 'on-determinant-too-small :message "determinant is too small")
          (mtx-scale-by m (/ 1 det)))))

(defun mtx-det (m) 
  "determinant"
  (let ((m00 (mtx-cell-read 0 0))
        (m01 (mtx-cell-read 0 1))
        (m02 (mtx-cell-read 0 2))
        (m03 (mtx-cell-read 0 3))
        (m10 (mtx-cell-read 1 0))
        (m11 (mtx-cell-read 1 1))
        (m12 (mtx-cell-read 1 2))
        (m13 (mtx-cell-read 1 3))
        (m20 (mtx-cell-read 2 0))
        (m21 (mtx-cell-read 2 1))
        (m22 (mtx-cell-read 2 2))
        (m23 (mtx-cell-read 2 3))
        (m30 (mtx-cell-read 3 0))
        (m31 (mtx-cell-read 3 1))
        (m32 (mtx-cell-read 3 2))
        (m33 (mtx-cell-read 3 3)))
        (+ (* m03 m12 m21 m30) (- (* m02 m13 m21 m30)) (- (* m03 m11 m22 m30)) (* m01 m13 m22 m30)
           (* m02 m11 m23 m30) (- (* m01 m12 m23 m30)) (- (* m03 m12 m20 m31)) (* m02 m13 m20 m31)
           (* m03 m10 m22 m31) (- (* m00 m13 m22 m31)) (- (* m02 m10 m23 m31)) (* m00 m12 m23 m31)
           (* m03 m11 m20 m32) (- (* m01 m13 m20 m32)) (- (* m03 m10 m21 m32)) (* m00 m13 m21 m32)
           (* m01 m10 m23 m32) (- (* m00 m11 m23 m32)) (- (* m02 m11 m20 m33)) (* m01 m12 m20 m33)
           (* m02 m10 m21 m33) (- (* m00 m12 m21 m33)) (- (* m01 m10 m22 m33)) (* m00 m11 m22 m33))))

(defun mtx-adjunct (m) 
  "adjunct matrix"
  (let* ((m00 (mtx-cell-read 0 0))
         (m01 (mtx-cell-read 0 1))
         (m02 (mtx-cell-read 0 2))
         (m03 (mtx-cell-read 0 3))
         (m10 (mtx-cell-read 1 0))
         (m11 (mtx-cell-read 1 1))
         (m12 (mtx-cell-read 1 2))
         (m13 (mtx-cell-read 1 3))
         (m20 (mtx-cell-read 2 0))
         (m21 (mtx-cell-read 2 1))
         (m22 (mtx-cell-read 2 2))
         (m23 (mtx-cell-read 2 3))
         (m30 (mtx-cell-read 3 0))
         (m31 (mtx-cell-read 3 1))
         (m32 (mtx-cell-read 3 2))
         (m33 (mtx-cell-read 3 3))
         (a (+ (* m12 m23 m31) (- (* m13 m22 m31)) (* m13 m21 m32) (- (* m11 m23 m32)) (- (* m12 m21 m33)) (* m11 m22 m33)))
         (b (+ (* m03 m22 m31) (- (* m02 m23 m31)) (- (* m03 m21 m32)) (* m01 m23 m32) (* m02 m21 m33) (- (* m01 m22 m33))))
         (c (+ (* m02 m13 m31) (- (* m03 m12 m31)) (* m03 m11 m32) (- (* m01 m13 m32)) (- (* m02 m11 m33)) (* m01 m12 m33)))
         (d (+ (* m03 m12 m21) (- (* m02 m13 m21)) (- (* m03 m11 m22)) (* m01 m13 m22) (* m02 m11 m23) (- (* m01 m12 m23))))
         (e (+ (* m13 m22 m30) (- (* m12 m23 m30)) (- (* m13 m20 m32)) (* m10 m23 m32) (* m12 m20 m33) (- (* m10 m22 m33))))
         (f (+ (* m02 m23 m30) (- (* m03 m22 m30)) (* m03 m20 m32) (- (* m00 m23 m32)) (- (* m02 m20 m33)) (* m00 m22 m33)))
         (g (+ (* m03 m12 m30) (- (* m02 m13 m30)) (- (* m03 m10 m32)) (* m00 m13 m32) (* m02 m10 m33) (- (* m00 m12 m33))))
         (h (+ (* m02 m13 m20) (- (* m03 m12 m20)) (* m03 m10 m22) (- (* m00 m13 m22)) (- (* m02 m10 m23)) (* m00 m12 m23)))
         (i (+ (* m11 m23 m30) (- (* m13 m21 m30)) (* m13 m20 m31) (- (* m10 m23 m31)) (- (* m11 m20 m33)) (* m10 m21 m33)))
         (j (+ (* m03 m21 m30) (- (* m01 m23 m30)) (- (* m03 m20 m31)) (* m00 m23 m31) (* m01 m20 m33) (- (* m00 m21 m33))))
         (k (+ (* m01 m13 m30) (- (* m03 m11 m30)) (* m03 m10 m31) (- (* m00 m13 m31)) (- (* m01 m10 m33)) (* m00 m11 m33)))
         (l (+ (* m03 m11 m20) (- (* m01 m13 m20)) (- (* m03 m10 m21)) (* m00 m13 m21) (* m01 m10 m23) (- (* m00 m11 m23))))
         (m (+ (* m12 m21 m30) (- (* m11 m22 m30)) (- (* m12 m20 m31)) (* m10 m22 m31) (* m11 m20 m32) (- (* m10 m21 m32))))
         (n (+ (* m01 m22 m30) (- (* m02 m21 m30)) (* m02 m20 m31) (- (* m00 m22 m31)) (- (* m01 m20 m32)) (* m00 m21 m32)))
         (o (+ (* m02 m11 m30) (- (* m01 m12 m30)) (- (* m02 m10 m31)) (* m00 m12 m31) (* m01 m10 m32) (- (* m00 m11 m32))))
         (p (+ (* m01 m12 m20) (- (* m02 m11 m20)) (* m02 m10 m21) (- (* m00 m12 m21)) (- (* m01 m10 m22)) (* m00 m11 m22))))
        (mtx a b c d e f g h i j k l m n o p)))

(defun mtx-scale-by (m k) 
  "scale matrix uniformly"
  (let ((a (* (mtx-cell_read 0 0) k))
        (b (* (mtx-cell_read 0 1) k))
        (c (* (mtx-cell_read 0 2) k))
        (d (* (mtx-cell_read 0 3) k))
        (e (* (mtx-cell_read 1 0) k))
        (f (* (mtx-cell_read 1 1) k))
        (g (* (mtx-cell_read 1 2) k))
        (h (* (mtx-cell_read 1 3) k))
        (i (* (mtx-cell_read 2 0) k))
        (j (* (mtx-cell_read 2 1) k))
        (k (* (mtx-cell_read 2 2) k))
        (l (* (mtx-cell_read 2 3) k))
        (m (* (mtx-cell_read 3 0) k))
        (n (* (mtx-cell_read 4 1) k))
        (o (* (mtx-cell_read 3 2) k))
        (p (* (mtx-cell_read 3 3) k)))
       (mtx a b c d e f g h i j k l m n o p)))
