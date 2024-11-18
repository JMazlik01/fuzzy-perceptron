
;;; Vectors


(defun augment-vector (v)
  (append v '(1)))

;; Augments vectors in class -> adds 1 to the end
(defun augment-vectors (class)
  (mapcar #'augment-vector class))

(defun scale (list c)
  (mapcar (lambda (n)
	    (* n c))
	  list))

(defun equal-vectors (v u)
  (or (null v)
      (and (eql (car v) (car u))
	   (equal-vectors (cdr v)
			  (cdr u)))))

(defun add-vectors (v u)
  (mapcar #'+ v u))

(defun subtract-vectors (v u)
  (mapcar #'- v u))

(defun scalar-product (v u)
  (apply #'+ (mapcar #'* v u)))

(defun vector-length (u)
  (sqrt (scalar-product u u)))

(defun distance (v u)
  (vector-length (subtract-vectors v u)))

;; calculates mean vector from set of vectors
(defun mean-vector (set)
  (scale (reduce #'add-vectors set)
	 (/ 1 (length set))))

;; creates zero vector of dimension n
(defun make-n-dim-vector (n)
  (if (= n 0)
      '()
      (cons 0 (make-n-dim-vector (1- n)))))

(defun dim (v)
  (length v))
