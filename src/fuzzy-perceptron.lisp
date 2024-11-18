
;;; Fuzzy perceptron


;;; FUZZY PARTITION

(defun partition-function (d1 d2 d L &optional class2)
  (let ((u (+ 0.5
	      (/ (- (exp (/ (* L (- d2 d1))
			    d))
		    (exp (- L)))
		 (* 2 (- (exp L) (exp (- L))))))))
    (if class2
	(list (- 1 u) u)
	(list u (- 1 u)))))

(defun class1partition (d L)
  (lambda (d1 d2)
    (partition-function d1 d2 d L)))

(defun class2partition (d L)
  (lambda (d1 d2)
    (partition-function d2 d1 d L t)))

(defun class-distance (class1 class2)
  (distance (mean-vector class1)
	    (mean-vector class2)))

(defun class-partition (m1 m2 class c)
  (mapcar (lambda (dist)
	    (funcall c (first dist) (second dist)))
	  (mapcar (lambda (v)
		    (list (distance m1 v)
			  (distance m2 v)))
		  class)))

(defun u (matrix i k)
  (if (< 1 k)
      (u (cdr matrix) i (1- k))
      (if (= i 1)
	  (first (car matrix))
	  (second (car matrix)))))

;;; PREPSAT NA 1D POLE? BUDE TO RYCHLEJSI
(defun partition-matrix (class1 class2 L)
  (let* ((mean1 (mean-vector class1))
	 (mean2 (mean-vector class2))
	 (d (distance mean1 mean2))
	 (c1 (class1partition d L))
	 (c2 (class2partition d L)))
    (append
     (class-partition mean1 mean2 class1 c1)
     (class-partition mean1 mean2 class2 c2))))


;;; FUZZY PERCEPTRON ALGORITHM

;;; Returns BETA value as in article
;;; eps = 0.02 had good results
(defun beta (L eps)
  (+ eps
     (/ (- 1 (exp (- L)))
	(* 2 (- (exp L) (exp (- L)))))))

(defun fuzzy-weight-update (W X u1 u2 c m beta)
  (if (<= (scalar-product W X) 0)
      (values (add-vectors W (scale (scale X c)
				    (expt (abs (- u1 u2)) m)))
	      (not (<= (- 0.5 beta) u1 (+ 0.5 beta))))
      (values W nil)))

(defun fuzzy-update (ts W u c m beta &optional (updatep nil))
  (if (null ts)
      (values W updatep)
      (multiple-value-bind (nW update)
	  (fuzzy-weight-update W (first ts) (u u 1 1) (u u 2 1) c m beta)
	(fuzzy-update (cdr ts) nW (cdr u) c m beta (or updatep update)))))

(defun fuzzy-perceptron-algorithm (class1 class2 partition W c m beta &key (bound 2000))
  (labels ((fuzzy-perceptron-algorithm-h (ts partition W c m beta count)
	     (multiple-value-bind (nW update)
		 (fuzzy-update ts W partition c m beta)
	       (if (and update (< count bound))
		   (fuzzy-perceptron-algorithm-h ts partition nW c m beta (1+ count))
		   (values  nW count)))))
    (fuzzy-perceptron-algorithm-h (initialize-classes class1 class2) partition W c m beta 0)))
			
(defun train-fuzzy-perceptron (class1 class2 &key (L 2.5) (m 1.2) (c 0.1) (eps 0.02))
  (let* ((partition (partition-matrix class1 class2 L))
	 (beta (beta L eps)))
    (multiple-value-bind (w count)
	(fuzzy-perceptron-algorithm class1 class2 partition (make-n-dim-vector (class-dim class1)) c m beta)
    (perceptron w :iter count :info (list 'L L 'm m 'c c 'eps eps 'beta beta)))))
