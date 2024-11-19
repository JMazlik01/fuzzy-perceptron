
;;; Fuzzy perceptron


;;; FUZZY PARTITION

;; partition function from original article
;; class 1 and class 2 elements differ in d1 and d2 order
(defun partition-function (d1 d2 d L)
  (+ 0.5
     (/ (- (exp (/ (* L (- d2 d1))
		   d))
	   (exp (- L)))
	(* 2 (- (exp L) (exp (- L)))))))


(defun class-distance (class1 class2)
  (distance (mean-vector class1)
	    (mean-vector class2)))

(defun empty-partition (class1 class2)
  (make-array (+ (length class1)
		 (length class2))
	      :initial-element 0))

(defun class-partition (class par-fun)
  (let ((part (make-array (length class)
			  :initial-element 0)))
    (loop
      for v in class
      for i = 0 then (1+ i)
      do (setf (elt part i)
	       (funcall par-fun v)))
    part))
			  
(defun fuzzy-partition (class1 class2 L)
  (let ((m1 (mean-vector class1))
	(m2 (mean-vector class2))
	(d (class-distance class1 class2)))
    (concatenate
     'vector
     (class-partition
      class1
      (lambda (v)
	(partition-function (distance v m1)
			    (distance v m2)
			    d
			    L)))
     (class-partition
      class2
      (lambda (v)
	(- 1  (partition-function (distance v m2)
				  (distance v m1)
				  d
				  L)))))))

(defun part-elem (matrix i k)
  (if (= i 1)
      (elt matrix k)
      (- 1 (elt matrix k))))


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

(defun fuzzy-update (ts W u c c-i m beta &optional (updatep nil))
  (if (null ts)
      (values W updatep)
      (multiple-value-bind (nW update)
	  (fuzzy-weight-update W (first ts) (part-elem u 1 c-i) (part-elem u 2 c-i) c m beta)
	(fuzzy-update (cdr ts) nW u c (1+ c-i) m beta (or updatep update)))))

(defun fuzzy-perceptron-algorithm (class1 class2 partition W c m beta &key (bound 2000))
  (labels ((fuzzy-perceptron-algorithm-h (ts partition W c m beta count)
	     (multiple-value-bind (nW update)
		 (fuzzy-update ts W partition c 0 m beta)
	       (if (and update (< count bound))
		   (fuzzy-perceptron-algorithm-h ts partition nW c m beta (1+ count))
		   (values  nW count)))))
    (fuzzy-perceptron-algorithm-h (initialize-classes class1 class2) partition W c m beta 0)))
			
(defun train-fuzzy-perceptron (class1 class2 &key (L 2.5) (m 1.2) (c 0.1) (eps 0.02))
  (let* ((partition (fuzzy-partition class1 class2 L))
	 (beta (beta L eps)))
    (multiple-value-bind (w count)
	(fuzzy-perceptron-algorithm class1 class2 partition (make-n-dim-vector (class-dim class1)) c m beta)
    (perceptron w :iter count :info (list 'L L 'm m 'c c 'eps eps 'beta beta)))))

;; returns function that trains perceptron
;; partition for trining data is cached
(defun fuzzy-perceptron-cached (class1 class2 L)
  (let ((partition (fuzzy-partition class1 class2 L))
	(w (make-n-dim-vector (class-dim class1))))
    (lambda (m c eps)
      (multiple-value-bind (w count)
	  (fuzzy-perceptron-algorithm class1 class2 partition w c m (beta L eps))
	(perceptron w :iter count :info (list 'L L 'm m 'c c 'eps eps 'beta (beta L eps)))))))
