
;;; Fuzzy and Crisp perceptron algorithms

;;; DATA

(defun data-folder ()
  (make-pathname
   :directory (append
	       (butlast (pathname-directory *default-pathname-defaults*))
	       '("data"))))

(defvar *data-script* (merge-pathnames (data-folder) "process-data.lisp"))
(defvar *data-file* (merge-pathnames (data-folder) "iris.lisp"))

(load *data-script*)


;;; VECTOR OPERATIONS

;; Augments vectors in class -> adds 1 to the end
(defun augment-vectors (class)
  (mapcar (lambda (v)
	    (append v '(1)))
	  class))

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


;;; DATA PREPROCESSING

;; Initializes training set
(defun initialize-classes (class1 class2)
  (append (augment-vectors class1)
	  (mapcar (lambda (v)
		    (scale v -1))
		  (augment-vectors class2))))

;; Creates arbitrary weight vector of dimension n
(defun make-random-vector (n)
  (if (= n 0)
      '()
      (cons 1 (make-random-vector (1- n)))))

;; Creates arbitrary weight vector for training set
(defun weight-vector (training-set)
  (let ((dim (length (first training-set))))
    (make-random-vector dim)))

(defun dim (v)
  (length v))

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

;;; PREPSAT NA 2D POLE? RYCHLEJSI
(defun partition-matrix (class1 class2 L)
  (let* ((mean1 (mean-vector class1))
	 (mean2 (mean-vector class2))
	 (d (distance mean1 mean2))
	 (c1 (class1partition d L))
	 (c2 (class2partition d L)))
    (append
     (class-partition mean1 mean2 class1 c1)
     (class-partition mean1 mean2 class2 c2))))


;;; PERCEPTRON DATA STRUCTURE

(defun classifier (W)
  (lambda (X)
    (if (<= 0 (scalar-product X W)) 1 2)))

(defun perceptron (w)
  (list :weights w :classifier (classifier w)))

(defun classify (perceptron v)
  (funcall (getf perceptron :classifier)
	   v))

;;; CRISP PERCEPTRON ALGORITHM

;; upper limit for perceptron algorithm iterations
(defvar *perceptron-bound* 5000)

(defun crisp-weight-update (W X c)
  (if (<= (scalar-product W X) 0)
      (values (add-vectors W (scale X c))
	      t)
      (values W nil)))

(defun crisp-update (ts W c &optional (updatedp nil))
  (if (null ts)
      (values W updatedp)
      (multiple-value-bind (nW update)
	  (crisp-weight-update W (car ts) c)
	(crisp-update (cdr ts) nW c (or updatedp update)))))

;; crisp perceptron algorithm
(defun crisp-perceptron-algorithm (class1 class2 c W)
  (labels ((perceptron-algorithm-h (training-set c W count)
	     (if (< *perceptron-bound* count)
		 (progn (format t "Perceptron algorithm is above iteration limit: ~a"
				*perceptron-bound*)
			W)
		 (multiple-value-bind (nW updatedp)
		     (crisp-update training-set W c)
		   (if updatedp
		       (perceptron-algorithm-h training-set c nW (1+ count))
		       nW)))))
    (butlast (perceptron-algorithm-h (initialize-classes class1 class2) c W 0))))

;; Returns trained perceptron object
(defun train-crisp-perceptron (class1 class2 &optional (c 0.5))
  (let ((weights (crisp-perceptron-algorithm class1 class2 c (make-random-vector (1+ (dim (first class1)))))))
    (perceptron weights)))
		
			    
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
	  (fuzzy-weight-update W (first ts) (first (first u)) (second (first u)) c m beta)
	(fuzzy-update (cdr ts) nW (cdr u) c m beta (or updatep update)))))

(defun fuzzy-perceptron-algorithm (ts partition W c m beta)
  (labels ((fuzzy-perceptron-algorithm-h (ts partition W c m beta count)
	     (if (< *perceptron-bound* count)
		 (progn (format t "Too many iterations ~a~%" count)
			(butlast W))
	     (multiple-value-bind (nW update)
		 (fuzzy-update ts W partition c m beta)
	       (if update
		   (fuzzy-perceptron-algorithm-h ts partition nW c m beta (1+ count))
		   (butlast nW))))))
    (fuzzy-perceptron-algorithm-h ts partition W c m beta 0)))
			
(defun train-fuzzy-perceptron (class1 class2 &optional (L 1) (m 2.5) (c 0.5) (eps 0.02))
  (let* ((partition (partition-matrix class1 class2 L))
	 (ts (initialize-classes class1 class2))
	 (beta (beta L eps))
	 (w (fuzzy-perceptron-algorithm ts partition (weight-vector ts) c m beta)))
    (perceptron w)))

;;; TESTS

(defun perceptron-test (perceptron-f &key (dataset #'dataset1))
  (let* ((dataset (funcall dataset *data-file*))
	 (perceptron (funcall perceptron-f
		      (class1-training dataset)
		      (class2-training dataset)))
	 (c1-test (class1-test dataset))
	 (c2-test (class2-test dataset)))
    (values
     perceptron
     (mapcar (lambda (v)
	       (classify perceptron v))
	     c1-test)
     (mapcar (lambda (v)
	       (classify perceptron v))
	     c2-test))))

(defun fuzzy-perceptron-test ()
  (perceptron-test #'train-fuzzy-perceptron))

(defun crisp-perceptron-test ()
  (perceptron-test #'train-crisp-perceptron))
