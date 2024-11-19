
;;; Crisp perceptron algorithm

;; updates single weight
(defun crisp-weight-update (W X c)
  (if (<= (scalar-product W X) 0)
      (values (add-vectors W (scale X c)) t)
      (values W nil)))

;; single weight update iteration
(defun crisp-update (ts W c updated)
  (if (null ts)
      (values W updated)
      (multiple-value-bind (nW update)
	  (crisp-weight-update W (car ts) c)
	(crisp-update (cdr ts) nW c (or updated update)))))

;; crisp perceptron algorithm
(defun crisp-perceptron-algorithm (class1 class2 c W bound)
  (labels ((perceptron-algorithm-h (ts c W count)
	     (multiple-value-bind (nW updated)
		 (crisp-update ts W c nil)
	       (if (and updated (< count bound))
		   (perceptron-algorithm-h ts c nW (1+ count))
		   (values nW count)))))
    (multiple-value-bind (w count)
	(perceptron-algorithm-h (initialize-classes class1 class2) c W 0)
      (values w count))))

;; Returns trained perceptron object
(defun train-crisp-perceptron (class1 class2 &key (c 0.1) (bound 2000))
  (multiple-value-bind (w count)
      (crisp-perceptron-algorithm class1 class2 c (make-n-dim-vector (class-dim class1)) bound)
    (perceptron w :iter count :info (list 'c c))))
