
;;; Perceptron data structure

(defun perceptron (w &key iter info)
  (list :weights w :classifier (classifier w) :iter iter :info info))

(defun weights (perceptron)
  (getf perceptron :weights))

(defun iter (perceptron)
  (getf perceptron :iter))

(defun info (perceptron)
  (getf perceptron :info))

(defun classify (perceptron v)
  (funcall (getf perceptron :classifier)
	   v))

(defun classifier (W)
  (lambda (X)
    (if (<= 0 (scalar-product (augment-vector X) W)) 1 2)))

(defun to-string (perceptron)
  (let ((w (weights perceptron))
	(i (iter perceptron))
	(inf (info perceptron)))
    (format nil "~{~a,~}~a,~{~a~^,~}" w i (even-elements inf))))

;;; Evaluation

;; Returns success rate of classsifying into class1 and class2
(defun evaluate-perceptron (p c1 c2)
  (let ((h1 (count 1 (mapcar (lambda (v)
			       (classify p v)) c1)))
	(h2 (count 2 (mapcar (lambda (v)
			       (classify p v)) c2)))
	(l (length (append c1 c2))))
    (values h1 h2 (/ (+ h1 h2) l))))

(defun evaluate-perceptron-print (p c1 c2)
  (multiple-value-bind (h1 h2 sr)
      (evaluate-perceptron p c1 c2)
    (format t "Class 1: ~a~%Class 2: ~a~%Success rate: ~f~%" h1 h2 sr)))



