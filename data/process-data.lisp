
;;; Process iris.csv dataset to list format

(require 'split-sequence)

(defun data-path (file)
  (merge-pathnames file))

(defun dataset-csv-path ()
  (data-path "iris.csv"))

(defun read-data ()
  (let ((path (dataset-csv-path)))
    (with-open-file (s path)
      (loop for line = (read-line s nil)
	    while line collect line))))

(defun process-single-iris (iris)
  (append (mapcar (lambda (num)
		    (read-from-string num))
		  (butlast iris))
	  (last iris)))

(defun process-data ()
  (let* ((raw-data (read-data))
	 (processed (mapcar (lambda (str)
			      (split-sequence:split-sequence #\, str))
			    (cdr raw-data))))
    (mapcar #'process-single-iris processed)))

(defun write-lisp-format ()
  (let ((processed-data (process-data))
	(path (data-path "iris.lisp")))
    (with-open-file (s path :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (format s "~a~%" processed-data))
    t))

;;; READING DATA AND PREPARING CLASSES

(defun read-lisp-data ()
  (let ((path (data-path "iris.lisp")))
    (with-open-file (s path :if-does-not-exist nil)
      (read s))))

(defun iris-name (n)
  (fifth n))

;; Divides class into two subclasses
(defun divide-training-test (class)
  (list
   (odd-elements class)
   (even-elements class)))

(defun classes (c1 c2)
  (let ((data (read-lisp-data)))
    (list
     (divide-training-test (mapcar #'butlast (remove-if-not (lambda (n) (eql (iris-name n) c1)) data)))
     (divide-training-test (mapcar #'butlast (remove-if-not (lambda (n) (eql (iris-name n) c2)) data))))))

;; VIRGINICA is class1 and VERSICOLOR is class2
(defun dataset1 ()
  (classes 'virginica 'versicolor))

;; SETOSA class1 and VIRGINICA class2
(defun dataset2 ()
  (classes 'setosa 'virginica))

;;VERSICOLOR class1 and SETOSA class2
(defun dataset3 ()
  (classes 'versicolor 'setosa))

(defun odd-elements (list)
  (if (null list)
      '()
      (cons (car list)
	    (even-elements (cdr list)))))

(defun even-elements (list)
  (if (null list)
      nil
      (odd-elements (cdr list))))

;;; DATASET SELECTORS

(defun class1-training (dataset)
  (first (first dataset)))

(defun class2-training (dataset)
  (first (second dataset)))

(defun class1-test (dataset)
  (second (first dataset)))

(defun class2-test (dataset)
  (second (second dataset)))
