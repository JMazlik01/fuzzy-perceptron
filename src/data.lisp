
;;; Process iris.csv dataset to list format

(defun data-folder ()
  (merge-pathnames "data/"))

;; path to data file
(defun data-path (file)
  (merge-pathnames (format nil "data/~a" file)))

(defvar *iris-data-file* (data-path "iris.lispdata"))
(defvar *iris-csv-file* (data-path "iris.csv"))

(defvar *mnist-train-0* (data-path "train0.lispdata"))
(defvar *mnist-train-1* (data-path "train1.lispdata"))
(defvar *mnist-test-0* (data-path "test0.lispdata"))
(defvar *mnist-test-1* (data-path "test1.lispdata"))

;; returns file content as list of strings
(defun read-data (path)
  (with-open-file (s path)
    (loop for line = (read-line s nil)
	  while line collect line)))

;; reads lispdata file
(defun read-lisp-data (path)
  (with-open-file (s path :if-does-not-exist nil)
    (read s)))

;; writes content to file
(defun write-content (file content)
  (let ((path (merge-pathnames (format nil "data/~a" file))))
    (with-open-file (s path :direction :output :if-does-not-exist :create :if-exists :append)
      (format s "~a" content))))

;;; IRIS DATASET FUNCTIONS

;; lispdata file target path
(defun iris-dataset-target-path ()
  (data-path "iris.lispdata"))

;; retyping iris features from string to numbers
(defun process-single-iris (iris)
  (append (mapcar (lambda (num)
		    (read-from-string num))
		  (butlast iris))
	  (last iris)))

;; transforms each iris entry to lisp list
(defun process-iris-csv-data (path)
  (let* ((raw-data (read-data path))
	 (processed (mapcar (lambda (str)
			      (split-sequence:split-sequence #\, str))
			    (cdr raw-data))))
    (mapcar #'process-single-iris processed)))

;; writes iris data in lisp
(defun write-iris-in-lisp-format (csv-path target-path)
  (let ((processed-data (process-iris-csv-data csv-path)))
    (with-open-file (s target-path :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (format s "~a~%" processed-data))
    t))

(defun iris-name (n)
  (fifth n))

;;; MNIST DATASET FUNCTIONS

(defun mnist-classes (f1 f2)
  (list
   (read-lisp-data f1)
   (read-lisp-data f2)))

;;; DATA PREPROCESSING

;; augments vectors and multiplies class 2 vectors by -1
(defun initialize-classes (class1 class2)
  (append (augment-vectors class1)
	  (mapcar (lambda (v)
		    (scale v -1))
		  (augment-vectors class2))))

(defun odd-elements (list)
  (if (null list)
      '()
      (cons (car list)
	    (odd-elements (cddr list)))))

(defun even-elements (list)
  (if (null list)
      nil
      (odd-elements (cdr list))))

;; divides class into two subclasses
(defun divide-training-test (class)
  (list
   (odd-elements class)
   (even-elements class)))

;; returns dimension of vectors in a class
(defun class-dim (class)
  (1+ (dim (first class))))

;;; IRIS PREPROCESSING

;; returns iris classes with name1 and name2 species
(defun iris-classes (path name1 name2)
  (let ((data (read-lisp-data path)))
    (list
     (mapcar #'butlast (remove-if-not (lambda (n) (eql (iris-name n) name1)) data))
     (mapcar #'butlast (remove-if-not (lambda (n) (eql (iris-name n) name2)) data)))))

;; returns iris classes with features 3 and 4
(defun iris-classes-f-3-4 (path name1 name2)
  (mapcar (lambda (class)
	    (mapcar (lambda (v)
		      (last v 2)) class))
	  (iris-classes path name1 name2)))

;; returns iris classes divided into training and test data
(defun iris-divided-classes (path name1 name2)
  (mapcar #'divide-training-test
	  (iris-classes path name1 name2)))

;; dataset corresponding to original article dataset
(defun article-dataset-full (path)
  (iris-classes path 'versicolor 'virginica))

;; dataset corresponding to original article - features 3 and 4
(defun article-dataset-f-3-4 (path)
  (iris-classes-f-3-4 path 'virginica 'versicolor))


