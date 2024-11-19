
;;; Fuzzy and crisp perceptron tests

(defun classify-class (p class)
  (mapcar (lambda (v)
	    (classify p v))
	  class))

(defun perceptron-test (article-f)
  (lambda (perceptron-f)
    (let* ((data (funcall article-f *iris-data-file*))
	   (c1 (first data))
	   (c2 (second data))
	   (p (funcall perceptron-f c1 c2))
	   (h1 (count 1 (classify-class p c1)))
	   (h2 (count 2 (classify-class p c2)))
	   (len (+ (length c1) (length c2))))
      (values p h1 h2 (* 1.0 (/ (+ h1 h2) len))))))

;;; ORIGINAL ARTICLE TEST FEATURES 1 - 4

(defun perceptron-test-1 ()
  (perceptron-test #'article-dataset-full))

(defun crisp-perceptron-test-1 (bound)
  (funcall
   (perceptron-test-1)
   (lambda (c1 c2)
     (train-crisp-perceptron c1 c2 :bound bound))))

(defun fuzzy-perceptron-test-1 (L m &key (eps 0.02) (c 0.1))
  (funcall
   (perceptron-test-1)
   (lambda (c1 c2)
     (train-fuzzy-perceptron c1 c2 :L L :m m :eps eps :c c))))

;;; ORIGINAL ARTICLE TEST FEATURES 3 - 4

(defun perceptron-test-2 ()
  (perceptron-test #'article-dataset-f-3-4))

(defun crisp-perceptron-test-2 (bound)
  (funcall
   (perceptron-test-2)
   (lambda (c1 c2)
     (train-crisp-perceptron c1 c2 :bound bound))))

(defun fuzzy-perceptron-test-2 (L m &key (eps 0.02) (c 0.1))
  (funcall
   (perceptron-test-2)
   (lambda (c1 c2)
     (train-fuzzy-perceptron c1 c2 :L L :m m :eps eps :c c))))
