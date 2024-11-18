
;;; Load file

;; hack but works so far
(push *default-pathname-defaults* asdf:*central-registry*)

(asdf:defsystem "perceptron"
  :description "Perceptron algorithms with tests"
  :version "0.0.1"
  :license "MIT"
  :depends-on ("split-sequence")
  :serial t
  :components
  ((:file "vectors")
   (:file "data")
   (:file "perceptron")
   (:file "crisp-perceptron")
   (:file "fuzzy-perceptron")
   (:file "tests")))
