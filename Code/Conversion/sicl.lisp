(cl:in-package #:cleavir-development)

(defclass sicl (cleavir)
  ((%compilation-environment
    :initform (make-instance 'sicl-extrinsic-environment:environment))))
