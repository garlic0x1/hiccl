(defpackage #:hiccl/utils
  (:use :cl)
  (:export #:extract-attrs))
(in-package :hiccl/utils)

;; ----------------------------------------------------------------------------
(defun extract-attrs (body)
  (let* ((attrs (loop :for (k v) :on body :by 'cddr
                      :while (keywordp k)
                      :collect (cons k v)))
         (children (nthcdr (* 2 (length attrs)) body)))
    (values attrs children)))
