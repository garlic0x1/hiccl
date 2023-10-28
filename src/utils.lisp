(defpackage #:hiccl/utils
  (:use :cl)
  (:export #:format-attr #:extract-attrs-and-children))
(in-package :hiccl/utils)

(defun format-attr (attr)
  (let ((k (car attr)) (v (cdr attr))) (format nil "~(~a~)=\"~a\"" k v)))

(defun extract-attrs-and-children (body)
  (let* ((attrs (loop :for (k v) :on body :by 'cddr
                      :while (keywordp k)
                      :collect (cons k v)))
         (children (nthcdr (* 2 (length attrs)) body)))
    (values attrs children)))
