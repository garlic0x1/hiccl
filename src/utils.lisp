(defpackage #:hiccl/utils
  (:use :cl)
  (:export #:curry #:format-attr #:extract-attrs-and-children))
(in-package :hiccl/utils)

(defun curry (f x) (lambda (&rest args) (apply f (cons x args))))

(defun format-attr (attr)
  (let ((k (car attr)) (v (cdr attr)))
    (if v
        (format nil "~(~a~)=\"~a\"" k v)
        (format nil "~(~a~)" k))))

(defun extract-attrs-and-children (body)
  (let* ((attrs (loop :for (k v) :on body :by 'cddr
                      :while (keywordp k)
                      :collect (cons k v)))
         (children (nthcdr (* 2 (length attrs)) body)))
    (values attrs children)))
