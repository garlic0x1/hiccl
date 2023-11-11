(defpackage #:hiccl/utils
  (:use :cl)
  (:export #:curry #:extract-attrs-and-children))
(in-package :hiccl/utils)

;; ----------------------------------------------------------------------------
(defun curry (f &rest a) (lambda (&rest b) (apply f (nconc a b))))

;; ----------------------------------------------------------------------------
(defun extract-attrs-and-children (body)
  (let* ((attrs (loop :for (k v) :on body :by 'cddr
                      :while (keywordp k)
                      :collect (cons k v)))
         (children (nthcdr (* 2 (length attrs)) body)))
    (values attrs children)))
