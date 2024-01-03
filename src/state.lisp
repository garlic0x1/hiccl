(defpackage #:hiccl/state
  (:use :cl))
(in-package #:hiccl/state)

(defclass renderer ()
  ((out
    :initarg :out
    :initform (error "Must specify output")
    :accessor renderer-out)
   (indent
    :initform 0
    :accessor renderer-indent)
   (inline?
    :initform nil
    :accessor renderer-inline)))
