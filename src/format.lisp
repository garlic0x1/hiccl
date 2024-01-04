(defpackage #:hiccl/format 
  (:use :cl)
  (:import-from #:trivia #:match)
  )
(in-package :hiccl/format)

(defparameter states '(:base :open :close))

(defclass formatter ()
  ((in 
    :initarg :in
    :initform (error "Must provide input stream")
    :accessor formatter-in)
   (out
    :initarg :out
    :initform (error "Must provide output stream")
    :accessor formatter-out)
   (indent
    :initform 0
    :accessor formatter-indent)
   (stack 
    :initform '()
    :accessor formatter-stack)
   (state 
    :initform :base
    :accessor formatter-state)))

(defmethod print-indent ((obj formatter))
  (dotimes (_i (formatter-indent obj))
    (write-char #\  (formatter-out obj))))

(defmethod handle-char ((obj formatter) c)
  (write-char c (formatter-out obj))
  (match (cons (formatter-state obj) c)
    ((cons :base #\<)
     (setf (formatter-state obj) :open))

    ((cons :open #\/)
     (setf (formatter-state obj) :close))

    ((cons :open #\>)
     (setf (formatter-state obj) :base)
     (setf (formatter-stack obj) (cons "" (formatter-stack obj))))

    ((cons :close #\>)
     (setf (formatter-state obj) :base)
     (setf (formatter-stack obj) (cdr (formatter-stack obj))))

    ((cons :open c)
     (setf (car (formatter-stack obj))
           (concatenate 'string
                        (car (formatter-stack obj))
                        (string c))))))

(defmethod format-html ((obj formatter))
  (loop :for c := (read-char (formatter-in obj))
        :do (handle-char obj c)))
