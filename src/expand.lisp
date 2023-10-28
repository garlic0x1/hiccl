(defpackage #:hiccl/expand
  (:use :cl :binding-arrows)
  (:import-from #:serapeum :split-sequence-if)
  (:import-from #:trivia :match)
  (:export #:expand))
(in-package :hiccl/expand)

(defun expand-tag (input &key tag class id (state :tag))
  (if input
      (match (cons (car input) state)
        ((cons #\# _state) (expand-tag
                            (cdr input)
                            :tag tag
                            :class class
                            :id (concatenate 'string id " ")
                            :state :id))
        ((cons #\. _state) (expand-tag
                            (cdr input)
                            :tag tag
                            :class (concatenate 'string class " ")
                            :id id
                            :state :class))
        ((cons char :tag) (expand-tag
                           (cdr input)
                           :tag (concatenate 'string tag (string char))
                           :class class
                           :id id
                           :state :tag))
        ((cons char :class) (expand-tag
                           (cdr input)
                           :tag tag
                           :class (concatenate 'string class (string char))
                           :id id
                           :state :class))
        ((cons char :id) (expand-tag
                           (cdr input)
                           :tag tag
                           :class class
                           :id (concatenate 'string id (string char))
                           :state :id)))
      (values tag class id ;; `((:class ,class) (:id ,id))
              )))

(defun ensure-has (alist key)
  (if (assoc key alist) alist (cons (cons key nil) alist)))

(defun prepare-attrs (attrs)
  (-> attrs
    (ensure-has :class)
    (ensure-has :id)))

(defun expand (tag attrs)
  (multiple-value-bind (tag class id) (expand-tag (coerce (str:downcase (string tag)) 'list))
    (values tag (loop :for pair :in (prepare-attrs attrs)
                      :for k := (car pair)
                      :for v := (cdr pair)
                      :for nv := (match k
                                   (:class (str:trim (concatenate 'string v class)))
                                   (:id (str:trim (concatenate 'string v id)))
                                   (t v))
                      :collect (cons k nv)))))
