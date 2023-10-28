(defpackage #:hiccl/render
  (:use :cl)
  (:nicknames #:hiccl)
  (:export #:render))
(in-package :hiccl/render)

(defgeneric apply-tag (tag body)
  ;; Comment special tag
  (:method ((tag (eql :comment)) body)
    (format nil "<!--~%~{~a~%~}-->" body))

  ;; Alternative comment tag
  (:method ((tag (eql :!--)) body)
    (format nil "<!--~%~{~a~%~}-->" body))

  ;; Doctype special tag
  (:method ((tag (eql :doctype)) body)
    (format nil "<!DOCTYPE~{ ~a~}>" body))

  ;; Dummy tag (emits children in sequence)
  (:method ((tag (eql :<>)) body)
    (format nil "~{~a~^~%~}" (mapcar #'render body)))

  ;; Raw string
  (:method ((tag (eql :raw)) body)
    (format nil "~{~a~^~%~}" body))

  ;; Default strategy
  (:method (tag body)
    (multiple-value-bind (attrs children) (extract-attrs-and-children body)
      (multiple-value-bind (tag attrs) (hiccl/expand::expand tag attrs)
          (format nil "<~(~a~)~{ ~a~}>~%~{~a~%~}</~(~a~)>"
                  tag
                  (mapcar #'format-attr attrs)
                  (mapcar #'render children)
                  tag)))))

(defgeneric render (sxml &key out)
  ;; Render symbols raw
  (:method ((sxml symbol) &key out)
    (format out "~a" sxml))

  ;; Render strings escaped
  (:method ((sxml string) &key out)
    (format out "~a" (hiccl/sanitize:sanitize sxml)))

  ;; Render lists as XML nodes
  (:method ((sxml list) &key out)
    (format out "~a" (apply-tag (car sxml) (cdr sxml)))))
