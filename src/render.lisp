(defpackage #:hiccl/render
  (:nicknames #:hiccl)
  (:use :cl :hiccl/utils)
  (:import-from #:hiccl/sanitize #:sanitize)
  (:export #:render #:render-forms))
(in-package :hiccl/render)

;;
;; Render XML attributes
;; If value is nil, the attribute is treated as boolean
;;

(defun render-attr (attr)
  (let ((k (car attr))
        (v (cdr attr)))
    (if v
        (format nil "~(~a~)=\"~a\"" (sanitize k) (sanitize v))
        (format nil "~(~a~)" (sanitize k)))))

;;
;; Handle SXML nodes by tag
;;

(defgeneric apply-tag (out tag body)
  ;; Comment special tag
  (:method (out (tag (eql :comment)) body)
    (format out "<!--~%~{~a~%~}-->" body))

  ;; Alternative comment tag
  (:method (out (tag (eql :!--)) body)
    (format out "<!--~%~{~a~%~}-->" body))

  ;; Doctype special tag
  (:method (out (tag (eql :doctype)) body)
    (format out "<!DOCTYPE~{ ~a~}>" body))

  ;; Dummy tag (emits children in sequence)
  (:method (out (tag (eql :<>)) body)
    (dolist (c body) (render-form out c)))

  ;; Raw string
  (:method (out (tag (eql :raw)) body)
    (format out "~{~a~%~}" body))

  ;; Default strategy
  (:method (out tag body)
    (multiple-value-bind (attrs children) (extract-attrs-and-children body)
      (multiple-value-bind (tag attrs) (hiccl/expand::expand tag attrs)
        (format out "<~(~a~)~{ ~a~}>~%" tag (mapcar #'render-attr attrs))
        (dolist (c children) (render-form out c))
        (format out "</~(~a~)>~%" tag)))))

;;
;; Render one SXML form to output
;;

(defgeneric render-form (out sxml)
  ;; Render symbols raw
  (:method (out (sxml symbol))
    (format out "~a~%" sxml))

  ;; Render numbers literally
  (:method (out (sxml number))
    (format out "~a~%" sxml))

  ;; Render strings escaped
  (:method (out (sxml string))
    (format out "~a~%" (hiccl/sanitize:sanitize sxml)))

  ;; Render lists as XML nodes
  (:method (out (sxml list))
    (apply-tag out (car sxml) (cdr sxml))))

(defun render-forms (output &rest forms)
  (if output
      (dolist (f forms) (render-form output f))
      (with-output-to-string (capture) (apply (curry #'render-forms capture) forms))))

;;
;; This is the primary exposed utility
;; It is wrapped as a macro in order to use &body and have nicer editor support
;; If this is a problem for you, use hiccl:render-forms
;;
;; Example:
;;   (hiccl:render t '(:div.hi :class "world" "<3"))
;;
;; Output:
;;   <div id="" class="world hi">
;;   &lt;3
;;   </div>
;;

(defmacro render (output &body forms)
  `(render-forms ,output ,@forms))
