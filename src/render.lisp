(defpackage #:hiccl/render
  (:nicknames #:hiccl)
  (:use :cl :hiccl/utils)
  (:export #:render))
(in-package :hiccl/render)

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
    (format out "~{~a~^~%~}" (mapcar (curry #'render-form out) body)))

  ;; Raw string
  (:method (out (tag (eql :raw)) body)
    (format out "~{~a~^~%~}" body))

  ;; Default strategy
  (:method (out tag body)
    (multiple-value-bind (attrs children) (extract-attrs-and-children body)
      (multiple-value-bind (tag attrs) (hiccl/expand::expand tag attrs)
        (format out "<~(~a~)~{ ~a~}>" tag (mapcar #'format-attr attrs))
        (dolist (c children) (render-form out c))
        (format out "</~(~a~)>" tag)))))

;;
;; Render one SXML form to output
;;

(defgeneric render-form (out sxml)
  ;; Render symbols raw
  (:method (out (sxml symbol))
    (format out "~a" sxml))

  ;; Render numbers literally
  (:method (out (sxml number))
    (format out "~a" sxml))

  ;; Render strings escaped
  (:method (out (sxml string))
    (format out "~a" (hiccl/sanitize:sanitize sxml)))

  ;; Render lists as XML nodes
  (:method (out (sxml list))
    (apply-tag out (car sxml) (cdr sxml))))

;;
;; This is the primary exposed function
;; Example:
;;   (hiccl:render t '(:div.hi :class "world" "<3"))
;;
;; Output:
;;   <div id="" class="world hi">
;;   &lt;3
;;   </div>
;;

(defun render* (output &rest forms)
  (if output
      (dolist (f forms) (render-form output f))
      (with-output-to-string (capture) (apply (curry #'render* capture) forms))))

(defmacro render (output &body forms)
  `(render* ,output ,@forms))
