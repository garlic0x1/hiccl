(defpackage #:hiccl
  (:use #:cl)
  (:export #:render))
(in-package #:hiccl)

(defun make-adjustable-string ()
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun extract-attrs (body)
  (loop :for head :on body :by 'cddr
        :for (k v) := head
        :while (and (cdr head) (keywordp k))
        :collect (cons k v) :into attrs
        :finally (return (values attrs head))))

(defun escape-table (c)
  (case c
    ((#+lispworks #\No-break-space #-lispworks #\No-break_space ) "&nbsp;")
    ((#\&) "&amp;")
    ((#\<) "&lt;")
    ((#\>) "&gt;")
    ((#\") "&quot;")
    ((#\') "&#39;")))

(defun sanitize (obj)
  (declare (values string &optional))
  (loop :with res := (make-adjustable-string)
        :for ch :across (string obj)
        :for esc := (escape-table ch)
        :do (if esc
                (loop :for ch :across esc :do (vector-push-extend ch res))
                (vector-push-extend ch res))
        :finally (return res)))

(defun expand-tag (str)
  (declare (type string str))
  (loop :with state := ':tag
        :and tag   := (make-adjustable-string)
        :and id    := (make-adjustable-string)
        :and class := (make-adjustable-string)
        :and idp   :and classp
        :for ch :across str
        :do (case ch
              ((#\#) (setf state ':id idp t))
              ((#\.) (setf state ':class classp t) (vector-push-extend #\Space class))
              (otherwise
               (case state
                 ((:tag)   (vector-push-extend ch tag))
                 ((:id)    (vector-push-extend ch id))
                 ((:class) (vector-push-extend ch class)))))
        :finally (return (values tag (if classp class nil) (if idp id nil)))))

(defun prepare-attrs (attrs class id)
  (flet ((ensure-key-exists (alist key) (if (assoc key alist) alist (cons (cons key nil) alist))))
    (let ((attrs (if class (ensure-key-exists attrs :class) attrs)))
      (if id (ensure-key-exists attrs :id) attrs))))

(defun expand (tag attrs)
  (multiple-value-bind (tag class id) (expand-tag (string tag))
    (values tag
            (loop :for (k . v) :in (prepare-attrs attrs class id)
                  :collect (cons k (case k
                                     ((:class) (concatenate 'string v class))
                                     ((:id) (concatenate 'string v id))
                                     (otherwise v)))))))

(defun render-attr (out attr)
  (let ((k (car attr)) (v (cdr attr)))
    (if v
        (format out " ~a=\"~a\"" (sanitize k) (sanitize v))
        (format out " ~a" (sanitize k)))))

(defgeneric apply-tag (out tag body)
  (:method (out (tag (eql :comment)) body) (format out "<!-- ~{~a~} -->" body))
  (:method (out (tag (eql :!--)) body)     (format out "<!-- ~{~a~} -->" body))
  (:method (out (tag (eql :doctype)) body) (format out "<!DOCTYPE~{ ~a~}>" body))
  (:method (out (tag (eql :raw)) body)     (format out "~{~a~}" body))
  (:method (out (tag (eql :<>)) body)      (render-forms out body))
  (:method (out tag body)
    (multiple-value-bind (attrs children) (extract-attrs body)
      (multiple-value-bind (tag attrs) (expand tag attrs)
        (format out "<~a" tag)
        (dolist (a attrs) (render-attr out a))
        (write-char #\> out)
        (render-forms out children)
        (format out "</~a>" tag)))))

(defgeneric render-form (out sxml)
  (:method (out (sxml null))   nil)                                ; don't render NIL
  (:method (out (sxml symbol)) (format out "~a" sxml))             ; render symbols raw
  (:method (out (sxml string)) (write-string (sanitize sxml) out)) ; sanitize strings
  (:method (out (sxml number)) (format out "~a" sxml))
  (:method (out (sxml list))   (apply-tag out (car sxml) (cdr sxml))))

(defun render-forms (output forms)
  (if output
      (dolist (f forms) (render-form output f))
      (with-output-to-string (capture) (funcall #'render-forms capture forms))))

(defmacro render (output &body forms) `(render-forms ,output (list ,@forms)))
