(defpackage #:hiccl
  (:use
   #:cl)
  (:export
   #:render))

(in-package #:hiccl)

;;;
;;; Utilities
;;;

(defun extract-attrs (body)
  (declare (type list body)
           (values list list &optional))

  (loop :for head :on body :by 'cddr
        :for (k v) := head
        :while (and v (keywordp k))
        :collect (cons k v) :into attrs
        :finally (return (values attrs head))))

;;;
;;; Sanitize
;;;

(defun escape-table (c)
  (declare (type character c)
           (values (or null string) &optional))

  (case c
    ((#+lispworks #\No-break-space #-lispworks #\No-break_space ) "&nbsp;")
    ((#\&) "&amp;")
    ((#\<) "&lt;")
    ((#\>) "&gt;")
    ((#\") "&quot;")
    ((#\') "&#39;")))

(defun sanitize (obj)
  (declare (type (or string symbol) obj)
           (values string &optional))

  (serapeum:escape (string obj) #'escape-table))

;;;
;;; Expand
;;;

(defun expand-tag (str)
  (declare (type string str)
           (values string (or null string) (or null string) &optional))

  (flet ((make-adjustable-string ()
           (make-array 0 :element-type 'character
                         :adjustable t
                         :fill-pointer 0)))

    (loop
      :with state := ':tag
      :and tag    := (make-adjustable-string)
      :and id     := (make-adjustable-string)
      :and class  := (make-adjustable-string)
      :and idp    := nil
      :and classp := nil
      :for ch :across str
      :do (case ch
            ((#\#)
             (setf state ':id)
             (setf idp t)
             (vector-push-extend #\Space id))
            ((#\.)
             (setf state ':class)
             (setf classp t)
             (vector-push-extend #\Space class))
            (otherwise
             (case state
               ((:tag)
                (vector-push-extend ch tag))
               ((:id)
                (vector-push-extend ch id))
               ((:class)
                (vector-push-extend ch class)))))
      :finally (return (values tag (if classp class nil) (if idp id nil))))))

(defun prepare-attrs (attrs class id)
  (declare (type list attrs)
           (type (or null string) class id)
           (values list &optional))

  (flet ((ensure-key-exists (alist key)
           (if (assoc key alist) alist (cons (cons key nil) alist))))
    (let ((attrs (if class (ensure-key-exists attrs :class) attrs)))
      (if id (ensure-key-exists attrs :id) attrs))))

(defun expand (tag attrs)
  (declare (type symbol tag)
           (type list attrs)
           (values string list &optional))

  (multiple-value-bind (tag class id) (expand-tag (string tag))
    (values
     tag
     (loop
       :for (k . v) :in (prepare-attrs attrs class id)
       :collect (cons k (case k
                          ((:class) (concatenate 'string v class))
                          ((:id) (concatenate 'string v id))
                          (otherwise v)))))))

;;;
;;; Render
;;;

(defun render-attr (out attr)
  "Render HTML attributes, null values treated as boolean attributes."
  (declare (type stream out)
           (type cons attr))

  (let ((k (car attr)) (v (cdr attr)))
    (if v
        (format out " ~(~a~)=\"~a\"" (sanitize k) (sanitize v))
        (format out " ~(~a~)" (sanitize k)))))

(defgeneric apply-tag (out tag body)
  ;; Comment special tag
  (:method (out (tag (eql :comment)) body)
    (declare (type stream out))
    (format out "<!-- ~{~a~} -->" body))

  ;; Alternative comment tag
  (:method (out (tag (eql :!--)) body)
    (declare (type stream out))
    (format out "<!-- ~{~a~} -->" body))

  ;; Doctype special tag
  (:method (out (tag (eql :doctype)) body)
    (declare (type stream out))
    (format out "<!DOCTYPE~{ ~a~}>" body))

  ;; Dummy tag (emits children in sequence)
  (:method (out (tag (eql :<>)) body)
    (declare (type stream out))
    (render-forms out body))

  ;; Raw string
  (:method (out (tag (eql :raw)) body)
    (declare (type stream out))
    (format out "~{~a~}" body))

  ;; Default strategy
  (:method (out tag body)
    (declare (type stream out))
    (multiple-value-bind (attrs children) (extract-attrs body)
      (multiple-value-bind (tag attrs) (expand tag attrs)
        (format out "<~(~a~)" tag)
        (dolist (a attrs) (render-attr out a))
        (write-char #\> out)
        (render-forms out children)
        (format out "</~(~a~)>" tag)))))

(defgeneric render-form (out sxml)
  ;; Dont render nil
  (:method (out (sxml null)) nil)

  ;; Render symbols raw
  (:method (out (sxml symbol))
    (declare (type stream out))
    (format out "~a" sxml))

  ;; Render numbers literally
  (:method (out (sxml number))
    (declare (type stream out))
    (format out "~a" sxml))

  ;; Render strings escaped
  (:method (out (sxml string))
    (declare (type stream out))
    (format out "~a" (sanitize sxml)))

  ;; Render lists as XML nodes
  (:method (out (sxml list))
    (declare (type stream out))
    (apply-tag out (car sxml) (cdr sxml))))

(defun render-forms (output forms)
  (if output
      (dolist (f forms) (render-form output f))
      (with-output-to-string (capture) (funcall #'render-forms capture forms))))

(defmacro render (output &body forms)
  `(render-forms ,output (list ,@forms)))
