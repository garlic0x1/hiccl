(in-package :hiccl)

(defun indent (depth)
  (format nil "~v@{~A~:*~}" depth "  "))

(defun format-attr (attr)
  (format nil "~(~a~)=\"~a\"" (car attr) (cdr attr)))

(defgeneric render (sxml &key &allow-other-keys)
  (:method ((sxml symbol) &key out (depth 0))
    (format out "~a~a"
            (indent depth)
            sxml))

  (:method ((sxml string) &key out (depth 0))
    (format out "~a~a"
            (indent depth)
            (escape sxml #'escape-table)))

  (:method ((sxml list) &key out (depth 0))
    (let* ((tag (car sxml))
           (attrs (loop :for (k v) :on (cdr sxml) :by 'cddr
                        :while (keywordp k)
                        :collect (cons k v)))
           (children (nthcdr (+ 1 (* 2 (length attrs))) sxml)))
      (format out "~a<~(~a~)~{ ~a~}>~{~%~a~}~%~a</~(~a~)>"
              (indent depth)
              tag
              (mapcar #'format-attr attrs)
              (mapcar (lambda (child) (render child :out out :depth (+ 1 depth))) children)
              (indent depth)
              tag))))
