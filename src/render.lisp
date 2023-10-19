(in-package :hiccl)

(defgeneric render (sxml &key &allow-other-keys)
  (:method ((sxml symbol) &key out)
    (format out "~a" sxml))

  (:method ((sxml string) &key out)
    (format out "~a" (escape sxml #'escape-table)))

  (:method ((sxml list) &key out)
    (let* ((tag (car sxml))
           (attrs (loop :for (k v) :on (cdr sxml) :by 'cddr
                        :while (keywordp k)
                        :collect (cons k v)))
           (children (nthcdr (+ 1 (* 2 (length attrs))) sxml)))
      (format out "<~(~a~)~{ ~a~}>~%~{~a~%~}</~(~a~)>"
              tag
              (mapcar (lambda (attr) (format nil "~(~a~)=\"~a\"" (car attr) (cdr attr))) attrs)
              (mapcar #'render children)
              tag)))

  ;; not as convenient as i thought, since CL vecs are deep quoted unlike clj
  ;; (:method ((sxml vector) &key out)
  ;;   (let* ((tag (first-elt sxml))
  ;;          (attrs (loop :for batch :in (batches (subseq sxml 1) 2)
  ;;                       :while (keywordp (first-elt batch))
  ;;                       :collect batch))
  ;;          (children (subseq sxml (+ 1 (* 2 (length attrs))))))
  ;;     (format out "<~(~a~) ~{~a~^ ~}>~{~a~}</~(~a~)>"
  ;;             tag
  ;;             (mapcar (lambda (attr) (format nil "~(~a~)=\"~a\"" (elt attr 0) (elt attr 1))) attrs)
  ;;             (map 'list #'render children)
  ;;             tag)))
  )
