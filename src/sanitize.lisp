(defpackage #:hiccl/sanitize
  (:use :cl)
  (:import-from #:serapeum :defconst)
  (:export #:sanitize))
(in-package :hiccl/sanitize)

;; extracted from spinneret:escape-string

(defconst no-break-space
  #+lispworks #\No-break-space
  #-lispworks #\No-break_space)

(defun escape-table (c)
  (case c
    (#.no-break-space "&nbsp;")
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\" "&quot;")
    (#\' "&#39;")))

(defun sanitize (str) (escape sxml #'escape-table))
