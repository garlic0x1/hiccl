(in-package :hiccl)

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
