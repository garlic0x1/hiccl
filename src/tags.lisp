(defpackage #:hiccl/tags
  (:use :cl)
  (:export #:inline?))
(in-package :hiccl/tags)

;; ----------------------------------------------------------------------------
(defun inline? (tag)
  (find tag inline-tags))

;; ----------------------------------------------------------------------------
(defparameter inline-tags
  '(:a
    :abbr
    :acronym
    :b
    :bdo
    :big
    :br
    :button
    :cite
    :code
    :dfn
    :em
    :i
    :img
    :input
    :kbd
    :label
    :map
    :object
    :output
    :q
    :samp
    :script
    :select
    :small
    :span
    :strong
    :sub
    :sup
    :textarea
    :time
    :tt
    :var))
