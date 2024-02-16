(defpackage #:hiccl-test
  (:use :cl :fiveam :alexandria)
  (:import-from #:plump #:node-p #:parse #:child-elements #:children #:serialize)
  (:import-from #:lquery #:$)
  (:import-from #:hiccl #:render))
(in-package :hiccl-test)

(def-suite :hiccl
  :description "Tests for Hiccl")
(in-suite :hiccl)

;;
;; Ensure we are at least generating valid HTML
;;

;; ----------------------------------------------------------------------------
(test :basic
  (is (node-p (parse (render nil '(:hi :world "al<>" (:div "lol"))))))
  (is (node-p (parse (render nil '(:hi (:<> (:lol |lol|))))))))

;;
;; Ensure attributes and JSX shorthand work
;;

;; ----------------------------------------------------------------------------
(test :attributes
  (let ((basic-attrs (parse (render nil '(:div :hi "world"))))
        (macro-attrs (parse (render nil '(:div.class1.class2#id1.class3 :id "id2" "text"))))
        (bool-attrs (render nil '(:div :bool1 nil :bool2 nil "hi"))))
    (is (equal "world" (first-elt ($ basic-attrs "div" (attr :hi)))))
    (is (equal "class1 class2 class3" (first-elt ($ macro-attrs "div" (attr :class)))))
    (is (equal "id2 id1" (first-elt ($ macro-attrs "div" (attr :id)))))
    ;; bool attrs
    (is (equal "<div bool1 bool2>hi</div>" bool-attrs))))

;;
;; Ensure nested nodes work
;;

;; ----------------------------------------------------------------------------
(test :nesting
  (let* ((sxml '(:div (:1 (:2 "a") (:2 "b")) (:1 "c")))
         (node (parse (render nil sxml))))
    (is (= 1 (length (child-elements node))))
    (is (= 2 (length (child-elements (first-elt ($ node "1"))))))
    (is (= 1 (length (children (elt ($ node "1") 1)))))))

;;
;; Ensure at least some level of sanitization happens
;;

;; ----------------------------------------------------------------------------
(test :sanitization
  (is (equal "<div>&lt;&gt;</div>"
             (render nil '(:div "<>"))))

  (is (equal "<div>&#39;&quot;</div>"
             (render nil '(:div "'\""))))

  (is (equal "<div at&lt;&gt;r=\"&lt;&gt;\">&#39;&quot;</div>"
             (render nil '(:div :at<>r "<>" "'\"")))))

;; ----------------------------------------------------------------------------
(defstruct user username email)
(defmethod hiccl::render-form (out (obj user))
  (hiccl:render out
    `(:div.user
      (:div.username ,(user-username obj))
      (:div.email ,(user-email obj)))))

;; ----------------------------------------------------------------------------
(test :extension
  (is (equal
       "<div class=\"user\"><div class=\"username\">garlic</div><div class=\"email\">garlic@email.com</div></div>"
       (render nil (make-user :username "garlic" :email "garlic@email.com"))))

  (is (equal
       "<!-- hi -->"
       (render nil '(:!-- "hi"))))

  (is (equal
       "<!-- hi -->"
       (render nil '(:comment "hi")))))

;; ----------------------------------------------------------------------------
(test :whitespace
  (is (equal
       "<div>hi world</div>"
       (render nil '(:div "hi" " " "world")))))

;; ----------------------------------------------------------------------------
(test :raw
  (is (equal
       "<div>hi</lol>"
       (render nil '(:raw "<div>hi</lol>"))))

  (is (equal
       "<div>hi</lol>"
       (render nil '|<div>hi</lol>|)))

  ;; reader capitalizes the keyword
  (is (not (equal
            "<div>hi</lol>"
            (render nil :<div>hi</lol>)))))

;; ----------------------------------------------------------------------------
(test :extract-attrs
  (multiple-value-bind (attrs children)
      (hiccl/utils:extract-attrs '(:k "v" :k2 "v2" "body" :body))
    (is (= 2 (length children)))
    (is (equal "v" (assoc-value attrs :k)))
    (is (equal "v2" (assoc-value attrs :k2))))

  (multiple-value-bind (attrs children)
      (hiccl/utils:extract-attrs '(:k nil :c))
    (is (equal nil (assoc-value attrs :k)))
    (is (= 1 (length attrs)))
    (is (= 1 (length children)))))

;;
;; AI generated tests below
;;

;; ----------------------------------------------------------------------------
(test :empty-tag
  (is (equal
       "<img src=\"image.jpg\" alt=\"\"></img>"
       (render nil '(:img :src "image.jpg" :alt "")))))

;; ----------------------------------------------------------------------------
(test :conditional-rendering
  (is (equal
       "<div></div>"
       (render nil `(:div ,(when nil "lol")))))

  (is (equal
       "<div>hi</div>"
       (render nil `(:div ,(when t "hi")))))

  (is (equal
       "<div>hi</div>"
       (render nil `(:div ,(if t "hi" "bye"))))))

;; ----------------------------------------------------------------------------
(test :html5-doctype
  (is (equal
       "<!DOCTYPE html><h1>Hello, World!</h1>"
       (render nil '(:<>
                     (:doctype "html")
                     (:h1 "Hello, World!"))))))
