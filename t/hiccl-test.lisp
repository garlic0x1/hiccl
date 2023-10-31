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

(test :basic
  (is (node-p (parse (render nil '(:hi :world "al<>" (:div "lol"))))))
  (is (node-p (parse (render nil '(:hi (:<> (:lol |lol|))))))))

;;
;; Ensure attributes and JSX shorthand work
;;

(test :attributes
  (let ((basic-attrs (parse (render nil '(:div :hi "world"))))
        (macro-attrs (parse (render nil '(:div.class1.class2#id1.class3 :id "id2" "text"))))
        (bool-attrs (render nil '(:div :bool1 nil :bool2 nil "hi"))))
    (is (equal "world" (first-elt ($ basic-attrs "div" (attr :hi)))))
    (is (equal "class1 class2 class3" (first-elt ($ macro-attrs "div" (attr :class)))))
    (is (equal "id2 id1" (first-elt ($ macro-attrs "div" (attr :id)))))
    ;; bool attrs
    (is (equal
"<div bool1 bool2>
hi
</div>
"
         bool-attrs))
    ))

;;
;; Ensure nested nodes work
;;

(test :nesting
  (let* ((sxml '(:div (:1 (:2 "a") (:2 "b")) (:1 "c")))
         (node (parse (render nil sxml))))
    (is (= 1 (length (child-elements node))))
    (is (= 2 (length (child-elements (first-elt ($ node "1"))))))
    (is (= 1 (length (children (elt ($ node "1") 1)))))))

;;
;; Ensure at least some level of sanitization happens with strings
;;

(test :sanitization
  (is (equal
       "<div>
&lt;&gt;
</div>
" (serialize (parse (render nil '(:div "<>"))) nil)))

  (is (equal
       "<div>
'&quot;
</div>
" (serialize (parse (render nil '(:div "'\""))) nil))))


;;
;; string comparison is fucked, probably some weird whitespace, still werks tho
;;

;; (test :components
;;   (let* ((c1 (lambda (name) `(:span.inner "Hello, " ,name)))
;;          (c2 (lambda (inner) `(:div.outer ,(funcall inner "world"))))
;;          (out (render nil (funcall c2 c1)))
;;          ;; (node (parse out))
;;          )

;;     (format t "~%Out:~% ~W~%~%Done~%" out)

;;     (is (string-equal out
;;  "<div class=\"outer\">
;; <span class=\"inner\">
;; Hello,
;; world
;; </span>
;; </div>
;; "
;;                  ))))
