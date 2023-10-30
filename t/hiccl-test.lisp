(defpackage #:hiccl-test
  (:use :cl :fiveam)
  (:import-from #:plump #:node-p #:parse)
  (:import-from #:lquery #:$)
  (:import-from #:hiccl #:render))
(in-package :hiccl-test)

(def-suite :hiccl
  :description "Tests for Hiccl")
(in-suite :hiccl)

(test :basic
  (is (node-p (parse (render nil '(:hi :world "al<>" (:div "lol"))))))
  (is (node-p (parse (render nil '(:hi (:<> (:lol |lol|))))))))

(test :attributes
  (let ((basic-attrs (parse (render nil '(:div :hi "world"))))
        (macro-attrs (parse (render nil '(:div.class1.class2#id1.class3 :id "id2" "text")))))
    (is (equal "world" (elt ($ basic-attrs "div" (attr :hi)) 0)))
    (is (equal "class1 class2 class3" (elt ($ macro-attrs "div" (attr :class)) 0)))
    (is (string= "id2 id1" (elt ($ macro-attrs "div" (attr :id)) 0)))))
