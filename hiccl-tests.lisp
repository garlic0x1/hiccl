(fiasco:define-test-package #:hiccl/test)

(in-package #:hiccl/test)

(deftest basic ()
  "Ensure we are at least generating valid HTML" 
  (is (plump:node-p (plump:parse (hiccl:render nil '(:hi :world "al<>" (:div "lol"))))))
  (is (plump:node-p (plump:parse (hiccl:render nil '(:hi (:<> (:lol |lol|))))))))

(deftest attributes ()
  "Ensure attributes and JSX shorthand work"
  (let ((basic-attrs (plump:parse (hiccl:render nil '(:div :hi "world"))))
        (macro-attrs (plump:parse (hiccl:render nil '(:div.class1.class2#id1.class3 :id "id2" "text"))))
        (bool-attrs (hiccl:render nil '(:div :bool1 nil :bool2 nil "hi"))))
    (is (string= "world" (alexandria:first-elt (lquery:$ basic-attrs "div" (attr :hi)))))
    (is (string-equal " class1 class2 class3" (alexandria:first-elt (lquery:$ macro-attrs "div" (attr :class)))))
    (is (string-equal "id2id1" (alexandria:first-elt (lquery:$ macro-attrs "div" (attr :id)))))
    ;; bool attrs
    (is (string= "<DIV BOOL1 BOOL2>hi</DIV>" bool-attrs))))

(deftest nesting ()
  "Ensure nested nodes work"
  (let* ((sxml '(:div (:1 (:2 "a") (:2 "b")) (:1 "c")))
         (node (plump:parse (hiccl:render nil sxml))))
    (is (= 1 (length (plump:child-elements node))))
    (is (= 2 (length (plump:child-elements (alexandria:first-elt (lquery:$ node "1"))))))
    (is (= 1 (length (plump:children (elt (lquery:$ node "1") 1)))))))

(deftest sanitization ()
  "Ensure at least some level of sanitization happens"
  (is (string= "<DIV>&lt;&gt;</DIV>" (hiccl:render nil '(:div "<>"))))
  (is (string= "<DIV>&#39;&quot;</DIV>" (hiccl:render nil '(:div "'\""))))
  (is (string= "<DIV AT&lt;&gt;R=\"&lt;&gt;\">&#39;&quot;</DIV>" (hiccl:render nil '(:div :at<>r "<>" "'\"")))))

(defstruct user username email)
(defmethod hiccl::render-form (out (obj user))
  (hiccl:render out
    `(:div.user
      (:div.username ,(user-username obj))
      (:div.email ,(user-email obj)))))

(deftest extension ()
  (is (string= "<!-- hi -->" (hiccl:render nil '(:!-- "hi"))))
  (is (string= "<!-- hi -->" (hiccl:render nil '(:comment "hi"))))
  (is (string= "<DIV CLASS=\" USER\"><DIV CLASS=\" USERNAME\">garlic</DIV><DIV CLASS=\" EMAIL\">garlic@email.com</DIV></DIV>"
       (hiccl:render nil (make-user :username "garlic" :email "garlic@email.com")))))

(deftest whitespace ()
  (is (string= "<DIV>hi world</DIV>" (hiccl:render nil '(:div "hi" " " "world")))))

(deftest raw ()
  (is (string= "<div>hi</lol>" (hiccl:render nil '(:raw "<div>hi</lol>"))))
  (is (string= "<div>hi</lol>" (hiccl:render nil '|<div>hi</lol>|)))
  ;; reader capitalizes the keyword
  (is (not (string= "<div>hi</lol>" (hiccl:render nil :<div>hi</lol>)))))

(deftest extract-attrs ()
  (multiple-value-bind (attrs children)
      (hiccl::extract-attrs '(:k "v" :k2 "v2" "body" :body))
    (is (= 2 (length children)))
    (is (string= "v" (alexandria:assoc-value attrs :k)))
    (is (string= "v2" (alexandria:assoc-value attrs :k2))))

  (multiple-value-bind (attrs children)
      (hiccl::extract-attrs '(:k nil :c))
    (is (null (alexandria:assoc-value attrs :k)))
    (is (= 1 (length attrs)))
    (is (= 1 (length children)))))

(deftest empty-tag ()
  (is (equal "<IMG SRC=\"image.jpg\" ALT=\"\"></IMG>" (hiccl:render nil '(:img :src "image.jpg" :alt "")))))

(deftest html5-doctype ()
  (is (string=
       "<!DOCTYPE html><H1>Hello, World!</H1>"
       (hiccl:render nil '(:<>
                     (:doctype "html")
                     (:h1 "Hello, World!"))))))
