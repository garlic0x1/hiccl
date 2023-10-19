(in-package :hiccl)

(defmacro keyword-set (&body body)
    (assert (every #'keywordp body))
    `(load-time-value (set-hash-table ',body :test 'eq) t))

;; Set to nil to ignore warnings
(defparameter *validate-tags?* t)

(defparameter *html5-elements*
    (keyword-set
      :a :abbr :address :area :article :aside :audio :b :base :bdi :bdo :blockquote
      :body :br :button :canvas :caption :cite :code :col :colgroup :command :data
      :datalist :dd :del :details :dfn :dialog :div :dl :dt :em :embed :fieldset
      :figcaption :figure :footer :form :head :h1 :h2 :h3 :h4 :h5 :h6 :header
      :hgroup :hr :html :i :iframe :img :input :ins :kbd :keygen :label :legend :li
      :link :main :map :mark :math :menu :meta :meter :nav :noscript :object :ol
      :optgroup :option :output :p :param :picture :pre :progress :q :rp :rt :ruby :s :samp
      :script :section :select :small :source :span :strong :style :sub :svg :summary
      :sup :table :tbody :td :template :textarea :tfoot :th :thead :time :title :tr
      :track :u :ul :var :video :wbr))

(defun validate-tag (tag)
  "Emit a warning if an invalid tag is used"
  (when (and *validate-tags?* (not (gethash tag *html5-elements*)))
    (warn (format nil "Invalid tag ~A" tag)))
  tag)
