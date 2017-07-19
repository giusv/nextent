;;;; package.lisp 
;; (push #p"d:/giusv/lisp/nextent/" asdf:*central-registry*)

(defpackage :lol
  (:use :cl)
  (:export :this :it
           :aif 
           :pandoriclet :get-pandoric  :dlambda :flatten :group :mkstr :symb :keyw))
(defpackage :utils
  (:use :cl :lol)
  (:export :random-number :random-string :random-boolean
           :rest-key :rest-plain
           :plist-keys :plist-values
           :plist-p
           :singular
           :bindall
           :glue
           :lower
           :lower-camel :upper-camel
           :split-str :interleave
           :append* 
           :write-file
           :my-debug
           :closure-equal
           :hash-table-keys
           :hash-table-values
           :overlaps))

(defpackage :parser
  (:use :cl :lol :utils)
  (:export :tuple :result :apply-parser :parse :bind :fail :item :do-with :sat :sym :choose :choose-among :zero :plus :choice 
           :many :many1 :sepby :sepby1 :sublist :pair :optional :atomic :var-init :req-var :opt-var :lambda-list :arg-names)) 


(defpackage :grammar
  (:use :cl :lol :utils)
  (:export :defprim :defprod
           :synth :synth-all :synth-plist :synth-plist-merge))

(defpackage :doc
  (:use :cl :lol :utils :grammar)
  (:export :empty :text :nest :vcat :hcat :hcat+
           :parens :brackets :braces :single-quotes :double-quotes :back-quotes :angular
           :comma :dot :semi :colon :forward-slash :equals :blank
           :punctuate :prepend :postpend
           :textify))

(defpackage :html
  (:use :cl :lol :utils :grammar)
  (:export :tag 
           :taglist
           :span-color
           :html :head :title :meta :link :body :h1 :h2 :h3 :h4 :h5 :div :span :li :dl :dt :dd :ul :ol :pre :i 
           :strong :code :script
           :table :thead :tbody :tr :th :td
           :section :article :aside :p :a
           :button :input :textarea
           :label
           :form
           :nav))

(defpackage :expr
  (:use :cl :lol :utils :grammar)
  (:export :const :attr :variab :value :param
           :+true+ :+false+ :+and+ :+or+ :+not+ :+equal+ :+less-than+ :+greater-than+ :+null+))

(defpackage :lang
  (:use :cl :lol :utils :grammar :doc)
  (:export :bb-empty :bb-comment :bb-const 
           :bb-type :bb-primitive-type :bb-array-type :bb-object-type :bb-template-type :bb-wildcard-type
           :bb-pair :bb-array :bb-element
           :bb-object :bb-annotation :bb-annotation2 :bb-with-annotations :bb-class :bb-interface :bb-method :bb-signature 
           :bb-import :bb-package :bb-new :bb-call :bb-static :bb-dynamic :bb-enum :bb-chain :bb-constructor :bb-arrow 
           :bb-list :bb-unit :bb-template :bb-assign :bb-return :bb-throw :bb-statement
           :bb-null :bb-nil 
           :bb-try :bb-catch% :bb-catch
           :bb-switch :bb-case :bb-break
           :bb-if
           :bb-+ :bb-- :bb-* :bb-/
           :bb-or :bb-and :bb-not
           :bb-true :bb-false
           :bb-equal :bb-greater-than :bb-less-than 
           :c-empty :c-comment :c-pair :c-const :c-type :c-array-type :c-array :c-for
           :c-statement :c-list :c-signature :c-include :c-assign :c-increment :c-decrement 
           :c-call :c-dynamic 
           :c-element :c-unit :c-return :c-if :fprim :c-equal :c-nil))

(defpackage :java
  (:use :cl :lol :utils :grammar :doc)
  (:export :java-empty :java-comment :java-const 
           :java-type :java-primitive-type :java-array-type :java-object-type :java-template-type :java-wildcard-type
           :java-pair :java-array :java-element
           :java-object :java-annotation :java-annotation2 :java-with-annotations :java-class :java-interface :java-method :java-signature 
           :java-import :java-package :java-new :java-call :java-static :java-dynamic :java-enum :java-chain :java-constructor :java-arrow 
           :java-list :java-unit :java-template :java-assign :java-return :java-throw :java-statement
           :java-null :java-nil 
           :java-try :java-catch% :java-catch
           :java-switch :java-case :java-break
           :java-if
           :java-+ :java-- :java-* :java-/
           :java-or :java-and :java-not
           :java-true :java-false
           :java-equal :java-greater-than :java-less-than))

(defpackage :url
  (:use :cl :lol :utils :parser :grammar :doc :lang)
  (:export :void :static-chunk :dynamic-chunk :expression-chunk :path-parameter :query-parameter :login-parameter 
           :backward-chain :multi :forward-chain :queried
           :url))

(defpackage :server
  (:use :cl :lol :utils :grammar :lang)
  (:export :defresource
           :*resources*
           :defservice
           :*services*
           :rest-service 
           :rest-collection
           :rest-singleton
           :rest-item% :rest-item
           :rest-get% :rest-get
           :rest-post% :rest-post
           :rest-put :rest-delete
           :with-fields
           :concat% :concat
           :empty :create-entity% :create-entity
           :update-entity% :update-entity
           :exec-query% :exec-query
           :find-entity% :find-entity
           :create-transfer% :create-transfer
           :mu% :mu :mapcomm% :mapcomm :fork
           :respond))


(defpackage :data
  (:use :cl :lol :utils :parser :grammar :lang) 
  (:export :with-data :with-data%
           :remote
           :rand
           :jnull :jbool :jnumber :jstring :jobject :jarray
           :jsbool :jsnumber :jsstring :jsprop :jsobject :jsarray
           :filter :ident :prop :elem :comp
           :atype :attribute :primary-key :entity :relationship
           :defent :defrel :deformat :defquery
           :query :relation :product :project :restrict :equijoin :with-queries 
           :named-query
           :*entities* :*relationships* :*formats* :*queries*))

(defpackage :validator
  (:use :cl :lol :utils :grammar :lang)
  (:export :required))

(defpackage :gui
  (:use :cl :lol :utils :grammar :lang)
  (:export :input :button
           :vert :vert*
           :horz :horz*
           :abst
           :static :static%
           :dynamic :dynamic%
           :label
           :listing :listing%
           :alt
           :form% :form :bnd :obj% :obj :arr% :arr
           :table :table%
           :description :description%
           :panel
           :link :navbar))

(defpackage :ml
  (:use :cl :lol :utils :grammar :lang)
  (:export :perceptron
           :sum
           :loss))

(defpackage :nextent
  (:use :cl :lol :utils :doc :grammar :lang))

(defpackage :pgen
  (:use :cl :lol :utils :doc :grammar :java))

(defpackage :lgen
  (:use :cl :lol :utils :doc :grammar :java))

(defpackage :indy
  (:use :cl :lol :utils :parser :doc :grammar))
