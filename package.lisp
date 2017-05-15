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
           :plist-p))

(defpackage :parser
  (:use :cl :lol :utils)
  (:export :tuple :result :apply-parser :parse :bind :fail :item :do-with :sat :sym :choose :zero :plus :choice 
           :many :many1 :sepby :sepby1 :sublist :pair :optional :atomic :var-init :req-var :opt-var :lambda-list :arg-names)) 


(defpackage :grammar
  (:use :cl :lol :utils)
  (:export :defprim 
           :synth :synth-all :synth-plist :synth-plist-merge))

(defpackage :doc
  (:use :cl :lol :utils :grammar)
  (:export :empty :text :nest :vcat :hcat :hcat+
           :parens :brackets :braces :single-quotes :double-quotes :back-quotes :angular
           :comma :dot :semi :colon :forward-slash :equals :blank
           :punctuate :prepend :postpend
           :lower-camel :upper-camel
           :split-str :interleave
           :append*
           :textify
           :write-file))

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
  (:export :const 
           :attr
           :value))


(defpackage :url
  (:use :cl :lol :utils :parser :grammar :doc)
  (:export :void :static-chunk :dynamic-chunk :expression-chunk :path-parameter :query-parameter :login-parameter 
           :backward-chain :multi :forward-chain :queried
           :url))

(defpackage :lang
  (:use :cl :lol :utils :grammar :doc)
  (:export :bb-empty :bb-comment :bb-const :bb-type :bb-pair :bb-array :bb-element
           :bb-object :bb-annotation :bb-with-annotations :bb-class :bb-interface :bb-method 
           :bb-import :bb-new :bb-call :bb-static :bb-dynamic :bb-chain :bb-constructor :bb-arrow 
           :bb-list :bb-unit :bb-template :bb-assign :bb-return))

(defpackage :server
  (:use :cl :lol :utils :grammar :lang)
  (:export :rest-service :rest-static 
           :rest-dynamic% :rest-dynamic
           :rest-get% :rest-get
           :rest-post :rest-put :rest-delete))


(defpackage :data
  (:use :cl :lol :utils :parser :grammar :lang) 
  (:export :with-data :with-data%
           :remote
           :rand
           :jnull :jbool :jnumber :jstring :jobject :jarray
           :jsbool :jsnumber :jsstring :jsprop :jsobject :jsarray
           :filter :ident :prop :elem :comp
           :attribute :primary-key :foreign-key :entity :relationship
           :defent :defrel
           :*entities* :*relationships*))

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

(defpackage :nextent
  (:use :cl :lol :utils :doc :grammar :lang))

