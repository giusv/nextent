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
           :rest-key :rest-plain))

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
  (:export :empty :text :nest :vcat :hcat 
           :parens :brackets :braces :single-quotes :double-quotes :back-quotes
           :comma :dot :semi :colon :forward-slash :equals
           :punctuate :prepend :postpend
           :lower-camel :upper-camel
           :split-str :interleave
           :append*
           :write-file))

(defpackage :html
  (:use :cl :lol :utils :grammar)
  (:export :tag 
           :taglist
           :span-color
           :html :head :title :meta :link :body :h1 :h2 :h3 :h4 :h5 :div :span :li :dl :dt :dd :ul :ol :pre :i 
           :strong :code :script
           :table :tr :th :td
           :section :article :aside :p :a
           :button :input :textarea
           :label
           :form))

(defpackage :expr
  (:use :cl :lol :utils :grammar)
  (:export :const 
           :attr))


(defpackage :url
  (:use :cl :lol :utils :parser :grammar :doc)
  (:export :void :static-chunk :dynamic-chunk :expression-chunk :path-parameter :query-parameter :login-parameter 
           :backward-chain :multi :forward-chain :queried
           :url))

(defpackage :web
  (:use :cl :lol :utils :grammar :doc)
  (:export :ng-empty :ng-const :ng-pair :ng-array :ng-object :ng-primitive :ng-class :ng-method 
           :ng-import :ng-new :ng-call :ng-static :ng-dynamic :ng-chain :ng-constructor :ng-arrow 
           :ng-list :ng-unit :ng-template :ng-assign))


(defpackage :data
  (:use :cl :lol :utils :parser :grammar :web) 
  (:export :with-data :with-data%
           :remote
           :rand
           :jnull :jbool :jnumber :jstring :jobject :jarray
           :jsbool :jsnumber :jsstring :jsprop :jsobject :jsarray
           :filter :ident :prop :elem :comp))

(defpackage :gui
  (:use :cl :lol :utils :grammar :web)
 (:export :input :button
           :vert :vert*
           :horz :horz*
           :abst
           :static :static%
           :label
           :listing :listing%
           :alt
           :form :bnd :obj% :obj :arr))

(defpackage :nextent
  (:use :cl :lol :utils :doc :grammar :web))

