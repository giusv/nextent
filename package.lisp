;;;; package.lisp (push #p"d:/giusv/lisp/nextent/" asdf:*central-registry*)

(defpackage :lol
  (:use :cl)
  (:export :pandoriclet :get-pandoric :this :dlambda :flatten :group :mkstr :symb :keyw))

(defpackage :parser
  (:use :cl :lol)
  (:export :tuple :result :apply-parser :parse :bind :fail :item :do-with :sat :sym :choose :zero :plus :choice 
           :many :many1 :sepby :sepby1 :sublist :pair :optional :atomic :var-init :req-var :opt-var :lambda-list :arg-names)) 


(defpackage :grammar
  (:use :cl :lol)
  (:export :defprim 
           :synth :synth-all :synth-plist :synth-plist-merge
           :rest-key :rest-plain))

(defpackage :doc
  (:use :cl :lol :grammar)
  (:export :empty :text :nest :vcat :hcat 
           :parens :brackets :braces :single-quotes :double-quotes :back-quotes
           :comma :dot :semi :colon :forward-slash :equals
           :punctuate :prepend :postpend
           :lower-camel :upper-camel
           :split-str :interleave
           :append*
           :write-file))

(defpackage :html
  (:use :cl :lol :grammar)
  (:export :tag 
           :taglist
           :span-color
           :html :head :title :meta :link :body :h1 :h2 :h3 :h4 :h5 :div :span :li :dl :dt :dd :ul :ol :pre :i 
           :strong :code :script
           :table :tr :th :td
           :section :article :aside :p :a
           :button :input :textarea))

;; (defpackage :expr
;;   (:use :cl :lol :grammar))


(defpackage :url
  (:use :cl :lol :parser :grammar)
  (:export :void :static-chunk :dynamic-chunk :expression-chunk :path-parameter :query-parameter :login-parameter 
           :backward-chain :multi :forward-chain :queried))


(defpackage :web
  (:use :cl :lol :grammar :doc)
  (:export :ng-const :ng-pair :ng-array :ng-primitive :ng-class :ng-method 
           :ng-import :ng-new :ng-call :ng-chain :ng-constructor :ng-arrow 
           :ng-list :ng-unit :ng-template))

(defpackage :gui
  (:use :cl :lol :grammar :web)
  (:export :input :button
           :vert :vert*
           :horz :horz*
           :abst
           :static :static%))

(defpackage :nextent
  (:use :cl :lol :doc :grammar ))

