;;;; package.lisp (push #p"d:/giusv/lisp/nextent/" asdf:*central-registry*)

(defpackage :lol
  (:use :cl)
  (:export :pandoriclet
           :this
           :dlambda
           :flatten
           :group
           :symb
           :keyw))

(defpackage :parser
  (:use :cl :lol)
  (:export :arg-names))

(defpackage :grammar
  (:use :cl :lol)
  (:export :defprim 
           :synth :synth-all :synth-plist :synth-plist-merge
           :rest-key :rest-plain))

(defpackage :doc
  (:use :cl :lol :grammar)
  (:export :empty :text :nest :vcat :hcat 
           :parens :brackets :braces :single-quotes :double-quotes
           :comma :dot :semi :colon :forward-slash :equals
           :punctuate :prepend :postpend
           :lower-camel :upper-camel
           :split-str :interleave))

(defpackage :html
  (:use :cl :lol :grammar)
  (:export :taglist
           :div
           :input
           :button
           :li
           :ul))

;; (defpackage :expr
;;   (:use :cl :lol :grammar))


(defpackage :gui
  (:use :cl :lol :grammar)
  (:export :input :button
           :vert))

(defpackage :web
  (:use :cl :lol :grammar :doc)
  (:export :ng-const :ng-pair :ng-array :ng-primitive :ng-class :ng-method 
           :ng-import :ng-new :ng-call :ng-chain :ng-constructor :ng-arrow 
           :ng-unit))

(defpackage :nextent
  (:use :cl :lol :grammar :doc :html :web))

