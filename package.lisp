;;;; package.lisp

(defpackage :lol
  (:use :cl)
  (:export :pandoriclet
           :this
           :dlambda
           :flatten))

(defpackage :parser
  (:use :cl :lol)
  (:export :arg-names))

(defpackage :grammar
  (:use :cl :lol)
  (:export :defprim 
           :synth
           :synth-all
           :rest-key
           :rest-plain))

(defpackage :doc
  (:use :cl :lol :grammar)
  (:export :empty
           :text
           :nest
           :vcat
           :hcat))

(defpackage :html
  (:use :cl :lol :grammar)
  (:export :taglist
           :div
           :input))

;; (defpackage :expr
;;   (:use :cl :lol :grammar))


(defpackage :gui
  (:use :cl :lol :grammar)
  (:export :input))

(defpackage :web
  (:use :cl :lol :grammar)
  (:export :pair))

(defpackage :nextent
  (:use :cl :lol :grammar :doc :html))

