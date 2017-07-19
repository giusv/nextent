;;;; nextent.asd

(asdf:defsystem :nextent
  :serial t
  :description "Describe nextent here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "lol")
               (:file "utils")
               (:file "parser")
               (:file "grammar")
               (:file "doc")
               ;; (:file "html")
               (:module "lang"
                        :serial t
                        :components ((:file "blub")
                                     (:file "c")
                                     (:file "java")))
               ;; (:file "expression")
               ;; (:file "validator") 
               ;; (:file "url")
               ;; (:file "query") 
               ;; (:module "data"
               ;;          :serial t
               ;;          :components ((:file "data")
               ;;                       (:file "json")
               ;;                       (:file "jsonschema")
               ;;                       (:file "entity")))
               
               ;; (:module "gui"
               ;;          :serial t
               ;;          :components ((:file "input")
               ;;                       (:file "button")
               ;;                       (:file "link")
               ;;                       (:file "navbar")
               ;;                       (:file "vert")
               ;;                       (:file "horz")
               ;;                       (:file "label")
               ;;                       (:file "panel")
               ;;                       (:file "listing")
               ;;                       (:file "table")
               ;;                       (:file "description")
               ;;                       (:file "abst")
               ;;                       (:file "static")
               ;;                       (:file "dynamic")
               ;;                       (:file "alt")
               ;;                       (:file "form")))
              
               ;; (:module "server"
               ;;          :serial t
               ;;          :components ((:file "rest")
               ;;                       (:file "actions")))
               ;; (:module "ml"
               ;;          :serial t
               ;;          :components ((:file "node")))
               
               ;; (:module "indy"
               ;;          :serial t
               ;;          :components ((:file "indy")
               ;;                       (:file "indyparser")))
               ;; (:module "pgen"
               ;;          :serial t
               ;;          :components ((:file "pgen")))
               (:module "lgen"
                        :serial t
                        :components ((:file "lgen")))
               ;; (:file "ml")
               ;; (:file "nextent")
               ))
