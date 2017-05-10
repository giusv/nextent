;;;; nextent.lisp

(in-package :nextent)

(defun to-string (x)
  (synth :string (synth :doc (synth :typescript x))))

(defun process (name code) 
  (format t "~%~%~a~%--------------------------------------------------~%~%~a~%--------------------------------------------------~%" name (to-string code))
  (write-file name (to-string code)))

(defparameter place-format
  (data:jsobject 'place "aaa"
                 (data:jsprop 'name t (data:jsstring 'name "aaa"))))

(defparameter city-format
  (data:jsobject 'city "aaa"
                 (data:jsprop 'name t (data:jsstring 'name "aaa"))
                 (data:jsprop 'places t (data:jsarray 'places "aaa" place-format))))

(defparameter trip-format
  (data:jsobject 'trip "aaa"
                 (data:jsprop 'name t (data:jsstring 'name "aaa"))
                 (data:jsprop 'cities t (data:jsarray 'cities "aaa" city-format))))

(defparameter model-list (list place-format city-format trip-format))


(defparameter gui
  (gui:vert
   (gui:navbar 'nav 
               (gui:link 'home (expr:const "home") (url:void))
               (gui:link 'nested (expr:const "nested") (url:url `(nested)))
               (gui:link 'nested2 (expr:const "form") (url:url `(my-form)))
               (gui:link 'nested2 (expr:const "nested2") (url:url `(nested / nested2)))) 
   (gui:alt 
    (gui:vert 
     (gui:panel 'panel-test 
                (gui:label (expr:const "header2"))
                (gui:label (expr:const "body2")))
     (gui:button 'test (doc:text "level 0 1")) 
     (data:with-data ((places (data:remote 'places place-format 
                                           (url:url `(home)))))
       (gui:table 'table places (row)
         :|Name| (gui:label (expr:attr row 'name))
         :|Value| (gui:label (expr:attr row 'value)))))
    (gui:static 'nested nil 
                (gui:alt (gui:label (expr:const "level 1"))
                         (gui:static 'nested2 nil 
                                     (gui:vert (gui:label (expr:const "level 2"))
                                               (data:with-data ((places 
                                                                 (data:rand 'places (data:jsarray 'places "aaa" place-format))))
                                                 (gui:table 'table places (row)
                                                   :|Name| (gui:label (expr:attr row 'name))
                                                   :|Value| (gui:label (expr:attr row 'value))
                                                   :|Description| (gui:description 'description row 
                                                                    :|Name| (expr:attr row 'name)
                                                                    :|Value| (expr:attr row 'value))
                                                   :|Details| (gui:button 'details (doc:text "Details"))
                                                   :|Panel| (gui:panel 'panel (gui:label (expr:attr row 'name)) (gui:label (expr:attr row 'value)))))))))
    (gui:dynamic 'dyn id 
                 (gui:label (expr:value id)))
    (gui:static 'my-form nil
                (gui:form 'trip-form trip-format
                          ((name name (gui:input 'name (expr:const "Trip name")))
                           (cities cities (gui:arr 'cities city-format 
                                                   ((city-name city-name (gui:input 'city-name (expr:const "City name"))) 
                                                    (places places (gui:arr 'places place-format
                                                                            ((place-name place-name (gui:input 'place-name (expr:const "Place name"))))
                                                                            place-name)))
                                                   (gui:vert city-name places))))
                          (gui:vert name cities))))))



(let* ((basedir "d:/giusv/angular/template/src/app/")
       (app-models (mapcar (lambda (format) (synth :model format)) 
                           model-list))
       (app-components (synth :components gui nil))
       (app-component-names (cons (ng-static 'app-component)
                                  (mapcar (lambda (component)
                                            (ng-static (symb (synth :name component) "-COMPONENT")))
                                          app-components)))
       (app (ng-unit 'app
                     (ng-import "@angular/core" 'component)
                     (ng-import "@angular/forms" 'form-array 'form-builder 'form-group 'form-control)
                     (ng-primitive 'component
                                   :selector (ng-const (string-downcase 'app))
                                   :template (ng-template (synth :template gui)))
                     (ng-class 'app-component
                               :fields (list (synth :controller gui))))) 
       (app-module (ng-unit 'app
                            (ng-import "@angular/core" 'ng-module)
                            (ng-import "@angular/platform-browser" 'browser-module)
                            (ng-import "@angular/http" 'http-module)
                            (ng-import "@angular/forms" 'reactive-forms-module)
                            (ng-import "@angular/router" 'router-module 'routes)
                            (ng-import "./app.component" 'app-component) ;; FIXME
                            (mapcar (lambda (component)
                                      (ng-import (mkstr "./" (string-downcase (synth :name component)) ".component") 
                                                 (symb (synth :name component) "-COMPONENT")))
                                    app-components)
                            (ng-pair 'app-routes (ng-type 'routes) :const t 
                                     :init (ng-array (synth :routes gui nil)))
                            (ng-primitive 'ng-module
                                          :imports (ng-array (ng-static 'browser-module)
                                                             (ng-static 'http-module)
                                                             (ng-static 'reactive-forms-module)
                                                             (ng-chain (ng-static 'router-module) 
                                                                       (ng-call 'for-root (ng-dynamic 'app-routes))))
                                          :declarations (ng-array app-component-names) 
                                          :bootstrap (ng-array (ng-static 'app-component)))
                            (ng-class 'app-module)))
       (app-components (synth :components gui nil))) 
  (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
  (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
  (mapcar (lambda (component) 
            (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
          app-components)
  (mapcar (lambda (model) 
            (process (mkstr basedir (string-downcase (synth :name model)) ".ts") model))
          app-models))



;; ;; (pprint (synth :pretty (synth :random (data:jsarray 'places "aaa" place-format))))
;; ;; (pprint (synth :pretty (synth :model (synth :random (data:jsarray 'places "aaa" place-format)))))
;; ;; (defparameter gui 
;; ;;   (gui:form 'hero-form nil 
;; ;;             (gui:arr 'secrets nil 
;; ;;                      (gui:obj 'secret nil 
;; ;;                               ((secret 
;; ;;                                 secret 
;; ;;                                 (gui:input 'secret (expr:const "Secret Lair")))
;; ;;                                ;; (accomplice 
;; ;;                                ;;  accomplice
;; ;;                                ;;  (gui:obj 'accomplice nil 
;; ;;                                ;;           ((name name (gui:input 'name (expr:const "name"))))
;; ;;                                ;;           name))
;; ;;                                ;; (accomplices 
;; ;;                                ;;  accomplices 
;; ;;                                ;;  (gui:arr 'accomplices nil 
;; ;;                                ;;           (gui:obj 'accomplice nil 
;; ;;                                ;;                    ((name name (gui:input 'name (expr:const "name"))))
;; ;;                                ;;                    name)))
;; ;;                                )
;; ;;                               (gui:vert secret ;; accomplices
;; ;;                                         )))
;; ;;             ;; (gui:obj 'comp-data nil 
;; ;;             ;;                      ((name name (gui:input 'name (expr:const "Name")))
;; ;;             ;;                       (address address (gui:input 'address (expr:const "Address")))
;; ;;             ;;                       (secrets secrets 
;; ;;             ;;                                (gui:arr 'secrets nil 
;; ;;             ;;                                         (gui:obj 'secret nil 
;; ;;             ;;                                                  ((secret 
;; ;;             ;;                                                    secret 
;; ;;             ;;                                                    (gui:input 'secret (expr:const "Secret Lair")))
;; ;;             ;;                                                   ;; (accomplice 
;; ;;             ;;                                                   ;;  accomplice
;; ;;             ;;                                                   ;;  (gui:obj 'accomplice nil 
;; ;;             ;;                                                   ;;           ((name name (gui:input 'name (expr:const "name"))))
;; ;;             ;;                                                   ;;           name))
;; ;;             ;;                                                   ;; (accomplices 
;; ;;             ;;                                                   ;;  accomplices 
;; ;;             ;;                                                   ;;  (gui:arr 'accomplices nil 
;; ;;             ;;                                                   ;;           (gui:obj 'accomplice nil 
;; ;;             ;;                                                   ;;                    ((name name (gui:input 'name (expr:const "name"))))
;; ;;             ;;                                                   ;;                    name)))
;; ;;             ;;                                                   )
;; ;;             ;;                                                  (gui:vert secret ;; accomplices
;; ;;             ;;                                                            )))))
;; ;;             ;;                      (gui:vert name address secrets))
;; ;;             ))
;; (defparameter secret-format
;;   (data:jsobject 'secret "aa"
;;                  (data:jsprop 'secret t (data:jsstring 'secret "aaa"))))
;; (defparameter hero-format 
;;   (data:jsobject 'hero "aaa"
;;                  (data:jsprop 'name t (data:jsstring 'name "aaa"))
;;                  (data:jsprop 'address t (data:jsstring 'address "aaa"))
;;                  (data:jsprop 'addresses t (data:jsarray 'addresses "aaa" secret-format))))
;; (defparameter role-format
;;   (data:jsobject 'ruolo "ddd"
;;                  (data:jsprop 'nome t (data:jsstring 'ruolo "Ruolo assunto nel sinistro"))))

;; (defparameter person-format 
;;   (data:jsobject 'persona "Formato JSON dei dati relativi a una persona"
;;                  (data:jsprop 'id-persona nil (data:jsstring 'id-persona "Identificativo univoco della persona")) 
;;                  (data:jsprop 'nome t (data:jsstring 'nome "Nome")) 
;;                  (data:jsprop 'cognome t (data:jsstring 'cognome "Cognome")) 
;;                  (data:jsprop 'codice-fiscale nil (data:jsstring 'codice-fiscale "Codice fiscale")) 
;;                  (data:jsprop 'partita-iva nil (data:jsstring 'partita-iva "Partita IVA")) 
;;                  (data:jsprop 'luogo-nascita t (data:jsstring 'luogo-nascita "Luogo di nascita"))
;;                  (data:jsprop 'data-nascita t (data:jsstring 'data-nascita "Data di nascita"))  
;;                  (data:jsprop 'ruoli nil (data:jsarray 'ruoli "Lista di ruoli assunti nel sinistro" role-format))))

;; (let ((json ;; (synth :random (data:jsarray 'test "aaa")))
;;        (synth :random schema)))
;;   (pprint (synth :pretty json))
;;   (format t "~%~a" (synth :string (synth :string json))))







;; (synth :output (synth :typescript (ng-unit (ng-import (ng-const "@angular/core") 'component 'onInit)
;;                                            (ng-primitive 'component 
;;                                                          :selector (ng-const "my-heroes") 
;;                                                          :template-url  (ng-const "test") 
;;                                                          :style-urls (ng-array (ng-const "test")))
;;                                            (ng-class 'hero-search 
;;                                                      :fields (list (ng-pair 'heroes0 'string :init (ng-new 'heroes))
;;                                                                    (ng-pair 'heroes 'string :init (ng-array (ng-const "aaa")))
;;                                                                    (ng-pair 'heroes2 'string :init (ng-call 'get (ng-const "aaa")))
;;                                                                    (ng-pair 'heroes3 'string :init (ng-chain (ng-call 'get (ng-const "aaa"))
;;                                                                                                              (ng-call 'set (ng-const "aaa"))
;;                                                                                                              (ng-call 'set (ng-const "bbb"))))
;;                                                                    (ng-pair 'heroes4 'string :init (ng-call 'catch (ng-arrow (list (ng-pair 'e 'error)) (ng-call 'test (ng-const 'e)))) :const t))
;;                                                      :constructor (ng-constructor (list (ng-pair 'heroes 'string)))
;;                                                      :methods (list (ng-method (text "on-init") 
;;                                                                                (list (ng-pair 'heroes 'string))
;;                                                                                'void))))) 0)

 ;; (synth :output (nest 10 (ng-const "~a" 24)) 0)
;; (pprint (synth :output (synth :doc (html:div :class "a" (doc:ng-const "SS"))) 0))
;; (pprint (synth :pretty (html:div :class "a" (ng-const "ss"))))
;; (synth :output (synth :doc (synth :template (gui:input 'name (ng-const "Name") :init (ng-const "hello")))) 0)

;; (pprint (synth :pretty (ng-unit
;;                         (ng-import (ng-const "@angular/core") 'component 'onInit)
;;                         (ng-primitive 'component 
;;                                       :selector (ng-const "my-heroes") 
;;                                       :template-url  (ng-const "test") 
;;                                       :style-urls (ng-array (ng-const "test")))
;;                         (ng-class 'hero-search 
;;                                   :fields (list (ng-pair 'heroes0 'string :init (ng-new 'heroes))
;;                                                 (ng-pair 'heroes 'string :init (ng-array (ng-const "aaa")))
;;                                                 (ng-pair 'heroes2 'string :init (ng-call 'get (ng-const "aaa")))
;;                                                 (ng-pair 'heroes3 'string :init (ng-chain (ng-call 'get (ng-const "aaa"))
;;                                                                                           (ng-call 'set (ng-const "aaa"))
;;                                                                                           (ng-call 'set (ng-const "bbb"))))
;;                                                 (ng-pair 'heroes4 'string :init (ng-call 'catch (ng-arrow (list (ng-pair 'e 'error)) (ng-call 'test (ng-const 'e)))) :const t))
;;                                   :constructor (ng-constructor (list (ng-pair 'heroes 'string)))
;;                                   :methods (list (ng-method 'on-init 
;;                                                             (list (ng-pair 'heroes 'string))
;;                                                             'void))))))

;; (synth :typescript (ng-pair 'e 'error))
;; (synth :typescript (ng-unit
;;                     (ng-import (text "@angular/core") 'component 'onInit)
;;                     ;; (ng-primitive 'component 
;;                     ;;               :selector (text "my-heroes") 
;;                     ;;               :template-url  (text "test") 
;;                     ;;               :style-urls (ng-array (text "test")))
;;                     ;; (ng-class 'hero-search 
;;                     ;;           :fields (list (ng-pair 'heroes0 'string :init (ng-new 'heroes))
;;                     ;;                         (ng-pair 'heroes 'string :init (ng-array (text "aaa")))
;;                     ;;                         (ng-pair 'heroes2 'string :init (ng-call 'get (text "aaa")))
;;                     ;;                         (ng-pair 'heroes3 'string :init (ng-chain (ng-call 'get (text "aaa"))
;;                     ;;                                                                   (ng-call 'set (text "aaa"))
;;                     ;;                                                                   (ng-call 'set (text "bbb"))))
;;                     ;;                         (ng-pair 'heroes4 'string :init (ng-call 'catch (ng-arrow (list (ng-pair 'e 'error)) (ng-call 'test (text 'e)))) :const t))
;;                     ;;           :constructor (ng-constructor (list (ng-pair 'heroes 'string)))
;;                     ;;           :methods (list (ng-method 'on-init 
;;                     ;;                                     (list (ng-pair 'heroes 'string))
;;                     ;;                                     'void)))
;;                     ))

;; (pprint (synth :pretty (div)))

;; (synth :output (html:div :class "a" (text "aaa")))
;; (pprint (parse (many (atomic)) '(a b c &optional d)))


;; (let ((l '(a b c &optional d1 (d2 0 d2-supplied-p) &rest e &key f (g 99 g-supplied-p))))
;;   ;; (pprint (parse (lambda-list) l))
;;   (pprint (arg-names l))
;;   ;; (let ((args (parse (lambda-list) l)))
;;   ;;   (pprint (apply #'append (mapcar (lambda (x) (getf args x))  (list :req :opt :rest :key))))
;;   ;;   )
;;   )
;; (pprint (parse (ttt) (list 'a 'b)))
;; (pprint (parse (ttt) '(a b)))
;; (pprint (parse (var-init) '((a b))))


;; (pprint (parse (var-init) '((name init))))
;;; "nextent" goes here. Hacks and glory await!

