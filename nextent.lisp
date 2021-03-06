;;;; nextent.lisp

(in-package :nextent)

(data:deformat parameter-format
    (data:jsobject 'parameter "Formato DATA:JSON del valore di un parametro relativo a un indicatore"
              (data:jsprop 'parameter-id nil (data:jsstring 'id-parameter "Identificativo univoco del parametro"))
              (data:jsprop 'name t (data:jsstring 'name "Name"))
              (data:jsprop 'value t (data:jsstring 'value "Valore"))))

(data:deformat indicator-format 
    (data:jsobject 'indicator "Formato DATA:JSON di un indicatore dinamico"
              (data:jsprop 'name t (data:jsstring 'name "Nome indicatore"))
              (data:jsprop 'source-code t (data:jsstring 'code "Codice sorgente"))
              (data:jsprop 'start-date t (data:jsstring 'start-date "Data inizio validit&agrave;"))
              (data:jsprop 'parameters t (data::jsarray 'name "aaa" parameter-format))))

(data:defent indicator-entity
    (data:entity 'indicator 
                 :primary (data:attribute 'indicator-id (data:atype :integer))
                 :fields (list (data:attribute 'name (data:atype :string :size 20) "Nome dell'indicatore")
                               (data:attribute 'source-code (data:atype :string :size 200)
                                               "Codice sorgente scritto dall'utente")
                               (data:attribute 'object-code (data:atype :string :size 200) 
                                               "Codice oggetto prodotto dal compilatore")
                               (data:attribute 'start-date (data:atype :string :size 8)
                                               "Data inizio validita"))))


(data:defent parameter-entity
    (data:entity 'parameter
                 :primary (data:attribute 'parameter-id (data:atype :integer))
                 :fields (list (data:attribute 'name (data:atype :string :size 20) 
                                               "Name del parametro")
                               (data:attribute 'value (data:atype :string :size 20) 
                                               "Valore del parametro"))))

(data:defrel indicator-parameters
    (data:relationship 'parameters-indicator indicator-entity parameter-entity :one-to-many))


(server:defresource indicator-item
    (server:rest-item 'indicator ((indicator (url:path-parameter 'indicator :integer))) 
                      (list 
                       (server:rest-get () 
                                        (server:concat
                                         (inst (server:find-entity indicator-entity indicator))
                                         (ret (server:with-fields ((name name)
                                                                   (code source-code)
                                                                   (start-date start-date)
                                                                   (parameters parameter-list)) inst
                                                (server:create-transfer indicator-format 
                                                                        :name name
                                                                        :source-code code
                                                                        :start-date start-date
                                                                        :parameters (server:mapcomm 
                                                                                    (server:mu parameter
                                                                                               (server:with-fields ((parameter-name name) (parameter-value value)) parameter
                                                                                                 (server:create-transfer parameter-format 
                                                                                                  :name parameter-name
                                                                                                  :value parameter-value)))
                                                                                    parameters)))) 
                                         ((server:respond :ok ret))))
                       (server:rest-put indicator-format 
                                        (server:concat
                                         ((server:with-fields ((name name)
                                                               (code source-code)
                                                               (start-date start-date)
                                                               (parameters parameters)) indicator-format
                                            (server:update-entity indicator-entity indicator
                                                                  :name name
                                                                  :source-code code
                                                                  :start-date start-date
                                                                  :parameter-list (server:mapcomm 
                                                                              (server:mu parameter
                                                                                         (server:with-fields ((parameter-name name) (parameter-value value)) parameter
                                                                                           (server:create-entity 
                                                                                            parameter-entity
                                                                                            :name parameter-name
                                                                                            :value parameter-value)))
                                                                              parameters))))
                                         ((server:respond :no-content)))))
                      ;; parameters-collection
                      ))
(data:defquery indicator-by-name (name) indicator-entity
  (data:with-queries ((inds (data:relation 'indicators))
                      (pars (data:relation 'parameters)))
    (data:project (data:restrict (data:product inds pars)
                                 (expr:+and+ 
                                  (expr:+equal+ (expr:attr inds 'id)
                                                (expr:attr pars 'id))
                                  (expr:+equal+ (expr:attr inds 'name)
                                                name))))))
(data:defquery all-indicators () indicator-entity
  (data:with-queries ((inds (data:relation 'indicators)))
    (data:project inds)))

(server:defresource indicators-collection
    (server:rest-collection 
     'indicators
     (list 
      (server:rest-get 
       () 
       (server:concat
        (indicator-list (server:exec-query (all-indicators)))
        (ret (server:mapcomm 
              (server:mu indicator
                         (server:with-fields ((name name)
                                              (code source-code)
                                              (start-date start-date)
                                              (parameters parameter-list)) indicator
                           (server:create-transfer indicator-format 
                                                   :name name
                                                   :source-code code
                                                   :start-date start-date
                                                   :parameters (server:mapcomm 
                                                                (server:mu parameter
                                                                           (server:with-fields ((parameter-name name) (parameter-value value)) parameter
                                                                             (server:create-transfer parameter-format 
                                                                                                     :name parameter-name
                                                                                                     :value parameter-value)))
                                                                parameters))))
              indicator-list))
        ((server:respond :ok ret))))
      (server:rest-post% indicator-format 
                         (server:concat
                          (ret (server:with-fields ((name name)
                                                    (code source-code)
                                                    (start-date start-date)) indicator-format
                                 (server:create-entity indicator-entity
                                                       :name name
                                                       :source-code code
                                                       :start-date start-date))) 
                          ((server:respond :created)))))
     indicator-item))

;; (defparameter parameters-collection
;;   (server:rest-collection 'parameters
;;                          (list 
;;                           (server:rest-put (data:jsarray 'parameter-array "aaa" parameter-format)
;;                                            (server:concat
;;                                             (server:mapcomm 
;;                                              (server:mu param
;;                                                         (server:with-fields ((name name)
;;                                                                              (value value)) param
;;                                                           (server:create-entity 
;;                                                                 parameter-entity
;;                                                                 :name name
;;                                                                 :value value)))
;;                                              trip-list)
;;                                             (ret (server:with-fields ((name name)
;;                                                                       (code source-code)
;;                                                                       (start-date start-date)) inst
;;                                                    (server:update-entity indicator-format 
;;                                                                          :name name
;;                                                                          :code code
;;                                                                          :start-date start-date))) 
;;                                             ((server:respond :no-content)))))
;;                          parameter-item))


(server:defservice server (server:rest-service 'indicator-service (url:void) indicators-collection))

(let* ((package '|it.bancaditalia.nextent|)
       ;; (basedir "D:/Dati/Profili/m026980/workspace/nextent/src/main/java/it/bancaditalia/nextent/")
       (basedir "D:/giusv/temp/nextent/")
       (app-entities (loop for value being the hash-values of data:*entities* collect value))
       (app-formats (loop for value being the hash-values of data:*formats* collect value))
       (app-services (loop for value being the hash-values of server:*services* collect value))) 
  ;; (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
  ;; (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
  ;; (mapcar (lambda (component) 
  ;;           (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
  ;;         app-components)
  (mapcar (lambda (entity) 
            (let ((filename (mkstr basedir "model/" (upper-camel (synth :name entity)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :entity entity package)))))))
          app-entities) 
  (mapcar (lambda (format) 
            (let ((filename (mkstr basedir "jto/" (upper-camel (symb (synth :name format) '|-J-T-O|)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :jto format package)))))))
          app-formats) 
  (mapcar (lambda (service) 
            (let ((filename (mkstr basedir "service/" (upper-camel (synth :name service)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :jax-class service package)))))))
          app-services)
  (mapcar (lambda (service) 
            (let ((filename (mkstr basedir "ejb/" (upper-camel (symb (synth :name service) '|-Bean|)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :bean-class service package)))))))
          app-services))



;; (defun to-string (x)
;;   (synth :string (synth :doc (synth :typescript x))))

;; (defun process (name code) 
;;   ;; (format t "~%~%~a~%--------------------------------------------------~%~%~a~%--------------------------------------------------~%" name (to-string code))
;;   (write-file name (to-string code)))


;; (pprint (synth-all :pretty (synth :source (car (data::get-sources trip-entity)))))
;; (pprint (synth-all :pretty (data::get-sources trip-entity)))

;; (pprint (synth :pretty trip-entity))
;; (pprint trip-entity)

;; (synth-all :output (synth-all :java (synth-all :entity 
;;                                                (loop for value being the hash-values of data:*entities* collect value))) 0)

;; (synth-all :output (synth-all :java (synth-all :eao-interface 
;;                                                (loop for value being the hash-values of data:*entities* collect value))) 0)
;; (synth :output (synth :java (synth :jax-class server)) 0)

;; (synth :output (synth :java (synth :bean-class server)) 0)
;; (synth :output  (apply #'doc:postpend (doc:semi) t
                           ;; (remove nil (synth-all :ddl (list trip-entity city-entity place-entity trip-city city-place)))) 0)

;; (synth-all :output (synth-all :java (synth-all :model (list trip-format city-format place-format) :server '|com.example.json|)) 0)

;; (defparameter place-format
;;   (data:jsobject 'place "aaa"
;;                  (data:jsprop 'name t (data:jsstring 'name "aaa"))))

;; (defparameter city-format
;;   (data:jsobject 'city "aaa"
;;                  (data:jsprop 'name t (data:jsstring 'name "aaa"))
;;                  (data:jsprop 'places t (data:jsarray 'places "aaa" place-format))))

;; (defparameter trip-format
;;   (data:jsobject 'trip "aaa"
;;                  (data:jsprop 'name t (data:jsstring 'name "aaa"))
;;                  (data:jsprop 'cities t (data:jsarray 'cities "aaa" city-format))))

;; (defparameter model-list (list place-format city-format trip-format))


;; (defparameter gui
;;   (gui:vert
;;    (gui:navbar 'nav 
;;                (gui:link 'home (expr:const "home") (url:void))
;;                (gui:link 'nested (expr:const "nested") (url:url `(nested)))
;;                (gui:link 'nested2 (expr:const "form") (url:url `(my-form)))
;;                (gui:link 'dynamic (expr:const "dynamic") (url:url `(nested / param)))) 
;;    (gui:alt 
;;     (gui:vert 
;;      (gui:panel 'panel-test 
;;                 (gui:label (expr:const "header2"))
;;                 (gui:label (expr:const "body2")))
;;      (gui:button 'test (doc:text "level 0 1")) 
;;      (data:with-data ((places (data:remote 'places place-format 
;;                                            (url:url `(home)))))
;;        (gui:table 'table places (row)
;;          :|Name| (gui:label (expr:attr row 'name))
;;          :|Value| (gui:label (expr:attr row 'value)))))
;;     (gui:static 'nested nil 
;;                 (gui:alt (gui:label (expr:const "nested"))
;;                          (gui:dynamic 'dyn (id) 
;;                                       (gui:label (expr:value id)))))
;;     (gui:static 'nested2 nil 
;;                 (gui:vert (gui:label (expr:const "nested 2"))
;;                           (data:with-data ((places 
;;                                             (data:rand 'places (data:jsarray 'places "aaa" place-format))))
;;                             (gui:table 'table places (row)
;;                               :|Name| (gui:label (expr:attr row 'name))
;;                               :|Value| (gui:label (expr:attr row 'value))
;;                               :|Description| (gui:description 'description row 
;;                                                :|Name| (expr:attr row 'name)
;;                                                :|Value| (expr:attr row 'value))
;;                               :|Details| (gui:button 'details (doc:text "Details"))
;;                               :|Panel| (gui:panel 'panel (gui:label (expr:attr row 'name)) 
;;                                                   (gui:label (expr:attr row 'value)))))))    
;;     (gui:static 'my-form nil
;;                 (gui:form 'trip-form trip-format
;;                           ((name name (gui:input 'name (expr:const "Trip name")))
;;                            (cities cities (gui:arr 'cities city-format 
;;                                                    ((city-name city-name (gui:input 'city-name (expr:const "City name"))) 
;;                                                     (places places (gui:arr 'places place-format
;;                                                                             ((place-name place-name (gui:input 'place-name (expr:const "Place name"))))
;;                                                                             place-name)))
;;                                                    (gui:vert city-name places))))
;;                           (gui:vert name cities))))))



;; (let* ((basedir "d:/giusv/angular/template/src/app/")
;;        (app-models (mapcar (lambda (format) (synth :model format)) 
;;                            model-list))
;;        (app-components (synth :components gui nil))
;;        (app-component-names (cons (bb-static 'app-component)
;;                                   (mapcar (lambda (component)
;;                                             (bb-static (symb (synth :name component) "-COMPONENT")))
;;                                           app-components)))
;;        (app (bb-unit 'app
;;                      (bb-import "@angular/core" 'component)
;;                      (bb-import "@angular/forms" 'form-array 'form-builder 'form-group 'form-control)
;;                      (bb-annotation 'component
;;                                    :selector (bb-const (string-downcase 'app))
;;                                    :template (bb-template (synth :template gui)))
;;                      (bb-class 'app-component
;;                                :fields (list (synth :controller gui))))) 
;;        (app-module (bb-unit 'app
;;                             (bb-import "@angular/core" 'bb-module)
;;                             (bb-import "@angular/platform-browser" 'browser-module)
;;                             (bb-import "@angular/http" 'http-module)
;;                             (bb-import "@angular/forms" 'reactive-forms-module)
;;                             (bb-import "@angular/router" 'router-module 'routes)
;;                             (bb-import "./app.component" 'app-component) ;; FIXME
;;                             (mapcar (lambda (component)
;;                                       (bb-import (mkstr "./" (string-downcase (synth :name component)) ".component") 
;;                                                  (symb (synth :name component) "-COMPONENT")))
;;                                     app-components)
;;                             (bb-pair 'app-routes (bb-type 'routes) :const t 
;;                                      :init (bb-array (synth :routes gui nil)))
;;                             (bb-annotation 'bb-module
;;                                           :imports (bb-array (bb-static 'browser-module)
;;                                                              (bb-static 'http-module)
;;                                                              (bb-static 'reactive-forms-module)
;;                                                              (bb-chain (bb-static 'router-module) 
;;                                                                        (bb-call 'for-root (bb-dynamic 'app-routes))))
;;                                           :declarations (bb-array app-component-names) 
;;                                           :bootstrap (bb-array (bb-static 'app-component)))
;;                             (bb-class 'app-module)))
;;        (app-components (synth :components gui nil))) 
;;   (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
;;   (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
;;   (mapcar (lambda (component) 
;;             (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
;;           app-components)
;;   (mapcar (lambda (model) 
;;             (process (mkstr basedir (string-downcase (synth :name model)) ".ts") model))
;;           app-models))



;; ;; ;; (pprint (synth :pretty (synth :random (data:jsarray 'places "aaa" place-format))))
;; ;; ;; (pprint (synth :pretty (synth :model (synth :random (data:jsarray 'places "aaa" place-format)))))
;; ;; ;; (defparameter gui 
;; ;; ;;   (gui:form 'hero-form nil 
;; ;; ;;             (gui:arr 'secrets nil 
;; ;; ;;                      (gui:obj 'secret nil 
;; ;; ;;                               ((secret 
;; ;; ;;                                 secret 
;; ;; ;;                                 (gui:input 'secret (expr:const "Secret Lair")))
;; ;; ;;                                ;; (accomplice 
;; ;; ;;                                ;;  accomplice
;; ;; ;;                                ;;  (gui:obj 'accomplice nil 
;; ;; ;;                                ;;           ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;                                ;;           name))
;; ;; ;;                                ;; (accomplices 
;; ;; ;;                                ;;  accomplices 
;; ;; ;;                                ;;  (gui:arr 'accomplices nil 
;; ;; ;;                                ;;           (gui:obj 'accomplice nil 
;; ;; ;;                                ;;                    ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;                                ;;                    name)))
;; ;; ;;                                )
;; ;; ;;                               (gui:vert secret ;; accomplices
;; ;; ;;                                         )))
;; ;; ;;             ;; (gui:obj 'comp-data nil 
;; ;; ;;             ;;                      ((name name (gui:input 'name (expr:const "Name")))
;; ;; ;;             ;;                       (address address (gui:input 'address (expr:const "Address")))
;; ;; ;;             ;;                       (secrets secrets 
;; ;; ;;             ;;                                (gui:arr 'secrets nil 
;; ;; ;;             ;;                                         (gui:obj 'secret nil 
;; ;; ;;             ;;                                                  ((secret 
;; ;; ;;             ;;                                                    secret 
;; ;; ;;             ;;                                                    (gui:input 'secret (expr:const "Secret Lair")))
;; ;; ;;             ;;                                                   ;; (accomplice 
;; ;; ;;             ;;                                                   ;;  accomplice
;; ;; ;;             ;;                                                   ;;  (gui:obj 'accomplice nil 
;; ;; ;;             ;;                                                   ;;           ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;             ;;                                                   ;;           name))
;; ;; ;;             ;;                                                   ;; (accomplices 
;; ;; ;;             ;;                                                   ;;  accomplices 
;; ;; ;;             ;;                                                   ;;  (gui:arr 'accomplices nil 
;; ;; ;;             ;;                                                   ;;           (gui:obj 'accomplice nil 
;; ;; ;;             ;;                                                   ;;                    ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;             ;;                                                   ;;                    name)))
;; ;; ;;             ;;                                                   )
;; ;; ;;             ;;                                                  (gui:vert secret ;; accomplices
;; ;; ;;             ;;                                                            )))))
;; ;; ;;             ;;                      (gui:vert name address secrets))
;; ;; ;;             ))
;; ;; (defparameter secret-format
;; ;;   (data:jsobject 'secret "aa"
;; ;;                  (data:jsprop 'secret t (data:jsstring 'secret "aaa"))))
;; ;; (defparameter hero-format 
;; ;;   (data:jsobject 'hero "aaa"
;; ;;                  (data:jsprop 'name t (data:jsstring 'name "aaa"))
;; ;;                  (data:jsprop 'address t (data:jsstring 'address "aaa"))
;; ;;                  (data:jsprop 'addresses t (data:jsarray 'addresses "aaa" secret-format))))
;; ;; (defparameter role-format
;; ;;   (data:jsobject 'ruolo "ddd"
;; ;;                  (data:jsprop 'nome t (data:jsstring 'ruolo "Ruolo assunto nel sinistro"))))

;; ;; (defparameter person-format 
;; ;;   (data:jsobject 'persona "Formato JSON dei dati relativi a una persona"
;; ;;                  (data:jsprop 'id-persona nil (data:jsstring 'id-persona "Identificativo univoco della persona")) 
;; ;;                  (data:jsprop 'nome t (data:jsstring 'nome "Nome")) 
;; ;;                  (data:jsprop 'cognome t (data:jsstring 'cognome "Cognome")) 
;; ;;                  (data:jsprop 'codice-fiscale nil (data:jsstring 'codice-fiscale "Codice fiscale")) 
;; ;;                  (data:jsprop 'partita-iva nil (data:jsstring 'partita-iva "Partita IVA")) 
;; ;;                  (data:jsprop 'luogo-nascita t (data:jsstring 'luogo-nascita "Luogo di nascita"))
;; ;;                  (data:jsprop 'data-nascita t (data:jsstring 'data-nascita "Data di nascita"))  
;; ;;                  (data:jsprop 'ruoli nil (data:jsarray 'ruoli "Lista di ruoli assunti nel sinistro" role-format))))

;; ;; (let ((json ;; (synth :random (data:jsarray 'test "aaa")))
;; ;;        (synth :random schema)))
;; ;;   (pprint (synth :pretty json))
;; ;;   (format t "~%~a" (synth :string (synth :string json))))







;; ;; (synth :output (synth :typescript (bb-unit (bb-import (bb-const "@angular/core") 'component 'onInit)
;; ;;                                            (bb-annotation 'component 
;; ;;                                                          :selector (bb-const "my-heroes") 
;; ;;                                                          :template-url  (bb-const "test") 
;; ;;                                                          :style-urls (bb-array (bb-const "test")))
;; ;;                                            (bb-class 'hero-search 
;; ;;                                                      :fields (list (bb-pair 'heroes0 'string :init (bb-new 'heroes))
;; ;;                                                                    (bb-pair 'heroes 'string :init (bb-array (bb-const "aaa")))
;; ;;                                                                    (bb-pair 'heroes2 'string :init (bb-call 'get (bb-const "aaa")))
;; ;;                                                                    (bb-pair 'heroes3 'string :init (bb-chain (bb-call 'get (bb-const "aaa"))
;; ;;                                                                                                              (bb-call 'set (bb-const "aaa"))
;; ;;                                                                                                              (bb-call 'set (bb-const "bbb"))))
;; ;;                                                                    (bb-pair 'heroes4 'string :init (bb-call 'catch (bb-arrow (list (bb-pair 'e 'error)) (bb-call 'test (bb-const 'e)))) :const t))
;; ;;                                                      :constructor (bb-constructor (list (bb-pair 'heroes 'string)))
;; ;;                                                      :methods (list (bb-method (text "on-init") 
;; ;;                                                                                (list (bb-pair 'heroes 'string))
;; ;;                                                                                'void))))) 0)

;;  ;; (synth :output (nest 10 (bb-const "~a" 24)) 0)
;; ;; (pprint (synth :output (synth :doc (html:div :class "a" (doc:bb-const "SS"))) 0))
;; ;; (pprint (synth :pretty (html:div :class "a" (bb-const "ss"))))
;; ;; (synth :output (synth :doc (synth :template (gui:input 'name (bb-const "Name") :init (bb-const "hello")))) 0)

;; ;; (pprint (synth :pretty (bb-unit
;; ;;                         (bb-import (bb-const "@angular/core") 'component 'onInit)
;; ;;                         (bb-annotation 'component 
;; ;;                                       :selector (bb-const "my-heroes") 
;; ;;                                       :template-url  (bb-const "test") 
;; ;;                                       :style-urls (bb-array (bb-const "test")))
;; ;;                         (bb-class 'hero-search 
;; ;;                                   :fields (list (bb-pair 'heroes0 'string :init (bb-new 'heroes))
;; ;;                                                 (bb-pair 'heroes 'string :init (bb-array (bb-const "aaa")))
;; ;;                                                 (bb-pair 'heroes2 'string :init (bb-call 'get (bb-const "aaa")))
;; ;;                                                 (bb-pair 'heroes3 'string :init (bb-chain (bb-call 'get (bb-const "aaa"))
;; ;;                                                                                           (bb-call 'set (bb-const "aaa"))
;; ;;                                                                                           (bb-call 'set (bb-const "bbb"))))
;; ;;                                                 (bb-pair 'heroes4 'string :init (bb-call 'catch (bb-arrow (list (bb-pair 'e 'error)) (bb-call 'test (bb-const 'e)))) :const t))
;; ;;                                   :constructor (bb-constructor (list (bb-pair 'heroes 'string)))
;; ;;                                   :methods (list (bb-method 'on-init 
;; ;;                                                             (list (bb-pair 'heroes 'string))
;; ;;                                                             'void))))))

;; ;; (synth :typescript (bb-pair 'e 'error))
;; ;; (synth :typescript (bb-unit
;; ;;                     (bb-import (text "@angular/core") 'component 'onInit)
;; ;;                     ;; (bb-annotation 'component 
;; ;;                     ;;               :selector (text "my-heroes") 
;; ;;                     ;;               :template-url  (text "test") 
;; ;;                     ;;               :style-urls (bb-array (text "test")))
;; ;;                     ;; (bb-class 'hero-search 
;; ;;                     ;;           :fields (list (bb-pair 'heroes0 'string :init (bb-new 'heroes))
;; ;;                     ;;                         (bb-pair 'heroes 'string :init (bb-array (text "aaa")))
;; ;;                     ;;                         (bb-pair 'heroes2 'string :init (bb-call 'get (text "aaa")))
;; ;;                     ;;                         (bb-pair 'heroes3 'string :init (bb-chain (bb-call 'get (text "aaa"))
;; ;;                     ;;                                                                   (bb-call 'set (text "aaa"))
;; ;;                     ;;                                                                   (bb-call 'set (text "bbb"))))
;; ;;                     ;;                         (bb-pair 'heroes4 'string :init (bb-call 'catch (bb-arrow (list (bb-pair 'e 'error)) (bb-call 'test (text 'e)))) :const t))
;; ;;                     ;;           :constructor (bb-constructor (list (bb-pair 'heroes 'string)))
;; ;;                     ;;           :methods (list (bb-method 'on-init 
;; ;;                     ;;                                     (list (bb-pair 'heroes 'string))
;; ;;                     ;;                                     'void)))
;; ;;                     ))

;; ;; (pprint (synth :pretty (div)))

;; ;; (synth :output (html:div :class "a" (text "aaa")))
;; ;; (pprint (parse (many (atomic)) '(a b c &optional d)))


;; ;; (let ((l '(a b c &optional d1 (d2 0 d2-supplied-p) &rest e &key f (g 99 g-supplied-p))))
;; ;;   ;; (pprint (parse (lambda-list) l))
;; ;;   (pprint (arg-names l))
;; ;;   ;; (let ((args (parse (lambda-list) l)))
;; ;;   ;;   (pprint (apply #'append (mapcar (lambda (x) (getf args x))  (list :req :opt :rest :key))))
;; ;;   ;;   )
;; ;;   )
;; ;; (pprint (parse (ttt) (list 'a 'b)))
;; ;; (pprint (parse (ttt) '(a b)))
;; ;; (pprint (parse (var-init) '((a b))))


;; ;; (pprint (parse (var-init) '((name init))))
;; ;;; "nextent" goes here. Hacks and glory await!

