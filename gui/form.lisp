(in-package :gui)

(defprim form (name schema element)
  (:pretty () (list 'form (list :name name :schema (synth :pretty schema) 
                                :element (synth :pretty element))))
  (:req (path) (html:taglist 
                (doc:text "Form identificato con ~a collegato al seguente formato dati:" (lower-camel name)) 
                (html:p (synth :req schema))
                (synth :req element path)
                (html:p (doc:text "Esso produce il seguente oggetto JSON:")
                        (html:code (synth :string (synth :model this))))))
  (:brief (path) (synth :req this path))
  (:model () (apply #'jobject (apply #'append (synth-all :model bindings))))
  (:reqlist (*) nil)
  (:template () (html:tag (string-downcase name) (doc:empty)))
  (:controller () (ng-empty))
  (:components (father) 
               (let ((unit-name 
                      (if father 
                          (symb father "-" name)
                          name))
                     (template 
                      (ng-template 
                       (html:form 
                        ;; (keyw (synth :string (doc:brackets (doc:text "~a" (doc:lower-camel (synth :type element))))))
                        :|[formGroup]| (doc:lower-camel name)
                        (synth :form-template element (list name)))))
                     (controller 
                      (list (ng-pair name (synth :type element))
                            (ng-constructor (list (ng-pair 'fb (ng-type'form-builder) :private t))
                                            (ng-assign (ng-chain (ng-dynamic 'this) (ng-dynamic name))
                                                       (synth :form element)
                                                       ;; (ng-chain (ng-dynamic 'this) (ng-dynamic 'fb)
                                                       ;;           (ng-call 'group 
                                                       ;;                    (ng-object (keyw (synth :name element)) 
                                                       ;;                               (synth :form element))))
                                                       ))
                            (synth :form-controller element (list name))))) 
                 (list (ng-unit unit-name
                                (ng-import (ng-const "@angular/core") 'component)
                                (ng-import (ng-const "@angular/forms") 'form-array 'form-builder 'form-group)
                                (ng-primitive 'component
                                              :selector (ng-const (string-downcase name))
                                              :template template )
                                (ng-class (mkstr unit-name "-component")
                                          :fields controller)))))
  (:routes (*) nil))

(defprim bnd (name element)
  (:pretty () (list 'bnd (list :name name :element (synth :pretty element))))
  (:req (path) (html:div nil (synth :req element path)))
  (:brief (path) (synth :req this path))
  (:model () (list (keyw name) (synth :model element)))
  (:reqlist (*) nil)
  (:form-template (path) (synth :form-template element path))
  (:form-controller (path) (synth :form-controller element path))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (list (keyw name) (synth :form element)))
  (:type () (error "should not be visible") ))

(defprim obj% (name schema bindings element)
  (:pretty () (list 'obj (list :name name :schema (synth :pretty schema) 
                               :bindings (synth-plist-both :pretty :pretty bindings) 
                               :element (synth :pretty element))))
  (:req (path) (html:taglist 
                (doc:text "Sezione identificata con ~a collegata al seguente formato dati:" (lower-camel name)) 
                (html:a :href (concatenate 'string "#" (synth :string (synth :brief schema) 0))
                        (synth :brief schema))
                ;; (html:p (synth :brief schema))
                (synth :req element path)
                (html:p (doc:text "Essa produce il seguente oggetto JSON:")
                        (html:code (synth :string (synth :model this))))))
  (:brief (path) (synth :req this))
  (:model () (apply #'jobject (apply #'append (synth-all :model bindings))))
  (:reqlist (*) nil)
  (:form-template (path) (html:taglist (synth-all :form-template bindings path))
             ;; (html:div :|formGroupName| (doc:lower-camel name)
             ;;           (synth-all :form-template bindings (doc:append* path name)))
             )
  (:form-controller (path) (ng-list (synth-all :form-controller bindings path)))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (ng-chain (ng-dynamic 'this) (ng-dynamic 'fb)
                      (ng-call 'group (ng-object (apply #'append (synth-all :form bindings))))))
  (:type () (ng-type 'form-group)))

(defmacro obj (name schema binds elem)
  `(let* ,(mapcar #'(lambda (bind)
		      (destructuring-bind (name key elem) bind
			`(,name (bnd ',key ,elem))))
		  binds) 
     (let ((f (obj% ,name ,schema (list ,@(mapcar #'car binds)) ,elem))) 
       (values f f))))

(defprim arr (name schema element)
  (:pretty () (list 'arr (list :name name 
                               :schema (synth :pretty schema) 
                               :element (synth :pretty element))))
  (:req (path) (html:taglist 
                (doc:text "Sezione dinamica identificata con ~a collegata al seguente formato dati:" (doc:lower-camel name)) 
                (html:p (synth :brief schema)) 
                (synth :req element path)
                (html:p (doc:text "Essa produce il seguente oggetto JSON:")
                        (html:code (synth :string (synth :model this))))))
  (:brief (path) (synth :req this path))
  (:model () (jarray (synth :model element)))
  (:reqlist (*) nil)
  (:form-template (path) (html:div 
                          :|formArrayName| (doc:lower-camel name)
                          (html:div :|*ngFor| (doc:text "let control of ~{~a.controls~^.~}; let i=index" 
                                                        (mapcar #'doc:lower-camel (doc:append* path name)))
                                    :|[formGroupName]| "i" 
                                    (synth :form-template element path))
                          (html:button :|(click)| (doc:text "add~aElement()" (doc:upper-camel name)) 
                                       :|type| "button"
                                       (doc:text "Add element"))
                          (html:button :|(click)| (doc:text "remove~aElement()" (doc:upper-camel name)) 
                                       :|type| "button"
                                       (doc:text "Remove element"))))
  (:form-controller (path) (ng-list 
                            (ng-method (doc:text "add~aElement" (doc:upper-camel name)) nil 'void
                                       (ng-pair 'elements (ng-type 'form-array) :const t :init 
                                                (ng-chain (ng-dynamic 'this)
                                                          (ng-dynamic (car path))
                                                          (mapcar (lambda (elem)
                                                                    (ng-call 'get (ng-const (doc:lower-camel elem))))
                                                                  (doc:append* (cdr path) name))
                                                          :as 'form-array))
                                       (ng-chain (ng-dynamic 'elements)
                                                 (ng-call 'push (synth :form element)
                                                          ;; (ng-chain (ng-dynamic 'this)
                                                          ;;           (ng-dynamic 'fb)
                                                          ;;           (ng-call 'group (ng-new (synth :name element))))
                                                          )))))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (ng-chain (ng-dynamic 'this) (ng-dynamic 'fb)
                      (ng-call 'array (ng-array (synth :form element)))))
  (:type () (ng-type 'form-array)))

;; (defmacro arr* (name schema elem)
;;   `(let* ,(mapcar #'(lambda (bind)
;; 		      (destructuring-bind (name key elem) bind
;; 			`(,name (bnd ',key ,elem))))
;; 		  binds)
;;      ))
