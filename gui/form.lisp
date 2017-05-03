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
                        (synth :form-template element name nil))))
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
                            (synth :form-controller element (list (list* name 'form-group)))))) 
                 (list (ng-unit unit-name
                                (ng-import (ng-const "@angular/core") 'component)
                                (ng-import (ng-const "@angular/forms") 'form-array 'form-builder 'form-group 'form-control)
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
  (:form-template (loopvar indexes) (synth :form-template element loopvar indexes))
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
  (:form-template (loopvar indexes) (html:taglist (synth-all :form-template bindings loopvar indexes)))
  (:form-controller (path) (ng-list (synth-all :form-controller bindings path ;; (append path (list (list* name 'form-group)))
                                               )))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (ng-new 'form-group (ng-object (apply #'append (synth-all :form bindings))))
         ;; (ng-chain (ng-dynamic 'this) (ng-dynamic 'fb)
         ;;              (ng-call 'group (ng-object (apply #'append (synth-all :form bindings)))))
         )
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
  (:form-template (loopvar indexes) 
                  (let ((new-loopvar (doc:lower-camel (gensym "v")))
                        (new-index (doc:lower-camel (gensym "i"))))
                    (html:div 
                     :|formArrayName| (doc:lower-camel name)
                     (html:div :|*ngFor| (doc:text "let ~a of ~a.controls['~a'].controls; let ~a=index" 
                                                   new-loopvar 
                                                   (doc:lower-camel loopvar)
                                                   (doc:lower-camel name)
                                                   ;; (mapcar #'doc:lower-camel (doc:append* (cdr path) name))
                                                   new-index)
                               :|class| "panel panel-default"
                              
                               (html:div 
                                :|class| "panel heading" 
                                (html:span :|class| "glyphicon glyphicon-remove pull-right"
                                           :|(click)| (doc:hcat (doc:text "remove~aElement" (doc:upper-camel name))
                                                                 (doc:parens (apply #'doc:punctuate (doc:comma) nil
                                                                                    (mapcar (lambda (index)
                                                                                              (doc:text "~a" (doc:lower-camel index)))
                                                                                            (append indexes (list new-index)))))) (doc:empty))) 
                               (html:div 
                                :|class| "panel body"
                                :|[formGroupName]| new-index
                                (synth :form-template element new-loopvar (append indexes (list new-index)))))
                     (html:button :|(click)| (doc:hcat (doc:text "add~aElement" (doc:upper-camel name))
                                                       (doc:parens (apply #'doc:punctuate (doc:comma) nil
                                                                          (mapcar (lambda (index)
                                                                                    (doc:text "~a" (doc:lower-camel index)))
                                                                                  indexes)))) 
                                  :|type| "button"
                                  (doc:text "+" ))
                     )))
  (:form-controller (path) 
                    (let ((newpath (append path (list (list* (doc:lower-camel (mkstr name)) 'form-array))) )
                          (newindex (gensym "i") ))
                      (ng-list 
                       ;; (ng-comment (doc:text "~{~a ~^->~}" newpath))
                       (ng-method (doc:text "add~aElement" (doc:upper-camel name)) 
                                  (mapcar (lambda (index)
                                            (ng-pair (car index) (ng-type 'number :primitive t)))
                                          (remove-if-not (lambda (elem) (eq 'form-group (cdr elem)))
                                                         (cdr newpath)))
                                  'void
                                  (ng-chain (reduce (lambda (acc elem)
                                                      (ng-chain acc 
                                                                (ng-element 'controls (ng-const (car elem))) 
                                                                :as (cdr elem)))
                                                    (cdr newpath)
                                                    :initial-value (ng-chain (ng-dynamic 'this)
                                                                             (ng-dynamic (caar newpath))))
                                            (ng-call 'push (synth :form element)
                                                     ;; (ng-chain (ng-dynamic 'this)
                                                     ;;           (ng-dynamic 'fb)
                                                     ;;           (ng-call 'group (ng-new (synth :name element))))
                                                     ))
                                  )
                       (ng-method (doc:text "remove~aElement" (doc:upper-camel name)) 
                                  (mapcar (lambda (index)
                                            (ng-pair (car index) (ng-type 'number :primitive t)))
                                          (append (remove-if-not (lambda (elem) (eq 'form-group (cdr elem)))
                                                          (cdr newpath)) (list (list* newindex nil))))
                                  'void
                                  (ng-chain (reduce (lambda (acc elem)
                                                      (ng-chain acc 
                                                                (ng-element 'controls (ng-const (car elem))) 
                                                                :as (cdr elem)))
                                                    (cdr newpath)
                                                    :initial-value (ng-chain (ng-dynamic 'this)
                                                                             (ng-dynamic (caar newpath))))
                                            (ng-call 'remove-at (ng-dynamic newindex)))

                                  )
                       (synth :form-controller element (append path (list (list* (doc:lower-camel (mkstr name)) 'form-array)
                                                                          (list* (gensym) 'form-group))) ))))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (ng-new 'form-array (ng-array (synth :form element)))
         ;; (ng-chain (ng-dynamic 'this) (ng-dynamic 'fb)
         ;;              (ng-call 'array (ng-array (synth :form element))))
         )
  (:type () (ng-type 'form-array)))

;; (defmacro arr* (name schema elem)
;;   `(let* ,(mapcar #'(lambda (bind)
;; 		      (destructuring-bind (name key elem) bind
;; 			`(,name (bnd ',key ,elem))))
;; 		  binds)
;;      ))
