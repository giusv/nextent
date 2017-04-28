;;https://hackage.haskell.org/package/json-schema-0.7.4.1/docs/Data-JSON-Schema-Types.html
;;http://cswr.github.io/JsonSchema/spec/grammar/
;; (defprod jsdoc (jsdoc ((id jid) (defs jdefs) (sch jsch)))
;;   (to-json () (apply #'jobject 
;; 		     :id id
;; 		     :definitions (apply #'jobject defs)
;; 		     (synth-plist to-json defs))))

(in-package :data)

(defprim jsbool (name desc)
  (:pretty () (list 'jsbool (list :name (doc:lower-camel name) :desc desc)))
  (:req () (doc:text "booleano. ~a" desc))
  (:brief () (doc:text "booleano. ~a" desc))
  (:random () (jbool (expr:const (random-boolean))))
  (:schema () this)
  (:model () (error "no model for jsbool"))
  (:type () (ng-type 'bool :primitive t))
  (:init () (ng-const 'true)))

(defprim jsstring (name desc)
  (:pretty () (list 'jsstring (list :name (doc:lower-camel name) :desc desc)))
  (:req () (doc:text "stringa. ~a" desc))
  (:brief () (doc:text "stringa. ~a" desc))
  (:random () (jstring (expr:const (random-string 10))))
  (:schema () this)
  (:model () (error "no model for jsstring"))
  (:type () (ng-type 'string :primitive t))
  (:init () (ng-const "")))

(defprim jsnumber (name desc)
  (:pretty () (list 'jsnumber (list :name (doc:lower-camel name) :desc desc)))
  ;; (:req () (doc:text "~a: numero" (doc:lower-camel name)))
  (:req () (doc:text "numero. ~a" desc))
  (:brief () (doc:text "numero. ~a" desc))
  (:random () (jnumber (expr:const (random-number 0 100))))
  (:schema () this)
  (:model () (error "no model for jsnumber"))
  (:type () (ng-type 'integer :primitive t))
  (:init () (ng-const 0)))

;; handle choice in instantiation
;; (defprim (jschoice (&rest (schemas (list jsschema))))
;;   (:pretty () `(jschoice :schemas (synth-all :pretty schemas)))
;;   (:req () (vcat (doc:text "scelta tra i seguenti schemi:")
;; 		   (nest 4 (apply #'vcat (synth-all :req schemas))))))

(defprim jsobject (name desc &rest props)
  (:pretty () (list 'jsobject (list :name (doc:lower-camel name) :desc desc :props (synth-all :pretty props))))
  (:req () (html:taglist 
            (doc:text "~a:  ~a." (doc:lower-camel name) desc)
            (html:p (doc:text" Esso &egrave; costituito dalle seguenti propriet&agrave;:"))
            (html:ul (synth-all :req props))))
  (:brief () (doc:text "~a" (upper-camel name)))
  (:random () (apply #'jobject (apply #'append (synth-all :random props))))
  (:schema () this)
  (:model () (ng-unit name (ng-class name :fields (synth-all :type props))))
  (:type () (ng-type name))
  (:init () nil))

(defprim jsprop (name required content)
  (:pretty () (list 'jsprop (list :name name :required required :content (synth :pretty content))))
  (:req () (li nil  (hcat (doc:text "~a" (doc:lower-camel name)) 
                          (if required (doc:text " (obbligatoria)") 
                              (doc:text " (facoltativa)")) 
                          (doc:text ": ")) (synth :brief content)))
  (:random () (list (keyw name) (synth :random content)))
  (:schema () this)
  (:model () (error "no model for jsprop"))
  (:type () (ng-pair name (synth :type content) :init (synth :init content)))
  (:init () (error "should not be reachable")))

(defprim jsarray (name desc element)
  (:pretty () (list 'jsarray (list :name (doc:lower-camel name) :desc desc :element (synth :pretty element)))) 
  (:req () (html:taglist
            (doc:text "array (~a) denominato ~a costituito dal seguente elementento:" desc (doc:lower-camel name))
            (synth :req element)))
  (:brief () (doc:text "~a" (upper-camel name)))
  (:random () (let* ((length (random-number 2 5))
                     (values (loop for i from 0 to length collect (synth :random element)))) 
                (apply #'jarray values)))
  (:schema () this)
  (:model () (error "no model for jsarray"))
  (:type () (ng-type (synth :name element) :array t))
  (:init () nil))


(defun get-ident ()
  #'(lambda (jsschema)
      (list jsschema)))

(defun get-elem ()
  #'(lambda (jsschema)
      (list (synth :elem jsschema))))

(defun get-prop (name)
  #'(lambda (jsschema)
      (synth-all :content 
		 (remove-if-not #'(lambda (prop) 
				    (eql name (synth :name prop)))
				(synth :props jsschema)))))

(defun compose-filter (f g)
  #'(lambda (jsschema)
      (let ((temps (funcall f jsschema))) ;;temps is a list of jsschema
	(apply #'append (mapcar #'(lambda (temp) 
				    (funcall g temp))
				temps)))))

(defun compose-filters (&rest filters)
  #'(lambda (jsschema)
      (funcall (reduce #'compose-filter filters) jsschema)))



(defprim ident ()
  (:pretty () (list 'ident))
  (:func () (get-ident))
  ;; (:req () (doc:text "ident"))
  (:req () (doc:text "ident")))

(defprim prop (name)
  (:pretty () (list 'prop (list :name name)))
  (:func () (get-prop name))
  ;; (:req () (doc:text "~a" (doc:lower-camel name)))
  (:req () (span nil (doc:text "~a" (doc:lower-camel name)))))

(defprim elem ()
  (:pretty () (list 'elem))
  (:func () (get-elem))
  ;; (:req () (doc:text "#"))
  (:req () (span nil (doc:text "#"))))

(defprim comp (&rest filters)
  (:pretty () (list 'comp (list :filters (synth-all :pretty filters))))
  (:func () (apply #'compose-filters (synth-all :func filters)))
  ;; (:req () (apply #'punctuate (forward-slash) nil (synth-all :req filters)))
  (:req () (span nil (apply #'punctuate (forward-slash) nil (synth-all :req filters)))))

(defun parse-filter ()
  (do-with ((filters (sepby (item) (sym '>>>))))
    (result (apply #'comp (mapcar #'eval filters)))))

(defun filter (filt obj) 
  (car (funcall (synth :func filt) obj)))

;; (pprint (synth :pretty (car (funcall (get-prop 'addresses) *user*))))
;; (pprint (synth :pretty (car (funcall (get-elem) *addresses*))))

;; (pprint (synth :pretty (car (funcall (comp (comp (get-prop 'addresses) (get-elem)) (get-prop 'city)) *user*))))
;; (defparameter *address*
;;   (jsobject (jsprop 'city t (jsstring 'city-string))
;;             (jsprop 'state t (jsstring 'state-string))))
;; (defparameter *addresses* (jsarray 'address-array *address*))
;; (defparameter *user* 
;;   (jsobject 'user-object 
;;             (jsprop 'name t (jsstring 'name-string))
;; 	    (jsprop 'addresses t *addresses*)
;; 	    (jsprop 'numbers t (jsarray 'numbers-array (jsnumber 'number-number)))))



;; (pprint (synth :pretty (car (funcall (compose-filters (get-prop 'addresses) (get-elem) (get-prop 'city)) *user*))))
;; (pprint (synth :pretty (car (funcall (synth :func (comp (prop 'addresses) (elem) (prop 'city))) *user*))))
;; (pprint (synth :pretty (car (funcall 
;; 			     (synth :func (parse (parse-filter) 
;; 						   '((prop 'addresses) >>> (elem)))) *user*))))
