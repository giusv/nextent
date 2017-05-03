(in-package :data)

(defprim remote (name schema url)
  (:pretty () (list 'remote (list :name name :schema (synth :pretty schema) :url (synth :pretty url))))
  (:html ()  (html:taglist
                (doc:text "Sorgente dati remota identificata come ")
                (html:span-color (string-downcase name))
                (doc:text ", istanza dello schema dati ~a " (doc:lower-camel (synth :name schema)))
                (doc:text "e popolata al caricamento dell'elemento tramite richiesta HTTP GET verso l'URL ")
                (html:p (html:code (synth :url url)))))
  (:template () ())
  (:controller () (ng-method (symb 'get "-" 'name)
                             nil
                             (ng-type 'observable :template (ng-type (synth :name schema) :array t))))
  (:components () nil))

(defprim rand (name schema)
  (:pretty () (list 'rand (list :name name :schema (synth :pretty schema))))
  (:html ()  (html:taglist
                (doc:text "Sorgente dati identificata come ")
                (html:span-color (string-downcase name))
                (doc:text ", istanza dello schema dati ~a " (doc:lower-camel (synth :name schema)))
                (doc:text "e popolata al caricamento dell'elemento tramite generazione casuale")))
  (:template () ())
  (:controller () (ng-pair name 'any :init (synth :model (synth :random schema))))
  (:components () nil))

(defprim with-data% (bindings element)
  (:pretty () (list 'with-data (list :bindings (synth-all :pretty bindings) :element (synth :pretty element))))
  (:req (path) (html:taglist (doc:text "Tale elemento fa uso delle seguenti sorgenti dati:")
                             (html:ul (mapcar #'listify (synth-all :req bindings))) 
                             (synth :req element path)))
  (:brief (path) (synth :brief element path))
  (:reqlist (path) (synth :reqlist element path))
  (:template () (synth :template element))
  (:controller () (ng-list (synth-all :controller bindings) ))
  (:components () nil)
  (:routes (path) nil))


(defmacro with-data (binds &body element)
  `(let* ,(mapcar #'(lambda (bind)
		      (destructuring-bind (name source) bind
			`(,name ,source)))
		  binds)
     (with-data% (list ,@(mapcar #'car binds)) ,@element)))

