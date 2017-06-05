(in-package :server)

(defprim rest-service (name url &rest resources)
  (:pretty () (list 'rest-service (list :name name :url (synth :pretty url) :resources (synth-all :pretty resources))))
  (:jax-class () (bb-unit name
                      (bb-with-annotations 
                       (list (bb-annotation '|Path| (doc:double-quotes (synth :url url))))
                       (bb-class name :public t
                                 :methods (apply #'append (synth-all :jax-methods resources name url))))))
  (:bean-class () (bb-unit (symb name "-BEAN-IMPL")
                           (bb-with-annotations 
                            (list (bb-annotation '|Stateless|))
                            (bb-class (symb name "-BEAN-IMPL") 
                                      :public t
                                      :interfaces (list (symb name "-BEAN"))
                                      :constructor (bb-constructor name nil)
                                      :fields (list (bb-with-annotations (list (bb-annotation '|PersistenceContext|))
                                                                         (bb-pair 'entity-manager (bb-type 'entity-manager) :private t)))
                                      :methods (apply #'append (synth-all :bean-methods resources url)))))))

(defprim rest-singleton (name actions)
  (:pretty () (list 'rest-singleton (list :name name :actions (synth-all :pretty actions))))
  (:jax-methods (bean path)  (let* ((chunk (url:static-chunk name))
                                    (newpath (url:backward-chain chunk path)))
                               (synth-all :jax-method actions bean newpath chunk)))
  (:bean-methods (path) (let* ((chunk (url:static-chunk name))
                               (newpath (url:backward-chain chunk path)))
                          (synth-all :bean-method actions newpath chunk 'single))))

(defprim rest-collection (name actions &rest resources)
  (:pretty () (list 'rest-collection (list :name name :resources (synth-all :pretty resources) 
                                           :actions (synth-all :pretty actions))))
  (:jax-methods (bean path)  (let* ((chunk (url:static-chunk name))
                                    (newpath (url:backward-chain chunk path)))
                               (apply #'append (synth-all :jax-method actions bean newpath chunk)
                                      (synth-all :jax-methods resources bean newpath))))
  (:bean-methods (path) (let* ((chunk (url:static-chunk name))
                               (newpath (url:backward-chain chunk path)))
                          (append (synth-all :bean-method actions newpath chunk 'collection)
                                  (apply #'append (synth-all :bean-methods resources newpath))))))

;; (defmacro rest-collection (name actions &rest resources)
;;   `(rest-collection% ,name ,actions ,@resources))

(defprim rest-item% (name param actions &rest resources)
  (:pretty () (list 'rest-item (list :name name :param (synth :pretty param) :resources (synth-all :pretty resources) 
                                     :actions (synth-all :pretty actions))))
  (:jax-methods (bean path) (let* ((chunk (synth :url param))
                                   (newpath (url:backward-chain chunk path)))
                              (apply #'append (synth-all :jax-method actions bean newpath chunk)
                                     (synth-all :jax-methods resources bean newpath))))
  (:bean-methods (path) (let* ((chunk (synth :url param))
                               (newpath (url:backward-chain chunk path)))
                          (append (synth-all :bean-method actions newpath chunk 'single)
                                  (apply #'append (synth-all :bean-methods resources newpath))))))

(defmacro rest-item (name (param) actions &rest resources)
  `(let ((,(car param) ,(cadr param)))
     (rest-item% ,name ,(car param) ,actions ,@resources)))



(defun parlist (type pars)
  (mapcar (lambda (par)
            (bb-with-annotations (list (bb-annotation type (doc:double-quotes (doc:text "~a" (doc:lower-camel par)))))
                                 (bb-pair (doc:lower-camel par) (bb-type 'String)) :newline nil)) 
          pars))

(defmacro with-lookup (bean-name body)
  `(bb-try (bb-list
            (bb-statement (bb-pair 'context (bb-type 'context) :init (bb-new 'initial-context)))
            (bb-statement (bb-pair ,bean-name (bb-type ,bean-name) :init (bb-chain (bb-dynamic 'context) 
                                                                                 (bb-call 'lookup (bb-const (mkstr "java:module/" (doc:lower-camel ,bean-name)) )) :as ,bean-name)))
            ,body)
           (list (bb-catch (e :naming-exception) 
                           (bb-statement (bb-chain (bb-dynamic e)
                                                   (bb-call 'print-stack-trace)))))))
(defprim rest-get% (queries action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-get (list :queries (synth-all :pretty queries) :action (synth :pretty action) :mtypes mtypes)))
  (:jax-method (bean path chunk) 
               (bb-with-annotations 
                (list (bb-annotation '|GET|)
                      (bb-annotation '|Path| (doc:double-quotes (synth :url path)))
                      (if mtypes (apply #'bb-annotation '|Produces| 
                                        (mapcar (lambda (type) (doc:double-quotes (doc:text "~a" type))) mtypes))))
                (bb-method (doc:text "get~a" (doc:upper-camel (synth :name chunk)))
                           (synth-all :declaration (append queries (synth :path-parameters path)))
                           ;; (parlist '|PathParam| (synth :path-parameters path))
                           ;; (parlist '|QueryParam| queries)
                           
                           (bb-type 'response)
                           ;; (bb-chain (bb-dynamic 'this) (bb-call 'validate)) 
                           (let* ((bean-name (symb bean "-BEAN")))
                             (with-lookup bean-name
                               (bb-statement (bb-chain (bb-dynamic bean-name)  
                                                       (apply #'bb-call (symb 'retrieve "-" (synth :name chunk))
                                                              (mapcar #'bb-dynamic (synth :path-parameters path))))))))))
  (:bean-method (path chunk type) 
                (bb-method (doc:text "retrieve~a" (doc:upper-camel (synth :name chunk)))
                           (synth-all :declaration (append queries (synth :path-parameters path)))
                           (cond ((eq type 'single) 
                                  (bb-type (symb (synth :name chunk) "-J-T-O")))
                                 ((eq type 'collection) 
                                  (bb-type 'list 
                                           :template (bb-type (symb (singular (synth :name chunk)) "-J-T-O")))))
                           (synth :logic action))))

;; (defmacro rest-get ((&rest queries) action &key mtypes)
;;   `(let ,(mapcar #'(lambda (query) 
;;                      `(,query ',query))
;;                  queries)
;;      (rest-get% (list ,@queries) ,action ,@(if mtypes `(:mtypes ,mtypes)))))

(defmacro rest-get ((&rest queries) action &key mtypes)
  `(let ,(mapcar #`(,(car a1) ,(cadr a1)) queries)
     (rest-get% (list ,@(mapcar #'car queries)) ,action ,@(if mtypes `(:mtypes ,mtypes)))))


(defprim rest-post% (format action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-post (list :format format :action (synth :pretty action) :mtypes mtypes)))
  (:jax-method (bean path chunk) 
               (bb-with-annotations 
                (list (bb-annotation '|POST|)
                      (bb-annotation '|Path| (doc:double-quotes (synth :url path)))
                      (if mtypes 
                          (apply #'bb-annotation '|Consumes|  
                                 (mapcar (lambda (type) (doc:double-quotes (doc:text "~a" type))) mtypes))))
                (let ((name (symb (synth :name format) "-J-T-O"))) 
                  (bb-method (doc:text "post~a" (doc:upper-camel (singular (synth :name chunk)))) 
                             (doc:append* (synth-all :declaration (synth :path-parameters path))
                                          (bb-pair name (bb-type name)))
                             (bb-type :void)
                             (let* ((bean-name (symb bean "-BEAN")))
                               (with-lookup bean-name
                                 (bb-statement (bb-chain (bb-dynamic bean-name) 
                                                         (apply #'bb-call (symb 'save "-" (singular (synth :name chunk)))
                                                                (doc:append* (mapcar #'bb-dynamic (synth :path-parameters path))
                                                                             (bb-dynamic name)))))))))))
  (:bean-method (path chunk type)
                (let ((name (symb (synth :name format) "-J-T-O"))) 
                  (bb-method (doc:text "add~a" (doc:upper-camel (singular (synth :name chunk))))
                             (doc:append* (synth-all :declaration (synth :path-parameters path))
                                          (bb-pair name (bb-type name)))
                             (bb-type 'string)
                             (synth :logic action)))))

(defmacro rest-post (format (&rest fields) action &key mtypes)
  `(with-fields ,fields ,format 
     (rest-post% ,format ,action ,@(if mtypes `(:mtypes ,mtypes)))))

(defprim rest-put (format action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-put (list :format format :action (synth :pretty action) :mtypes mtypes)))
  (:jax-method (bean path chunk)
               (bb-with-annotations (list (bb-annotation '|PUT|)
                                          (bb-annotation '|Path| (doc:double-quotes (synth :url path)))
                                          (if mtypes 
                                              (apply #'bb-annotation '|Consumes|  
                                                     (mapcar (lambda (type) (doc:double-quotes (doc:text "~a" type))) mtypes))))
                                    (let ((name (symb (synth :name format) "-J-T-O"))) 
                                      (bb-method (doc:text "put~a" (doc:upper-camel (synth :name chunk))) 
                                                 (doc:append* (synth-all :declaration (synth :path-parameters path))
                                                              (bb-pair name (bb-type name)))
                                                 (bb-type (synth :name chunk))
                                                 (let* ((bean-name (symb bean "-BEAN")))
                                                   (with-lookup bean-name
                                                     (bb-statement (bb-chain (bb-dynamic bean-name) 
                                                                             (apply #'bb-call (symb 'update "-" (synth :name chunk))
                                                                                    (doc:append* (mapcar #'bb-dynamic (synth :path-parameters path))
                                                                                                 (bb-dynamic name)))))))))))
  (:bean-method (path chunk type) (let ((name (symb (synth :name format) "-J-T-O"))) 
                                    (bb-method (doc:text "update~a" (doc:upper-camel (synth :name chunk)))
                                               (doc:append* (synth-all :declaration (synth :path-parameters path))
                                                            (bb-pair name (bb-type name)))
                                               (bb-type :void)
                                               (synth :logic action)))))
