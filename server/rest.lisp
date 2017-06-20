(in-package :server)

(defparameter *resources* (make-hash-table))
(defmacro defresource (name resource)
  `(progn (defparameter ,name ,resource) 
         (setf (gethash ',name *resources*) ,name)))

(defparameter *services* (make-hash-table))
(defmacro defservice (name service)
  `(progn (defparameter ,name ,service) 
         (setf (gethash ',name *services*) ,name)))

(defprim rest-service (name url &rest resources)
  (:pretty () (list 'rest-service (list :name name :url (synth :pretty url) :resources (synth-all :pretty resources))))
  (:jax-class (package) (bb-unit name
                                 (bb-package (symb package '|.service|))
                                 (bb-import '|javax.ws.rs| '|Path| '|Consumes| '|Produces| '|GET| '|POST| '|PUT| '|DELETE| '|PathParam| '|QueryParam|)
                                 (bb-import '|javax.naming| '|Context| '|InitialContext| '|NamingException|)
                                 (bb-import '|javax.ws.rs.core| '|Response|)
                                 (bb-import '|javax.ws.rs.core.Response| '|ResponseBuilder|)
                                 (bb-import (symb package '|.ejb|) '|*|)
                                 (bb-import (symb package '|.jto|) '|*|)
                                 (bb-with-annotations 
                                  (list (bb-annotation2 '|Path| (bb-const (synth :string (synth :url url)))))
                                  (bb-class name :public t
                                            :methods (apply #'append (synth-all :jax-methods resources name url))))))
  (:bean-class (package) (let ((bean-name (symb name "-BEAN")))
                           (bb-unit bean-name
                                    (bb-package (symb package '|.ejb|))
                                    (bb-import '|javax.ejb| '|EJB| '|Stateless|)
                                    (bb-import '|javax.persistence| '|EntityManager| '|PersistenceContext|)
                                    (bb-import (symb package '|.jto|) '|*|)
                                    (bb-import (symb package '|.model|) '|*|)
                                    (bb-import '|java.util| '|List|)
                                    (bb-import '|java.util| '|Arrays|)
                                    (bb-with-annotations 
                                     (list (bb-annotation2 '|Stateless|))
                                     (bb-class bean-name 
                                               :public t
                                               ;; :interfaces (list (symb name "-BEAN"))
                                               ;; :constructor (bb-constructor name nil)
                                               :fields (list (bb-with-annotations (list (bb-annotation2 '|PersistenceContext|))
                                                                                  (bb-statement (bb-pair 'entity-manager (bb-type 'entity-manager) :private t))))
                                               :methods (apply #'append (synth-all :bean-methods resources url))))))))

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
            (bb-with-annotations (list (bb-annotation2 type (bb-const (synth :string (doc:text "~a" (lower-camel par))))))
                                 (bb-pair (lower-camel par) (bb-type 'String)) :newline nil)) 
          pars))

(defmacro with-lookup (bean-name body)
  `(bb-try (bb-list
            (bb-statement (bb-pair 'context (bb-type 'context) :init (bb-new 'initial-context)))
            (bb-statement (bb-pair ,bean-name (bb-type ,bean-name) :init (bb-chain (bb-dynamic 'context) 
                                                                                 (bb-call 'lookup (bb-const (mkstr "java:module/" (lower-camel ,bean-name)) )) :as (bb-object-type ,bean-name))))
            ,body)
           (list (bb-catch (e :naming-exception) 
                           (bb-statement (bb-chain (bb-dynamic e)
                                                   (bb-call 'print-stack-trace)))))))
(defprim rest-get% (queries action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-get (list :queries (synth-all :pretty queries) :action (synth :pretty action) :mtypes mtypes)))
  (:jax-method (bean path chunk) 
               (bb-with-annotations
                (list (bb-annotation2 '|GET|)
                      (bb-annotation2 '|Path| (bb-const (synth :string (synth :url path))))
                      (if mtypes 
                          (bb-annotation2 '|Produces| 
                                          (apply #'bb-array (mapcar 
                                                             (lambda (type) (bb-const (mkstr type))) 
                                                             mtypes)))))
                (bb-method (doc:text "get~a" (upper-camel (synth :name chunk)))
                           (synth-all :declaration (append queries (synth :path-parameters path)) t)
                           (bb-object-type 'response)
                           (let* ((bean-name (symb bean "-BEAN")))
                             (with-lookup bean-name
                               (bb-statement (bb-chain (bb-dynamic bean-name)  
                                                       (apply #'bb-call (symb 'retrieve "-" (synth :name chunk))
                                                              ;; (mapcar #'bb-dynamic (append queries (synth :path-parameters path)))
                                                              (synth-all :call (append queries (synth :path-parameters path)))))))))))
  (:bean-method (path chunk type) 
                (bb-method (doc:text "retrieve~a" (upper-camel (synth :name chunk)))
                           (synth-all :declaration (append queries (synth :path-parameters path)))
                           (cond ((eq type 'single) 
                                  (bb-object-type (symb (synth :name chunk) "-J-T-O")))
                                 ((eq type 'collection) 
                                  (bb-array-type (bb-object-type (symb (singular (synth :name chunk)) "-J-T-O")))))
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
                (list (bb-annotation2 '|POST|)
                      (bb-annotation2 '|Path| (bb-const (synth :string (synth :url path))))
                      (if mtypes 
                          (bb-annotation2 '|Consumes|  
                                          (apply #'bb-array (mapcar 
                                                             (lambda (type) (bb-const (mkstr type))) 
                                                             mtypes)))))
                (let ((name (symb (synth :name format) "-J-T-O"))) 
                  (bb-method (doc:text "post~a" (upper-camel (singular (synth :name chunk)))) 
                             (append* (synth-all :declaration (synth :path-parameters path) t)
                                      (bb-pair (synth :name format) (bb-type name)))
                             (bb-type 'response)
                             (let* ((bean-name (symb bean "-BEAN")))
                               (with-lookup bean-name
                                 (bb-statement (bb-chain (bb-dynamic bean-name) 
                                                         (apply #'bb-call (symb 'add "-" (singular (synth :name chunk)))
                                                                (append*  (synth-all :call (synth :path-parameters path))
                                                                          (bb-dynamic (synth :name format))))))))))))
  (:bean-method (path chunk type)
                (let ((name (symb (synth :name format) "-J-T-O"))) 
                  (bb-method (doc:text "add~a" (upper-camel (singular (synth :name chunk))))
                             (append* (synth-all :declaration (synth :path-parameters path))
                                      (bb-pair (synth :name format) (bb-type name)))
                             (bb-type 'string)
                             (synth :logic action)))))

(defmacro rest-post (format (&rest fields) action &key mtypes)
  `(with-fields ,fields ,format 
     (rest-post% ,format ,action ,@(if mtypes `(:mtypes ,mtypes)))))

(defprim rest-put (format action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-put (list :format format :action (synth :pretty action) :mtypes mtypes)))
  (:jax-method (bean path chunk)
               (bb-with-annotations (list (bb-annotation2 '|PUT|)
                                          (bb-annotation2 '|Path| (bb-const (synth :string (synth :url path))))
                                          (if mtypes 
                                              (bb-annotation2 '|Consumes|  
                                                              (apply #'bb-array (mapcar 
                                                                                 (lambda (type) (bb-const (mkstr type))) 
                                                                                 mtypes)))))
                                    (let ((name (symb (synth :name format) "-J-T-O"))) 
                                      (bb-method (doc:text "put~a" (upper-camel (synth :name chunk))) 
                                                 (append* (synth-all :declaration (synth :path-parameters path) t)
                                                          (bb-pair (synth :name format) (bb-type name)))
                                                 (bb-type 'response)
                                                 (let* ((bean-name (symb bean "-BEAN")))
                                                   (with-lookup bean-name
                                                     (bb-statement (bb-chain (bb-dynamic bean-name) 
                                                                             (apply #'bb-call (symb 'update "-" (synth :name chunk))
                                                                                    (append*  (synth-all :call (synth :path-parameters path))
                                                                                              (bb-dynamic (synth :name format))))))))))))
  (:bean-method (path chunk type) (let ((name (symb (synth :name format) "-J-T-O"))) 
                                    (bb-method (doc:text "update~a" (upper-camel (synth :name chunk)))
                                               (append* (synth-all :declaration (synth :path-parameters path))
                                                        (bb-pair (synth :name format) (bb-type name)))
                                               (bb-type :void)
                                               (synth :logic action)))))
