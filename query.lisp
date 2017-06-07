(in-package :query)

(defmacro with-queries (bindings query)
  `(let ,(mapcar #`(,(car a1) (query ',(car a1) ,(cadr a1)))
                bindings)
     ,query))

(defprim query (name value)
  (:pretty () (list 'query (list :name name :value value)))
  (:schema () (synth :attributes value))
  (:sql () (doc:hcat+ (doc:parens (synth :sql value)) (doc:text "~a" name))))

(defprim relation (name)
  (:pretty () (list 'relation (list :name name)))
  (:schema () (synth :attributes name))
  (:sql () (doc:text "~a" name)))

(defprim product (&rest queries) 
  (:pretty () (list 'product (list :queries (synth-all :pretty queries))))
  (:schema () (apply #'append (synth-all :schema queries)))
  (:sql ()  (apply #'doc:punctuate (comma) nil (synth-all :sql queries))))

(defprim project (query &rest attributes) 
  (:pretty () (list 'project (list :attributes attributes :query (synth :pretty query))))
  (:schema () attributes)
  (:sql () (doc:hcat+
                      (doc:text "SELECT")
                      (if attributes 
                          (apply #'punctuate (comma) nil (synth-all :sql attributes))
                          (doc:text "*"))
                      (doc:text "FROM")
                      (synth :sql query))))
(defprim restrict (query expression) 
  (:pretty () (list 'restrict (list :expression (synth :pretty expression) :query (synth :pretty query))))
  (:schema () (synth :schema query))
  (:sql () (doc:hcat+
            (synth :sql query)
            (doc:text "WHERE")
            (synth :sql expression))))

(defprim equijoin (query1 query2 &rest attributes) 
  (:pretty () (list 'equijoin (list :query1 (synth :pretty query1) :query2 (synth :pretty query2) :attributes (synth-all :pretty attributes))))
  (:schema () (let ((union (append (synth :schema query1)
                                   (synth :schema query2))))
                (reduce #'(lambda (acc attr) (remove attr acc :count 1)) 
                        attributes
                        :initial-value union))))

;; (pprint (synth :pretty 
;;                (let ((trips (relation 'trips))
;;                      (cities (relation 'cities)))
;;                  (project (restrict (product trips cities)
;;                                     (expr:+true+))
;;                           'id 'name))))

(let ((q (with-queries ((tr (relation 'trips))
                        (ct (relation 'cities)))
           (project (restrict (product tr ct)
                              (expr:+equal+ (expr:attr tr 'id)
                                            (expr:attr ct 'id))))))) 
  (synth :output (synth :sql q) 0))




;; (defparameter *query* (restrict (equijoin (relation 'news-entity)
;;                                           (relation 'subscription-entity) 
;;                                           'news-id) 
;;                                 (+equal+ (+true+) (+true+))))

;; (pprint (synth output (synth to-doc (synth to-html *query*)) 0))


;;(synth attributes *people*)

