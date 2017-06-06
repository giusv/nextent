(in-package :query)

(defmacro with-relations (bindings query)
  `(let ,(mapcar #`(,(car a1) (relation ',(cadr a1) ',(car a1)))
                bindings)
     ,query))

(defprim relation (name range)
  (:pretty () (list 'relation (list :name name :range range)))
  (:schema () (synth :attributes name))
  (:sql () (doc:text "~a ~a" name range) ))

(defprim product (&rest queries) 
  (:pretty () (list 'product (list :queries (synth-all :pretty queries))))
  (:schema () (apply #'append (synth-all :schema queries)))
  (:sql ()  (doc:punctuate (comma) t (mapcar #'doc:parens (synth-all :sql queries)))))

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

(let ((q (with-relations ((tr trips)
                          (ct cities))
           (project (restrict (product tr ct)
                              (expr:+equal+ (expr:attr tr 'id)
                                            (expr:attr ct 'id)))
                    'id 'name))))
  (pprint (synth :pretty q))
  (synth :output (synth :sql q) 0))




;; (defparameter *query* (restrict (equijoin (relation 'news-entity)
;;                                           (relation 'subscription-entity) 
;;                                           'news-id) 
;;                                 (+equal+ (+true+) (+true+))))

;; (pprint (synth output (synth to-doc (synth to-html *query*)) 0))


;;(synth attributes *people*)

