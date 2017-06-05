(in-package :query)

(defprim relation (name)
  (:pretty () (list 'relation (list :name name)))
  (:schema () (synth attributes (symbol-value name)))
  (:sql () ))

(defprim project (query &rest attributes) 
  (:pretty () (list 'project (list :attributes attributes :query (synth :pretty query))))
  (:schema () attributes))

(defprim restrict (query expression) 
  (:pretty () (list 'restrict (list :expression (synth :pretty expression) :query (synth :pretty query))))
  (:schema () (synth :schema query)))

(defprim equijoin (query1 query2 &rest attributes) 
  (:pretty () (list 'equijoin (list (:query1 (synth :pretty query1) :query2 (synth :pretty query2) :attributes (synth-all :pretty attributes)) )))
  (:schema () (let ((union (append (synth :schema query1)
                                   (synth :schema query2))))
                (reduce #'(lambda (acc attr) (remove attr acc :count 1)) 
                        attributes
                        :initial-value union))))


;; (defparameter *query* (restrict (equijoin (relation 'news-entity)
;;                                           (relation 'subscription-entity) 
;;                                           'news-id) 
;;                                 (+equal+ (+true+) (+true+))))

;; (pprint (synth output (synth to-doc (synth to-html *query*)) 0))


;;(synth attributes *people*)

