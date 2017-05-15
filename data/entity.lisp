(in-package :data)

(defprim attribute (name type &optional desc)
  (:pretty () (list 'attribute (list :name name :type type :desc desc))) 
  (:java (&rest annotations) (bb-with-annotations 
                              (cons (bb-annotation '|Column| :name (doc:textify  name))
                                    annotations)
                              (bb-pair name (bb-type type) :private t)))
  ;; (:html () (text "Attributo ~a (~a): ~a" (lower-camel name) (lower-camel type) desc))
  )

(defprim foreign-key (attributes reference cardinality)
  (:pretty () (list 'foreign-key (list :attributes attributes :reference reference :cardinality cardinality))) 
  (:java () (bb-list (synth :java attributes (list (bb-annotation cardinality)))))
  ;; (:attributes () (synth-all name attributes))
  ;; (:html () (text "Foreign key verso ~a costituita dai seguenti attributi:  ~{~a~^, ~}" (lower-camel reference)
  ;;                 (mapcar #'lower-camel attributes)))
  )

(defprim primary-key (attribute)
  (:pretty () (list 'primary-key (list :attribute (synth :pretty attribute)))) 
  (:java () (synth :java attribute (list (bb-annotation '|Id|))))
  ;; (:attributes () (synth-all name attributes))
  ;; (:html () (multitags 
  ;;              (text "Primary key costituita dai seguenti attributi:")
  ;;              (apply #'ul nil
  ;;                     (mapcar #'listify (synth-all :html attributes)))))
  )

(defprim entity (name &key desc primary fields foreigns)
  (:java () (bb-class name
                      :fields (doc:append*
                               (synth :java primary)
                               (synth-all :java fields)
                               (synth-all :java foreigns))))
  (:pretty () (list 'entity :name name 
                    :desc desc
                    :primary (synth :pretty primary)
                    :fields (synth-all :pretty fields)
                    :foreigns (synth-all :pretty foreigns)))
  ;; (:attributes () (apply #'append (synth attributes primary) (synth-all name fields) (synth-all attributes foreigns)))
  ;; (:html () (section nil 
  ;;                    ;; (h5 nil (text "~a" (upper-camel name)))
                       
  ;;                    (text "~a. Essa è costituita da:" desc)
  ;;                    (apply #'ul nil
  ;;                           (mapcar #'listify (synth-all :html (append (list primary) fields foreigns))))
  ;;                    ;; (p nil (synth :html primary))
  ;;                    ;; (append (synth-all :html fields)
  ;;                    ;;         (synth-all :html foreigns))
  ;;                    ))
  )

;; (defun pretty-java (entity)
;;   (synth pretty (synth java entity) 0))



;; (defparameter *people* (entity 'people 
;; 			       (primary-key 
;; 				(attribute 'id 'string))
;; 			       (list (attribute 'name 'string)
;; 				     (attribute 'city 'string))
;; 			       (foreign-key
;; 				'cities
;; 				(attribute 'city-id1 'string)
;; 				(attribute 'city-id2 'string))
;; 			       (foreign-key
;; 				'cars
;; 				(attribute 'car-id1 'string))))

;; (synth attributes *people*)
