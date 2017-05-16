(in-package :data)

(defprim attribute (name type &optional desc)
  (:pretty () (list 'attribute (list :name name :type type :desc desc))) 
  (:entity (&rest annotations) (bb-with-annotations 
                              (cons (bb-annotation '|Column| :name (doc:double-quotes (doc:textify name)))
                                    annotations)
                              (bb-pair name (bb-type type) :private t)
                              :newline t))
  (:accessors () (list (bb-method (doc:text "get~a" (doc:upper-camel name)) nil (bb-type type)
                                  (bb-return (bb-dynamic name)))
                       (bb-method (doc:text "set~a" (doc:upper-camel name)) (list (bb-pair name (bb-type type))) (bb-type 'void :primitive t)
                                  (bb-assign (bb-chain (bb-dynamic 'this) 
                                                       (bb-dynamic name))
                                             (bb-dynamic name)))))
  ;; (:html () (text "Attributo ~a (~a): ~a" (lower-camel name) (lower-camel type) desc))
  )

;; (defprim foreign-key (attributes reference cardinality)
;;   (:pretty () (list 'foreign-key (list :attributes attributes :reference reference :cardinality cardinality))) 
;;   (:entity () (bb-list (synth :entity attributes (list (bb-annotation cardinality)))))
;;   ;; (:attributes () (synth-all name attributes))
;;   ;; (:html () (text "Foreign key verso ~a costituita dai seguenti attributi:  ~{~a~^, ~}" (lower-camel reference)
;;   ;;                 (mapcar #'lower-camel attributes)))
;;   (:accessors () (synth-all :accessors attributes)))

(defprim primary-key (attribute)
  (:pretty () (list 'primary-key (list :attribute (synth :pretty attribute)))) 
  (:entity () (synth :entity attribute (list (bb-annotation '|Id|))))
  ;; (:attributes () (synth-all name attributes))
  ;; (:html () (multitags 
  ;;              (text "Primary key costituita dai seguenti attributi:")
  ;;              (apply #'ul nil
  ;;                     (mapcar #'listify (synth-all :html attributes)))))
  (:accessors () (synth :accessors attribute)))

(defun get-sources (entity)
  (loop for rel being the hash-values of *relationships*
     ;; do (pprint (synth :name (synth :owner rel)))
     ;; (pprint (synth :name entity))
     ;; (pprint (eq  (synth :name (synth :owner rel)) (synth :name entity)))
     collect (if (eq (synth :name (synth :owner rel)) (synth :name entity)) rel)))

(defun get-targets (entity)
  (loop for rel being the hash-values of *relationships*
     ;; do (pprint (synth :name (synth :owner rel)))
     ;; (pprint (synth :name entity))
     ;; (pprint (eq  (synth :name (synth :owner rel)) (synth :name entity)))
     collect (if (eq (synth :name (synth :subordinate rel)) (synth :name entity)) rel)))


(defprim entity (name &key desc primary fields)
  (:pretty () (list 'entity :name name
                    :desc desc
                    :primary (synth :pretty primary)
                    :fields (synth-all :pretty fields)))
  (:entity () (bb-with-annotations 
               (list (bb-annotation '|Entity|)
                     (bb-annotation '|Table| :name (doc:double-quotes (doc:textify name))))
               (bb-class name
                         :fields (doc:append*
                                  (synth :entity primary)
                                  (synth-all :entity fields)
                                  (synth-all :source (get-sources this))
                                  (synth-all :target (get-targets this)))
                         :methods (append (synth :accessors primary)
                                          (apply #'append (synth-all :accessors fields))))))
  (:eao-interface () (bb-interface (symb name "-EAO")
                                   :public t
                                   :methods (list (bb-method (doc:textify (doc:lower-camel (symb "ADD-" name))) nil (bb-type name))))))

(defprim relationship (name owner subordinate cardinality &optional (participation t))
  (:pretty () (list 'relationship (list :name name
                                        :owner (synth :pretty owner)
                                        :subordinate (synth :pretty subordinate)
                                        :cardinality cardinality
                                        :participation participation)))
  (:source () (case cardinality
                (:one-to-one (bb-with-annotations (list (bb-annotation '|OneToOne|))
                                                  (bb-pair (synth :name subordinate) (bb-type (synth :name subordinate)) :private t)))
                (:many-to-one (bb-with-annotations (list (bb-annotation '|ManyToOne|))
                                                   (bb-pair (synth :name subordinate) (bb-type (synth :name subordinate)) :private t)))
                (:one-to-many (bb-with-annotations (list (bb-annotation '|OneToMany|
                                                                        :|mappedBy| (doc:double-quotes (doc:textify (doc:lower-camel (synth :name owner))))))
                                                   (bb-pair (symb (synth :name subordinate) "-SET") (bb-type 'Set :template (bb-type (synth :name subordinate))) :private t)))
                (:many-to-many (bb-with-annotations (list (bb-annotation '|ManyToMany|))
                                   (bb-pair (symb (synth :name subordinate) "-SET") (bb-type 'Set :template (bb-type (synth :name subordinate))) :private t))))) 
  (:target () (case cardinality
                (:one-to-one (if participation (bb-with-annotations 
                                                (list (bb-annotation '|OneToOne|
                                                                     :|mappedBy| (doc:double-quotes (doc:textify (doc:lower-camel (synth :name owner))))
                                                                     :|optional| (doc:textify '|false|))) 
                                                (bb-pair (synth :name owner) (bb-type (synth :name owner)) :private t))))
                (:many-to-one (bb-with-annotations 
                               (list (bb-annotation '|OneToMany|
                                                    :|mappedBy| (doc:double-quotes (doc:textify (doc:lower-camel (synth :name owner)))))) 
                               (bb-pair (symb (synth :name owner) "-SET") (bb-type 'Set :template (bb-type (synth :name owner))) :private t)))
                (:one-to-many (bb-with-annotations 
                               (list (bb-annotation '|ManyToOne|)) 
                               (bb-pair (synth :name owner) (bb-type (synth :name owner)) :private t)))
                (:many-to-many (bb-with-annotations 
                               (list (bb-annotation '|ManyToMany|
                                                    :|mappedBy| (doc:double-quotes (doc:textify (doc:lower-camel (symb (synth :name subordinate) "-SET")))))) 
                               (bb-pair (symb (synth :name owner) "-SET") (bb-type 'Set :template (bb-type (synth :name owner))) :private t))))))

(defparameter *entities* (make-hash-table))
(defparameter *relationships* (make-hash-table))

(defmacro defent (name entity)
  `(progn (defparameter ,name ,entity) 
         (setf (gethash ',name *entities*) ,name)))

(defmacro defrel (name relationship)
  `(progn (defparameter ,name ,relationship) 
          (setf (gethash ',name *relationships*) ,name)))




