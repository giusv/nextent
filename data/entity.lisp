(in-package :data)

(defprim atype (name &key (size 0 size-supplied-p) (nullable t))
  (:pretty () (list 'atype (list :name name :size size :nullable nullable))) 
  (:ddl () (doc:hcat (case name
                       (:string (doc:text "VARCHAR2"))
                       (:integer (doc:text "INTEGER")))
                     (if size-supplied-p (doc:parens (doc:text "~a" size)))
                     (if (not nullable) (doc:text " NOT NULL")))))

(defprim attribute (name type &optional desc)
  (:pretty () (list 'attribute (list :name name :type (synth pretty type) :desc desc))) 
  (:entity (&rest annotations) (progn (pprint annotations)
                                      (bb-with-annotations 
                                       (cons (bb-annotation '|Column| :name (doc:double-quotes (doc:textify name)))
                                             annotations)
                                       (bb-pair name (bb-type (synth pretty type)) :private t)
                                       :newline t)))
  (:accessors () (list (bb-method (doc:text "get~a" (doc:upper-camel name)) nil (bb-type type)
                                  (bb-return (bb-dynamic name)))
                       (bb-method (doc:text "set~a" (doc:upper-camel name)) (list (bb-pair name (bb-type type))) (bb-type :void)
                                  (bb-assign (bb-chain (bb-dynamic 'this) 
                                                       (bb-dynamic name))
                                             (bb-dynamic name)))))
  (:paramdecl () (bb-pair name (bb-type type)))
  (:ddl () (doc:hcat (doc:text "~20a" name)
                     (synth :ddl type))))

(defprim primary-key (attribute)
  (:pretty () (list 'primary-key (list :attribute (synth :pretty attribute)))) 
  (:entity () (synth :entity attribute (bb-annotation '|Id|)))
  (:accessors () (synth :accessors attribute))
  (:paramdecl () (synth :paramdecl attribute))
  (:ddl () (doc:hcat (synth :ddl attribute) (doc:text " PRIMARY KEY"))))

(defprim foreign-key (name reference)
  (:pretty () (list 'foreign-key (list :name ))) 
  (:ddl () (doc:hcat (doc:text "FOREIGN KEY ~a REFERENCES ~a" name reference))))

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
                                  (synth :entity (primary-key primary))
                                  (synth-all :entity fields)
                                  (synth-all :source (get-sources this))
                                  (synth-all :target (get-targets this)))
                         :methods (append (synth :accessors primary)
                                          (apply #'append (synth-all :accessors fields))))))
  (:eao-interface () (bb-interface (symb name "-EAO")
                                   :public t
                                   :methods (list (bb-method (doc:textify (doc:lower-camel (symb "ADD-" name))) 
                                                             (remove nil (append (synth-all :paramdecl fields)
                                                                                 (synth-all :target-paramdecl (get-sources this))
                                                                                 (synth-all :source-paramdecl (get-targets this))))
                                                             (bb-type name)) 
                                                  (bb-method (doc:textify (doc:lower-camel (symb "CANCEL-" name)))
                                                             (list (synth :paramdecl primary)) 
                                                             (bb-type name)))))
  (:ddl () (doc:vcat (doc:text "CREATE TABLE ~a" name)
                     (doc:parens (doc:nest 4 (apply #'doc:punctuate (doc:comma) t (synth-all :ddl (doc:append* (primary-key primary) fields
                                                                                                               (synth-all :foreign-key (get-sources this))
                                                                                                               )))) :newline t))))

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
                                (bb-pair (symb (synth :name owner) "-SET") (bb-type 'Set :template (bb-type (synth :name owner))) :private t)))))
  (:target-paramdecl () (case cardinality
                   (:one-to-one (if participation (bb-pair (synth :name subordinate) (bb-type (synth :name subordinate)))))
                   (:many-to-one (bb-pair (synth :name subordinate) (bb-type (synth :name subordinate))))
                   (:one-to-many (bb-pair (symb (synth :name subordinate) "-SET") (bb-type 'Set :template (bb-type (synth :name subordinate)))))
                   (:many-to-many (bb-pair (symb (synth :name subordinate) "-SET") (bb-type 'Set :template (bb-type (synth :name subordinate)))))))
  (:source-paramdecl () (case cardinality
                   (:one-to-one (bb-pair (synth :name owner) (bb-type (synth :name owner))))
                   (:many-to-one (bb-pair (symb (synth :name owner) "-SET") (bb-type 'Set :template (bb-type (synth :name owner)))))
                   (:one-to-many (bb-pair (synth :name owner) (bb-type (synth :name owner))))
                   (:many-to-many (bb-pair (symb (synth :name owner) "-SET") (bb-type 'Set :template (bb-type (synth :name owner)))))))
  (:foreign-key () (case cardinality
                   (:one-to-one (foreign-key (synth :name owner) (synth :name owner)))
                   (:many-to-one (foreign-key (synth :name owner) (synth :name owner)))
                   (:one-to-many (foreign-key (synth :name owner) (synth :name owner)))
                   (:many-to-many ()))))

(defparameter *entities* (make-hash-table))
(defparameter *relationships* (make-hash-table))

(defmacro defent (name entity)
  `(progn (defparameter ,name ,entity) 
         (setf (gethash ',name *entities*) ,name)))

(defmacro defrel (name relationship)
  `(progn (defparameter ,name ,relationship) 
          (setf (gethash ',name *relationships*) ,name)))




