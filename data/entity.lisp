(in-package :data)

(defprim atype (name &key (size 0 size-supplied-p) (nullable t))
  (:pretty () (list 'atype (list :name name :size size :nullable nullable))) 
  (:ddl () (doc:hcat (case name
                       (:string (doc:text "VARCHAR2"))
                       (:integer (doc:text "INTEGER")))
                     (if size-supplied-p (doc:parens (doc:text "~a" size)))
                     (if (not nullable) (doc:text " NOT NULL"))))
  (:entity () (case name
                       (:string (bb-type :string))
                       (:integer (bb-type :integer)))))

(defprim attribute (name type &optional desc)
  (:pretty () (list 'attribute (list :name name :type (synth :pretty type) :desc desc))) 
  (:entity (&rest annotations) (bb-with-annotations 
                                (cons (bb-annotation2 '|Column| (bb-object :|name| (bb-const (mkstr name))))
                                      annotations)
                                (bb-statement (bb-pair name (synth :entity type) :private t))
                                :newline t))
  (:accessors () (list (bb-method (doc:text "get~a" (upper-camel name)) nil (synth :entity type)
                                  (bb-return (bb-dynamic name)))
                       (bb-method (doc:text "set~a" (upper-camel name)) (list (bb-pair name (synth :entity type))) (bb-type :void)
                                  (bb-statement (bb-assign (bb-chain (bb-dynamic 'this) 
                                                        (bb-dynamic name))
                                              (bb-dynamic name))))))
  (:paramdecl () (bb-pair name (synth :entity type)))
  (:ddl () (doc:hcat (doc:text "~20a" name)
                     (synth :ddl type))))

(defprim primary-key (attribute)
  (:pretty () (list 'primary-key (list :attribute (synth :pretty attribute)))) 
  (:entity () (synth :entity attribute (bb-annotation2 '|Id|)))
  (:accessors () (synth :accessors attribute))
  (:paramdecl () (synth :paramdecl attribute))
  (:ddl () (doc:hcat (synth :ddl attribute) (doc:text " NOT NULL PRIMARY KEY"))))

(defprim foreign-key (attribute reference)
  (:pretty () (list 'foreign-key (list :attribute (synth :pretty attribute) :reference (synth :pretty reference)))) 
  (:ddl () (let ((new-attribute (attribute (symb (synth :name reference) "_" (synth :name attribute)) (synth :type attribute)))) 
             ;; (synth :pandoric-set attribute 'name (symb (synth :name reference) "-" (synth :name attribute)))
             
             (doc:hcat (synth :ddl new-attribute)
                       (doc:text " REFERENCES ~a(~a)" (synth :name reference) (synth :name (synth :primary reference)))))))

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

(defun get-queries (entity)
  (loop for q being the hash-values of *queries*
     ;; do (pprint (synth :name (synth :entity q)))
       ;; (pprint (synth :pretty q))
       ;; (pprint (synth :name entity))
       ;; (pprint (eq (synth :name (synth :entity q)) (synth :name entity)))
     collect (if (eq (synth :name (synth :entity q)) (synth :name entity)) q)))
(defprim entity (name &key desc primary fields)
  (:pretty () (list 'entity :name name
                    :desc desc
                    :primary (synth :pretty primary)
                    :fields (synth-all :pretty fields)))
  (:entity (package) (bb-unit name
                              (bb-package (symb package '|.model|))
                              (bb-import '|javax.persistence| '|Column| '|Entity| '|Id| '|Table| '|ManyToOne| '|OneToMany| '|OneToOne| '|ManyToMany| '|NamedQueries| '|NamedQuery|)
                              ;; (bb-import '|java.util| '|List|)
                              (bb-with-annotations 
                               (list 
                                (bb-annotation2 '|SuppressWarnings| (bb-const "unused"))
                                (bb-annotation2 '|Entity|)
                                (bb-annotation2 '|Table| (bb-object :|name| (bb-const (mkstr name))))
                                (aif (get-queries this)
                                     (bb-annotation2 '|NamedQueries| 
                                                     (apply #'bb-array (synth-all :annotation it)))))
                               (bb-class name
                                         :public t
                                         :fields (append*
                                                  (synth :entity (primary-key primary))
                                                  (synth-all :entity fields)
                                                  (synth-all :source (get-sources this))
                                                  (synth-all :target (get-targets this)))
                                         :methods (append (synth :accessors primary)
                                                          (apply #'append (synth-all :accessors fields)))))))
  (:eao-interface () (bb-interface (symb name "-EAO")
                                   :public t
                                   :methods (list (bb-method (doc:textify (lower-camel (symb "ADD-" name))) 
                                                             (remove nil (append (synth-all :paramdecl fields)
                                                                                 (synth-all :target-paramdecl (get-sources this))
                                                                                 (synth-all :source-paramdecl (get-targets this))))
                                                             (bb-type name)) 
                                                  (bb-method (doc:textify (lower-camel (symb "CANCEL-" name)))
                                                             (list (synth :paramdecl primary)) 
                                                             (bb-type name)))))
  (:ddl () (doc:vcat (doc:text "CREATE TABLE ~a" name)
                     (doc:parens (doc:nest 4 (apply #'doc:punctuate (doc:comma) t (synth-all :ddl (remove nil (append* (primary-key primary) fields
                                                                                                                       (synth-all :target-foreign-key (get-sources this))
                                                                                                                       (synth-all :source-foreign-key (get-targets this))))))) :newline t))))

(defun stuff (annotations name type)
  (bb-list (bb-with-annotations annotations
                                (bb-statement (bb-pair name type :private t)))
           (bb-method (doc:text "get~a" (upper-camel name)) 
                      nil type
                      (bb-return (bb-dynamic name)))
           (bb-method (doc:text "set~a" (upper-camel name))
                      (list (bb-pair name type)) (bb-type :void)
                      (bb-statement (bb-assign (bb-chain (bb-dynamic 'this) 
                                                         (bb-dynamic name))
                                               (bb-dynamic name))))))
(defprim relationship (name owner subordinate cardinality &optional (participation t))
  (:pretty () (list 'relationship (list :name name
                                        :owner (synth :pretty owner)
                                        :subordinate (synth :pretty subordinate)
                                        :cardinality cardinality
                                        :participation participation)))
  (:source () (case cardinality
                (:one-to-one (stuff (list (bb-annotation2 '|OneToOne|)) 
                                    (synth :name subordinate)
                                    (bb-type (synth :name subordinate))))
                (:many-to-one (stuff (list (bb-annotation2 '|ManyToOne|)) 
                                     (synth :name subordinate)
                                     (bb-type (synth :name subordinate))))
                (:one-to-many (stuff (list (bb-annotation2 '|OneToMany|
                                                           (bb-object :|mappedBy| (bb-const (mkstr (lower-camel (synth :name owner)))))))
                                     (symb (synth :name subordinate) "-LIST")
                                     (bb-type (synth :name subordinate) :array t)))
                (:many-to-many (stuff (list (bb-annotation2 '|ManyToMany|))
                                      (symb (synth :name subordinate) "-LIST")
                                      (bb-type (synth :name subordinate) :array t))))) 
  (:target () (case cardinality
                (:one-to-one (if participation 
                                 (stuff (list (bb-annotation2 '|OneToOne|
                                                              (bb-object  :|mappedBy| (bb-const (mkstr (lower-camel (synth :name owner))))
                                                                          :|optional| (bb-const (mkstr '|false|))))) 
                                        (synth :name owner) (bb-type (synth :name owner)))))
                (:many-to-one (stuff 
                               (list (bb-annotation2 '|OneToMany|
                                                    (bb-object :|mappedBy| (bb-const (mkstr (lower-camel (synth :name owner))))))) 
                               (symb (synth :name owner) "-LIST") (bb-type (synth :name owner) :array t)))
                (:one-to-many (stuff 
                               (list (bb-annotation2 '|ManyToOne|)) 
                               (synth :name owner) (bb-type (synth :name owner))))
                (:many-to-many (stuff 
                                (list (bb-annotation2 '|ManyToMany|
                                                      (bb-object :|mappedBy| (bb-const (mkstr (lower-camel (symb (synth :name subordinate) "-LIST"))))))) 
                                (symb (synth :name owner) "-LIST") (bb-type (synth :name owner) :array t)))))
  (:target-paramdecl () (case cardinality
                          (:one-to-one (if participation (bb-pair (synth :name subordinate) (bb-type (synth :name subordinate)))))
                          (:many-to-one (bb-pair (synth :name subordinate) (bb-type (synth :name subordinate))))
                          (:one-to-many (bb-pair (symb (synth :name subordinate) "-LIST") (bb-type (synth :name subordinate) :array t)))
                          (:many-to-many (bb-pair (symb (synth :name subordinate) "-LIST") (bb-type (synth :name subordinate) :array t)))))
  (:source-paramdecl () (case cardinality
                          (:one-to-one (bb-pair (synth :name owner) (bb-type (synth :name owner))))
                          (:many-to-one (bb-pair (symb (synth :name owner) "-LIST") (bb-type (synth :name owner) :array t)))
                          (:one-to-many (bb-pair (synth :name owner) (bb-type (synth :name owner))))
                          (:many-to-many (bb-pair (symb (synth :name owner) "-LIST") (bb-type (synth :name owner) :array t)))))
  (:target-foreign-key () (case cardinality
                            (:one-to-one (foreign-key (synth :primary owner) subordinate))
                            (:many-to-one (foreign-key (synth :primary owner) subordinate))
                            (:one-to-many ())
                            (:many-to-many ())))
  (:source-foreign-key () (case cardinality
                            (:one-to-one ())
                            (:many-to-one ())
                            (:one-to-many (foreign-key (synth :primary owner) owner))
                            (:many-to-many ())))
  (:ddl () (case cardinality
             (:many-to-many (doc:vcat (doc:text "CREATE TABLE ~a" (symb (synth :name owner) "_" (synth :name subordinate)))
                                      (doc:parens (doc:nest 4 (apply #'doc:punctuate (doc:comma) t 
                                                                     (cons (synth :ddl (primary-key (attribute 'id (atype :integer))))
                                                                           (synth-all :ddl (list (foreign-key (synth :primary owner) owner)
                                                                                                 (foreign-key (synth :primary subordinate) subordinate)))))) :newline t)))
             (:one-to-one ())
             (:one-to-many ())
             (:many-to-one ()))))

(defparameter *entities* (make-hash-table))
(defparameter *relationships* (make-hash-table))

(defmacro defent (name entity)
  `(progn (defparameter ,name ,entity) 
         (setf (gethash ',name *entities*) ,name)))

(defmacro defrel (name relationship)
  `(progn (defparameter ,name ,relationship) 
          (setf (gethash ',name *relationships*) ,name)))




