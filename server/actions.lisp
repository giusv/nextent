(in-package :server)

(defprim empty ()
  (:pretty () (list 'empty))
  (:logic () ()))


;; (symbol-macrolet ((x '(value x))
;;                   (y '(value y)))
;;   (pprint (list x y)))

(defmacro with-fields ((&rest names) format &body actions)
  `(symbol-macrolet ,(mapcar #`(,(car a1) (expr:attr ,format ',(cadr a1))) names)
     ,@actions))

(defprim create-entity% (entity result bindings)
  (:pretty () (list 'create-entity (list :entity entity :result result :bindings (synth-plist :pretty bindings))))
  
  (:logic () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                    (new-entity (bb-dynamic new-entity-name)))
               (bb-list
                (bb-statement (bb-pair new-entity-name (bb-type (synth :name entity)) 
                                       :init (bb-new (synth :name entity))))
                (synth-plist-merge
                 (lambda (binding)
                   (bb-statement (bb-chain new-entity
                                           (bb-call (symb "SET-" (car binding)) (synth :blub (cadr binding))))))
                 bindings)
                (bb-statement (bb-chain (bb-dynamic 'entity-manager)
                                        (bb-call 'persist new-entity)))
                (bb-return (bb-chain new-entity (bb-call (symb "GET-" (synth :name (synth :primary entity)))) (bb-call 'to-string))))))
  (:type () (bb-object-type (synth :name entity)))
  (:blub () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                    (new-entity (bb-dynamic new-entity-name)))
               (bb-list
                (bb-statement (bb-pair new-entity-name (bb-type (synth :name entity)) 
                                       :init (bb-new (synth :name entity))))
                (synth-plist-merge
                 (lambda (binding)
                   (bb-statement (bb-chain new-entity
                                           (bb-call (symb "SET-" (car binding)) (synth :blub (cadr binding)))))) 
                 bindings)
                (bb-return new-entity)))))


(defmacro create-entity (entity &rest bindings)
  `(let ((result (gensym (symbol-name (symb (synth :name ,entity))))))
     (values (create-entity% ,entity result (list ,@bindings)) (expr:variab result))))

(defprim update-entity% (entity id result bindings)
  (:pretty () (list 'update-entity (list :entity entity :id :id :result result :bindings (synth-plist :pretty bindings))))
  
  (:logic () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                    (new-entity (bb-dynamic new-entity-name)))
               (bb-list
                (bb-statement (bb-pair new-entity-name (bb-type (synth :name entity)) 
                                       :init (bb-chain (bb-dynamic 'entity-manager)
                                                       (bb-call 'find (bb-chain (bb-static (synth :name entity)) (bb-dynamic 'class))
                                                                (synth :call id)))))
                (synth-plist-merge
                 (lambda (binding)
                   (bb-statement (bb-chain new-entity
                                           (bb-call (symb "SET-" (car binding)) (synth :blub (cadr binding))))))
                 bindings))))
  (:type () (bb-object-type (synth :name entity)))
  (:blub () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                   (new-entity (bb-dynamic new-entity-name)))
              (bb-list
               (bb-statement (bb-pair new-entity-name (bb-type (synth :name entity)) 
                                       :init (bb-chain (bb-dynamic 'entity-manager)
                                                       (bb-call 'find (bb-dynamic id)))))
               (synth-plist-merge
                (lambda (binding)
                  (bb-statement (bb-chain new-entity
                                          (bb-call (symb "SET-" (car binding)) (synth :blub (cadr binding)))))) 
                bindings)
               (bb-return new-entity)))))


(defmacro update-entity (entity id &rest bindings)
  `(let ((result (gensym (symbol-name (symb (synth :name ,entity))))))
     (values (update-entity% ,entity ,id result (list ,@bindings)) (expr:variab result))))


(defprim find-entity% (entity result id)
  (:pretty () (list 'find-entity (list :entity entity :result result :id id)))
  (:logic () (bb-statement (bb-pair result (bb-type (synth :name entity)) 
                                    :init (bb-chain (bb-dynamic 'entity-manager)
                                                    (bb-call 'find (bb-chain (bb-static (synth :name entity)) (bb-dynamic 'class)) 
                                                             (synth :call id)))))))

(defmacro find-entity (entity id)
  `(let ((result (gensym (symbol-name (synth :name ,entity)))))
     (values (find-entity% ,entity result ,id) (expr:variab result))))

(defprim exec-query% (query result)
  (:pretty () (list 'exec-query (list :query (synth :pretty query) :result :result)))
  (:logic () (bb-statement (bb-chain (bb-dynamic 'entity-manager)
                                     (synth :call query)))))
(defmacro exec-query (query)
  `(let ((result (gensym (symbol-name (synth :name ,query)))))
     (values (exec-query% ,query result) (expr:variab result))))

(defprim concat% (&rest actions)
  (:pretty () (list 'concat (synth-all :pretty actions)))
  (:logic () (bb-list (synth-all :logic actions))))

(defmacro concat (&rest bindings)
  (let ((new-bindings (mapcar #'(lambda (binding)
			  (cons (gensym) binding)) 
			      bindings)))
    `(bindall ,new-bindings
      (concat% ,@(mapcar #'car new-bindings)))))


(defprim mu% (input command)
  (:pretty () (list 'mu (list :input input :command command)))
  (:logic () (bb-arrow (list (bb-dynamic input)) (synth :logic command)))
  (:blub () (bb-arrow (list (bb-dynamic input)) (synth :blub command)))
  (:type () (synth :type command)))

(defmacro mu (input command)
  `(let* ((,input ',input)) 
     (mu% ,input ,command)))


(defprim mapcomm (command collection)
  (:pretty () (list 'mapcomm (list :command command :collection collection)))
  (:logic () (bb-statement (bb-chain (synth :blub collection) 
                                     (bb-call 'map (synth :logic command)))))
  (:blub () (bb-chain (bb-static 'arrays)
                      (bb-call 'stream (synth :blub collection))
                      (bb-call 'map (synth :blub command))
                      (bb-call 'to-array)
                      :as (synth :type this)))
  (:type () (bb-array-type (synth :type command))))

(defprim fork (condition success failure)
  (:pretty () (list 'fork (list :condition condition :success success :failure failure)))
  (:logic () (bb-if (synth :blub condition) 
                    (synth :logic success) 
                    (synth :logic failure))))

(defprim create-transfer% (target result bindings)
  (:pretty () (list 'create-transfer (list :target target :result result :bindings (synth-plist :pretty bindings))))
  (:logic () (let* ((new-class (symb (synth :name target) "-J-T-O")))
               (bb-list
                (bb-statement (bb-pair result (bb-type new-class)
                                       :init (bb-new new-class)))
                (synth-plist-merge
                 (lambda (binding)
                   (bb-statement (bb-chain (bb-dynamic result)
                                           (bb-call (symb "SET-" (car binding)) (synth :blub (cadr binding))))))
                 bindings)))))

(defmacro create-transfer (target &rest bindings)
  `(let ((result (gensym (symbol-name (symb (synth :name ,target) "-J-T-O")))))
     (values (create-transfer% ,target result (list ,@bindings)) (expr:variab result))))

(defprim respond (code &optional item)
  (:pretty () (list 'respond (list :code code :item item))) 
  (:logic () (case code
               ((:ok) (bb-return (bb-dynamic (synth :name item))))
               ((:no-content) (bb-empty))
               ((:not-found) (bb-throw (bb-new 'exception))))))

