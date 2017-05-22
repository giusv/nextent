(in-package :server)

(defprim empty ()
  (:pretty () (list 'empty))
  
  (:logic () ()))


(symbol-macrolet ((x '(value x))
                  (y '(value y)))
  (pprint (list x y)))

(defmacro with-fields ((&rest names) format &body actions)
  `(symbol-macrolet ,(mapcar #`(,a1 (expr:attr ,format ',a1)) names)
     ,@actions))

(with-fields (name place) 'json (pprint (list name place)))

(defprim create-instance% (entity result bindings)
  (:pretty () (list 'create-instance (list :entity entity :result result :bindings (synth-plist :pretty bindings))))
  
  (:logic () (let ((new-entity (bb-dynamic (gensym (symbol-name (synth :name entity))))))
               (bb-list
                (bb-assign new-entity (bb-new (synth :name entity)))
                (synth-plist-merge
                 (lambda (binding)
                   (bb-chain new-entity
                             (bb-call (symb "SET-" (car binding)) (synth :blub (cadr binding)))))
                 bindings)
                (bb-assign (bb-dynamic result)
                           (bb-chain (bb-dynamic 'entity-manager)
                                     (bb-call 'persist)))))))
(defmacro create-instance (entity &rest bindings)
  `(create-instance% ,entity (gensym) (list ,@bindings)))


