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
                (bb-chain (bb-dynamic 'entity-manager)
                          (bb-call 'persist new-entity))
                (bb-return (bb-chain new-entity (bb-call (symb "GET-" (synth :name (synth :primary entity))))))))))
(defmacro create-instance (entity &rest bindings)
  `(let ((result (gensym)))
     (values (create-instance% ,entity result (list ,@bindings)) result)))


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
  (:logic () (bb-arrow (list (bb-dynamic input)) (synth :logic command))))

(defmacro mu (input command)
  `(let* ((,input ',input)) 
     (mu% ,input ,command)))


(defprim mapcomm (command collection)
  (:pretty () (list 'mapcomm (list :command command :collection collection)))
  (:logic () (bb-chain (synth :blub collection) 
                       (bb-call 'map (synth :logic command)))))

