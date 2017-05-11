(in-package :server)

(defprim rest-service (name url &rest collections)
  (:pretty () (list 'rest-service (list :name name :url (synth :pretty url) :collections (synth-all :pretty collections))))
  (:interface () (bb-unit name)))

(defprim rest-collection (name url resource &rest actions)
  (:pretty () (list 'rest-collection (list :name name :url (synth :pretty url) :resource (synth :pretty resource) 
                                        :actions (synth-all :pretty actions))))
  (:interface () (bb-unit name)))

(defprim rest-resource (name url &rest actions)
  (:pretty () (list 'rest-resource (list :name name :url (synth :pretty url)
                                         :actions (synth-all :pretty actions))))
  (:interface () (bb-unit name)))

(defmacro defendp (name)
  (let ((full-name (symb "REST-" name)))
    `(defprim ,full-name (url process)
         (:interface () 
                     (bb-annotation ,(string-upcase name))
                     ))))

(defendp get)
(defendp post)
(defendp put)
(defendp delete)


