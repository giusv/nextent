(data:deformat place-format
  (data:jsobject 'place "aaa"
                 (data:jsprop 'name t (data:jsstring 'name "aaa"))))

(data:deformat city-format
  (data:jsobject 'city "aaa"
                 (data:jsprop 'name t (data:jsstring 'name "aaa"))
                 (data:jsprop 'places t (data:jsarray 'places "aaa" place-format))))

(data:deformat trip-format
  (data:jsobject 'trip "aaa"
                 (data:jsprop 'name t (data:jsstring 'name "aaa"))
                 (data:jsprop 'cities t (data:jsarray 'cities "aaa" city-format))))


(data:defent trip-entity
    (data:entity 'trip
                 :primary (data:attribute 'id (data:atype :integer))
                 :fields (list (data:attribute 'name (data:atype :string :size 20))
                               (data:attribute 'when (data:atype :string :size 20)))))

;; (synth :output (synth :sql (q (expr:const "name"))) 0)
;; (synth :output (synth :java (synth :annotation q)) 0)
;; (synth :output (synth :java (synth :call (q (expr:const 1)))) 0)
(data:defquery trip-by-name (name) trip-entity
  (data:with-queries ((tr (data:relation 'trips))
                      (ct (data:relation 'cities)))
    (data:project (data:restrict (data:product tr ct)
                                 (expr:+and+ 
                                  (expr:+equal+ (expr:attr tr 'id)
                                                (expr:attr ct 'id))
                                  (expr:+equal+ (expr:attr tr 'name)
                                                name))))))


(data:defent city-entity
    (data:entity 'city 
                 :primary (data:attribute 'id (data:atype :integer))
                 :fields (list (data:attribute 'name (data:atype :string :size 20)))))

(data:defent place-entity
    (data:entity 'place 
                 :primary (data:attribute 'id (data:atype :integer))
                 :fields (list (data:attribute 'name (data:atype :string :size 20))
                               (data:attribute 'sights (data:atype :string :size 20)))))

(data:defrel trip-city
    (data:relationship 'trip-city trip-entity city-entity :one-to-many))

(data:defrel city-place
    (data:relationship 'city-place city-entity place-entity :one-to-many))

(defparameter place-item
  (server:rest-item 'place ((place (url:path-parameter 'place :integer))) 
                    (list 
                     (server:rest-get () 
                                      (server:concat
                                       (inst (server:find-entity place-entity place)) 
                                       (ret (server:with-fields ((name name)) inst
                                              (server:create-transfer place-format 
                                                                      :name name))) 
                                       ((server:respond :ok ret))))
                     (server:rest-put place-format 
                                      (server:concat
                                       (ret (server:with-fields ((place-name name)) place-format
                                              (server:update-entity place-entity
                                                                    place
                                                                    :name place-name))) 
                                       ((server:respond :no-content)))))))

(defparameter place-collection 
  (server:rest-collection 'places 
                          (list
                           (server:rest-get () (server:empty))
                           (server:rest-post place-format nil (server:empty)))
                          place-item))

(defparameter city-item
  (server:rest-item 'city  ((city (url:path-parameter 'city :integer)))
                    (list (server:rest-get () (server:empty)) 
                          (server:rest-put city-format (server:empty)))
                    place-collection))

(defparameter city-collection 
  (server:rest-collection 'cities  (list (server:rest-get ((city (url:query-parameter 'city :integer :validators (list (validator:required))))) (server:empty)) 
                                         (server:rest-post city-format nil (server:empty)))
                          city-item))

(defparameter trip-item 
  (server:rest-item 'trip ((trip (url:path-parameter 'trip :integer))) 
                    (list (server:rest-get () (server:empty)) 
                          (server:rest-put trip-format
                                           (server:empty))) 
                    city-collection))

(defparameter trip-collection
  (server:rest-collection 
   'trips
   (list (server:rest-get ((name (url:query-parameter 'name :string))) 
                          (server:concat
                           (trip-list (server:exec-query (trip-by-name name)))
                           (ret (server:mapcomm 
                                 (server:mu trip
                                            (server:with-fields ((trip-name name)) trip
                                              (server:create-transfer trip-format 
                                                                      :name trip-name
                                                                      :cities (server:mapcomm 
                                                                               (server:mu city
                                                                                          (server:with-fields ((city-name name)) city
                                                                                            (server:create-transfer city-format 
                                                                                                                    :name city-name)))
                                                                               trip-list) )))
                                 trip-list))
                           ((server:respond :ok ret)))) 
         (server:rest-post% trip-format 
                            (server:with-fields ((trip-name name) (cities cities)) trip-format
                              (server:concat
                               (inst (server:create-entity 
                                      trip-entity
                                      :name trip-name
                                      :city-list (server:mapcomm 
                                                  (server:mu city
                                                             (server:with-fields ((city-name name) (places places)) city-format
                                                               (server:create-entity 
                                                                city-entity
                                                                :name city-name
                                                                :place-list (server:mapcomm 
                                                                             (server:mu place
                                                                                        (server:with-fields ((place-name name) (places places)) place-format
                                                                                          (server:create-entity 
                                                                                           place-entity
                                                                                           :name place-name)))
                                                                             places))))
                                                  cities)))))))
   trip-item))
(server:defservice server (server:rest-service 'trip-service (url:void) trip-collection))

(let* ((package '|it.bancaditalia.nextent|)
       (basedir "D:/Dati/Profili/m026980/workspace/nextent/src/main/java/it/bancaditalia/nextent/")
       ;; (basedir "D:/giusv/temp/nextent/")
       (app-entities (loop for value being the hash-values of data:*entities* collect value))
       (app-formats (loop for value being the hash-values of data:*formats* collect value))
       (app-services (loop for value being the hash-values of server:*services* collect value))) 
  ;; (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
  ;; (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
  ;; (mapcar (lambda (component) 
  ;;           (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
  ;;         app-components)
  (mapcar (lambda (entity) 
            (let ((filename (mkstr basedir "model/" (upper-camel (synth :name entity)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :entity entity package)))))))
          app-entities) 
  (mapcar (lambda (format) 
            (let ((filename (mkstr basedir "jto/" (upper-camel (symb (synth :name format) '|-J-T-O|)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :jto format package)))))))
          app-formats) 
  (mapcar (lambda (service) 
            (let ((filename (mkstr basedir "service/" (upper-camel (synth :name service)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :jax-class service package)))))))
          app-services)
  (mapcar (lambda (service) 
            (let ((filename (mkstr basedir "ejb/" (upper-camel (symb (synth :name service) '|-Bean|)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :bean-class service package)))))))
          app-services))
