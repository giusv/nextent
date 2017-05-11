(in-package :data)

(defprim jnull ()
  (:pretty () (list 'jnull))
  (:string () (doc:text ""))
  (:model () nil))

(defprim jbool (value)
  (:pretty () (list 'jbool (list :value (synth :pretty value))))
  (:string () (synth :string value))
  (:model () (bb-const value)))

(defprim jnumber (value)
  (:pretty () (list 'jnumber (list :value (synth :pretty value))))
  (:string () (synth :string value))
  (:model () (bb-const value)))

(defprim jstring (value)
  (:pretty () (list 'jstring (list :value (synth :pretty value))))
  (:string () (synth :string value))
  (:model () (bb-const value)))

(defprim jarray (&rest values)
  (:pretty () (list 'jarray (list :values (synth-all :pretty values))))
  (:string () (doc:brackets (apply #'doc:punctuate (doc:comma) t (synth-all :string values)) :padding 1 :newline nil))
  (:model () (apply #'bb-array (synth-all :model values))))

(defprim jobject (&rest values)
  (:pretty () (list 'jobject (list :values (synth-plist :pretty values))))
  (:string () (doc:braces 
               (doc:nest 4 (apply #'doc:punctuate (doc:comma) t 
                                  (synth-plist-merge 
                                   #'(lambda (pair) (doc:hcat (doc:text "\"~a\": " (doc:lower-camel (first pair)))
                                                          (synth :string (second pair)))) 
                                   values)))
               :newline t))
  (:model () (apply #'bb-object (synth-plist :model values))))

;; (defprim (jobject2 (&rest (values (plist json))))
;;   (:pretty () (list 'alt (:elements (synth-plist :pretty elements)))))

;; (defparameter *json* (jobject :name (jstring (const "john"))
;; 			      :age (jnumber 31)
;; 			      :numbers (jarray (jnumber 98) (jnumber 234))
;; 			      :address (jobject :city (jstring (const "rome"))
;; 						:state (jstring (const "usa")))))

;; (synth :string *json*)
;; (synth output (synth :string (jstring (const "ss"))) 0)
;; (synth output (synth :string *json*) 0)
