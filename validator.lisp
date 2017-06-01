(in-package :validator)

(defprim required () 
  (:pretty () (list 'required))
  (:valexp (name) (bb-null name)))
