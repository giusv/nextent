(in-package :validator)

(defprim required () 
  (:pretty () (list 'required))
  (:annotation () (bb-annotation '|NotNull|)))
