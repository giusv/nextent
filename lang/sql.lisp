(in-package :sql)

(defprim select (from &key fields where)
  (:pretty () (list 'select (list :from from :fields (synth-all :pretty fields) :where (synth :pretty where))))
  (:text () (vcat (apply #'doc:hcat+
                         (append* (doc:text "SELECT")
                                      (if fields (apply #'punctuate (comma) nil (synth-all :text fields))
                                          (doc:text "*")))))))
