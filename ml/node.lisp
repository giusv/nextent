(in-package :ml)

(defprim sigm (name &rest inputs)
  (:pretty () (list 'sigm (list :name name :inputs (synth-all pretty inputs))))
  (:allocation () (labels ((init (prefix len) 
                             (c-statement (c-pair (symb prefix "_" name) 
                                                  (c-array-type (c-type 'double) len)))))
                    (c-list (init 'x (length inputs))
                            (init 'y 1)
                            (init 'w (length inputs))
                            (init 'dy 1)
                            (init 'delta 1)
                            (init 'de (length inputs)))))
  (:forward () (let ((i (gensym "i")))
                 (c-list 
                  (c-statement (c-pair i (c-type 'int)))
                  (c-for i (c-const 0) (c-const (length inputs)) 
                         (c-increment (c-element (symb 'y "_" name) (c-const i))
                                      (c-empty)))))))


;; (let ((p (sigm '|1|)))
;;   (pprint (synth :output (synth :c (synth :allocation p)) 0))
;;   (pprint (synth :output (synth :c (synth :forward p)) 0)))

;; (let ((p (sigm 23))
;;       (q (sigm 24)))
;;   (pprint (synth :pandoric-set p 'n 15))
;;   ;; (pprint (funcall p :pandoric-set 'n 3))
;;   (pprint (synth :pretty p))
;;   (pprint (synth :pretty q))
;;   ;; (pprint (synth :pretty (setf (get-pandoric #'sigm 'n) 3)))
;;   ;; (pprint (synth :pretty (synth :pandoric-set p 'n 3) ))
;;   )
