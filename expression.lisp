(in-package :expr)

(defprim const (lit)
  (:pretty () (list 'const (list :lit lit)))
  (:req () (doc:double-quotes (doc:text "~a" lit)))
  ;; (:url () (doc:text "~a" lit))
  ;; (:chunk () lit)
  (:string () (synth :template this))
  (:template () (cond ((stringp lit) (doc:double-quotes (doc:text "~a" lit)))
                      ((numberp lit) (doc:text "~a" lit))
                      ((eq nil lit) (doc:text "false"))
                      ((eq t lit) (doc:text "true"))
                      (t (error "unknown constant type")))))

(defprim attr (name exp)
  (:pretty () (list 'attr (list :name name :exp exp)))
  (:req () (html:taglist (html:span-color (doc:lower-camel name)) 
                         (doc:text "!~a"  (doc:lower-camel exp))))
  (:string () (doc:text "~a!~a" (doc:lower-camel (synth :name name)) (doc:lower-camel exp)))
  (:template () (doc:text "{{~a.~a}}" (doc:lower-camel (synth :name name)) (doc:lower-camel exp))))





;; (defprim (argument ((name symbol)))
;;   (:pretty () `(argument (:name ,name)))
;;   ;; (:req () (text "~a" name))
;;   (:html () (span-color (lower-camel name))))

;; (defprim (attr ((name datasource)
;;                     (exp symbol)))
;;   (:pretty () `(attr (:name ,name :exp ,exp)))
;;   ;; (:req () (text "~a!~a" (lower-camel (synth name name)) (lower-camel exp)))
;;   (:html () (multitags (span-color (lower-camel (synth name name))) 
;;                          (text "!~a"  (lower-camel exp))))
;;   ;; (:html () (span (list :class "label label-danger") (text "~a!~a" (lower-camel (synth name name)) (lower-camel exp))))
;;   (:string () (text "~a!~a" (lower-camel (synth name name)) (lower-camel exp))))

;; (defprim (variab ((name string)))
;;   (:pretty () `(attr (:name ,name)))
;;   (:string () (textify name))
;;   (:req () (text "~a" name))
;;   (:html () (span-color (lower-camel name))))
;;   ;; (:html () (span (list :class "label label-danger") (text "~a" name))))

;; (defprim (value ((elem element)))
;;   (:pretty () `(value (:elem ,elem)))
;;   ;; (:req () (text "valore dell'elemento:" (lower-camel (synth name elem))))
;;   (:html () (span-color (lower-camel (synth name elem))))
;;   ;; (:html () (span (list :class "label label-default") (text "valore dell'elemento: ~a" (synth name elem))))1
;;   (:url () (brackets (text "val(~a)" (lower-camel (synth name elem)))))
;;   (:chunk () (text "val(~a)" (lower-camel (synth name elem))))
;;   (:string () (text "val(~a)" (lower-camel (synth name elem)))))

;; (defprim (payload ((elem element)))
;;   (:pretty () `(payload (:elem ,elem)))
;;   ;; (:html () (pre nil (synth :string (synth :model elem))))
;;   (:html () (synth :string (synth :model elem))))

;; (defprim (status ((action action)))
;;   (:pretty () `(status (:action ,action)))
;;   (:html () (text "Codice HTTP di risposta")))

;; (defprim (autokey ())
;;   (:pretty () `(autokey))
;;   (:html () (text "Chiave generata automaticamente")))

;; (defprim (current-date ())
;;   (:pretty () `(current-date))
;;   (:html () (text "Data odierna")))


;; (defprim (cat (&rest (exps exp)))
;;   (:pretty () `(cat (:exps ,(synth-all :pretty exps))))
;;   (:html () (brackets (hcat 
;;                          (text "concatenazione delle espressioni:")
;;                          (apply #'punctuate (comma) t (synth-all :req exps)))))
;;   ;; (:html () (apply #'span (list :class "label label-default") (synth-all :html exps)))
;;   (:string () (text "~{~a~^ ++ ~}" (synth-all :string exps))))

;; (defmacro def-bexp (operator &optional (arity 0))
;;   (let ((name (symb "+" operator "+")))
;;     `(defprim bexp (,name 
;; 		    ,(if (eq arity 'unbounded)
;; 			 `(&rest (exps bexp))
;; 			 (loop for i from 1 to arity collect `(,(symb "EXP" i) exp))))
;;        (:html () (brackets (hcat (text "~a " (lower-camel ',name))
;;                                    ,@(if (eq arity 'unbounded)
;;                                          `((parens (apply #'punctuate (comma) nil 
;;                                                           (synth-all :doc 
;;                                                                      (synth-all :html exps)))))
;;                                          `((punctuate (comma) nil ,@(loop for i from 1 to arity collect 
;;                                                                          `(synth :doc (synth :html ,(symb "EXP" i))))))))))

;;        (:pretty () (list ',name 
;; 			 ,(if (eq arity 'unbounded)
;; 			      `(list :exps (synth-all :pretty exps))
;; 			      `(list ,@(apply #'append (loop for i from 1 to arity collect (list (keyw "EXP" i) `(synth :pretty ,(symb "EXP" i)))))))))
;;        ;; (:html () (span nil (synth :req 
;;        ;; 				    ,(if (eq arity 'unbounded)
;;        ;; 					 `(apply #',name exps)
;;        ;; 					 `(,name ,@(loop for i from 1 to arity collect (symb "EXP" i)))))))
;;        )))
;; ;; (def-bexp and 2)
;; (defmacro def-bexps (&rest bexps)
;;   `(progn
;;      ,@(mapcar #'(lambda (bexp)
;; 		   `(def-bexp ,(car bexp) ,@(cdr bexp)))
;; 	       bexps)))

;; ;;(def-bexp true)
;; ;; (def-bexp equal 2)

;; (def-bexps (true) (false) (and unbounded) (or unbounded) (not 1) (equal 2) (less-than 2) (greater-than 2) (null 1))

