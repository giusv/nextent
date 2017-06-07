(in-package :expr)

(defprim const (lit)
  (:pretty () (list 'const (list :lit lit)))
  (:req () (doc:double-quotes (doc:text "~a" lit)))
  ;; (:url () (doc:text "~a" lit))
  ;; (:chunk () lit)
  (:string () (synth :template this))
  (:template () (cond ((stringp lit) (doc:text "~a" lit))
                      ((numberp lit) (doc:text "~a" lit))
                      ((eq nil lit) (doc:text "false"))
                      ((eq t lit) (doc:text "true"))
                      (t (error "unknown constant type"))))
  (:blub () (lang:bb-const lit))
  (:sql () (doc:text "~a" lit)))

(defprim attr (name exp)
  (:pretty () (list 'attr (list :name name :exp exp)))
  (:req () (html:taglist (html:span-color (doc:lower-camel name)) 
                         (doc:text "!~a"  (doc:lower-camel exp))))
  (:string () (doc:text "~a!~a" (doc:lower-camel (synth :name name)) (doc:lower-camel exp)))
  (:template () (doc:text "{{~a.~a}}" (doc:lower-camel name) (doc:lower-camel exp)))
  (:blub () (lang:bb-chain (lang:bb-dynamic (synth :name name)) (lang:bb-call (symb "GET-" exp))))
  (:sql () (doc:hcat (doc:text "~a" (synth :name name)) (doc:text ".~a" exp))))

(defprim value (exp)
  (:pretty () (list 'value (list :exp exp)))
  (:req () (html:taglist (html:span-color (doc:lower-camel name)) 
                         (doc:text "!~a"  (doc:lower-camel exp))))
  (:string () (doc:text "~a!~a" (doc:lower-camel (synth :name name)) (doc:lower-camel exp)))
  (:template () (doc:text "{{~a}}" (doc:lower-camel (synth :name exp)))))

(defprim cat (&rest exps)
  (:pretty () (list 'cat (:exps (synth-all :pretty exps))))
  (:string () (text "~{~a~^ ++ ~}" (synth-all :string exps)))
  (:blub () (reduce #'bb-+ exps)))


(defprim variab (name)
  (:pretty () (list 'variab (list :name name)))
  (:string () (textify name))
  (:blub () (lang:bb-dynamic name)))

(defprim value (name)
  (:pretty () (list 'value (list :name name)))
  (:string () (textify name))
  (:blub () (lang:bb-dynamic name)))


  ;; (:html () (span (list :class "label label-danger") (text "~a" name))))

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

(defmacro defbexp (operator &optional representation (arity 0))
  (let ((name (symb "+" operator "+")))
    `(defprim ,name 
         ,(if (eq arity 'unbounded)
              `(&rest exps)
              (loop for i from 1 to arity collect (symb "EXP" i)))
       (:pretty () (list ',name 
			 ,(if (eq arity 'unbounded)
			      `(list :exps (synth-all :pretty exps))
			      `(list ,@(apply #'append (loop for i from 1 to arity collect (list (keyw "EXP" i) `(synth :pretty ,(symb "EXP" i)))))))))
       (:blub () ,(if (eq arity 'unbounded)
                      `(,(intern (mkstr "BB-" operator) "LANG") (synth-all :blub exps))
                      `(,(intern (mkstr "BB-" operator) "LANG") ,@(loop for i from 1 to arity collect `(synth :blub ,(symb "EXP" i))))))
       (:sql () ,(if (eq arity 'unbounded)
                     `(apply #'doc:punctuate (doc:text " ~a " ',representation) nil (synth-all :sql exps))
                     `(doc:punctuate (doc:text " ~a " ',representation) nil ,@(loop for i from 1 to arity collect `(synth :sql ,(symb "EXP" i)))))))))


(defmacro defbexps (&rest bexps)
  `(progn
     ,@(mapcar #'(lambda (bexp)
		   `(defbexp ,(car bexp) ,@(cdr bexp)))
	       bexps)))

;;(def-bexp true)
;; (def-bexp equal 2)

(defbexps (true) (false) (and and unbounded) (or or unbounded) (not not 1) (equal = 2) (less-than < 2) (greater-than > 2) (null null 1))

