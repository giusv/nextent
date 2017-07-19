(in-package :pgen)

(defmacro my-debug (message &rest vars)
  `(progn (pprint ,message)
          ,@(mapcar (lambda (var)
                      `(progn (pprint ',var)
                              (pprint (if (consp ,var)
                                          (synth-all :pretty ,var)
                                          (synth :pretty ,var)))
                              (format t "~%")))
                    vars)
          (read)))

(defun put-prod (table key value)
  (if (nth-value 1 (gethash key table))
      (setf (gethash key table) (cons value (gethash key table)))
      (setf (gethash key table) (list value))))

(defun get-prods-by-head (table key)
  (gethash key table))

(defun get-prods-by-body (table key)
  (remove-if-not (lambda (prod) 
                   (member key (synth :body prod) :test #'closure-equal))
                 (productions table)))

(defparameter *grammar* (make-hash-table))
(defmacro defproduction (head &optional rule)
  `(let ((prod (production ,head ,rule))) 
     (put-prod *grammar* (synth :symbol ,head) prod)))

(defun closure-equal (x y)
   (equal (synth :pretty x) (synth :pretty y)))

(defun is-terminal (x)
  (synth :terminal x))

(defun is-start (x)
  (synth :start x))

(defprim epsilon ()
  (:pretty () (list 'epsilon))
  (:terminal () t)
  (:call () (bb-pairempty))
  (:docrhs () (empty)))

(defprim dollar ()
  (:pretty () (list 'dollar))
  (:terminal () t))



(defun hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))
(defun hash-table-values (table)
  (loop for value being the hash-values of table collect value))

(defprim inher-attr (type) 
  (:pretty () (list 'inher-attr (list type (synth :pretty type)))))

(defprim synth-attr (type &rest inputs) 
  (:pretty () (list 'synth-attr (list type (synth :pretty type) :inputs (synth-plist :pretty inputs))))
  (:parameters () (synth-plist-merge (lambda (pair)
                                       (java-pair (car pair) (synth :type (cadr pair))))
                                     inputs)))

(defprim terminal (symbol &optional type)
  (:pretty () (list 'terminal (list :symbol symbol :type type)))
  (:terminal () t)
  (:docrhs () (textify (lower-camel symbol))))

(defprim nonterminal (symbol synthesized &key start)
  (:pretty () (list 'nonterminal (list :symbol symbol :synthesized (synth :pretty synthesized) :start start)))
  (:terminal () nil)
  (:docrhs () (textify (upper-camel symbol)))
  (:code () 
         (let* ((prodmap (gethash symbol *ptable*))
                (cases (hash-table-keys prodmap))
                (attribute synthesized)) 
           (java-with-annotations 
            nil ;; (list (java-annotation2 '|SuppressWarnings| (java-const "unchecked"))
                ;;   (java-annotation2 '|SuppressWarnings| (java-const "unused")))
            (java-method (textify (lower-camel symbol)) (synth :parameters attribute) 
                       (aif (synth :type attribute)
                            it
                            (java-primitive-type 'void))
                       :throws (list (java-static 'i-o-exception))
                       (java-list
                        (apply #'java-switch (java-chain (java-dynamic 'look) (java-dynamic 'tag))
                               :default (java-throw (java-new (java-object-type 'error) 
                                                          (reduce #'java-+ 
                                                                  (list (java-const (reduce #'mkstr
                                                                                          (mapcar (lambda (case)
                                                                                                    (mkstr case ", "))
                                                                                                  cases)
                                                                                          :initial-value (mkstr "Error in production for " symbol ": expecting ")))
                                                                        (java-const "found ")
                                                                        (java-dynamic 'look)))))
                               (mapcar (lambda (case)
                                         (java-case (java-chain (java-static 'tag) (java-enum case)) 
                                                  (progn 
                                                    ;; (my-debug "in nonterminal" (car (gethash case prodmap)))
                                                    (synth :code (car (gethash case prodmap)) attribute))))
                                       cases)))))))
  (:doc () (let* ((productions (gethash symbol *grammar*))) 
             (hcat+ (textify (upper-camel symbol))
                    (text "->")
                    (apply #'punctuate (text " | ") nil (synth-all :docrhs productions))
                    (text "."))))
  (:type (attribute) (synth :type synthesized)))

(defparameter *nonterminals* (make-hash-table))
(defmacro defnonterminal (symbol synthesized &key start)
  `(let ((nonterm (nonterminal ',symbol ,synthesized :start ,start))) 
     (progn 
       (defparameter ,symbol nonterm)
       (setf (gethash ',symbol *nonterminals*) nonterm))))

(defprim production (head &optional rule)
  (:pretty () (list 'production (list :head (synth :pretty head) :body (synth-all :pretty (synth :body this)) :rule (synth :pretty rule))))
  (:code (attribute) (if rule 
                         (synth :code rule attribute)
                         (java-return)))
  (:body () (aif (synth :body rule)
                 it
                 (list (epsilon))))
  (:docrhs () (apply #'hcat+ (synth-all :docrhs (synth :body this))))) 

(defun first-set (x)
  (cond ((null x) nil)
        ((consp x)
         (let ((f (first-set (car x))))
           ;; (my-debug "in (consp x)" (car x) f (member (epsilon) f :test #'closure-equal) (set-difference f (list (epsilon))))
           (if (member (epsilon) f :test #'closure-equal)
               (union (set-difference f (list (epsilon))) (first-set (cdr x)) :test #'closure-equal)
               f)))
        (t (if (is-terminal x)
               (list x)
               (let ((productions (get-prods-by-head *grammar* (synth :symbol x))))
                 ;; (pprint (synth-all :pretty productions))
                 (reduce (lambda (acc prod)
                           ;; (pprint (synth :pretty prod)) 
                           (aif (synth :body prod)
                                (progn ;; (my-debug "in aif" prod it (first-set it))
                                       
                                       (union acc (first-set it) :test #'closure-equal))
                                (adjoin acc (epsilon) :test #'closure-equal)))
                         productions
                         :initial-value nil))))))

(defun follow-set-visited (x visited)
  (let ((productions (remove-if 
                      (lambda (prod) (closure-equal x (synth :head prod))) 
                      (get-prods-by-body *grammar* x))))
    ;; (my-debug "starting follow-set" x visited)
    (reduce (lambda (acc prod)
              (let* ((head (synth :head prod))
                     (body (synth :body prod))
                     (beta (cdr (member x body :test #'closure-equal)))
                     (follow2 (set-difference (first-set beta) (list (epsilon)) :test #'closure-equal))
                     (follow3 (if (or (null beta)
                                      (member (epsilon) (first-set beta) :test #'closure-equal))
                                  (progn 
                                    ;; (my-debug "in if" head)
                                    (if (member head visited :test #'closure-equal)
                                        follow2
                                        (union follow2 (follow-set-visited head (cons head visited)))))
                                  follow2)))
                ;; (my-debug "in let* follow-set" head body beta (first-set beta) follow2 follow3)
                (union acc follow3 :test #'closure-equal)))
            productions
            :initial-value (if (is-start x)
                               (list (dollar))
                               nil))))
(defun follow-set (x)
  (follow-set-visited x nil))

(defun productions (grammar)
  (flatten (hash-table-values grammar)))

(defun nonterminals (grammar)
  (reduce (lambda (acc prod)
            (adjoin (synth :head prod) acc :test #'closure-equal)) 
          (productions grammar)
          :initial-value nil))


(defun make-ptable (grammar)
  (let ((ptable (make-hash-table))
        (nonterms (nonterminals grammar)))
    (mapcar (lambda (nonterm)
              (setf (gethash (synth :symbol nonterm) ptable) (make-hash-table)))
            nonterms)
    (mapcar (lambda (prod)
              (let* ((head (synth :head prod))
                     (body (synth :body prod))
                     (f (first-set body)))
                (mapcar (lambda (term)
                          (if (closure-equal term (epsilon))
                              (let ((g (follow-set head)))
                                (mapcar (lambda (term)
                                          (if (closure-equal term (dollar)) 
                                              (put-prod (gethash (synth :symbol head) ptable) 'eof prod)
                                              (put-prod (gethash (synth :symbol head) ptable) (synth :symbol term) prod)))
                                        g))
                              (put-prod (gethash (synth :symbol head) ptable) (synth :symbol term) prod)))
                        f)))
            (productions grammar))
    ptable))

(defun pprint-ptable (ptable)
  (let ((nonterms (loop for key being the hash-keys of ptable collect key)))
    (format t "~%")
    (mapcar (lambda (nonterm) 
              (format t "nonterminal ~a:~%" nonterm)
              (let ((terms (loop for key being the hash-keys of (gethash nonterm ptable) collect key)))
                (mapcar (lambda (term)
                          (format t "~tterminal ~a:~a~%" 
                                  term
                                  (let ((prods (gethash term (gethash nonterm ptable)))) 
                                    (if (> (length prods) 1)
                                        (error "not LL(1)") 
                                        (synth :pretty (car prods)))))
                          (format t "~%"))
                        
                        terms))) 
            nonterms)))


;; (defprim with-inherited% (bindings expr)
;;   (:pretty () (list 'with-inherited (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
;;   (:code (attribute) (java-list (synth-all :code bindings attribute) 
;;                                 (if expr (synth :code expr attribute)
;;                                     (java-return))))
;;   (:body () (synth :body expr)))

(defprim binding% (lhs rhs)
  (:pretty () (list 'binding (list :lhs lhs :rhs (synth :pretty rhs))))
  (:code (attribute) (synth :code rhs attribute lhs))
  (:body () (synth :body rhs)))

(defprim with-bindings% (bindings expr)
  (:pretty () (list 'with-bindings (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
  (:code (attribute) (java-list (synth-all :code bindings attribute)
                              (if expr 
                                  (synth :code expr attribute)
                                  (java-break))))
  (:body () (remove nil (synth-all :body bindings))))

(defprim synthesize (expr)
  (:pretty () (list 'synthesize (list :expr (synth :pretty expr))))
  (:code (attribute) (java-return expr))
  (:body () nil))

(defmacro with-bindings ((&rest bindings) expr)
  `(let* ,(mapcar #`(,(car a1) (java-dynamic ',(car a1))) (remove-if (lambda (bind) (= 1 (length bind)))
                                                                   bindings))
     (with-bindings% (list ,@(mapcar (lambda (a1) 
                                 `(binding%
                                   ,@(if (atom (car a1))
                                         `(',(car a1) ,(cadr a1))
                                         `(nil ,(car a1)))))
                               bindings))
       ,expr)))


(defmacro with-inherited ((&rest bindings)  expr)
  `(let* ,(mapcar #`(,(car a1) (java-dynamic ',(car a1))) bindings)
     ;; (with-inherited% (list ,@(mapcar #`(binding% ',(car a1) (getf (synth :inputs ,head) ,(cadr a1))) bindings))
     ;;   ,expr)
     ,expr
     ))


(defprim invoke (symbol &rest parameters)
  (:pretty () (list 'invoke (list :symbol (synth :pretty symbol) :parameters (synth-all :pretty parameters))))
  (:code (attribute lhs) 
         (if lhs
             (java-statement (java-pair lhs (synth :type this attribute) 
                                    :init (if (is-terminal symbol)
                                              (error "terminal symbol in invoke")
                                              (apply #'java-call (synth :symbol symbol) parameters))))
             (java-statement (apply #'java-call (synth :symbol symbol) parameters))))
  (:type (attribute) (synth :type symbol attribute))
  (:body () symbol))

(defprim match (symbol)
  (:pretty () (list 'match (list :symbol (synth :pretty symbol))))
  (:code (attribute lhs) 
         (if (is-terminal symbol)
             (if (and (synth :type symbol) lhs)
                 (java-list (java-statement (java-pair lhs (synth :type symbol) 
                                                 :init (java-new (synth :type symbol)
                                                               (java-dynamic 'look :as (java-object-type 'word))))) 
                          (java-statement (java-call 'match (java-chain (java-static 'tag) (java-enum (synth :symbol symbol))))))
                 (java-statement (java-call 'match (java-chain (java-static 'tag) (java-enum (synth :symbol symbol))))))
             (error "nonterminal symbol in match")))
  (:body () symbol))
(defprim lexval (symbol)
  (:pretty () (list 'lexval (list :symbol (synth :pretty symbol))))
  (:code (attribute lhs) 
         (let ((type (synth :type symbol)))
           (if (is-terminal symbol)
               (java-list 
                (java-statement (java-pair lhs type 
                                       :init (java-new type (java-chain (java-dynamic 'look :as (java-object-type 'num)) 
                                                                    (java-dynamic 'value)))))
                (synth :code (match symbol) attribute nil))
               (error "nonterminal symbol in lexval"))))
  (:body () symbol))

(defprim lookup (symbol)
  (:pretty () (list 'lookup (list :symbol (synth :pretty symbol))))
  (:code (attribute lhs) (let ((type (synth :type symbol)))
                           (if (and (is-terminal symbol)
                                    (not (null type)))
                               (java-list 
                                (java-statement (java-pair lhs type :init (java-chain (java-dynamic 'top) 
                                                                                (java-call 'get (java-chain (java-dynamic 'look :as (java-object-type 'word)) (java-dynamic 'lexeme))) 
                                                                                :as type)))
                                (java-if (java-null (java-dynamic lhs))
                                       (java-statement (java-call 'error (java-+ (java-chain (java-dynamic 'look)
                                                                                     (java-call 'to-string))
                                                                           (java-const " undeclared"))))) 
                                (synth :code (match symbol) attribute nil))
                               (error "nonterminal symbol in lookup"))))
  (:body () symbol))
(defprim push-environment ()
  (:pretty () (list 'push-environment))
  (:code (attribute lhs) (java-list (java-statement (java-pair 'saved (java-object-type 'env) :init (java-dynamic 'top)))
                                  (java-statement (java-assign (java-dynamic 'top) (java-new (java-object-type 'env) (java-dynamic 'top))))))
  (:body () nil))

(defprim pop-environment ()
  (:pretty () (list 'pop-environment))
  (:code (attribute lhs) (java-statement (java-assign (java-dynamic 'top) (java-dynamic 'saved))))
  (:body () nil))

(defprim store (id value)
  (:pretty () (list 'store (list :id (synth :pretty id) :value (synth :pretty value))))
  (:code (attribute lhs) (java-statement (java-chain (java-dynamic 'top) 
                                                     (java-call 'put (java-chain id (java-call 'get-id) (java-dynamic 'lexeme)) value))))
  (:body () nil))

;; (defparameter etype (java-template-type 'expression (java-object-type 'float)))
(defparameter t-expr (java-object-type 'expression))
(defparameter t-aexpr (java-object-type 'arithmetic-expression))
(defparameter t-bexpr (java-object-type 'boolean-expression))
(defparameter t-or (java-object-type 'or))
(defparameter t-and (java-object-type 'and))
(defparameter t-not (java-object-type 'not))
(defparameter t-rel (java-object-type 'relation))
(defparameter t-plus (java-object-type 'plus))
(defparameter t-times (java-object-type 'times))
(defparameter t-bind (java-object-type 'binding))
(defparameter t-binds (java-template-type 'array-list t-bind))


(defparameter t-bconst (java-object-type 'boolean-constant))
(defparameter t-aconst (java-object-type 'numeric-constant))

(defun t-template (&optional (type (java-wildcard-type))) 
  (java-template-type 'template-expression type))
;; (defparameter t-bool (java-object-type 'boolean))
(defparameter t-string (java-object-type 'string))
(defparameter t-id (java-object-type 'identifier))
(defparameter t-int (java-object-type 'integer))
(defparameter t-let (java-object-type 'let))
(defparameter t-ind (java-object-type 'indicator))


(defparameter terminal-id (terminal :id t-id))
;; (defparameter expr (java-object-type 'expression))

(defnonterminal espressione (synth-attr t-expr) :start t)
(defnonterminal resto-espressione (synth-attr t-expr 
                               :expr (inher-attr t-expr)))
(defnonterminal termine (synth-attr t-expr))
(defnonterminal tp (synth-attr t-expr
                               :expr (inher-attr t-expr)))
(defnonterminal ff (synth-attr t-expr))

(defnonterminal indy-let (synth-attr t-expr))
(defnonterminal indy-binds (synth-attr t-binds))
(defnonterminal indy-binds-rest (synth-attr t-binds))
(defnonterminal indy-bind (synth-attr t-bind))

;; (defnonterminal parametri nil)
;; (defnonterminal resto-parametri nil)
;; (defnonterminal parametro nil)
;; (defnonterminal argomento (synth-attr t-id))
(defnonterminal espressione-booleana (synth-attr t-expr))
(defnonterminal resto-espressione-booleana (synth-attr t-expr
                                                       :expr (inher-attr t-expr)))
(defnonterminal termine-booleano (synth-attr t-expr))
(defnonterminal resto-termine-booleano (synth-attr t-expr
                                                   :expr (inher-attr t-expr)))
(defnonterminal fattore-booleano (synth-attr t-expr))
(defnonterminal relazione (synth-attr t-expr))
(defnonterminal resto-relazione (synth-attr t-expr
                                            :expr (inher-attr t-expr)))
(defnonterminal funzione (synth-attr t-expr))
(defnonterminal indicatore (synth-attr t-ind) :start t)


;; (indicatore cust1 (soggetto) ((occorrenze-veicoli numero) (occorrenze-sinistri numero))
;;             (maggiore (conta (sinistri (e (coinvolto soggetto sinistro)
;;                                 (maggiore (conta (veicoli (coinvolto veicolo sinistro))
;;                                                  occorrenze-veicoli))))
;;                              occorrenze-sinistri)))
            

;; (defproduction argomento 
;;     (with-bindings ((sogg (match (terminal :soggetto t-id))))
;;       (synthesize sogg)))

;; (defproduction argomento 
;;     (with-bindings ((veic (match (terminal :veicolo t-id))))
;;       (synthesize veic)))

(defproduction funzione
    (with-bindings (((match (terminal :sinistri)))
                    ((match (terminal :left)))
                    (node (invoke espressione-booleana))
                    ((match (terminal :right))))
      (synthesize node)))

;; (defproduction parametro
;;     (with-bindings ((id (match terminal-id))
;;                     ((match (terminal :assign)))
;;                     (node (invoke espressione-booleana))
;;                     ((store id id)))
;;       nil))

;; (defproduction resto-parametri)

;; (defproduction resto-parametri
;;     (with-bindings (((match (terminal :comma)))
;;                     ((invoke parametro))
;;                     ((invoke resto-parametri)))
;;       nil))

;; (defproduction parametri)
;; (defproduction parametri
;;     (with-bindings (((invoke parametro))
;;                     ((invoke resto-parametri)))
;;       nil))

(defproduction espressione-booleana 
    (with-bindings ((node (invoke indy-let)))
      (synthesize node)))



(defproduction espressione-booleana 
    (with-bindings ((node (invoke termine-booleano))
                    (syn (invoke resto-espressione-booleana node)))
      (synthesize syn)))

(defproduction resto-espressione-booleana 
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :or)))
                      (node (invoke termine-booleano))
                      (syn ;; (let ((type (t-template t-bool)))
                       ;;   (invoke resto-espressione-booleana
                       ;;           (java-new type
                       ;;                     (java-or (java-chain (java-chain expr :as type) (java-call 'get-value)) 
                       ;;                              (java-chain (java-chain node :as type) (java-call 'get-value))))))
                       (invoke resto-espressione-booleana (java-new t-or expr node))))
        (synthesize syn))))

(defproduction resto-espressione-booleana 
    (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction termine-booleano
    (with-bindings ((node (invoke fattore-booleano))
                    (syn (invoke resto-termine-booleano node)))
      (synthesize syn)))

(defproduction resto-termine-booleano 
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :and)))
                      (node (invoke fattore-booleano))
                      (syn ;; (let ((type (t-template t-bool)))
                       ;;   (invoke resto-termine-booleano
                       ;;           (java-new type
                       ;;                     (java-and (java-chain (java-chain expr :as type) (java-call 'get-value)) 
                       ;;                               (java-chain (java-chain node :as type) (java-call 'get-value))))))
                       (invoke resto-termine-booleano (java-new t-and expr node))))
        (synthesize syn))))
 
(defproduction resto-termine-booleano 
    (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction fattore-booleano 
    (with-bindings (((match (terminal :not)))
                    (syn (invoke fattore-booleano)))
      ;; (synthesize (let ((type (t-template t-bool)))
      ;;               (java-new type (java-not (java-chain (java-chain syn :as type) (java-call 'get-value)))))) 
      (synthesize (java-new t-not syn))))

(defproduction fattore-booleano 
    (with-bindings (((match (terminal :true))))
      (synthesize (java-new t-bconst (java-true)))
      ;; (synthesize (java-new (t-template t-bool) (java-true)))
      ))

(defproduction fattore-booleano 
    (with-bindings (((match (terminal :false))))
      (synthesize (java-new t-bconst (java-false)))
      ;; (synthesize (java-new (t-template t-bool) (java-false)))
      ))

(defproduction fattore-booleano 
    (with-bindings ((node (invoke relazione)))
      (synthesize node)))

(defproduction fattore-booleano 
    (with-bindings ((node (invoke funzione)))
      (synthesize node)))

(defproduction relazione 
    (with-bindings ((node (invoke espressione))
                    (syn (invoke resto-relazione node)))
      (synthesize syn)))

(defproduction resto-relazione
    (with-inherited ((expr :expr))
      (synthesize expr)))
(defproduction resto-relazione 
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :equal)))
                      (node (invoke espressione)))
        (synthesize ;; (let ((type (t-template t-bool)))
                    ;;   (java-new type (java-equal (java-chain (java-chain expr :as type) (java-call 'get-value)) 
                    ;;                          (java-chain (java-chain node :as type) (java-call 'get-value)))))
         (java-new t-rel (java-chain (java-static 'relop) (java-enum 'equal)) expr node)))))

(defproduction indicatore 
    (with-bindings (((push-environment))
                    (id (match terminal-id))
                    ;; (arg (invoke argomento))
                    ((match (terminal :left)))
                    (params (invoke indy-binds))
                    ((match (terminal :right)))
                    (expr (invoke espressione-booleana))
                    ((pop-environment)))
      (synthesize (java-new t-ind id params expr))))

(defproduction indy-let 
    (with-bindings (((match (terminal :let)))
                    ((push-environment))
                    (binds (invoke indy-binds))
                    ((match (terminal :in)))
                    (node (invoke espressione-booleana))
                    ((pop-environment)))
      (synthesize (java-new t-let binds node))))

(defproduction indy-bind 
(with-bindings ((id (match terminal-id))
                ((match (terminal :assign)))
                (node (invoke espressione))
                ((store id id)))
  (synthesize (java-new t-bind id node))))

(defproduction indy-binds-rest
    (synthesize (java-new t-binds)))

(defproduction indy-binds-rest 
    (with-bindings (((match (terminal :comma)))
                    (bind (invoke indy-bind))
                    (binds (invoke indy-binds-rest)))
      (synthesize (java-chain :as t-binds (java-static 'list-utils) (java-call 'cons bind binds)))))

(defproduction indy-binds 
    (with-bindings ((bind (invoke indy-bind))
                    (binds (invoke indy-binds-rest)))
      (synthesize (java-chain :as t-binds (java-static 'list-utils) (java-call 'cons bind binds)))))

(defproduction espressione (with-bindings ((node (invoke termine))
                  (syn (invoke resto-espressione node)))
    (synthesize syn)))

(defproduction resto-espressione (with-inherited ((expr :expr))
                    (with-bindings (((match (terminal :plus)))
                                    (node (invoke termine))
                                    (syn 
                                     ;; (let ((type (t-template t-int)))
                                     ;;   (invoke resto-espressione (java-new type
                                     ;;                      (java-+ (java-chain (java-chain expr :as type) (java-call 'get-value)) 
                                     ;;                            (java-chain (java-chain node :as type) (java-call 'get-value))))))
                                     (invoke resto-espressione (java-new t-plus expr node))))
                      (synthesize syn))))
(defproduction resto-espressione (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction termine (with-bindings ((node (invoke ff))
                  (syn (invoke tp node)))
    (synthesize syn)))

(defproduction tp (with-inherited ((expr :expr))
                    (with-bindings (((match (terminal :times)))
                                    (node (invoke ff))
                                    (syn 
                                     ;; (let ((type (t-template t-int)))
                                     ;;   (invoke tp (java-new type
                                     ;;                      (java-* (java-chain (java-chain expr :as type) (java-call 'get-value)) 
                                     ;;                            (java-chain (java-chain node :as type) (java-call 'get-value))))))
                                     (invoke tp (java-new t-times expr node))))
                      (synthesize syn))))
 
(defproduction tp 
    (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction ff 
    (with-bindings (((match (terminal :left)))
                    (node (invoke espressione-booleana))
                    ((match (terminal :right))))
      (synthesize node)))

(defproduction ff 
    (with-bindings ((node (lexval (terminal :num t-aconst))))
      (synthesize node)))

(defproduction ff 
    (with-bindings ((node (lookup (terminal :id t-expr))))
      (synthesize node)))


(defparameter *ptable* (make-ptable *grammar*))

;; (let ((sym espressione-booleana))
;;   (pprint (synth-all :pretty (first-set sym)))
;;   (pprint (synth-all :pretty (follow-set sym))))
;; (pprint (synth-all :pretty (nonterminals *grammar*)))
;; (pprint-ptable *ptable*)

(pprint (synth :output (apply #'vcat (synth-all :doc (hash-table-values *nonterminals*))) 0))
;; (pprint (synth-all :pretty (apply #'append (synth-all :body (get-prods-by-head *grammar* (synth :symbol ff))))))
(write-file "D:/giusv/temp/temp.java"
            (synth :string (apply #'vcat (synth-all :doc (synth-all :java (synth-all :code (hash-table-values *nonterminals*)))))))

;; (pprint (synth-all :output (synth-all :java (synth-all :code (list espressione resto-espressione termine tp ff))) 0))
;; (pprint (synth-all :pretty (get-prods-by-head *grammar* (synth :symbol espressione))))
;; (pprint (synth :doc espressione))
