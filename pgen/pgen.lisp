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
                       :throws (list (java-static 'i-o-exception)
                                     (java-static 'type-exception))
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

(defun members (x lst &key (test #'equal))
  (let ((m (member x lst :test test))) 
    (if (null m)
        nil
        (cons m (members x (cdr m))))))
;; (pprint (members 'a '(b a b)))

(defun follow-set-visited (x visited)
  (let ((productions (remove-if 
                      (lambda (prod) (closure-equal x (synth :head prod))) 
                      (get-prods-by-body *grammar* x))))
    ;; (my-debug "starting follow-set" x visited)
    (reduce (lambda (acc prod)
              (let* ((head (synth :head prod))
                     (body (synth :body prod))
                     ;; (beta (cdr (member x body :test #'closure-equal)))
                     (betas (mapcar #'cdr (members x body :test #'closure-equal)))
                     (follow (reduce (lambda (follow-acc beta) 
                                       (let* ((follow2 (set-difference (first-set beta) (list (epsilon)) :test #'closure-equal))
                                              (follow3 (if (or (null beta)
                                                               (member (epsilon) (first-set beta) :test #'closure-equal))
                                                           (if (member head visited :test #'closure-equal)
                                                               follow2
                                                               (union follow2 (follow-set-visited head (cons head visited))))
                                                           follow2)))
                                         (union follow-acc follow3)))
                                     betas
                                     :initial-value nil)))
                ;; (my-debug "in let* follow-set" head body beta (first-set beta) follow2 follow3)
                (union acc follow :test #'closure-equal)))
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
(defparameter t-string (java-object-type 'string))
(defparameter t-id (java-object-type 'identifier))
(defparameter t-int (java-object-type 'integer))
(defparameter t-if (java-object-type 'if))
(defparameter t-let (java-object-type 'let))
(defparameter t-ind (java-object-type 'indicator))
(defparameter t-expr (java-object-type 'expression))
(defparameter t-fcall (java-object-type 'function-call))
(defparameter t-fdecl (java-object-type 'function-declaration))
(defparameter t-aexpr (java-object-type 'arithmetic-expression))
(defparameter t-bexpr (java-object-type 'boolean-expression))
(defparameter t-or (java-object-type 'or))
(defparameter t-and (java-object-type 'and))
(defparameter t-not (java-object-type 'not))
(defparameter t-rel (java-object-type 'relation))
(defparameter t-plus (java-object-type 'plus))
(defparameter t-minus (java-object-type 'minus))
(defparameter t-times (java-object-type 'times))
(defparameter t-divide (java-object-type 'divide))
(defparameter t-bind (java-object-type 'binding))
(defparameter t-binds (java-template-type 'array-list t-bind))
(defparameter t-exprs (java-template-type 'array-list t-expr))
(defparameter t-fpars (java-template-type 'array-list t-id))
(defparameter t-ipar (java-object-type 'parameter))
(defparameter t-ipars (java-template-type 'array-list t-ipar))
(defparameter t-type (java-object-type 'type))


(defparameter t-bconst (java-object-type 'boolean-constant))
(defparameter t-aconst (java-object-type 'numeric-constant))

(defun t-template (&optional (type (java-wildcard-type))) 
  (java-template-type 'template-expression type))
;; (defparameter t-bool (java-object-type 'boolean))



(defparameter terminal-id (terminal :id t-id))
;; (defparameter expr (java-object-type 'expression))

(defnonterminal espressione (synth-attr t-expr) :start t)
(defnonterminal resto-espressione (synth-attr t-expr 
                                              :expr (inher-attr t-expr)))
(defnonterminal termine (synth-attr t-expr))
(defnonterminal resto-termine (synth-attr t-expr
                                          :expr (inher-attr t-expr)))
(defnonterminal fattore(synth-attr t-expr))

(defnonterminal chiamata-id (synth-attr t-expr))
(defnonterminal resto-chiamata-id (synth-attr t-expr
                                              :expr (inher-attr t-expr)))

(defnonterminal sia (synth-attr t-expr))
(defnonterminal se (synth-attr t-expr))

;; (defnonterminal dichiarazione-parametri-indicatore (synth-attr t-ipars))
;; (defnonterminal resto-dichiarazione-parametri-indicatore (synth-attr t-ipars))

(defnonterminal dichiarazione-parametri-funzione (synth-attr t-fpars))
(defnonterminal resto-dichiarazione-parametri-funzione (synth-attr t-fpars))

(defnonterminal invocazione-parametri-funzione (synth-attr t-exprs))
(defnonterminal resto-invocazione-parametri-funzione (synth-attr t-exprs))

(defnonterminal tipo (synth-attr t-type))
(defnonterminal legami (synth-attr t-binds))
(defnonterminal resto-legami (synth-attr t-binds))
(defnonterminal legame (synth-attr t-bind))
(defnonterminal resto-legame (synth-attr t-bind 
                                         :id (inher-attr t-id)))

;; (defnonterminal parametri nil)
;; (defnonterminal resto-parametri nil)
;; (defnonterminal parametro nil)
(defnonterminal argomento (synth-attr t-id))
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
;; (defnonterminal funzione (synth-attr t-expr))
(defnonterminal indicatore (synth-attr t-ind) :start t)


;; (indicatore cust1 (soggetto) ((occorrenze-veicoli numero) (occorrenze-sinistri numero))
;;             (maggiore (conta (sinistri (e (coinvolto soggetto sinistro)
;;                                 (maggiore (conta (veicoli (coinvolto veicolo sinistro))
;;                                                  occorrenze-veicoli))))
;;                              occorrenze-sinistri)))
            

(defproduction argomento 
    (with-bindings ((sogg (match (terminal :soggetto t-id)))
                    ((store sogg (java-new t-id sogg (java-chain (java-static 'type) (java-enum 'soggetto))))))
      (synthesize sogg)))

(defproduction argomento 
    (with-bindings ((veic (match (terminal :veicolo t-id)))
                    ((store veic (java-new t-id veic (java-chain (java-static 'type) (java-enum 'veicolo))))))
      (synthesize veic)))

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
    (with-bindings ((node (invoke sia)))
      (synthesize node)))

(defproduction espressione-booleana 
    (with-bindings ((node (invoke se)))
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

;; (defproduction fattore-booleano 
;;     (with-bindings ((node (invoke funzione)))
;;       (synthesize node)))

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
                    ((match (terminal :left)))                    
                    (arg (invoke argomento))
                    ((match (terminal :right)))
                    ((match (terminal :left)))
                    (params (invoke legami))
                    ((match (terminal :right)))
                    (expr (invoke espressione-booleana))
                    ((pop-environment)))
      (synthesize (java-new t-ind id arg params expr))))

(defproduction sia 
    (with-bindings (((match (terminal :let)))
                    ((push-environment))
                    (binds (invoke legami))
                    ((match (terminal :in)))
                    (node (invoke espressione-booleana))
                    ((pop-environment)))
      (synthesize (java-new t-let binds node))))

(defproduction se
    (with-bindings (((match (terminal :se)))
                    (expr (invoke espressione-booleana))
                    ((match (terminal :allora)))
                    (then (invoke espressione-booleana))
                    ((match (terminal :altrimenti)))
                    (otherwise (invoke espressione-booleana)))
      (synthesize (java-new t-if (java-chain expr :as t-bexpr) then otherwise))))

(defproduction resto-invocazione-parametri-funzione
    (synthesize (java-new t-exprs)))

(defproduction resto-invocazione-parametri-funzione 
    (with-bindings (((match (terminal :comma)))
                    (par (invoke espressione-booleana))
                    (pars (invoke resto-invocazione-parametri-funzione)))
      (synthesize (java-chain :as t-exprs (java-static 'list-utils) (java-call 'cons par pars)))))

(defproduction invocazione-parametri-funzione 
    (with-bindings ((par (invoke espressione-booleana))
                    (pars (invoke resto-invocazione-parametri-funzione)))
      (synthesize (java-chain :as t-exprs (java-static 'list-utils) (java-call 'cons par pars)))))

(defproduction tipo
    (with-bindings (((match (terminal :number))))
      (synthesize (java-chain (java-static 'type) (java-enum 'number)))))

(defproduction tipo
    (with-bindings (((match (terminal :boolean))))
      (synthesize (java-chain (java-static 'type) (java-enum 'boolean)))))

(defproduction tipo
    (with-bindings (((match (terminal :veicolo))))
      (synthesize (java-chain (java-static 'type) (java-enum 'veicolo)))))

(defproduction tipo
    (with-bindings (((match (terminal :soggetto))))
      (synthesize (java-chain (java-static 'type) (java-enum 'soggetto)))))

;; (defproduction resto-dichiarazione-parametri-indicatore
;;     (synthesize (java-new t-fpars)))

;; (defproduction resto-dichiarazione-parametri-indicatore 
;;     (with-bindings (((match (terminal :comma)))
;;                     (par (match terminal-id))
;;                     ((store par par))
;;                     (pars (invoke resto-dichiarazione-parametri-indicatore)))
;;       (synthesize (java-chain :as t-ipars (java-static 'list-utils) (java-call 'cons par pars)))))

;; (defproduction dichiarazione-parametri-indicatore 
;;     (with-bindings ((par (match terminal-id))
;;                     ((match (terminal :colon)))
;;                     (type (invoke tipo))
;;                     ((match (terminal :assign)))
;;                     (value (invoke espressione-booleana))
;;                     ((store par (java-new t-ipar par type value)))
;;                     (pars (invoke resto-dichiarazione-parametri-indicatore)))
;;       (synthesize (java-chain :as t-ipars (java-static 'list-utils) (java-call 'cons (java-new t-ipar par type value) pars)))))

(defproduction resto-dichiarazione-parametri-funzione
    (synthesize (java-new t-fpars)))

(defproduction resto-dichiarazione-parametri-funzione 
    (with-bindings (((match (terminal :comma)))
                    (par (match terminal-id))
                    ((match (terminal :colon)))
                    (type (invoke tipo))
                    ((store par (java-new t-id par type)))
                    (pars (invoke resto-dichiarazione-parametri-funzione)))
      (synthesize (java-chain :as t-fpars (java-static 'list-utils) (java-call 'cons (java-new t-id par type) pars)))))

(defproduction dichiarazione-parametri-funzione 
    (with-bindings ((par (match terminal-id))
                    ((match (terminal :colon)))
                    (type (invoke tipo))
                    ((store par (java-new t-id par type)))
                    (pars (invoke resto-dichiarazione-parametri-funzione)))
      (synthesize (java-chain :as t-fpars (java-static 'list-utils) (java-call 'cons (java-new t-id par type) pars)))))

(defproduction resto-legami
    (synthesize (java-new t-binds)))

(defproduction resto-legami 
    (with-bindings (((match (terminal :comma)))
                    (bind (invoke legame))
                    (binds (invoke resto-legami)))
      (synthesize (java-chain :as t-binds (java-static 'list-utils) (java-call 'cons bind binds)))))

(defproduction legami 
    (with-bindings ((bind (invoke legame))
                    (binds (invoke resto-legami)))
      (synthesize (java-chain :as t-binds (java-static 'list-utils) (java-call 'cons bind binds)))))

(defproduction resto-legame 
    (with-inherited ((id :id)) 
      (with-bindings (((match (terminal :left))) 
                      ((push-environment)) 
                      (pars (invoke dichiarazione-parametri-funzione))
                      ((match (terminal :right)))
                      ((match (terminal :colon)))
                      (type (invoke tipo))
                      ((store id (java-new t-fdecl id type pars)))
                      ((match (terminal :assign)))
                      (expr (invoke espressione-booleana))
                      ((pop-environment))
                      ((store id (java-new t-fdecl id type pars expr))))
        (synthesize (java-new t-bind id (java-new t-fdecl id type pars expr))))))

(defproduction resto-legame 
    (with-inherited ((id :id)) 
      (with-bindings (((match (terminal :colon)))
                      (type (invoke tipo))
                      ((match (terminal :assign)))
                      ((store id (java-new t-id id type))) 
                      (node (invoke espressione-booleana)))
        (synthesize (java-new t-bind (java-new t-id id type) node)))))

(defproduction resto-legame 
    (with-inherited ((id :id)) 
      (with-bindings (((match (terminal :assign)))
                      ((store id id)) 
                      (node (invoke espressione-booleana)))
        (synthesize (java-new t-bind id node)))))

(defproduction legame 
    (with-bindings ((id (match terminal-id)) 
                    (binding (invoke resto-legame id)))
      (synthesize binding)))

(defproduction espressione (with-bindings ((node (invoke termine))
                  (syn (invoke resto-espressione node)))
    (synthesize syn)))

(defproduction resto-espressione 
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :plus)))
                      (node (invoke termine))
                      (syn (invoke resto-espressione (java-new t-plus expr node))))
        (synthesize syn))))

(defproduction resto-espressione 
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :minus)))
                      (node (invoke termine))
                      (syn (invoke resto-espressione (java-new t-minus expr node))))
        (synthesize syn))))

(defproduction resto-espressione 
    (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction termine
    (with-bindings ((node (invoke fattore))
                    (syn (invoke resto-termine node)))
      (synthesize syn)))

(defproduction resto-termine 
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :times)))
                      (node (invoke fattore))
                      (syn (invoke resto-termine (java-new t-times expr node))))
        (synthesize syn))))

(defproduction resto-termine 
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :divide)))
                      (node (invoke fattore))
                      (syn (invoke resto-termine (java-new t-divide expr node))))
        (synthesize syn))))
 
(defproduction resto-termine 
    (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction fattore
    (with-bindings (((match (terminal :left)))
                    (node (invoke espressione-booleana))
                    ((match (terminal :right))))
      (synthesize node)))

(defproduction fattore
    (with-bindings ((node (lexval (terminal :num t-aconst))))
      (synthesize node)))

;; (defproduction fattore
;;     (with-bindings ((node (lookup (terminal :id t-expr))))
;;       (synthesize node)))

(defproduction fattore
    (with-bindings ((node (invoke chiamata-id)))
      (synthesize node)))

;; (defproduction chiamata-id
;;     (with-bindings ((node (terminal :sinistri))
;;                     (syn (invoke resto-chiamata-id node)))
;;       (synthesize syn)))

(defproduction chiamata-id
    (with-bindings ((node (lookup (terminal :id t-expr)))
                    (syn (invoke resto-chiamata-id node)))
      (synthesize syn)))

(defproduction chiamata-id
    (with-bindings ((node (lookup (terminal :veicolo t-id))))
      (synthesize node)))

(defproduction chiamata-id
    (with-bindings ((node (lookup (terminal :soggetto t-id))))
      (synthesize node)))

;; return new FunctionCall((FunctionDeclaration) top.get(((Identifier) expr).getId().lexeme), pars);
(defproduction resto-chiamata-id
    (with-inherited ((expr :expr))
      (synthesize expr)))

(defproduction resto-chiamata-id
    (with-inherited ((expr :expr))
      (with-bindings (((match (terminal :left)))
                      (pars (invoke invocazione-parametri-funzione))
                      ((match (terminal :right))))
        (synthesize (java-new t-fcall 
                              ;; (java-chain :as t-fdecl 
                              ;;             (java-dynamic 'top)
                              ;;             (java-call 'get 
                              ;;                        (java-chain (java-chain :as t-id expr)
                              ;;                                    (java-call 'get-id)
                              ;;                                    (java-dynamic 'lexeme))))
                              (java-chain :as t-fdecl expr)
                              pars)))))

(defparameter *ptable* (make-ptable *grammar*))

;; (let ((sym espressione-booleana))
;;   (pprint (synth-all :pretty (first-set sym)))
;;   (pprint (synth-all :pretty (follow-set sym))))
;; (pprint (synth-all :pretty (nonterminals *grammar*)))
;; (pprint-ptable *ptable*)

(pprint (synth :output (apply #'vcat (synth-all :doc (hash-table-values *nonterminals*))) 0))
;; (pprint (synth-all :pretty (apply #'append (synth-all :body (get-prods-by-head *grammar* (synth :symbol fattore))))))
(write-file "D:/giusv/temp/temp.java"
            (synth :string (apply #'vcat (synth-all :doc (synth-all :java (synth-all :code (hash-table-values *nonterminals*)))))))

;; (pprint (synth-all :output (synth-all :java (synth-all :code (list espressione resto-espressione termine resto-termine fattore))) 0))
;; (pprint (synth-all :pretty (get-prods-by-head *grammar* (synth :symbol espressione))))
;; (pprint (synth :doc espressione))
