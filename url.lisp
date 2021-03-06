(in-package :url)
(defprim void ()
  (:url () (empty))
  (:pretty () (list 'void))
  ;; (:last () "default";; (error "no last elements in (void)")
  ;;        )
  (:path-parameters () nil))
(defprim static-chunk (name)
  (:url () (doc:text "~a" (string-downcase name)))
  (:pretty () (list 'static-chunk (list :name name)))
  ;; (:last () name)
  (:path-parameters () nil))

(defprim dynamic-chunk (name type &key validators)
  (:pretty () (list 'dynamic-chunk (list :name name :type type :validators (synth-all :pretty validators))))
  (:url () (doc:braces (doc:text "~a" (string-downcase name))))
  ;; (:last () name)
  (:path-parameters () (list (path-parameter name type :validators validators))))

;; (defprim expression-chunk (exp)
;;   (:url () (doc:braces (synth :chunk exp)))
;;   (:pretty () (list 'expression-chunk (list :exp exp)))
;;   (:last () (error "no last elements in expression chunks")))

(defprim path-parameter (name type &key validators)
  (:pretty () (list 'path-parameter (list :name name :type type :validators (synth-all :pretty validators))))
  (:type () (doc:text "path"))
  (:declaration (&optional full) 
                (let ((pair (bb-pair (symb (lower-camel name) "-ID") (bb-type type)))) 
                  (if full
                      (bb-with-annotations (cons (bb-annotation2 '|PathParam| (bb-object :|value| (bb-const (lower-camel name))))
                                                 (synth-all :annotation validators))
                                           pair
                                           :newline nil)
                      pair)))
  (:call () (lang:bb-dynamic (symb (lower-camel name) "-ID")))
  (:req () (html:taglist 
            (html:span-color (string-downcase name))
            (doc:text "(parametro path)")))
  (:url () (dynamic-chunk name type :validators validators)))

(defprim query-parameter (name type &key validators default)
  (:pretty () (list 'quey-parameter (list :name name :type type :default (synth :pretty default) :validators (synth-all :pretty validators))))
  (:url () (doc:hcat (doc:text "~a" (string-downcase name)) 
                     (if value 
                         (doc:hcat (equals) (synth :url value))
                         (empty))))
  (:req () (html:taglist 
            (html:span-color (string-downcase name))
            (doc:text "(parametro query)")))
  (:declaration (&optional full) 
                (let ((pair (bb-pair (lower-camel name) (bb-type type)))) 
                  (if full
                      (bb-with-annotations (cons (bb-annotation2 '|QueryParam| (bb-object :|value| (bb-const (lower-camel name))))
                                                 (synth-all :annotation validators))
                                           pair
                                           :newline nil)
                      pair)))
  (:call () (lang:bb-dynamic name))
  (:type () (doc:text "query")))

;; (defprim login-parameter (name)
;;   (:req () (html:taglist 
;;                (html:span-color (string-downcase name))
;;                (doc:text "(parametro login)")))
;;   (:type () (doc:text "login"))
;;   (:pretty () (list 'login-parameter (list :name name))))

;; backward-chain holds reversed path
(defprim backward-chain (segment pose)
  (:url () (if pose 
		 (doc:hcat (synth :url pose) (doc:text "/") (synth :url segment))))
  (:pretty () (list 'backward-chain (list :segment (synth :pretty segment) :pose (synth :pretty pose))))
  ;; (:last () (synth :last segment))
  (:path-parameters () (append (synth :path-parameters segment) (synth :path-parameters pose))))

(defprim multi (&rest poses)
  (:url () (doc:parens (apply #'punctuate (doc:text ",") nil (synth-all :url poses))))
  (:pretty () (list 'multi (list :poses (synth-all :pretty poses))))
  ;; (:last () (error "no last elements in multi"))
  (:path-parameters () (apply #'append (synth-all :path-parameters poses))))

(defprim forward-chain (segment pose)
  (:url () (doc:hcat (synth :url segment) (doc:text "/") (synth :url pose)))
  (:pretty () (list 'forward-chain (list :segment (synth :pretty segment) :pose (synth :pretty pose))))
  ;; (:last () (synth :last pose))
  (:path-parameters () (append (synth :path-parameters segment) (synth :path-parameters pose))))

(defprim queried (segment &rest parameters)
  (:url () (doc:hcat (synth :url segment) (doc:text "?") (apply #'punctuate (doc:text "&") nil (synth-all :url parameters))))
  (:pretty () (list 'queried (list :segment (synth :pretty segment) :parameters (synth-all :pretty parameters))))
  (:last () (synth :last segment)))

(defun parse-query-parameter ()
  (do-with ((name (item))
	    ((sym '=))
	    (value (choose (do-with (((sym '{))
				     (value (item))
				     ((sym '}))) 
			     (result value))
			   (do-with ((value (item))) 
		       (result (const value))))))
    (result (query-parameter name value))))
(defun parse-chunk ()
  (choose (do-with (((sym '{))
		    (seg (item))
		    ((sym '}))) 
	    (result (expression-chunk seg)))
	  (choose (do-with ((seg (item))
			    ((sym '?))
			    (pars (sepby1 (parse-query-parameter) (sym '&))))
		    (result (apply #'queried (static-chunk seg) pars)))
		  (do-with ((seg (item)))
		    (result (static-chunk seg))))))

(defun parse-url ()
  (do-with ((segs (sepby (choose (do-with (((sym '<))
					   (poses (sepby (parse-url) (sym '&)))
					   ((sym '>)))
				   (result (apply #'multi poses)))
				 (parse-chunk))
			 (sym '/)))) 
    ;; (result (reduce #'forward-chain segs :from-end t))
    (result (reduce #'forward-chain segs :from-end t))))

(defmacro merge-urls (head tail)
  ;; (reduce #'forward-chain head :from-end t :initial-value (url tail))
  (labels ((listify (x)
             (if (consp x)
                 x
                 (list x))))
    `(parse (parse-url) `(,@',(listify head)  / ,@',(listify tail)))))

;; (merge-urls (seg1 / seg2) (seg3 / seg4))

;; (merge-urls :seg1 (seg3 / seg4))
(defmacro url (u)
  `(parse (parse-url) ,u))
;;(synth output (synth :url (chain 'a (multi (chain 'b) (chain 'c)))) 0)
;; (synth output (synth :url (multi (chain 'b) (chain 'c))) 0)
;;(parse (parse-url) '(a </> b))

;; (synth output (synth :url (parse (parse-url) '({ a }))) 0)
;; (synth output (synth :url (parse (parse-url) `(b ? q = a & r = { ,(value (button 'ok nil)) }))) 0)
;; (pprint (synth :pretty (parse (parse-url) `(b ? q = a & r = { ,(value (button 'ok nil)) }))))

 

;; (synth output (synth :url (reduce #'forward-chain (parse (parse-url) '({ a } / b)) :from-end t :initial-value (void))) 0)

;; (synth output (synth :url (reduce #'forward-chain (parse (parse-url) '({ a } / < b & c >)) :from-end t :initial-value (void))) 0)

;; (synth output (synth :url (reduce #'forward-chain (parse (parse-url) '({ a } / < b & c >)) :from-end t :initial-value (void))) 0)

;; (reduce #'forward-chain (parse (parse-url) '({ a } / < b & c >)) :from-end t :initial-value (void))



;; (pprint (synth :pretty (parse (parse-url) '(a / < b & c >))))
;;(synth output (synth :url (parse (parse-url) '(a / < b & c >))) 0)

