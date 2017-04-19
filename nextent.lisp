;;;; nextent.lisp

(in-package :nextent)

 ;; (synth :output (nest 10 (text "~a" 24)) 0)
(pprint (synth :output (synth :doc (html:div :class "a" (doc:text "SS"))) 0))
(pprint (synth :pretty (html:div :class "a" (text "ss"))))
(synth :output (synth :doc (synth :template (gui:input 'name (text "Name") :init (text "hello")))) 0)

;; (pprint (synth :pretty (div)))

;; (synth :output (html:div :class "a" (text "aaa")))
;; (pprint (parse (many (atomic)) '(a b c &optional d)))


;; (let ((l '(a b c &optional d1 (d2 0 d2-supplied-p) &rest e &key f (g 99 g-supplied-p))))
;;   ;; (pprint (parse (lambda-list) l))
;;   (pprint (arg-names l))
;;   ;; (let ((args (parse (lambda-list) l)))
;;   ;;   (pprint (apply #'append (mapcar (lambda (x) (getf args x))  (list :req :opt :rest :key))))
;;   ;;   )
;;   )
;; (pprint (parse (ttt) (list 'a 'b)))
;; (pprint (parse (ttt) '(a b)))
;; (pprint (parse (var-init) '((a b))))


;; (pprint (parse (var-init) '((name init))))
;;; "nextent" goes here. Hacks and glory await!

