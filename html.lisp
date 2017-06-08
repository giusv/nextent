(in-package :html)

(defprim tag (name &rest args)
  (:pretty () (list name (list :attributes (synth-plist :pretty (rest-key args)) :body (synth-all :pretty (rest-plain args)))))
  (:doc () (labels ((stringify (item)
                         (if (or (numberp item) (stringp item) (keywordp item) (symbolp item))
                             item
                             (synth :string item)))
                       (open-tag (as) (doc:text "<~a~{ ~a=\"~a\"~}>" (string-downcase name) (mapcar #'stringify as)))
                       (close-tag () (doc:text "</~a>" (string-downcase name)))
                       (open-close-tag (as) (doc:text "<~a~{ ~a=\"~a\"~}/>" (string-downcase name) (mapcar #'stringify as))))
                (let ((attributes (rest-key args)) 
                      (body (apply #'append* (rest-plain args))))
                  (if (null body)
                      (open-close-tag attributes)
                      (doc:vcat (open-tag attributes)
                                (doc:nest 4 (apply #'doc:vcat (synth-all :doc body)))
                                (close-tag)))))))
(defprim taglist (&rest tags)
  (:pretty () (list 'taglist (list :tags (synth-all :pretty (apply #'append* tags)))))
  (:doc () (apply #'doc:vcat (synth-all :doc (apply #'append* tags)))))

(defmacro deftag (name)
  `(defprim ,name (&rest args)
     (:pretty () `(,',name (:attributes ,(rest-key args) :body ,(synth-all :pretty (rest-plain args)))))
     (:doc () (labels ((stringify (item)
                         (if (or (numberp item) (stringp item) (keywordp item) (symbolp item))
                             item
                             (synth :string item)))
                       (open-tag (as) (doc:text "<~a~{ ~a=\"~a\"~}>" ',(string-downcase name) (mapcar #'stringify as)))
                       (close-tag () (doc:text "</~a>" ',(string-downcase name)))
                       (open-close-tag (as) (doc:text "<~a~{ ~a=\"~a\"~}/>" ',(string-downcase name) (mapcar #'stringify as))))
                (let ((attributes (rest-key args))
                      ;; (body (rest-plain args))
                      (body (apply #'append* (rest-plain args))))
                  ;; (princ "rest-plain:") (pprint (rest-plain args)) (fresh-line)
                  ;; (princ "body:") (pprint body) (fresh-line)
                  ;; (pprint attributes)
                  (if (null body)
                      (open-close-tag attributes)
                      (doc:vcat (open-tag attributes)
                                (doc:nest 4 (apply #'doc:vcat (synth-all :doc body)))
                                (close-tag))))))))
(defmacro deftags (&rest names)
  `(progn
     ,@(mapcar #'(lambda (name)
		   `(deftag ,name))
	       names)))

(deftags html head title meta link nav body h1 h2 h3 h4 h5 div span li dl dt dd ul ol pre i strong code script
         table thead tbody tr th td
         section article aside p a
         button input textarea
         label
         form)

(defun span-color (name)
  (let ((n (mod (reduce #'+ (mapcar #'char-code (coerce name 'list))) 100))) 
    (span :|class| "label" 
          :|style| (concatenate 'string "background-color:" (string-downcase (nth (mod n (length html-colors)) html-colors)))
          (doc:text "~a" name))))

(defparameter html-colors 
  (list 'aliceblue
        'antiquewhite
        'aqua
        'aquamarine
        'azure
        'beige
        'bisque
        'black
        'blanchedalmond
        'blue
        'blueviolet
        'brown
        'burlywood
        'cadetblue
        'chartreuse
        'chocolate
        'coral
        'cornflowerblue
        'cornsilk
        'crimson
        'cyan
        'darkblue
        'darkcyan
        'darkgoldenrod
        'darkgray
        'darkgreen
        'darkkhaki
        'darkmagenta
        'darkolivegreen
        'darkorange
        'darkorchid
        'darkred
        'darksalmon
        'darkseagreen
        'darkslateblue
        'darkslategray
        'darkturquoise
        'darkviolet
        'deeppink
        'deepskyblue
        'dimgray
        'dodgerblue
        'firebrick
        'floralwhite
        'forestgreen
        'fuchsia
        'gainsboro
        'ghostwhite
        'gold
        'goldenrod
        'gray
        'green
        'greenyellow
        'honeydew
        'hotpink
        'indianred
        'indigo
        'ivory
        'khaki
        'lavender
        'lavenderblush
        'lawngreen
        'lemonchiffon
        'lightblue
        'lightcoral
        'lightcyan
        'lightgoldenrodyellow
        'lightgray
        'lightgreen
        'lightpink
        'lightsalmon
        'lightsalmon
        'lightseagreen
        'lightskyblue
        'lightslategray
        'lightsteelblue
        'lightyellow
        'lime
        'limegreen
        'linen
        'magenta
        'maroon
        'mediumaquamarine
        'mediumblue
        'mediumorchid
        'mediumpurple
        'mediumseagreen
        'mediumslateblue
        'mediumslateblue
        'mediumspringgreen
        'mediumturquoise
        'mediumvioletred
        'midnightblue
        'mintcream
        'mistyrose
        'moccasin
        'navajowhite
        'navy
        'oldlace
        'olive
        'olivedrab
        'orange
        'orangered
        'orchid
        'palegoldenrod
        'palegreen
        'paleturquoise
        'palevioletred
        'papayawhip
        'peachpuff
        'peru
        'pink
        'plum
        'powderblue
        'purple
        'rebeccapurple
        'red
        'rosybrown
        'royalblue
        'saddlebrown
        'salmon
        'sandybrown
        'seagreen
        'seashell
        'sienna
        'silver
        'skyblue
        'slateblue
        'slategray
        'snow
        'springgreen
        'steelblue
        'tan
        'teal
        'thistle
        'tomato
        'turquoise
        'violet
        'wheat
        'white
        'whitesmoke
        'yellow
        'yellowgreen))

;; (defprim div (&rest args)
;;   (:pretty () `(div (:attributes ,(rest-key args) :body ,@(synth-all :pretty (rest-plain args)))))
;;   (:doc () (labels ((open-tag (as) (doc:text "<~(~a~)~{ ~(~a~)=\"~(~a~)\"~}>" 'div as))
;;                     (close-tag () (doc:text "</~(~a~)>" 'div))
;;                     (open-close-tag (as) (doc:text "<~(~a~)~{ ~(~a~)=\"~a\"~}/>" 'div as)))
;;              (let ((attributes (rest-key args))
;;                    (body (rest-plain args)))
;;                (if (null body)
;;                    (open-close-tag attributes)
;;                    (vcat (open-tag attributes)
;;                          (nest 4 (apply #'vcat (synth-all :doc body)))
;;                          (close-tag)))))))
