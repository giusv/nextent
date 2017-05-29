

;; (defprim (aggregatore ((name symbol)
;;                             &rest (weights (list pair))))
;;   (to-list () (list 'aggregatore (list :name name :weights (synth-all to-list weights))))
;;   (to-production () (multitags (text "Costrutto principale per la definizione di un aggregatore. Esso prende in ingresso l'identificativo univoco dell'aggregatore e una lista di coppie indicatore-peso e ne calcola la somma pesata.")))
;;   (to-syntax () (text "(aggregatore nome (in-1 p-1) ...  (in-n p-n))")))

(defprim indicatore (name args pars expr)
  (:pretty () (list 'indicatore (list :name name :args (synth-all :pretty args) :pars (synth-all :pretty pars) :expr (synth :pretty expr))))
  (:implementation () (bb-unit name
                               (bb-class name 
                                         :public t
                                         :methods (apply #'append (synth-all :jax-methods resources name url))))))

(defmacro indicatore* (name (&rest args) (&rest pars) &body expr)
  (let ((exp-args (mapcar (lambda (arg) `(argomento ',arg)) args) )
        (exp-pars (mapcar (lambda (par) `(parametro ',(car par) ',(cadr par))) pars)))
    `(indicatore ',name (list ,@exp-args) (list ,@exp-pars)
       (let (,@(mapcar #`(,(car a1) ,(cadr a1)) (mapcar #'list (append args (mapcar #'car pars)) (append exp-args exp-pars)))) 
         ,@expr)))) 

(defprim periodo (mesi)
  (:pretty () (list 'periodo (list :mesi mesi)))
  (to-production () (multitags (text "Costrutto che prende in ingresso un numero e restituisce il costrutto la finestra mobile su cui calcolare le occorrenze di un determinato evento."))))

(defprim indice (name)
  (:pretty () (list 'indice (list :nome name)))
  (to-production () (multitags (text "Indice all'interno di una tabella."))))

(defprim parametro (name
                         type)
  (:pretty () (list 'parametro (list :nome name :tipo type)))
  (to-production () (multitags (text "Costrutto che prende in ingresso un nome e un tipo e restituisce il parametro da essi costituito."))))

(defprim argomento (type)
  (:pretty () (list 'argomento (list :tipo type)))
  (to-production () (multitags (text "Costrutto che prende in ingresso un tipo (a scelta tra soggetto e veicolo) e restituisce l'argomento da esso costituito."))))

(defmacro tabella (name anaph) 
  `(progn (defprim ,(symb name "%") (predicate)
      (:pretty () (list ',name (list :predicato (synth :pretty predicate))))
      (to-production () (multitags (text "Costrutto che prende in ingresso un predicato e restituisce la tabella dei ~a che soddisfano il predicato stesso. Nella definizione del predicato si pu&ograve; far uso dell'anafora \"~a\" che identifica il ~a di volta in volta sotto esame." ,(string-downcase (symbol-name name)) ,(string-downcase (symbol-name anaph)) ,(string-downcase (symbol-name anaph)))))
      (to-syntax () (text "(~a predicato)" ,(string-downcase name))))
          (defmacro ,name (predicate)
            `(let ((,',anaph (indice ',',anaph)))
              (,',(symb name "%") ,predicate)))))

(defmacro tabelle (&rest tabs)
  `(progn
     ,@(mapcar #'(lambda (tab)
		   `(tabella ,(car tab) ,(cadr tab)))
	       tabs)))
(tabelle (sinistri sinistro)
         (soggetti soggetto)
         (veicoli veicolo))

(synth :pretty (sinistri nil))
(defprim numero (query
                       cluster)
  (:pretty () `(numero (:e ,(synth :pretty query) :cluster ,(synth :pretty cluster))))
  (to-production () (multitags (text "Costrutto che prende in ingresso il risultato di un query (una tabella) e la lunghezza in mesi di una finestra temporale. Esso conta le occorrenze degli eventi descritti da ciascuna riga all'interno di una finestra mobile delle lunghezza specificata.")))
(to-syntax () (text "(numero tabella cluster)")))

(defprim e (&rest predicates)
  (:pretty () `(e (:predicati ,(synth-all :pretty predicates))))
  (to-production () (multitags (text "Predicato che prende in ingresso una lista di predicati e restituisce la loro congiunzione logica.")))
  (to-syntax () (text "(e pred1 ... pred n)")))

(defprim o (&rest predicates)
  (:pretty () `(o (:predicati ,(synth-all :pretty predicates))))
  (to-production () (multitags (text "Predicato che prende in ingresso una lista di predicati e restituisce la loro disgiunzione logica.")))
  (to-syntax () (text "(o pred1 ... pred n)")))

(defprim non (predicate)
  (:pretty () `(non (:predicato ,(synth :pretty predicate))))
  (to-production () (multitags (text "Predicato che prende in ingresso un predicato e ne restituisce la sua negazione logica.")))
  (to-syntax () (text "(non pred)")))



(defmacro ruolo (name) 
  `(defprim ,name (person
                          accident)
    (:pretty () (list ',name (list :soggetto person :sinistro accident)))
    (to-production () (multitags (text "Predicato che prende in ingresso i riferimenti a un soggetto e a un sinistro e restituisce vero se il soggetto &egrave; ~a nel sinistro" ,(string-downcase (symbol-name name)))))
    (to-syntax () (text "(~a persona sinistro)" ,(string-downcase (symbol-name name))))))

(defmacro ruoli (&rest names)
  `(progn
     ,@(mapcar #'(lambda (name)
		   `(ruolo ,name))
	       names)))


(ruoli coinvolto leso richiedente proprietario contraente deceduto testimone responsabile conducente patente-invalida)

(defmacro funzione (name desc) 
  `(defprim ,name (accident)
    (:pretty () (list ',name (list :sinistro accident)))
    (to-production () (multitags (text "Funzione che prende in ingresso il riferimento a un sinistro e restituisce ~a." ,desc)))
    (to-syntax () (text "(~a sinistro)" ,(string-downcase (symbol-name name))))))

(defmacro funzioni (&rest funcs)
  `(progn
     ,@(mapcar #'(lambda (func)
		   `(funzione ,(car func) ,(cadr func)))
	       funcs)))
(funzioni (numero-lesi "il numero di lesi presenti nel sinistro")
          (numero-fgvs "il numero di richieste FGVS presenti nel sinistro")
          (data-denuncia "la data della denuncia del sinistro")
          (data-accadimento "la data di accadimento del sinistro")
          (giorni-da-decorrenza "il numero di giorni trascorsi tra la decorrenza della polizza e il sinistro")
          (giorni-a-scadenza "il numero di giorni trascorsi tra il sinistro e la scadenza della polizza"))

(defmacro espressione (name prep) 
  `(defprim (,name ((num1 number) 
                         (num2 number)))
    (:pretty () (list ',name (list :num1 (synth :pretty num1) :num2 (synth :pretty num2))))
    (to-production () (multitags (text "Predicato che prende in ingresso due numeri e restituisce vero se il primo numero &egrave; ~a secondo." ,prep)))
    (to-syntax () (text "(~a num1 num2)" ,(string-downcase (symbol-name name))))))

(defmacro espressioni (&rest exps)
  `(progn
     ,@(mapcar #'(lambda (exp)
		   `(espressione ,(car exp) ,(cadr exp)))
	       exps)))
(espressioni (maggiore-o-uguale "maggiore o uguale del")
             (minore-o-uguale "minore o uguale del")
             (maggiore "maggiore del")
             (minore "minore del")
             (uguale "uguale al") 
             (diverso "diverso dal"))

;; (indicatore sco1 (soggetto) ((mesi numero) (occorrenze numero))
;;   (maggiore (numero (sinistri (or (coinvolto soggetto sinistro)
;;                                   (proprietario soggetto sinistro)))
;;                     (periodo mesi))
;;             occorrenze))


;; (indicatore sco4 (soggetto) ((mesi numero) (occorrenze numero))
;;             (maggiore (cluster 
;;                        (sinistri (e (maggiore (differenza 
;;                                                (denuncia sinistro)
;;                                                (accadimento sinistro))
;;                                               mesi)))
;;                        (periodo mesi))
;;                       occorrenze))

;; (indicatore sco6 (soggetto) ((mesi numero) (occorrenze numero))
;;             (maggiore (cluster (sinistri (testimone soggetto sinistro))
;;                                (periodo mesi))
;;                       occorrenze))
;; (indicatore sco7 (soggetto) ((mesi numero) (occorrenze numero))
;;             (maggiore (conta (sinistri (e (conducente soggetto sinistro)
;;                                           (patente-invalida soggetto sinistro))))
;;                       0))

;; (indicatore sco8 (soggetto) ((mesi numero) (occorrenze numero))
;;             (maggiore (cluster (sinistri (leso soggetto sinistro))
;;                                (periodo mesi))
;;                       occorrenze))

;; (indicatore sco9 (soggetto) ((mesi numero) (occorrenze numero))
;;             (maggiore (cluster (sinistri (fgvs sinistro))
;;                                (periodo mesi))
;;                       occorrenze))

;; (indicatore sco10 (soggetto) ((mesi numero) (occorrenze numero))
;;             (maggiore (cluster (sinistri (o (coinvolto soggetto sinistro)
;;                                             (proprietario soggetto sinistro)
;;                                             (maggiore (fgvs sinistro) 0)))
;;                                (periodo mesi))
;;                       occorrenze))
;; (indicatore con1 (soggetto) ((mesi numero) (occorrenze numero) (max-decorrenza numero) (max-scadenza numero))
;;             (maggiore (cluster (sinistri (o (contraente soggetto sinistro)
;;                                             (minore (giorni-decorrenza sinistro) max-decorrenza )
;;                                             (minore (giorni-scadenza sinistro) max-scadenza )))
;;                                (periodo mesi))
;;                       occorrenze))

;; (indicatore vei1 (veicolo) ((mesi numero) (occorrenze numero))
;;   (maggiore (numero (sinistri (presente veicolo sinistro))
;;                     (periodo mesi))
;;             occorrenze))

;; (indicatore vei3 (veicolo) ((mesi numero) (lesi numero) (occorrenze numero))
;;             (maggiore (numero (sinistri (e (presente veicolo sinistro)
;;                                            (maggiore (lesi sinistro) lesi))
;;                                         (periodo mesi)))
;;                       occorrenze))

;; (indicatore vei4 (veicolo) ((mesi numero) (lesi numero) (occorrenze numero))
;;             (maggiore (numero (sinistri (e (presente veicolo sinistro)
;;                                            (maggiore (lesi sinistro) lesi))
;;                                         (periodo mesi)))
;;                       occorrenze))
