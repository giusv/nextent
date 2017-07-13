(in-package :indy)

;; (defprod indicatore (indicatore ((nome string))
;;                                 (:pretty () (list 'indicatore (list :nome nome)))))

(defprim indicatore% (name args pars expr)
  (:pretty () (list 'indicatore (list :name name :args (synth-all :pretty args) :pars (synth-all :pretty pars) :expr (synth :pretty expr)))))

(defmacro indicatore (name (&rest args) (&rest pars) &body expr)
  (let ((exp-args (mapcar (lambda (arg) `(argomento ',arg)) args) )
        (exp-pars (mapcar (lambda (par) `(parametro ',(car par) ',(cadr par))) pars)))
    `(indicatore% ',name (list ,@exp-args) (list ,@exp-pars)
       (let (,@(mapcar #`(,(car a1) ,(cadr a1)) (mapcar #'list (append args (mapcar #'car pars)) (append exp-args exp-pars)))) 
         ,@expr)))) 

(defprim periodo (mesi)
  (:pretty () (list 'periodo (list :mesi mesi))))

(defprim indice (name)
  (:pretty () (list 'indice (list :nome name))))

(defprim parametro (name type)
  (:pretty () (list 'parametro (list :nome name :tipo type))))

(defprim argomento (type)
  (:pretty () (list 'argomento (list :tipo type))))

(defmacro tabella (name anaph) 
  `(progn (defprim ,(symb name "%") (predicate)
            (:pretty () (list ',name (list :predicato (synth :pretty predicate)))))
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
  (:pretty () `(numero (:e ,(synth :pretty query) :cluster ,(synth :pretty cluster)))))

(defprim e (&rest predicates)
  (:pretty () `(e (:predicati ,(synth-all :pretty predicates)))))

(defprim o (&rest predicates)
  (:pretty () `(o (:predicati ,(synth-all :pretty predicates)))))

(defprim non (predicate)
  (:pretty () `(non (:predicato ,(synth :pretty predicate)))))

(defprim variabile (name)
  (:pretty () `(variabile (:name ,name))))

(defprim costante (value)
  (:pretty () `(costante (:value ,value))))

(defmacro ruolo (name) 
  `(defprim ,name (person
                          accident)
    (:pretty () (list ',name (list :soggetto person :sinistro accident)))))

(defmacro ruoli (&rest names)
  `(progn
     ,@(mapcar #'(lambda (name)
		   `(ruolo ,name))
	       names)))


(ruoli coinvolto leso richiedente proprietario contraente deceduto testimone responsabile conducente patente-invalida)

(defmacro funzione (name desc) 
  `(defprim ,name (accident)
    (:pretty () (list ',name (list :sinistro accident)))))

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

(defmacro espressione (name) 
  `(defprim ,name (num1 num2)
    (:pretty () (list ',name (list :num1 (synth :pretty num1) :num2 (synth :pretty num2))))))

(defmacro espressioni (&rest exps)
  `(progn
     ,@(mapcar #'(lambda (exp)
		   `(espressione ,exp))
	       exps)))

(espressioni maggiore-o-uguale minore-o-uguale maggiore minore uguale diverso)


(pprint (synth :pretty 
               (indicatore sco1 (soggetto) ((mesi numero) (occorrenze numero))
                 (maggiore (numero (sinistri (or (coinvolto soggetto sinistro)
                                                 (proprietario soggetto sinistro)))
                                   (periodo mesi))
                           occorrenze))))
