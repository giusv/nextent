(in-package :indy)

(defun factor (env)
  (choose-among (do-with ((nested (sublist)))
                  (let ((f (parse (factor env) nested)))
                    (result (somma (variabile lhs) (costante rhs)))))
                (do-with ((lhs (factor env))
                          ((sym '/))
                          (rhs (factor env)))
                  (if (getf env lhs)
                      (result (minore (variabile lhs) (costante rhs)))
                      (fail)))))

(defun indicator ()
  (do-with ((name (item))
            (all-args (sublist))
            (all-pars (sublist))
            (exp (boolean-expression (list 'a (variabile 'a)))))
    (let ((args (parse (sepby1 (do-with ((arg (item)))
                                 (result (argomento arg))) (sym '&)) 
                       all-args))
          (pars (parse (sepby1 (do-with ((name (item))
                                         ((sym '=))
                                         (type (item)))
                                 (result (parametro name type)))
                               (sym '&))
                       all-pars)))
      (result (indicatore% name args pars exp)))))

(defun boolean-expression (env)
  (choose-among (do-with ((s (sia)))
                  (result (indice s)))
                (do-with ((terms (sepby (boolean-term env) (sym 'or))))
                  (result (apply #'o terms)))))

(defun boolean-term (env)
  (do-with ((factors (sepby (boolean-factor env) (sym 'and))))
    (result (apply #'e factors))))

(defun boolean-factor (env)
  (choose-among (do-with (((sym '!))
                          (f (boolean-factor env)))
                  (result (non f)))
                (relation env)))
(defun relation (env)
  (choose-among (do-with ((lhs (expression env))
                          ((sym '=))
                          (rhs (expression env)))
                  (result (uguale (variabile lhs) (costante rhs))))
                (do-with ((lhs (expression env))
                          ((sym '<))
                          (rhs (expression env)))
                  (if (getf env lhs)
                      (result (minore (variabile lhs) (costante rhs)))
                      (fail)))))

(defun expression (env)
  (choose-among (do-with ((lhs (term env))
                          ((sym '+))
                          (rhs (term env)))
                  (result (somma (variabile lhs) (costante rhs))))
                (do-with ((lhs (term env))
                          ((sym '-))
                          (rhs (term env)))
                  (if (getf env lhs)
                      (result (minore (variabile lhs) (costante rhs)))
                      (fail)))))

(defun term (env)
  (choose-among (do-with ((lhs (factor env))
                          ((sym '*))
                          (rhs (factor env)))
                  (result (somma (variabile lhs) (costante rhs))))
                (do-with ((lhs (factor env))
                          ((sym '/))
                          (rhs (factor env)))
                  (if (getf env lhs)
                      (result (minore (variabile lhs) (costante rhs)))
                      (fail)))))


(defun sia ()
  (sym 'sia))


(pprint (synth :pretty (parse (indicator) 
                              '(sco1 (soggetto & veicolo) (mesi = 5 & soglia = 3) & < 1 and a < 3 or (a < 0)))))



;; (parse (sepby1 (item) (sym '&)) '(a b))
