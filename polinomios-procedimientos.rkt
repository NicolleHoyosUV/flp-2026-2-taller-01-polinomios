#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

;; Taller 1 — Polinomios dispersos.
;; Parte 2: representación basada en procedimientos.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio


;; Representación: cada dato es un procedimiento que recibe un mensaje
;; (un símbolo) y responde con el campo pedido. El mensaje 'tipo devuelve
;; el nombre de la variante y es lo que usan los predicados.






;;____________AUXILIAR DE LA REPRESENTACION____________________
;;Nombre: mensaje-invalido
;;Contrato: symbol x symbol -> error
;;Proposito: Levanta un error cuando a un dato se le envía un mensaje que no entiende.
(define mensaje-invalido
  (lambda (quien msg)
    (eopl:error quien "Mensaje desconocido: ~s" msg)))
 
;;____________CONSTRUCTORES____________________
(define poli
  (lambda (var terms)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'poli]
        [(eq? msg 'var) var]
        [(eq? msg 'terms) terms]
        [else (mensaje-invalido 'poli msg)]))))

(define nombre-var
  (lambda (s)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'nombre-var]
        [(eq? msg 's) s]
        [else (mensaje-invalido 'nombre-var msg)]))))

(define sin-terminos
  (lambda ()
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'sin-terminos]
        [else (mensaje-invalido 'sin-terminos msg)]))))

(define mas-terminos
  (lambda (term resto)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'mas-terminos]
        [(eq? msg 'term) term]
        [(eq? msg 'resto) resto]
        [else (mensaje-invalido 'mas-terminos msg)]))))

(define termino
  (lambda (coef expo)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'termino]
        [(eq? msg 'coef) coef]
        [(eq? msg 'expo) expo]
        [else (mensaje-invalido 'termino msg)]))))

(define coef-ent
  (lambda (n)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'coef-ent]
        [(eq? msg 'n) n]
        [else (mensaje-invalido 'coef-ent msg)]))))

(define coef-rac
  (lambda (num den)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'coef-rac]
        [(eq? msg 'num) num]
        [(eq? msg 'den) den]
        [else (mensaje-invalido 'coef-rac msg)]))))

(define expo-nat
  (lambda (k)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'expo-nat]
        [(eq? msg 'k) k]
        [else (mensaje-invalido 'expo-nat msg)]))))


;;____________EXTRACTORES______________________
;;Cada extractor le envía al dato el mensaje que corresponde a su campo.
(define poli->var (lambda (p) (p 'var)))
(define poli->terms (lambda (p) (p 'terms)))
 
(define nombre-var->s (lambda (v) (v 's)))
 
(define mas-terminos->term (lambda (terms) (terms 'term)))
(define mas-terminos->resto (lambda (terms) (terms 'resto)))
 
(define termino->coef (lambda (term) (term 'coef)))
(define termino->expo (lambda (term) (term 'expo)))
 
(define coef-ent->n (lambda (coef) (coef 'n)))
(define coef-rac->num (lambda (coef) (coef 'num)))
(define coef-rac->den (lambda (coef) (coef 'den)))
 
(define expo-nat->k (lambda (expo) (expo 'k)))
 
;;____________PREDICADOS______________________
;;Nombre: es-tipo?
;;Contrato: any x symbol -> boolean
;;Proposito: Indica si x es un procedimiento de la representación cuya variante es tipo.
(define es-tipo?
  (lambda (x tipo)
    (and (procedure? x) (eq? (x 'tipo) tipo))))
 
(define poli? (lambda (p) (es-tipo? p 'poli)))
(define nombre-var? (lambda (v) (es-tipo? v 'nombre-var)))
(define sin-terminos? (lambda (terms) (es-tipo? terms 'sin-terminos)))
(define mas-terminos? (lambda (terms) (es-tipo? terms 'mas-terminos)))
(define termino? (lambda (t) (es-tipo? t 'termino)))
(define coef-ent? (lambda (coef) (es-tipo? coef 'coef-ent)))
(define coef-rac? (lambda (coef) (es-tipo? coef 'coef-rac)))
(define expo-nat? (lambda (expo) (es-tipo? expo 'expo-nat)))




;;____________CONVERSORES DE CONCRETO A ABSTRACTO_______________
(define abstracto-coef
  (lambda (c)
    (if (integer? c)
        (coef-ent c)
        (coef-rac (numerator c) (denominator c)))))
 
(define concreto-coef
  (lambda (c-tad)
    (if (coef-ent? c-tad)
        (coef-ent->n c-tad)
        (/ (coef-rac->num c-tad) (coef-rac->den c-tad)))))
 
(define abstracto-expo (lambda (e) (expo-nat e)))
(define concreto-expo (lambda (e-tad) (expo-nat->k e-tad)))



;;____________INTERFAZ DEL TAD___________________
;;funciones de polinomios-listas.rkt
(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino)
 
;;Nombre:polinomio-cero
;;Contrato:symbol -> polinomio
;;Proposito:Retorna el polinomio nulo en la variable que se da.
(define polinomio-cero
  (lambda (variable)
    (if (symbol? variable)
        (poli (nombre-var variable) (sin-terminos))
        (eopl:error 'polinomio-cero "La variable debe ser un símbolo: ~s" variable))))
;; Función auxiliar para recorrer la lista de términos
(define insertar-termino-aux
  (lambda (terms c e)
    (if (sin-terminos? terms)
        (mas-terminos (termino (abstracto-coef c) (abstracto-expo e)) (sin-terminos))
        (let ([actual-coef (concreto-coef (termino->coef (mas-terminos->term terms)))]
              [actual-expo (concreto-expo (termino->expo (mas-terminos->term terms)))]
              [resto (mas-terminos->resto terms)])
          (cond
            [(> e actual-expo)
             (mas-terminos (termino (abstracto-coef c) (abstracto-expo e)) terms)]
            [(= e actual-expo)
             (if (= (+ c actual-coef) 0)
                 resto
                 (mas-terminos (termino (abstracto-coef (+ c actual-coef)) (abstracto-expo e)) resto))]
            [else
             (mas-terminos (mas-terminos->term terms) (insertar-termino-aux resto c e))])))))
 
;;Nombre:insertar-termino
;;Contrato:polinomio x exact-number x exponente -> polinomio
;;Proposito:Inserta un término conservando el orden decreciente y la simplificación.
(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (cond
      [(not (and (rational? coeficiente) (exact? coeficiente)))
 (eopl:error 'insertar-termino "El coeficiente debe ser un número exacto")]
      [(or (not (integer? exponente)) (< exponente 0))
       (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo")]
      [(= coeficiente 0) polinomio]
      [else
       (poli (poli->var polinomio)
             (insertar-termino-aux (poli->terms polinomio) coeficiente exponente))])))
;; Función auxiliar para buscar el coeficiente
(define coeficiente-de-aux
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")
        (let ([actual-coef (concreto-coef (termino->coef (mas-terminos->term terms)))]
              [actual-expo (concreto-expo (termino->expo (mas-terminos->term terms)))])
          (cond
            [(= e actual-expo) actual-coef]
            [(< e actual-expo) (coeficiente-de-aux (mas-terminos->resto terms) e)]
            [else (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")])))))
 
;;Nombre:coeficiente-de
;;Contrato:polinomio x exponente -> exact-number
;;Proposito:Busca y retorna el coeficiente correspondiente al exponente dado.
(define coeficiente-de
  (lambda (polinomio exponente)
    (cond
      [(or (not (integer? exponente)) (< exponente 0))
       (eopl:error 'coeficiente-de "El exponente debe ser un entero no negativo")]
      [else
       (coeficiente-de-aux (poli->terms polinomio) exponente)])))
;; Función auxiliar para eliminar un término
(define eliminar-termino-aux
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")
        (let ([actual-expo (concreto-expo (termino->expo (mas-terminos->term terms)))])
          (cond
            [(= e actual-expo) (mas-terminos->resto terms)]
            [(< e actual-expo) (mas-terminos (mas-terminos->term terms)
                                              (eliminar-termino-aux (mas-terminos->resto terms) e))]
            [else (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")])))))
 
;;Nombre:eliminar-termino
;;Contrato:polinomio x exponente -> polinomio
;;Proposito:Elimina el término correspondiente al exponente dado.
(define eliminar-termino
  (lambda (polinomio exponente)
    (cond
      [(or (not (integer? exponente)) (< exponente 0))
       (eopl:error 'eliminar-termino "El exponente debe ser un entero no negativo")]
      [else
       (poli (poli->var polinomio)
             (eliminar-termino-aux (poli->terms polinomio) exponente))])))

