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


;; Representación: Es cada dato es un procedimiento que recibe un mensaje
;; (un símbolo) y responde con el campo pedido. El mensaje 'tipo devuelve
;; el nombre de la variante y es lo que usan los predicados.






;;____________AUXILIAR DE LA REPRESENTACION____________________

;;Nombre: dato-no-entiende
;;Contrato: symbol x symbol -> error
;;Proposito: Levanta un error cuando a un dato se le envía un mensaje que no entiende.
;;helper para no repetir el mismo eopl:error en los 8 constructores.
(define dato-no-entiende
  (lambda (quien msg)
    (eopl:error quien "El dato no se entiende: ~s" msg)))
 
;;____________CONSTRUCTORES____________________
(define poli
  (lambda (var terms)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'poli]
        [(eq? msg 'var) var]
        [(eq? msg 'terms) terms]
        [else (dato-no-entiende 'poli msg)]))))

(define nombre-var
  (lambda (s)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'nombre-var]
        [(eq? msg 's) s]
        [else (dato-no-entiende 'nombre-var msg)]))))

(define sin-terminos
  (lambda ()
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'sin-terminos]
        [else (dato-no-entiende 'sin-terminos msg)]))))

(define mas-terminos
  (lambda (term resto)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'mas-terminos]
        [(eq? msg 'term) term]
        [(eq? msg 'resto) resto]
        [else (dato-no-entiende 'mas-terminos msg)]))))

(define termino
  (lambda (coef expo)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'termino]
        [(eq? msg 'obtener-coef) coef]
        [(eq? msg 'expo) expo]
        [else (dato-no-entiende 'termino msg)]))))

(define coef-ent
  (lambda (n)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'coef-ent]
        [(eq? msg 'n) n]
        [else (dato-no-entiende 'coef-ent msg)]))))

(define coef-rac
  (lambda (num den)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'coef-rac]
        [(eq? msg 'num) num]
        [(eq? msg 'den) den]
        [else (dato-no-entiende 'coef-rac msg)]))))

(define expo-nat
  (lambda (k)
    (lambda (msg)
      (cond
        [(eq? msg 'tipo) 'expo-nat]
        [(eq? msg 'k) k]
        [else (dato-no-entiende 'expo-nat msg)]))))


;;____________EXTRACTORES______________________
;;Cada extractor le envía al dato el mensaje que corresponde a su campo.

(define poli->var (lambda (p) (p 'var)))
(define poli->terms (lambda (p) (p 'terms)))
 
(define nombre-var->s (lambda (v) (v 's)))
 
(define mas-terminos->term (lambda (terms) (terms 'term)))
(define mas-terminos->resto (lambda (terms) (terms 'resto)))
 
(define termino->coef (lambda (term) (term 'obtener-coef)))
(define termino->expo (lambda (term) (term 'expo)))
 
(define coef-ent->n (lambda (obtener-coef) (obtener-coef 'n)))
(define coef-rac->num (lambda (obtener-coef) (obtener-coef 'num)))
(define coef-rac->den (lambda (obtener-coef) (obtener-coef 'den)))
 
(define expo-nat->k (lambda (expo) (expo 'k)))
 
;;____________PREDICADOS______________________
;;Nombre: variante?
;;Contrato: any x symbol -> boolean
;;Proposito: Indica si x es un procedimiento de la representación cuyo tipo es una variante.
(define variante?
  (lambda (x tipo)
    (and (procedure? x) (eq? (x 'tipo) tipo))))
 
(define poli? (lambda (p) (variante? p 'poli)))
(define nombre-var? (lambda (v) (variante? v 'nombre-var)))
(define sin-terminos? (lambda (terms) (variante? terms 'sin-terminos)))
(define mas-terminos? (lambda (terms) (variante? terms 'mas-terminos)))
(define termino? (lambda (t) (variante? t 'termino)))
(define coef-ent? (lambda (obtener-coef) (variante? obtener-coef 'coef-ent)))
(define coef-rac? (lambda (obtener-coef) (variante? obtener-coef 'coef-rac)))
(define expo-nat? (lambda (expo) (variante? expo 'expo-nat)))




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
      [(not (and (number? coeficiente) (exact? coeficiente)))
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










;; EJEMPLOS DE CONSTRUCTORES Y OBSERVADOREs


;; (define ver (lambda (x) (display x) (newline))))

;; 1. Término con coeficiente entero: 4x^5
;; (define t1 (termino (coef-ent 4) (expo-nat 5)))
;; (coef-ent->n (termino->coef t1))                 ; => 4
;; (expo-nat->k (termino->expo t1))                 ; => 5
;; (termino? t1)                                    ; => #t

;; 2. Término con coeficiente racional: -(3/2)x^2
;; (define t2 (termino (coef-rac -3 2) (expo-nat 2)))
;; (coef-rac->num (termino->coef t2))               ; => -3
;; (coef-rac->den (termino->coef t2))               ; => 2
;; (coef-ent? (termino->coef t2))                   ; => #f

;; 3. Variable
;; (nombre-var->s (nombre-var 'x))                  ; => x

;; 4. Lista de términos
;; (define ts (mas-terminos t1 (mas-terminos t2 (sin-terminos))))
;; (mas-terminos? ts)                               ; => #t
;; (sin-terminos? (mas-terminos->resto (mas-terminos->resto ts)))  ; => #t

;; 5. Polinomio completo
;; (define pp (poli (nombre-var 'x) ts))
;; (poli? pp)                                       ; => #t
;; (nombre-var->s (poli->var pp))                   ; => x


;; ______________________________________________________________________________
;; EJEMPLOS DE USO DE LA INTERFAZ
;; ______________________________________________________________________________
;; (define P0 (polinomio-cero 'x))
;; (define P1 (insertar-termino P0 7 3))                                  ; 7x^3
;; (define P3 (insertar-termino (insertar-termino (insertar-termino P0 4 4) -1/2 2) 9 0))  ; 4x^4 - 1/2x^2 + 9

;; --- polinomio-cero ---
;; (nombre-var->s (poli->var (polinomio-cero 'x)))           ; => x
;; (nombre-var->s (poli->var (polinomio-cero 'y)))           ; => y
;; (sin-terminos? (poli->terms (polinomio-cero 'z)))         ; => #t
;; (poli? (polinomio-cero 'variable_larga))                  ; => #t
;; (polinomio-cero 123)                                      ; [Error] la variable debe ser un símbolo

;; --- insertar-termino ---
;; (coeficiente-de (insertar-termino P0 5 4) 4)              ; => 5
;; (coeficiente-de (insertar-termino P1 3 3) 3)              ; => 10
;; (sin-terminos? (poli->terms (insertar-termino P1 -7 3)))  ; => #t (se cancela)
;; (coeficiente-de (insertar-termino P1 0 10) 3)             ; => 7 (coeficiente cero no altera)
;; (insertar-termino P0 3 -2)                                ; [Error] exponente negativo

;; --- coeficiente-de ---
;; (coeficiente-de P3 4)                                     ; => 4
;; (coeficiente-de P3 2)                                     ; => -1/2
;; (coeficiente-de P3 0)                                     ; => 9
;; (coeficiente-de P1 3)                                     ; => 7
;; (coeficiente-de P3 3)                                     ; [Error] exponente inexistente

;; --- eliminar-termino ---
;; (coeficiente-de (eliminar-termino P3 4) 0)                ; => 9
;; (coeficiente-de (eliminar-termino P3 2) 4)                ; => 4
;; (coeficiente-de (eliminar-termino P3 0) 2)                ; => -1/2
;; (sin-terminos? (poli->terms (eliminar-termino P1 3)))     ; => #t
;; (eliminar-termino P3 5)                                   ; [Error] exponente inexistente