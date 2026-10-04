#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

;; Taller 1 — Polinomios dispersos.
;; Parte 1: representación basada en listas.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio

;;____________CONSTRUCTORES____________________
(define poli(lambda (var terms) (list 'poli var terms)))
(define nombre-var(lambda (s) (list 'nombre-var s)))
(define sin-terminos (lambda () (list 'sin-terminos)))
(define mas-terminos(lambda (term resto) (list 'mas-terminos term resto)))
(define termino(lambda (coef expo) (list 'termino coef expo)))
(define coef-ent(lambda (n) (list 'coef-ent n)))
(define coef-rac(lambda (num den) (list 'coef-rac num den)))
(define expo-nat(lambda (k) (list 'expo-nat k)))

;;____________EXTRACTORES______________________
(define poli->var (lambda (p) (cadr p)))
(define poli->terms (lambda (p) (caddr p)))

(define nombre-var->s (lambda (v) (cadr v)))

(define mas-terminos->term (lambda (terms) (cadr terms)))
(define mas-terminos->resto (lambda (terms) (caddr terms)))

(define termino->coef (lambda (term) (cadr term)))
(define termino->expo (lambda (term) (caddr term)))

(define coef-ent->n (lambda (coef) (cadr coef)))
(define coef-rac->num (lambda (coef) (cadr coef)))
(define coef-rac->den (lambda (coef) (caddr coef)))

(define expo-nat->k (lambda (expo) (cadr expo)))

;;____________PREDICADOS______________________
(define poli? (lambda (p) (and (list? p) (eqv? (car p) 'poli))))
(define nombre-var? (lambda (v) (and (list? v) (eqv? (car v) 'nombre-var))))
(define sin-terminos? (lambda (terms) (eqv? (car terms) 'sin-terminos)))
(define mas-terminos? (lambda (terms) (eqv? (car terms) 'mas-terminos)))
(define termino? (lambda (t) (and (list? t) (eqv? (car t) 'termino))))
(define coef-ent? (lambda (coef) (eqv? (car coef) 'coef-ent)))
(define coef-rac? (lambda (coef) (eqv? (car coef) 'coef-rac)))
(define expo-nat? (lambda (expo) (eqv? (car expo) 'expo-nat)))

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

;; ______________________________________________________________________________
;; 5 EJEMPLOS DE CONSTRUCCIÓN (Usando únicamente las 4 funciones de la interfaz)
;; ______________________________________________________________________________

;; 1. Polinomio nulo: P0 = 0 (en x)
;; (define P0 (polinomio-cero 'x))

;; 2. Un término entero: P1 = 7x^3
;; (define P1 (insertar-termino P0 7 3))

;; 3. Dos términos racionales: P2 = 3/4x^5 - 2x
;; (define P2 (insertar-termino (insertar-termino P0 3/4 5) -2 1))

;; 4. Tres términos con término independiente: P3 = 4x^4 - 1/2x^2 + 9
;; (define P3 (insertar-termino (insertar-termino (insertar-termino P0 4 4) -1/2 2) 9 0))

;; 5. Polinomio en otra variable: P4 = 5y^2 + 3y
;; (define P4 (insertar-termino (insertar-termino (polinomio-cero 'y) 5 2) 3 1))


;; ______________________________________________________________________________
;; 5 EJEMPLOS DE USO PARA CADA FUNCIÓN DE LA INTERFAZ
;; ______________________________________________________________________________

;; --- 1. polinomio-cero ---
;; (polinomio-cero 'x)                             ; 1.1: Creación estándar en variable 'x
;; (polinomio-cero 'y)                             ; 1.2: Creación en variable 'y
;; (polinomio-cero 'z)                             ; 1.3: Creación en variable 'z
;; (polinomio-cero 'variable_larga)                ; 1.4: Creación con identificador largo
;; (polinomio-cero 123)                            ; 1.5 [Error]: La variable debe ser un símbolo


;; --- 2. insertar-termino ---
;; (insertar-termino P0 5 4)                       ; 2.1: Insertar en polinomio nulo -> 5x^4
;; (insertar-termino P1 3 3)                       ; 2.2: Sumar a término existente (7+3) -> 10x^3
;; (insertar-termino P1 -7 3)                      ; 2.3: Cancelación por suma a cero -> Polinomio nulo
;; (insertar-termino P1 0 10)                      ; 2.4: Coeficiente cero no altera -> 7x^3
;; (insertar-termino P0 3 -2)                      ; 2.5 [Error]: Exponente negativo


;; --- 3. coeficiente-de ---
;; (coeficiente-de P3 4)                           ; 3.1: Coeficiente entero existente -> 4
;; (coeficiente-de P3 2)                           ; 3.2: Coeficiente racional -> -1/2
;; (coeficiente-de P3 0)                           ; 3.3: Término independiente -> 9
;; (coeficiente-de P1 3)                           ; 3.4: Coeficiente en polinomio de un solo término -> 7
;; (coeficiente-de P3 3)                           ; 3.5 [Error]: Exponente inexistente


;; --- 4. eliminar-termino ---
;; (eliminar-termino P3 4)                         ; 4.1: Eliminar término de mayor grado -> -1/2x^2 + 9
;; (eliminar-termino P3 2)                         ; 4.2: Eliminar término intermedio -> 4x^4 + 9
;; (eliminar-termino P3 0)                         ; 4.3: Eliminar término independiente -> 4x^4 - 1/2x^2
;; (eliminar-termino P1 3)                         ; 4.4: Eliminar único término -> Retorna polinomio nulo
;; (eliminar-termino P3 5)                         ; 4.5 [Error]: Intentar eliminar término inexistente