#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2

;; Taller 1 — Polinomios dispersos.
;; Parte 3: representación con datatypes.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio
;;   sumar             : polinomio x polinomio -> polinomio

(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino sumar)


;; DEFINICIÓN DE LOS DATATYPES

(define-datatype variable variable?
  (nombre-var
   (s symbol?)))

(define-datatype coeficiente coeficiente?
  (coef-ent
   (n integer?))
  (coef-rac
   (num integer?)
   (den positive?)))

(define-datatype exponente exponente?
  (expo-nat
   (k integer?)))

(define-datatype termino-tad termino?
  (termino
   (coef coeficiente?)
   (expo exponente?)))


(define-datatype terminos terminos?
  (sin-terminos)
  (mas-terminos
   (term termino?)
   (resto terminos?)))


(define-datatype polinomio polinomio?
  (poli
   (var variable?)
   (terms terminos?)))


;; FUNCIONES AUXILIARES
;; Obtiene el simbolo almacenado en una variable

(define nombre-variable
  (lambda (v)
    (cases variable v
      (nombre-var (s)
                  s))))


;; Obtiene el valor concreto de un coeficiente

(define valor-coeficiente
  (lambda (coef)
    (cases coeficiente coef
      (coef-ent (n)
                n)
      (coef-rac (num den)
                (/ num den)))))


;; Obtiene el valor concreto del exponente

(define valor-exponente
  (lambda (expo)
    (cases exponente expo
      (expo-nat (k)
                k))))

;; Convierte un número racional exacto en un coeficiente

(define construir-coeficiente
  (lambda (c)
    (cond
      ((integer? c)
       (coef-ent c))
      ((and (rational? c)
            (exact? c))
       (coef-rac (numerator c)
                 (denominator c)))
      (else
       (eopl:error 'construir-coeficiente
                   "El coeficiente debe ser un número racional exacto")))))


;; Convierte un entero no negativo en un exponente

(define construir-exponente
  (lambda (k)
    (if (and (integer? k)
             (>= k 0))
        (expo-nat k)
        (eopl:error 'construir-exponente
                    "El exponente debe ser un entero no negativo"))))


;; Construye un termino a partir de un coeficiente y un exponente

(define construir-termino
  (lambda (coef expo)
    (termino
     (construir-coeficiente coef)
     (construir-exponente expo))))


;; Construye el polinomio cero para una variable dada

(define polinomio-cero
  (lambda (variable)
    (if (symbol? variable)
        (poli
         (nombre-var variable)
         (sin-terminos))
        (eopl:error 'polinomio-cero
                    "La variable debe ser un símbolo"))))


;; Inserta un termino manteniendo los exponentes en orden descendente, si el exponente ya existe, suma coeficientes
;; Si la suma es cero, elimina el termino

(define insertar-termino
  (lambda (p coeficiente exponente)

    (if (not (and (integer? exponente)
                  (>= exponente 0)))
        (eopl:error 'insertar-termino
                    "El exponente debe ser un entero no negativo")
        (if (not (and (rational? coeficiente)
                       (exact? coeficiente)))
            (eopl:error 'insertar-termino
                        "El coeficiente debe ser un número racional exacto")

            (cases polinomio p

              (poli (var terms)

                    (poli
                     var
                     (insertar-en-terminos
                      terms
                      coeficiente
                      exponente))))))))


;; Inserta el termino en la posición correcta sin ordenar al final

(define insertar-en-terminos
  (lambda (terms coeficiente exponente)

    (cases terminos terms

      (sin-terminos ()
                    (if (= coeficiente 0)
                        (sin-terminos)
                        (mas-terminos
                         (construir-termino coeficiente exponente)
                         (sin-terminos))))

      (mas-terminos (term resto)

                    (let ((expo-actual
                           (valor-exponente
                            (cases termino-tad term
                              (termino (coef expo)
                                       expo)))))

                      (cond

                        ((> exponente expo-actual)
                         (mas-terminos
                          (construir-termino
                           coeficiente
                           exponente)
                          terms))

                        ((= exponente expo-actual)

                         (let ((nuevo-coef
                                (+ coeficiente
                                   (valor-coeficiente
                                    (cases termino-tad term
                                      (termino (coef expo)
                                               coef))))))

                           (if (= nuevo-coef 0)

                               resto

                               (mas-terminos
                                (construir-termino
                                 nuevo-coef
                                 exponente)
                                resto))))

                        (else
                         (mas-terminos
                          term
                          (insertar-en-terminos
                           resto
                           coeficiente
                           exponente)))))))))



;; Produce error si el término no existe

(define coeficiente-de
  (lambda (p exponente)

    (if (not (and (integer? exponente)
                  (>= exponente 0)))
        (eopl:error 'coeficiente-de
                    "El exponente debe ser un entero no negativo")

        (cases polinomio p

          (poli (var terms)

                (buscar-coeficiente
                 terms
                 exponente))))))


;; Busca el coeficiente correspondiente al exponente

(define buscar-coeficiente
  (lambda (terms exponente)

    (cases terminos terms

      (sin-terminos ()
                    (eopl:error 'coeficiente-de
                                "El exponente no existe"))

      (mas-terminos (term resto)

                    (cases termino-tad term

                      (termino (coef expo)

                               (let ((expo-actual
                                      (valor-exponente expo)))

                                 (cond

                                   ((= exponente expo-actual)
                                    (valor-coeficiente coef))

                                   ((> exponente expo-actual)
                                    (eopl:error
                                     'coeficiente-de
                                     "El exponente no existe"))

                                   (else
                                    (buscar-coeficiente
                                     resto
                                     exponente))))))))))



;; Produce error si el término no existe

(define eliminar-termino
  (lambda (p exponente)

    (if (not (and (integer? exponente)
                  (>= exponente 0)))
        (eopl:error 'eliminar-termino
                    "El exponente debe ser un entero no negativo")

        (cases polinomio p

          (poli (var terms)

                (poli
                 var
                 (eliminar-de-terminos
                  terms
                  exponente)))))))



;; Elimina el termino indicado

(define eliminar-de-terminos
  (lambda (terms exponente)

    (cases terminos terms

      (sin-terminos ()
                    (eopl:error 'eliminar-termino
                                "El exponente no existe"))

      (mas-terminos (term resto)

                    (cases termino-tad term

                      (termino (coef expo)

                               (let ((expo-actual
                                      (valor-exponente expo)))

                                 (cond

                                   ((= exponente expo-actual)
                                    resto)

                                   ((> exponente expo-actual)
                                    (eopl:error
                                     'eliminar-termino
                                     "El exponente no existe"))

                                   (else
                                    (mas-terminos
                                     term
                                     (eliminar-de-terminos
                                      resto
                                      exponente)))))))))))


(define sumar
  (lambda (p q)

    (cases polinomio p

      (poli (var-p terms-p)

            (cases polinomio q

              (poli (var-q terms-q)

                    (if (not
                         (equal?
                          (nombre-variable var-p)
                          (nombre-variable var-q)))

                        (eopl:error
                         'sumar
                         "Los polinomios deben tener la misma variable")

                        (poli
                         var-p
                         (sumar-terminos
                          terms-p
                          terms-q)))))))))



;; Suma dos listas de terminos recorriéndolas en paralelo los términos con igual exponente se combinan y si su suma
;; es cero se eliminan

(define sumar-terminos
  (lambda (terms-p terms-q)

    (cases terminos terms-p

      (sin-terminos ()

                    terms-q)

      (mas-terminos (term-p resto-p)

                    (cases terminos terms-q

                      (sin-terminos ()

                                    terms-p)

                      (mas-terminos (term-q resto-q)

                                    (cases termino-tad term-p

                                      (termino (coef-p expo-p)

                                        (cases termino-tad term-q

                                          (termino (coef-q expo-q)

                                            (let ((exp-p
                                                   (valor-exponente expo-p))
                                                  (exp-q
                                                   (valor-exponente expo-q))
                                                  (coef-p-valor
                                                   (valor-coeficiente coef-p))
                                                  (coef-q-valor
                                                   (valor-coeficiente coef-q)))

                                              (cond

                                                ((> exp-p exp-q)

                                                 (mas-terminos
                                                  term-p
                                                  (sumar-terminos
                                                   resto-p
                                                   terms-q)))

                                                ((< exp-p exp-q)

                                                 (mas-terminos
                                                  term-q
                                                  (sumar-terminos
                                                   terms-p
                                                   resto-q)))

                                                (else

                                                 (let ((suma
                                                        (+ coef-p-valor
                                                           coef-q-valor)))

                                                   (if (= suma 0)

                                                       (sumar-terminos
                                                        resto-p
                                                        resto-q)

                                                       (mas-terminos
                                                        (construir-termino
                                                         suma
                                                         exp-p)
                                                        (sumar-terminos
                                                         resto-p
                                                         resto-q)))))))))))))))))

;; EJEMPLOS DE CONSTRUCCIÓN DE DATOS

;; Ejemplo 1:
;; p1 representa 4x^5 - (3/2)x^2 + 7

(define p1
  (poli
   (nombre-var 'x)
   (mas-terminos
    (termino (coef-ent 4) (expo-nat 5))
    (mas-terminos
     (termino (coef-rac -3 2) (expo-nat 2))
     (mas-terminos
      (termino (coef-ent 7) (expo-nat 0))
      (sin-terminos))))))


;; Ejemplo 2:
;; p2 representa -4x^5 + (1/2)x^2 + 2x

(define p2
  (poli
   (nombre-var 'x)
   (mas-terminos
    (termino (coef-ent -4) (expo-nat 5))
    (mas-terminos
     (termino (coef-rac 1 2) (expo-nat 2))
     (mas-terminos
      (termino (coef-ent 2) (expo-nat 1))
      (sin-terminos))))))


;; Ejemplo 3:
;; p3 representa 3x^4 - 5x

(define p3
  (poli
   (nombre-var 'x)
   (mas-terminos
    (termino (coef-ent 3) (expo-nat 4))
    (mas-terminos
     (termino (coef-ent -5) (expo-nat 1))
     (sin-terminos)))))


;; Ejemplo 4:
;; p4 representa el polinomio cero en la variable y

(define p4
  (poli
   (nombre-var 'y)
   (sin-terminos)))


;; Ejemplo 5:
;; p5 representa 2x^6 + (5/3)x^3 - 1

(define p5
  (poli
   (nombre-var 'x)
   (mas-terminos
    (termino (coef-ent 2) (expo-nat 6))
    (mas-terminos
     (termino (coef-rac 5 3) (expo-nat 3))
     (mas-terminos
      (termino (coef-ent -1) (expo-nat 0))
      (sin-terminos))))))



;; EJEMPLOS DE polinomio-cero

;; Ejemplo 1
(define ejemplo-cero-1
  (polinomio-cero 'x))

;; Ejemplo 2
(define ejemplo-cero-2
  (polinomio-cero 'y))

;; Ejemplo 3
(define ejemplo-cero-3
  (polinomio-cero 'z))


;; EJEMPLOS DE insertar-termino

;; Ejemplo 1: insertar un término con exponente nuevo
(define ejemplo-insertar-1
  (insertar-termino p1 6 3))

;; Resultado esperado:
;; 4x^5 + 6x^3 - (3/2)x^2 + 7


;; Ejemplo 2: insertar sobre un exponente que ya existe
(define ejemplo-insertar-2
  (insertar-termino p1 3/2 2))

;; Resultado esperado:
;; 4x^5 + 7x^2 + 7


;; Ejemplo 3: insertar un coeficiente cero
(define ejemplo-insertar-3
  (insertar-termino p1 0 4))

;; Resultado esperado:
;; 4x^5 - (3/2)x^2 + 7


;; EJEMPLOS DE coeficiente-de

;; Ejemplo 1
(define ejemplo-coeficiente-1
  (coeficiente-de p1 5))
;; Resultado esperado: 4


;; Ejemplo 2
(define ejemplo-coeficiente-2
  (coeficiente-de p1 2))
;; Resultado esperado: -3/2


;; Ejemplo 3
(define ejemplo-coeficiente-3
  (coeficiente-de p1 0))
;; Resultado esperado: 7


;; EJEMPLOS DE eliminar-termino

;; Ejemplo 1
(define ejemplo-eliminar-1
  (eliminar-termino p1 5))

;; Resultado esperado:
;; -(3/2)x^2 + 7


;; Ejemplo 2
(define ejemplo-eliminar-2
  (eliminar-termino p1 2))

;; Resultado esperado:
;; 4x^5 + 7


;; Ejemplo 3
(define ejemplo-eliminar-3
  (eliminar-termino p1 0))

;; Resultado esperado:
;; 4x^5 - (3/2)x^2


;; EJEMPLOS DE sumar

;; Ejemplo 1:
;; p1 = 4x^5 - (3/2)x^2 + 7
;; p2 = -4x^5 + (1/2)x^2 + 2x
;; Resultado:
;; -x^2 + 2x + 7

(define ejemplo-sumar-1
  (sumar p1 p2))


;; Ejemplo 2:
;; p1 + p3
;; Resultado:
;; 4x^5 + 3x^4 - (3/2)x^2 - 5x + 7

(define ejemplo-sumar-2
  (sumar p1 p3))


;; Ejemplo 3:
;; p2 + p3
;; Resultado:
;; -4x^5 + 3x^4 + (1/2)x^2 - 3x

(define ejemplo-sumar-3
  (sumar p2 p3))
